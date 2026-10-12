# spread = imbalance price - spot price
# forecast: a mixture of recent spreads, weighted by the direction probabilities. for up and down,
# the spreads of the same direction in the last 60 days with the closest spot price (the size of the
# spread grows with the spot price), and 0 for no regulation

taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
k_near <- 300

# for each spot price in s, the k spreads in pool (spot, spread) with the closest spot price
near_spot <- function(pool, s, k = k_near) {
  pool <- pool[order(pool$spot), ]
  k <- min(k, nrow(pool))
  lapply(s, \(x) {
    lo <- max(1, min(findInterval(x, pool$spot) - k %/% 2, nrow(pool) - k + 1))
    pool$spread[lo:(lo + k - 1)]
  })
}

# quantiles and mean for each row of P (columns down, none, up)
# down / up = lists with the spreads to use for each row
mixture_quantiles <- function(P, down, up) {
  Q <- t(sapply(seq_len(nrow(P)), \(i) {
    d <- down[[i]]; u <- up[[i]]
    v <- c(d, 0, u)
    w <- c(rep(P[i, "down"] / length(d), length(d)), P[i, "none"], rep(P[i, "up"] / length(u), length(u)))
    o <- order(v)
    c(v[o][pmin(findInterval(taus - 1e-9, cumsum(w[o])) + 1, length(v))],
      P[i, "down"] * mean(d) + P[i, "up"] * mean(u))
  }))
  colnames(Q) <- c(paste0("q", taus * 100), "mean")
  Q
}

# the spreads known at a time, per direction: the last 60 days
spread_pools <- function(known, until, days = 60) {
  k <- known[!is.na(known$dir) & !is.na(known$spread) & known$time < until & known$time >= until - days(days), ]
  list(down = k[k$dir == -1, c("spot", "spread")], up = k[k$dir == 1, c("spot", "spread")])
}

# mixture for test rows with spot prices s
spread_forecast <- function(P, s, pools) {
  mixture_quantiles(P, near_spot(pools$down, s), near_spot(pools$up, s))
}

# benchmark: quantiles and mean of all spreads in the same hour over the last 28 days
clim_quantiles <- function(train, test, days = 28) {
  last <- max(train$date)
  recent <- train[train$date > last - days, ]
  by_hour <- lapply(0:23, \(h) {
    x <- recent$spread[recent$hour == h]
    c(quantile(x, taus, type = 1, names = FALSE), mean(x))
  })
  Q <- do.call(rbind, by_hour[test$hour + 1])
  colnames(Q) <- c(paste0("q", taus * 100), "mean")
  Q
}

# pinball loss averaged over the quantiles (lower is better)
pinball <- function(Q, y) {
  rowMeans(sapply(seq_along(taus), \(i) {
    u <- y - Q[, i]
    pmax(taus[i] * u, (taus[i] - 1) * u)
  }))
}

# value of the expected spread for picking a side: settle 1 mwh at the imbalance price instead of the
# spot price when the expected spread is positive, the other way round when it is negative (eur/mwh)
side_value <- function(expected, spread) sign(expected) * spread
