# spread = imbalance price - spot price
# forecast: mixture of the recent spreads in up and down quarters, weighted by the direction
# probabilities, plus 0 for no regulation

taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

# quantiles and mean for each row of P (columns down, none, up)
# v_down / v_up = recent spreads in down / up quarters
mixture_quantiles <- function(P, v_down, v_up) {
  grid <- sort(unique(c(v_down, 0, v_up)))
  Fd <- ecdf(v_down)(grid)
  Fu <- ecdf(v_up)(grid)
  F0 <- as.numeric(grid >= 0)
  Q <- t(apply(P, 1, \(p) {
    cdf <- p[["down"]] * Fd + p[["none"]] * F0 + p[["up"]] * Fu
    grid[pmin(findInterval(taus, cdf, left.open = TRUE) + 1, length(grid))]
  }))
  colnames(Q) <- paste0("q", taus * 100)
  cbind(Q, mean = P[, "down"] * mean(v_down) + P[, "up"] * mean(v_up))
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
