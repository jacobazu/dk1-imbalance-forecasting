# the forecasts the way they are made live, same steps as the backtests (02, 03 and 04)
# q must contain the quarters to forecast (spot known, dir still NA)

# recent spreads per direction, the last 60 days known at time `known_until`
spread_pools <- function(q, known_until) {
  k <- q[!is.na(q$dir) & !is.na(q$spread) & q$time < known_until & q$time >= known_until - days(60), ]
  list(down = k$spread[k$dir == -1], up = k$spread[k$dir == 1])
}

add_spread <- function(fc, pools) {
  P <- as.matrix(fc[, c("down", "none", "up")])
  Q <- mixture_quantiles(P, pools$down, pools$up)
  bind_cols(fc, as_tibble(Q[, c("q10", "q50", "q90", "mean")]))
}

# day-ahead: all quarters of day D, made at 14:00 the day before
forecast_dayahead <- function(q, D, window = 365) {
  q  <- add_recent(q)
  ok <- complete.cases(q[, feature_cols])
  train <- q[ok & !is.na(q$dir) & q$date >= D - window &
               (q$date <= D - 2 | (q$date == D - 1 & q$hour < 12)), ]
  test  <- q[ok & q$date == D, ]
  P <- list(clim = clim_probs(train, test),
            mnl  = mnl_probs(train, test),
            xgb  = xgb_probs(train, test, as.integer(D)))
  P$ens <- (P$mnl + P$xgb) / 2
  pools <- spread_pools(q, as_datetime(paste(D - 1, "12:00:00"), tz = "Europe/Copenhagen"))
  bind_rows(lapply(names(P), \(m) tibble(time = test$time, model = m, as_tibble(P[[m]])))) |>
    group_by(model) |>
    group_modify(\(x, k) add_spread(x, pools)) |>
    ungroup()
}

# intraday: at issue time T, the 16 quarters of the next 4 hours
forecast_intraday <- function(q, imb, rt, fc_energinet, issue, window = 365) {
  base <- c("hour", "qh", "wday", "holiday", "new_rule", "spot", "spot_rel", "spot_rank",
            "spot_diff", "spot_neg", "wpow", "solar")
  issues <- seq(issue - days(window) - hours(4), issue, by = "hour")
  d <- make_issues(q, make_state(q, imb), issues) |>
    left_join(make_realtime(rt, fc_energinet, issues), by = "issue") |>
    mutate(date = as_date(with_tz(time, "Europe/Copenhagen")))
  pools <- spread_pools(q, issue - minutes(60))

  bind_rows(lapply(1:4, \(b) {
    cols  <- c(intraday_cols, if (b == 1) realtime_cols)
    db    <- d[d$bucket == b & complete.cases(d[, c(base, cols)]), ]
    train <- db[!is.na(db$dir) & db$time < issue - minutes(60), ]
    test  <- db[db$issue == issue, ]
    if (nrow(test) == 0) return(NULL)
    P <- list(clim    = clim_probs(train, test),
              persist = persist_probs(train, test),
              mnl     = mnl_probs(train, test, extra = cols),
              xgb     = xgb_probs(train, test, as.integer(as_date(issue)), cols = c(base, cols, "lead")))
    P$ens <- (P$mnl + P$xgb) / 2
    bind_rows(lapply(names(P), \(m) tibble(issue = issue, time = test$time, bucket = b,
                                           model = m, as_tibble(P[[m]]))))
  })) |>
    group_by(model) |>
    group_modify(\(x, k) add_spread(x, pools)) |>
    ungroup()
}
