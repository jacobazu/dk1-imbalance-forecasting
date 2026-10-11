# the live forecasts, same steps as the backtests (02, 03 and 04)
# models are fitted once a week (fit_models) and used for every forecast that week
# data = list(imb, spot, rt, fc, weather), see get_live_data() in R/download.R

base_cols <- c("hour", "qh", "wday", "holiday", "new_rule", "spot", "spot_rel", "spot_rank",
               "spot_diff", "spot_neg", "wpow", "solar")

live_quarters <- function(data) {
  make_quarters(data$imb, data$spot, data$weather, dk_holidays(2025:2030)$date)
}

intraday_rows <- function(data, q, issues) {
  make_issues(q, make_state(q, data$imb), issues) |>
    left_join(make_realtime(data$rt, data$fc, issues), by = "issue") |>
    mutate(date = as_date(with_tz(time, "Europe/Copenhagen")))
}

# fitted at `as_of` on the year before, like the weekly refits in the backtests
fit_models <- function(data, as_of, window = 365) {
  q <- live_quarters(data)
  day <- as_date(with_tz(as_of, "Europe/Copenhagen"))
  qd <- add_recent(q)
  train_da <- qd[complete.cases(qd[, feature_cols]) & !is.na(qd$dir) &
                   qd$date >= day - window & qd$date < day, ]

  d <- intraday_rows(data, q, seq(as_of - days(window) - hours(4), as_of, by = "hour"))
  intraday <- lapply(1:4, \(b) {
    cols  <- c(intraday_cols, if (b == 1) realtime_cols)
    train <- d[d$bucket == b & !is.na(d$dir) & d$time < as_of - minutes(60) &
                 complete.cases(d[, c(base_cols, cols)]), ]
    list(cols = cols, mnl = fit_mnl(train, extra = cols),
         xgb = fit_xgb(train, as.integer(day), cols = c(base_cols, cols, "lead")))
  })
  list(fitted_at = as_of,
       dayahead = list(mnl = fit_mnl(train_da), xgb = fit_xgb(train_da, as.integer(day))),
       intraday = intraday)
}

# recent spreads per direction, the last 60 days known at time `known_until`
spread_pools <- function(q, known_until) {
  k <- q[!is.na(q$dir) & !is.na(q$spread) & q$time < known_until & q$time >= known_until - days(60), ]
  list(down = k$spread[k$dir == -1], up = k$spread[k$dir == 1])
}

add_spread <- function(fc, pools) {
  Q <- mixture_quantiles(as.matrix(fc[, c("down", "none", "up")]), pools$down, pools$up)
  bind_cols(fc, as_tibble(Q[, c("q10", "q50", "q90", "mean")]))
}

# next 4 hours from issue time T (published data until T - 60 min, real-time until T - 10 min)
forecast_intraday <- function(models, data, issue) {
  q <- live_quarters(data)
  d <- intraday_rows(data, q, seq(issue - days(30), issue, by = "hour"))
  pools <- spread_pools(q, issue - minutes(60))
  bind_rows(lapply(1:4, \(b) {
    m     <- models$intraday[[b]]
    test  <- d[d$issue == issue & d$bucket == b & complete.cases(d[, c(base_cols, m$cols)]), ]
    known <- d[d$bucket == b & !is.na(d$dir) & d$time < issue - minutes(60), ]
    if (nrow(test) == 0) return(NULL)
    P <- list(clim = clim_probs(known, test), mnl = pred_mnl(m$mnl, test), xgb = pred_xgb(m$xgb, test))
    P$ens <- (P$mnl + P$xgb) / 2
    bind_rows(lapply(c("ens", "clim"), \(k) tibble(issue = issue, time = test$time, bucket = b,
                                                   model = k, as_tibble(P[[k]]))))
  })) |>
    group_by(model) |>
    group_modify(\(x, k) add_spread(x, pools)) |>
    ungroup()
}

# all quarters of day D, made at 14:00 the day before
forecast_dayahead <- function(models, data, D) {
  q <- add_recent(live_quarters(data))
  test  <- q[complete.cases(q[, feature_cols]) & q$date == D, ]
  known <- q[!is.na(q$dir) & q$date >= D - 30 & (q$date <= D - 2 | (q$date == D - 1 & q$hour < 12)), ]
  if (nrow(test) == 0) return(NULL)
  P <- list(clim = clim_probs(known, test), mnl = pred_mnl(models$dayahead$mnl, test),
            xgb = pred_xgb(models$dayahead$xgb, test))
  P$ens <- (P$mnl + P$xgb) / 2
  pools <- spread_pools(q, as_datetime(paste(D - 1, "12:00:00"), tz = "Europe/Copenhagen"))
  bind_rows(lapply(c("ens", "clim"), \(k) tibble(date = D, time = test$time, model = k,
                                                 as_tibble(P[[k]])))) |>
    group_by(model) |>
    group_modify(\(x, k) add_spread(x, pools)) |>
    ungroup()
}
