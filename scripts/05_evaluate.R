# evaluation of the direction and spread backtests, small csv files for the dashboard
# horizons: day-ahead (made 14:00 the day before) and intraday 0-1h, 1-2h, 2-3h, 3-4h ahead

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
source("R/direction.R")
source("R/spread.R")

rule_date <- as_date("2026-03-04")

da <- readRDS("data/processed/direction_forecasts.rds") |>
  transmute(horizon = "day-ahead", time, date, dir, model, down, none, up)
id <- readRDS("data/processed/intraday_forecasts.rds") |>
  mutate(date = as_date(with_tz(time, "Europe/Copenhagen"))) |>
  transmute(horizon = paste0(bucket - 1, "-", bucket, "h"), time, date, dir, model, down, none, up)

# same test days for all horizons
days <- unique(da$date)
days <- days[days %in% id$date]
fc <- bind_rows(da, id) |>
  filter(date %in% days) |>
  mutate(ll = log_loss(cbind(down, none, up), dir),
         bs = brier(cbind(down, none, up), dir),
         period = if_else(date >= rule_date, "new rule", "old rule"))
horizons <- c("0-1h", "1-2h", "2-3h", "3-4h", "day-ahead")

# scores per horizon and model, skill = how much lower the log loss is than the benchmark
scores <- bind_rows(fc, mutate(fc, period = "all")) |>
  group_by(horizon, period, model) |>
  summarise(days = n_distinct(date), log_loss = mean(ll), brier = mean(bs), .groups = "drop") |>
  group_by(horizon, period) |>
  mutate(skill = 1 - log_loss / log_loss[model == "clim"]) |>
  ungroup() |>
  arrange(match(horizon, horizons), period, log_loss)
print(scores |> filter(period == "all"), n = Inf)

# diebold-mariano on the daily average loss, each model vs the benchmark and the ensemble vs the rest.
# newey-west variance with one week of lags: daily loss differences are autocorrelated
dm_test <- function(d, lag = 7) {
  n <- length(d)
  u <- d - mean(d)
  v <- sum(u^2) / n
  for (k in seq_len(min(lag, n - 1))) v <- v + 2 * (1 - k / (lag + 1)) * sum(u[-(1:k)] * u[1:(n - k)]) / n
  stat <- mean(d) / sqrt(v / n) * sqrt((n - 1) / n)
  c(stat = stat, p = 2 * pt(-abs(stat), df = n - 1))
}
daily <- fc |>
  group_by(horizon, date, model) |>
  summarise(ll = mean(ll), .groups = "drop") |>
  pivot_wider(names_from = model, values_from = ll)
pairs <- list(c("ens", "clim"), c("mnl", "clim"), c("xgb", "clim"), c("ens", "mnl"), c("ens", "xgb"),
              c("ens", "persist"))
tests <- bind_rows(lapply(horizons, \(h) bind_rows(lapply(pairs, \(ab) {
  x <- daily[daily$horizon == h, ]
  if (!all(ab %in% names(x)) || all(is.na(x[[ab[2]]]))) return(NULL)
  r <- dm_test(x[[ab[1]]] - x[[ab[2]]])
  tibble(horizon = h, model_a = ab[1], model_b = ab[2], diff = mean(x[[ab[1]]] - x[[ab[2]]]),
         dm_stat = r[["stat"]], dm_p = r[["p"]])
}))))
print(tests, n = Inf)

# calibration of the ensemble: forecast probability in bins vs how often it happened
calibration <- fc |>
  filter(model == "ens") |>
  pivot_longer(c(down, none, up), names_to = "class", values_to = "p") |>
  mutate(hit = dir == classes[class], bin = floor(p * 10) / 10) |>
  group_by(horizon, class, bin) |>
  summarise(n = n(), forecast = mean(p), observed = mean(hit), .groups = "drop") |>
  filter(n >= 50)

# spread: pinball loss, coverage of the 80% band, and the value of picking a side with the expected
# spread (eur/mwh per quarter), for the day-ahead and the intraday forecasts
sp <- bind_rows(
  readRDS("data/processed/spread_forecasts.rds") |> mutate(horizon = "day-ahead"),
  readRDS("data/processed/intraday_spread_forecasts.rds") |> mutate(horizon = paste0(bucket - 1, "-", bucket, "h"))) |>
  filter(date %in% days) |>
  mutate(model = sub("^mix_", "", model))
Q  <- as.matrix(sp[, paste0("q", taus * 100)])
sp <- sp |>
  mutate(pinball = pinball(Q, spread), inside = spread >= q10 & spread <= q90,
         value = side_value(mean, spread), period = if_else(date >= rule_date, "new rule", "old rule"))
spread_scores <- bind_rows(sp, mutate(sp, period = "all")) |>
  group_by(horizon, period, model) |>
  summarise(pinball = mean(pinball), cover_80 = mean(inside), value = mean(value), .groups = "drop") |>
  group_by(horizon, period) |>
  mutate(skill = 1 - pinball / pinball[model == "clim"]) |>
  ungroup() |>
  arrange(match(horizon, horizons), period, pinball)
print(spread_scores |> filter(period == "all"), n = Inf)
spread_daily <- sp |>
  filter(model %in% c("ens", "clim")) |>
  group_by(horizon, date, model) |>
  summarise(pinball = mean(pinball), value = mean(value), .groups = "drop") |>
  pivot_wider(names_from = model, values_from = c(pinball, value))
spread_tests <- bind_rows(lapply(horizons, \(h) {
  x <- spread_daily[spread_daily$horizon == h, ]
  bind_rows(lapply(c("pinball", "value"), \(v) {
    d <- x[[paste0(v, "_ens")]] - x[[paste0(v, "_clim")]]
    r <- dm_test(d)
    tibble(horizon = h, metric = v, diff = mean(d), dm_stat = r[["stat"]], dm_p = r[["p"]])
  }))
}))
print(spread_tests)

dir.create("results", showWarnings = FALSE)
write_csv(scores, "results/direction_scores.csv")
write_csv(tests, "results/direction_tests.csv")
write_csv(calibration, "results/calibration.csv")
write_csv(spread_scores, "results/spread_scores.csv")
write_csv(spread_tests, "results/spread_tests.csv")
write_csv(tibble(name = c("from", "to", "days", "rule_change"),
                 value = c(format(min(fc$date)), format(max(fc$date)), length(days), format(rule_date))),
          "results/backtest_info.csv")
