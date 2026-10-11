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

# diebold-mariano on the daily average log loss, each model vs the benchmark and the ensemble vs the rest
dm_test <- function(d) {
  n <- length(d)
  stat <- mean(d) / sqrt(var(d) / n) * sqrt((n - 1) / n)
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

# spread (day-ahead): pinball loss, error of the median and coverage of the 80% band
sp <- readRDS("data/processed/spread_forecasts.rds") |> filter(date %in% days)
Q  <- as.matrix(sp[, paste0("q", taus * 100)])
spread_scores <- sp |>
  mutate(pinball = pinball(Q, spread), abs_err = abs(spread - q50),
         inside = spread >= q10 & spread <= q90,
         period = if_else(date >= rule_date, "new rule", "old rule")) |>
  (\(x) bind_rows(x, mutate(x, period = "all")))() |>
  group_by(period, model) |>
  summarise(pinball = mean(pinball), mae_median = mean(abs_err), cover_80 = mean(inside),
            .groups = "drop")
print(spread_scores, n = Inf)

dir.create("results", showWarnings = FALSE)
write_csv(scores, "results/direction_scores.csv")
write_csv(tests, "results/direction_tests.csv")
write_csv(calibration, "results/calibration.csv")
write_csv(spread_scores, "results/spread_scores.csv")
write_csv(tibble(name = c("from", "to", "days", "rule_change"),
                 value = c(format(min(fc$date)), format(max(fc$date)), length(days), format(rule_date))),
          "results/backtest_info.csv")
