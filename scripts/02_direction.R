# direction backtest: forecast all quarters of day D at 14:00 on D-1, trained on the year before
# benchmark (last 28 days by hour), multinomial lasso, xgboost and their average

library(dplyr)
library(tidyr)
library(lubridate)
library(parallel)
library(xgboost)
source("R/holidays.R")
source("R/features.R")
source("R/direction.R")

imb <- readRDS("data/raw/imbalance.rds")
q <- make_quarters(imb, transmute(imb, time, spot = SpotPriceEUR), readRDS("data/raw/weather.rds"),
                   dk_holidays(2025:2027)$date) |>
  add_recent()
ok <- complete.cases(q[, feature_cols]) & !is.na(q$dir)

window <- 365
cores  <- as.integer(Sys.getenv("CORES", max(1, detectCores() - 1)))
full_days <- q |> filter(ok) |> count(date) |> filter(n >= 92) |> pull(date)
test_days <- full_days[full_days >= as_date("2025-11-01")]

t0 <- Sys.time()
out <- mclapply(test_days, \(D) tryCatch({
  train <- q[ok & q$date >= D - window & (q$date <= D - 2 | (q$date == D - 1 & q$hour < 12)), ]
  test  <- q[ok & q$date == D, ]
  P <- list(clim = clim_probs(train, test),
            mnl  = mnl_probs(train, test),
            xgb  = xgb_probs(train, test, as.integer(D)))
  P$ens <- (P$mnl + P$xgb) / 2
  bind_rows(lapply(names(P), \(m) tibble(time = test$time, date = D, hour = test$hour, dir = test$dir,
                                         model = m, as_tibble(P[[m]]))))
}, error = \(e) NULL), mc.cores = cores)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

fc <- bind_rows(out)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(fc, "data/processed/direction_forecasts.rds")

fc |>
  mutate(ll = log_loss(cbind(down, none, up), dir), bs = brier(cbind(down, none, up), dir),
         period = if_else(date >= as_date("2026-03-04"), "new rule", "old rule")) |>
  group_by(period, model) |>
  summarise(days = n_distinct(date), log_loss = mean(ll), brier = mean(bs), .groups = "drop") |>
  print(n = Inf)
