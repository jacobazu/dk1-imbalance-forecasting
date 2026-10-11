# spread backtest: imbalance price - spot price for each quarter of day D, made at 14:00 on D-1
# each direction model's probabilities mixed with the spreads of the last 60 days per direction,
# benchmark = all spreads in the same hour over the last 28 days
# perfect = the mixture with the true direction, how good it could get with a perfect direction

library(dplyr)
library(tidyr)
library(lubridate)
source("R/holidays.R")
source("R/features.R")
source("R/spread.R")

imb <- readRDS("data/raw/imbalance.rds")
q <- make_quarters(imb, transmute(imb, time, spot = SpotPriceEUR), readRDS("data/raw/weather.rds"),
                   dk_holidays(2025:2027)$date)
known <- q |> filter(!is.na(dir), !is.na(spread))
dirfc <- readRDS("data/processed/direction_forecasts.rds")

out <- lapply(sort(unique(dirfc$date)), \(D) {
  train <- known[known$date >= D - 60 & (known$date <= D - 2 | (known$date == D - 1 & known$hour < 12)), ]
  v_down <- train$spread[train$dir == -1]
  v_up   <- train$spread[train$dir == 1]
  test   <- known[known$date == D, ]

  res <- list(clim = clim_quantiles(train, test))
  for (m in unique(dirfc$model)) {
    P <- dirfc |> filter(date == D, model == m) |> select(time, down, none, up)
    P <- as.matrix(P[match(test$time, P$time), c("down", "none", "up")])
    res[[paste0("mix_", m)]] <- mixture_quantiles(P, v_down, v_up)
  }
  res$perfect <- mixture_quantiles(outer(test$dir, c(down = -1, none = 0, up = 1), "==") * 1, v_down, v_up)

  bind_rows(lapply(names(res), \(m) tibble(time = test$time, date = D, hour = test$hour,
                                           dir = test$dir, spread = test$spread, model = m,
                                           as_tibble(res[[m]]))))
})
fc <- bind_rows(out)
saveRDS(fc, "data/processed/spread_forecasts.rds")

Q <- as.matrix(fc[, paste0("q", taus * 100)])
fc |>
  mutate(pinball = pinball(Q, spread), abs_err_median = abs(spread - q50),
         inside = spread >= q10 & spread <= q90,
         period = if_else(date >= as_date("2026-03-04"), "new rule", "old rule")) |>
  group_by(period, model) |>
  summarise(pinball = mean(pinball), mae_median = mean(abs_err_median), cover_80 = mean(inside),
            .groups = "drop") |>
  print(n = Inf)
