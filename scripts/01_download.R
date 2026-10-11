# downloading DK1 imbalance prices (15 min, from 4 march 2025) and open-meteo weather forecasts

library(dplyr)
library(lubridate)
source("R/get_eds.R")
source("R/weather.R")

start <- as_date("2025-03-04")
end   <- Sys.Date() + 1
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# imbalance prices, one month at a time
months <- seq(floor_date(start, "month"), end, by = "month")
imb <- bind_rows(lapply(months, \(m) {
  message("imbalance ", format(m, "%Y-%m"))
  Sys.sleep(1)
  get_eds("ImbalancePrice", format(max(m, start)), format(min(m %m+% months(1), end)))
})) |>
  mutate(time = ymd_hms(TimeUTC, tz = "UTC")) |>
  distinct(time, .keep_all = TRUE) |>
  arrange(time)

saveRDS(imb, "data/raw/imbalance.rds")

# weather forecasts made 48 hours ahead
weather <- get_weather(start - 60, end)
saveRDS(weather, "data/raw/weather.rds")
cat("weather:", format(min(weather$time)), "to", format(max(weather$time)), "\n")

# quick look: quarters, missing prices and how often each direction happens, per period
# (the price rule changed on 19 march 2025, the direction rule on 4 march 2026)
imb |>
  mutate(period = case_when(time < as_datetime("2025-03-19") ~ "1: to 19 mar 2025",
                            time < as_datetime("2026-03-04") ~ "2: to 4 mar 2026",
                            TRUE                             ~ "3: from 4 mar 2026")) |>
  group_by(period) |>
  summarise(from = min(time), to = max(time), quarters = n(),
            missing_price = sum(is.na(ImbalancePriceEUR)),
            up = mean(DominatingDirection == 1, na.rm = TRUE),
            none = mean(DominatingDirection == 0, na.rm = TRUE),
            down = mean(DominatingDirection == -1, na.rm = TRUE),
            mean_spread = mean(ImbalancePriceEUR - SpotPriceEUR, na.rm = TRUE)) |>
  print(width = Inf)
