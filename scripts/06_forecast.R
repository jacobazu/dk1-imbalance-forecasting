# latest forecasts: next 4 hours (every hour) and tomorrow (after the day-ahead auction)
# NOW = time to forecast from (default the current time), DA_DATE = day for the day-ahead forecast

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(xgboost)
source("R/holidays.R")
source("R/features.R")
source("R/direction.R")
source("R/intraday.R")
source("R/spread.R")
source("R/live.R")

now     <- if (nzchar(Sys.getenv("NOW"))) ymd_hm(Sys.getenv("NOW"), tz = "UTC") else Sys.time()
now_dk  <- with_tz(now, "Europe/Copenhagen")
da_date <- if (nzchar(Sys.getenv("DA_DATE"))) as_date(Sys.getenv("DA_DATE")) else as_date(now_dk) + 1

imb <- readRDS("data/raw/imbalance.rds") |> filter(time < now - minutes(30))   # only what was published
rt  <- readRDS("data/raw/realtime.rds")  |> filter(time < now - minutes(10))
q   <- make_quarters(imb, readRDS("data/raw/spot.rds"), readRDS("data/raw/weather.rds"),
                     dk_holidays(2025:2027)$date)

dir.create("forecasts", showWarnings = FALSE)
issue <- floor_date(now, "hour")
fi <- forecast_intraday(q, imb, rt, readRDS("data/raw/energinet_forecasts.rds"), issue)
write_csv(mutate(fi, across(where(is.numeric), \(x) round(x, 4))), "forecasts/intraday_latest.csv")
cat("intraday forecast made at", format(with_tz(issue, "Europe/Copenhagen")), "danish time\n")

if (any(q$date == da_date)) {
  fd <- forecast_dayahead(q, da_date)
  write_csv(mutate(fd, date = da_date, across(where(is.numeric), \(x) round(x, 4))),
            "forecasts/dayahead_latest.csv")
  cat("day-ahead forecast for", format(da_date), "\n")
} else {
  message("no spot prices for ", da_date, " yet, no day-ahead forecast")
}
