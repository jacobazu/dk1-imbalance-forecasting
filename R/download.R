# downloads for the live run

library(dplyr)
library(tidyr)
library(lubridate)

# small chunks per request
chunks <- function(from, to, by) {
  s <- seq(as_date(from), as_date(to), by = by)
  tibble(from = s, to = pmin(lead(s, default = as_date(to) + 1), as_date(to) + 1))
}

get_imbalance <- function(from, to) {
  ch <- chunks(from, to, "month")
  bind_rows(lapply(seq_len(nrow(ch)), \(i) {
    Sys.sleep(0.5)
    get_eds("ImbalancePrice", format(ch$from[i]), format(ch$to[i]))
  })) |>
    mutate(time = ymd_hms(TimeUTC, tz = "UTC")) |>
    distinct(time, .keep_all = TRUE) |>
    arrange(time)
}

# power system right now, minute data as 5-minute averages
rt_cols <- c("Minutes1UTC", "aFRR_ActivatedDK1", "OffshoreWindPower", "OnshoreWindPower", "SolarPower",
             "Exchange_DK1_DE", "Exchange_DK1_NO", "Exchange_DK1_SE", "Exchange_DK1_NL", "Exchange_DK1_DK2")
get_realtime <- function(from, to) {
  ch <- chunks(from, to, "week")
  bind_rows(lapply(seq_len(nrow(ch)), \(i) {
    Sys.sleep(0.5)
    get_eds("PowerSystemRightNow", format(ch$from[i]), format(ch$to[i]), area = NULL, columns = rt_cols)
  })) |>
    mutate(time = floor_date(ymd_hms(Minutes1UTC, tz = "UTC"), "5 min")) |>
    select(-Minutes1UTC) |>
    group_by(time) |>
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), n_min = n(), .groups = "drop") |>
    mutate(across(-c(time, n_min), \(x) if_else(is.nan(x), NA_real_, x)))
}

# energinet's wind and solar forecasts, dk1 and dk2
get_energinet_forecasts <- function(from, to) {
  ch <- chunks(from, to, "month")
  bind_rows(lapply(seq_len(nrow(ch)), \(i) {
    Sys.sleep(0.5)
    get_eds("Forecasts_Hour", format(ch$from[i]), format(ch$to[i]), area = c("DK1", "DK2"))
  }))
}

# day-ahead prices per quarter
get_spot <- function(from, to) {
  get_eds("DayAheadPrices", format(as_date(from)), format(as_date(to) + 1)) |>
    transmute(time = ymd_hms(TimeUTC, tz = "UTC"), spot = DayAheadPriceEUR) |>
    distinct(time, .keep_all = TRUE) |>
    arrange(time)
}

# everything for one run, days back: 62 to forecast, 400 to refit
get_live_data <- function(now, days) {
  today <- as_date(with_tz(now, "Europe/Copenhagen"))
  imb <- get_imbalance(today - days, today + 1)
  spot_new <- get_spot(today - 1, today + 1)
  list(imb     = imb,
       spot    = bind_rows(spot_new, transmute(imb, time, spot = SpotPriceEUR)) |>
                   distinct(time, .keep_all = TRUE) |> arrange(time),
       rt      = get_realtime(today - if (days > 62) days else 1, today + 1),
       fc      = get_energinet_forecasts(today - min(days, 400), today + 2),
       weather = get_weather(today - days, today + 1))
}
