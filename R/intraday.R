# intraday forecasts: every hour on the hour, for the 16 quarters of the next 4 hours
# the last known quarter is the one that started 60 minutes before (published ~20 minutes after it ends)

library(dplyr)
library(tidyr)
library(lubridate)

# state of the system at each quarter, used as "last known" quarter
# q = output of make_quarters(), imb = the ImbalancePrice rows
make_state <- function(q, imb) {
  grid <- tibble(time = seq(min(q$time), max(q$time), by = "15 min"))
  x <- grid |>
    left_join(q |> select(time, dir, spread), by = "time") |>
    left_join(imb |> transmute(time, sd = SatisfiedDemand, afrr_up = aFRRUpMW,
                               afrr_down = aFRRDownMW), by = "time")
  roll <- function(v, k) as.numeric(stats::filter(v, rep(1 / k, k), sides = 1))
  x |>
    transmute(last_time  = time,
              last_down  = as.numeric(dir == -1), last_none = as.numeric(dir == 0),
              last_up    = as.numeric(dir == 1),
              up_1h      = roll(dir == 1, 4),  down_1h = roll(dir == -1, 4),
              up_4h      = roll(dir == 1, 16), down_4h = roll(dir == -1, 16),
              sd_last    = asinh(sd / 100), sd_1h = asinh(roll(sd, 4) / 100),
              afrr_up_1h = roll(afrr_up, 4) / 10, afrr_down_1h = roll(afrr_down, 4) / 10,
              spread_last = asinh(spread / 50), spread_1h = asinh(roll(abs(spread), 4) / 50))
}
state_cols <- c("last_down", "last_up", "up_1h", "down_1h", "up_4h", "down_4h", "sd_last", "sd_1h",
                "afrr_up_1h", "afrr_down_1h", "spread_last", "spread_1h")

# one row per issue hour and target quarter: lead = quarters after the last known one (4 to 19),
# bucket = hours ahead of the issue time (1 = the first hour)
make_issues <- function(q, state, issue_times) {
  crossing(issue = issue_times, k = 0:15) |>
    mutate(time = issue + minutes(15 * k), last_time = issue - minutes(60), lead = k + 4,
           bucket = k %/% 4 + 1, lead_in_hour = k %% 4) |>
    select(-k) |>
    inner_join(q, by = "time") |>
    inner_join(state, by = "last_time") |>
    mutate(down_x_lead = last_down * lead_in_hour, up_x_lead = last_up * lead_in_hour)
}
intraday_cols <- c(state_cols, "lead_in_hour", "down_x_lead", "up_x_lead")

# persistence benchmark: how often each direction followed the last known one at that lead
persist_probs <- function(train, test) {
  last_dir <- \(d) case_when(d$last_down == 1 ~ -1, d$last_up == 1 ~ 1, TRUE ~ 0)
  tab <- train |>
    mutate(from = last_dir(train)) |>
    count(lead, from, dir) |>
    complete(lead, from = classes, dir = classes, fill = list(n = 0)) |>
    mutate(n = n + 0.5) |>
    group_by(lead, from) |>
    mutate(p = n / sum(n)) |>
    ungroup() |>
    select(lead, from, dir, p) |>
    pivot_wider(names_from = dir, values_from = p)
  key <- paste(test$lead, last_dir(test))
  P <- as.matrix(tab[match(key, paste(tab$lead, tab$from)), as.character(classes)])
  colnames(P) <- names(classes)
  P
}

# real-time state at each issue time T, from power system right now (5-minute averages):
# everything up to 10 minutes before T. forecast errors = actual wind/solar in denmark minus
# energinet's day-ahead forecast (dk1 + dk2)
make_realtime <- function(rt, fc, issue_times) {
  da <- fc |>
    transmute(hour = ymd_hms(HourUTC, tz = "UTC"), type = ForecastType, da = ForecastDayAhead) |>
    group_by(hour, type) |>
    summarise(da = sum(da), .groups = "drop") |>
    pivot_wider(names_from = type, values_from = da)
  x <- rt |>
    mutate(hour = floor_date(time, "hour")) |>
    left_join(da, by = "hour") |>
    transmute(time, afrr = aFRR_ActivatedDK1,
              wind_err  = OffshoreWindPower + OnshoreWindPower - `Offshore Wind` - `Onshore Wind`,
              solar_err = SolarPower - Solar,
              flow_de = Exchange_DK1_DE, flow_no = Exchange_DK1_NO, flow_se = Exchange_DK1_SE,
              flow_nl = Exchange_DK1_NL, flow_dk2 = Exchange_DK1_DK2) |>
    arrange(time)
  # mean over the k 5-minute values up to 10 minutes before each issue time
  win <- function(v, k) {
    idx <- findInterval(as.numeric(issue_times - minutes(10)), as.numeric(x$time))
    cs <- c(0, cumsum(ifelse(is.na(v), 0, v))); cn <- c(0, cumsum(!is.na(v)))
    lo <- pmax(idx - k, 0)
    out <- (cs[idx + 1] - cs[lo + 1]) / (cn[idx + 1] - cn[lo + 1])
    out[!is.finite(out)] <- NA
    out
  }
  tibble(issue     = issue_times,
         afrr_15   = asinh(win(x$afrr, 3) / 50),  afrr_60 = asinh(win(x$afrr, 12) / 50),
         wind_err  = win(x$wind_err, 12) / 1000,  solar_err = win(x$solar_err, 12) / 1000,
         flow_de   = win(x$flow_de, 12) / 1000,   flow_no = win(x$flow_no, 12) / 1000,
         flow_se   = win(x$flow_se, 12) / 1000,   flow_nl = win(x$flow_nl, 12) / 1000,
         flow_dk2  = win(x$flow_dk2, 12) / 1000)
}
realtime_cols <- c("afrr_15", "afrr_60", "wind_err", "solar_err", "flow_de", "flow_no", "flow_se",
                   "flow_nl", "flow_dk2")

# how much energinet's newer dk1 wind/solar forecast for the target hour differs from the day-ahead
# forecast. the hourly 5-hour forecast is the average of 5-minute forecasts, the last one made 4h05
# before the end of the hour, so it is only used from 4 hours before the hour starts (in the live api
# it shows up about 5 hours before). no newer forecast (or none at all) = no revision
# rows = make_issues() output, needs issue and time
add_revisions <- function(rows, fc) {
  rev <- fc |>
    filter(PriceArea == "DK1") |>
    transmute(fc_hour = ymd_hms(HourUTC, tz = "UTC"),
              type = if_else(ForecastType == "Solar", "solar_rev", "wind_rev"),
              rev  = coalesce(Forecast5Hour - ForecastDayAhead, 0)) |>
    group_by(fc_hour, type) |>
    summarise(rev = sum(rev) / 1000, .groups = "drop") |>
    pivot_wider(names_from = type, values_from = rev)
  rows |>
    mutate(fc_hour = floor_date(time, "hour")) |>
    left_join(rev, by = "fc_hour") |>
    mutate(known     = fc_hour - hours(4) <= issue - minutes(10),
           wind_rev  = if_else(known, coalesce(wind_rev, 0), 0),
           solar_rev = if_else(known, coalesce(solar_rev, 0), 0)) |>
    select(-fc_hour, -known)
}
revision_cols <- c("wind_rev", "solar_rev")
