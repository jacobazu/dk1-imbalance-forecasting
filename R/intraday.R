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
