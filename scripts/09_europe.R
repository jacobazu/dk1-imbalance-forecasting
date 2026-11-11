# imbalance prices across europe from the entso-e transparency platform (regulation 543/2013, article 17.1.g),
# for the map on the dashboard. not a model input. each area is asked on its own, so one that fails is just missing.
# keeps the last 7 days in live/europe_imbalance.csv: area, the period's start (utc), its length, and the price.
# where a country has one price for being long and another for being short both are kept (category long / short)

library(httr2)
library(dplyr)
library(readr)
library(lubridate)

# eic codes of the imbalance areas. germany's price is the same in all four control areas, so the first that answers
areas <- read_csv("R/europe_areas.csv", show_col_types = FALSE)

to   <- floor_date(Sys.time(), "hour") + hours(1)
from <- to - hours(12)

parse_prices <- function(txt) {
  series <- regmatches(txt, gregexpr("(?s)<TimeSeries>.*?</TimeSeries>", txt, perl = TRUE))[[1]]
  bind_rows(lapply(series, \(s) {
    periods <- regmatches(s, gregexpr("(?s)<Period>.*?</Period>", s, perl = TRUE))[[1]]
    bind_rows(lapply(periods, \(p) {
      start <- ymd_hm(regmatches(p, regexpr("(?<=<start>)[^<]*", p, perl = TRUE)), tz = "UTC")
      res   <- regmatches(p, regexpr("(?<=<resolution>)[^<]*", p, perl = TRUE))
      mins  <- as.integer(sub("PT([0-9]+)M", "\\1", res))
      if (grepl("H", res)) mins <- 60L * as.integer(sub("PT([0-9]+)H", "\\1", res))
      pts <- regmatches(p, gregexpr("(?s)<Point>.*?</Point>", p, perl = TRUE))[[1]]
      if (!length(pts) || is.na(mins)) return(NULL)
      tag <- \(x, name) { v <- regmatches(x, regexpr(paste0("(?<=<", name, ">)[^<]*"), x, perl = TRUE)); if (length(v)) v else NA_character_ }
      tibble(time = start + minutes(mins * (as.integer(sapply(pts, tag, "position")) - 1)),
             minutes = mins,
             price = as.numeric(sapply(pts, tag, "imbalance_Price.amount")),
             # a04: the area had too much (long), a05: too little (short); missing for one-price areas
             category = recode(sapply(pts, tag, "imbalance_Price.category"), A04 = "long", A05 = "short", A06 = "both"))
    }))
  }))
}

fetch <- function(eic) {
  resp <- request("https://web-api.tp.entsoe.eu/api") |>
    req_url_query(securityToken = Sys.getenv("ENTSOE_TOKEN"), documentType = "A85", controlArea_Domain = eic,
                  periodStart = format(from, "%Y%m%d%H%M", tz = "UTC"), periodEnd = format(to, "%Y%m%d%H%M", tz = "UTC")) |>
    req_timeout(30) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  if (resp_status(resp) != 200) return(NULL)
  parse_prices(resp_body_string(resp))
}

if (Sys.getenv("ENTSOE_TOKEN") == "") stop("ENTSOE_TOKEN is not set")
new <- bind_rows(lapply(seq_len(nrow(areas)), \(i) {
  x <- NULL
  for (eic in strsplit(areas$eic[i], "|", fixed = TRUE)[[1]]) {   # several codes: the first that answers
    x <- tryCatch(fetch(eic), error = \(e) NULL)
    if (!is.null(x) && nrow(x)) break
  }
  if (is.null(x) || !nrow(x)) { message(areas$area[i], ": nothing"); return(NULL) }
  message(areas$area[i], ": ", nrow(x), " prices, latest ", format(max(x$time)))
  mutate(x, area = areas$area[i], .before = 1)
})) |>
  filter(!is.na(price)) |>
  mutate(category = coalesce(category, "both"))

file <- "live/europe_imbalance.csv"
old  <- if (file.exists(file)) read_csv(file, show_col_types = FALSE, col_types = cols(time = col_datetime(), .default = col_guess())) else NULL
all  <- bind_rows(new, old) |>
  distinct(area, time, category, .keep_all = TRUE) |>   # the newest copy first, so a corrected price wins
  filter(time >= Sys.time() - days(7)) |>
  arrange(area, time, category)
write_csv(mutate(all, time = format(time, "%Y-%m-%dT%H:%M:%SZ")), file)
cat(n_distinct(new$area), "of", nrow(areas), "areas answered\n")
