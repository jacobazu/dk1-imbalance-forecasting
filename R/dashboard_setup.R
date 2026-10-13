# shared by index.qmd and backtest.qmd

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(plotly)

Sys.setlocale("LC_TIME", "C")   # english dates

grey <- "#6b7280"
ink  <- "#1f2328"
dir_colors <- c(down = "#2563eb", none = "#cbd5e1", up = "#f97316")
dir_labels <- c(down = "Down-regulation", none = "No regulation", up = "Up-regulation")
model_labels <- c(ens = "Ensemble", mnl = "Multinomial lasso", xgb = "XGBoost",
                  clim = "Benchmark (last 28 days)")
horizon_labels <- c("0-1h" = "0–1 h", "1-2h" = "1–2 h", "2-3h" = "2–3 h", "3-4h" = "3–4 h",
                    "day-ahead" = "Day-ahead")

# all from forecasts/ and results/, nothing typed in
read_if <- function(f) if (file.exists(f)) read_csv(f, show_col_types = FALSE) else NULL
# latest forecasts from the logs
intra  <- read_if("forecasts/intraday_log.csv")
if (!is.null(intra)) intra <- filter(intra, issue == max(issue))
dayah  <- read_if("forecasts/dayahead_log.csv")
if (!is.null(dayah)) dayah <- filter(dayah, date == max(date))
live   <- read_if("results/live_scored.csv")
scores <- read_if("results/direction_scores.csv")
tests  <- read_if("results/direction_tests.csv")
calib  <- read_if("results/calibration.csv")
spread <- read_if("results/spread_scores.csv")
sptest <- read_if("results/spread_tests.csv")
info   <- read_if("results/backtest_info.csv")
info_v <- function(n) info$value[info$name == n]

pct   <- function(x, d = 0) paste0(format(round(100 * x, d), nsmall = d), "%")
eur   <- function(x) paste0(if (x >= 0) "+" else "−", format(round(abs(x)), big.mark = ","))
p_txt <- function(p) if (p < 0.001) "p < 0.001" else paste("p =", format(round(p, 3), nsmall = 3))
dk    <- function(t) with_tz(t, "Europe/Copenhagen")

skill_of <- function(h, m = "ens") scores$skill[scores$horizon == h & scores$model == m]
test_of  <- function(h, a = "ens", b = "clim") tests[tests$horizon == h & tests$model_a == a & tests$model_b == b, ]

chart_layout <- function(p, xtitle = "", ytitle = "") {
  p |>
    layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Segoe UI, Helvetica Neue, Arial, sans-serif", color = grey),
           xaxis = list(title = xtitle, showgrid = FALSE, zeroline = FALSE, color = grey, linecolor = "#e5ddd3"),
           yaxis = list(title = ytitle, color = grey, gridcolor = "#f1ebe4", zerolinecolor = "#cfc6bb"),
           legend = list(orientation = "h", x = 0, y = -0.18, font = list(color = ink)),
           hovermode = "x unified",
           hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#efe6dc", font = list(color = ink)),
           margin = list(t = 20)) |>
    config(displayModeBar = FALSE)
}

# stacked probabilities per quarter
prob_chart <- function(fc, xtitle, day = FALSE) {
  fc$x <- if (day) dk(fc$time) else fc$label
  p <- plot_ly(fc, x = ~x)
  for (cl in c("down", "none", "up")) {
    p <- p |> add_bars(y = fc[[cl]], name = dir_labels[[cl]], marker = list(color = dir_colors[[cl]]),
                       hovertemplate = paste0("%{y:.0%}<extra>", dir_labels[[cl]], "</extra>"))
  }
  p |> chart_layout(xtitle) |>
    layout(barmode = "stack", bargap = 0.15, yaxis = list(tickformat = ".0%", range = c(0, 1)),
           xaxis = if (day) list(type = "date", tickformat = "%H:%M", dtick = 2 * 3600 * 1000)
                   else list(type = "category"))
}

# expected spread with the 80% band
spread_chart <- function(fc, xtitle, day = FALSE) {
  fc$x <- if (day) dk(fc$time) else fc$label
  plot_ly(fc, x = ~x) |>
    add_lines(y = ~q10, line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") |>
    add_lines(y = ~q90, line = list(width = 0), fill = "tonexty", fillcolor = "rgba(249,115,22,0.15)",
              name = "80% band", hovertemplate = "%{y:.0f} €/MWh<extra>90%</extra>") |>
    add_lines(y = ~mean, name = "Expected spread", line = list(color = "#f97316", width = 3),
              hovertemplate = "%{y:.1f} €/MWh<extra>Expected</extra>") |>
    chart_layout(xtitle) |>
    layout(xaxis = if (day) list(type = "date", tickformat = "%H:%M", dtick = 2 * 3600 * 1000)
                   else list(type = "category"))
}

# latest intraday forecast (ensemble)
has_intra <- !is.null(intra)
if (has_intra) {
  ie <- intra |> filter(model == "ens") |> arrange(time) |>
    mutate(label = format(dk(time), "%H:%M"))
  made <- dk(ie$made_utc[1])
  h1 <- ie |> filter(bucket == 1) |> summarise(across(c(down, none, up, mean, q10, q90), mean))
  h1_dir <- names(which.max(unlist(h1[, c("down", "none", "up")])))
  ic <- intra |> filter(model == "clim", bucket == 1) |> summarise(across(c(down, none, up), mean))
}

# latest day-ahead forecast (ensemble)
has_da <- !is.null(dayah)
if (has_da) {
  de <- dayah |> filter(model == "ens") |> arrange(time) |>
    mutate(label = format(dk(time), "%H:%M"))
  da_day <- as_date(dayah$date[1])
  da_avg <- de |> summarise(across(c(down, none, up, mean), mean))
  da_dir_counts <- table(factor(c("down", "none", "up")[max.col(as.matrix(de[, c("down", "none", "up")]))],
                                levels = c("down", "none", "up")))
}

# live track record, ensemble vs benchmark
has_live <- !is.null(live) && nrow(live) > 0
if (has_live) {
  live_tab <- live |>
    group_by(horizon) |>
    summarise(quarters = n(), from = min(time), ens = mean(ll_ens), clim = mean(ll_clim), .groups = "drop") |>
    mutate(skill = 1 - ens / clim, h = factor(horizon_labels[horizon], levels = horizon_labels)) |>
    arrange(h)
  live_skill <- function(h) { r <- live_tab[live_tab$horizon == h, ]; if (nrow(r) == 0) "\u2013" else pct(r$skill, 1) }
  live_from  <- format(dk(min(live$time)), "%d %b %Y")
}

# accuracy text
has_eval <- !is.null(scores)
eval_txt <- if (has_eval) {
  s1 <- skill_of("0-1h"); s4 <- skill_of("3-4h"); sd <- skill_of("day-ahead")
  t1 <- test_of("0-1h"); td <- test_of("day-ahead")
  sprintf(paste("Log loss %s lower than the benchmark for the next hour (DM %s), %s three to four hours ahead",
                "and %s for tomorrow (DM %s). Imbalances are the errors in everyone's plans, so most are surprises."),
          pct(s1, 1), p_txt(t1$dm_p), pct(s4, 1), pct(sd, 1), p_txt(td$dm_p))
} else ""
period_txt <- if (!is.null(info)) sprintf("Backtest %s to %s, %s days, all horizons on the same days.",
                                          format(as_date(info_v("from")), "%d %b %Y"),
                                          format(as_date(info_v("to")), "%d %b %Y"), info_v("days")) else ""
has_spread <- !is.null(spread) && "horizon" %in% names(spread)
sp_of <- function(h, m, v = "pinball") spread[[v]][spread$horizon == h & spread$model == m]
sp_p  <- function(h, v) sptest$dm_p[sptest$horizon == h & sptest$metric == v]
eur1  <- function(x) paste0(if (x < 0) "−" else "", format(round(abs(x), 1), nsmall = 1), " €/MWh")
spread_txt <- if (has_spread) {
  sprintf(paste("Spread: pinball loss %s lower for the next hour and %s for tomorrow. Knowing the direction would",
                "make it %s lower, so most of the uncertainty is the direction."),
          pct(sp_of("0-1h", "ens", "skill"), 1), pct(sp_of("day-ahead", "ens", "skill"), 1),
          pct(sp_of("day-ahead", "perfect", "skill"), 0))
} else ""
value_txt <- if (has_spread) {
  sprintf(paste("Picking a side with the expected spread (1 MWh at the imbalance price or at spot) earns %s for",
                "the next hour (DM %s) and %s for tomorrow (DM %s). The benchmark earns %s and %s, knowing the",
                "direction %s. Spot stands in for the intraday price, which isn't public."),
          eur1(sp_of("0-1h", "ens", "value")), p_txt(sp_p("0-1h", "value")),
          eur1(sp_of("day-ahead", "ens", "value")), p_txt(sp_p("day-ahead", "value")),
          eur1(sp_of("0-1h", "clim", "value")), eur1(sp_of("day-ahead", "clim", "value")),
          eur1(sp_of("0-1h", "perfect", "value")))
} else ""
