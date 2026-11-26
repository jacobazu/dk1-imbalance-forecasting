# DK1 imbalance forecasting (WIP)

Forecasts of the imbalance in DK1, in R. For each quarter-hour it gives the chance that Energinet regulates up, down or not at all, and the spread between the imbalance price and the spot price.

Dashboard: [jacobazu.github.io/dk1-imbalance-forecasting](https://jacobazu.github.io/dk1-imbalance-forecasting)

Methods: [one page (PDF)](notes/method.pdf)

Models: multinomial lasso, XGBoost and their average. The benchmark is how often each direction happened in that hour over the last 28 days.

## Live first

Each forecast is saved before its quarter-hour starts, and the main page scores only those. The backtest has its own page.

## What the backtest shows

- The next hour is fairly predictable: 4.9% lower log loss than the benchmark, and picking a side with the spread forecast earns 12.5 €/MWh (p < 0.001). Imbalances tend to last, and the real-time state of the grid shows it.
- Tomorrow only a little: 1.5% lower log loss, and picking a side earns 1.1 €/MWh, which isn't significant (p = 0.28). If tomorrow's imbalance were easy to see, the market would already have adjusted for it.
- So the edge is in the next hour, which is traded on the intraday market. Intraday prices aren't public, so spot stands in for them and the real value can't be measured here.

## Not shown

Hours 3-4, tomorrow's forecast and a day-ahead to imbalance paper test didn't add enough. Since 26 Nov 2026 they still run and are saved, but aren't on the dashboard. [Why](notes/not-shown.md).

## Why these tests

- Diebold-Mariano with Newey-West: is the gain real or luck?
- Calibration: when it says 40%, does it happen about 40% of the time?
- Value of picking a side: is the forecast worth anything?

## License

Code: [MIT](LICENSE). Not trading advice.

Data from [Energi Data Service](https://www.energidataservice.dk) (Energinet), used under "Conditions for use of Danish public-sector data". Not covered by the MIT License or endorsed by Energinet. Weather from [Open-Meteo.com](https://open-meteo.com), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
