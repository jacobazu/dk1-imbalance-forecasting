# DK1 imbalance forecasting (WIP)

Forecasts of the imbalance in DK1, in R. For each quarter-hour it gives the chance that Energinet regulates up, down or not at all, and the spread between the imbalance price and the spot price.

Dashboard: [jacobazu.github.io/dk1-imbalance-forecasting](https://jacobazu.github.io/dk1-imbalance-forecasting)

- Models: multinomial lasso, XGBoost and their average. The benchmark is how often each direction happened in that hour over the last 28 days.
- Next hour: 4.9% lower log loss than the benchmark. Picking a side with the spread forecast earns 12.5 €/MWh.
- Tomorrow: 1.5% lower log loss, but the value isn't significant.

## Why these tests

- Diebold-Mariano with Newey-West: is the gain real or luck?
- Calibration: when it says 40%, does it happen about 40% of the time?
- Value of picking a side: is the forecast worth anything?

## License

Code: [MIT](LICENSE). Not trading advice.

Data from [Energi Data Service](https://www.energidataservice.dk) (Energinet), used under "Conditions for use of Danish public-sector data". Not covered by the MIT License or endorsed by Energinet. Weather from [Open-Meteo.com](https://open-meteo.com), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
