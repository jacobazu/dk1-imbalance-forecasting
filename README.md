# DK1 imbalance forecasting (WIP)

Forecasts of the imbalance in DK1 (western Denmark), in R. For each quarter-hour it gives the probability that Energinet regulates up, down or not at all, and the spread between the imbalance price and the spot price. It forecasts the next 4 hours every 3 hours, and all of tomorrow once a day after the auction. It runs on GitHub Actions.

Dashboard: [jacobazu.github.io/dk1-imbalance-forecasting](https://jacobazu.github.io/dk1-imbalance-forecasting)

## Models

- Multinomial lasso, lambda chosen by BIC.
- XGBoost.
- Ensemble: the average of the two.
- Benchmark: how often each direction happened in that hour over the last 28 days.

There is one model per hour ahead. The spread forecast mixes the direction probabilities with recent spreads that had a similar spot price.

## Results

Backtest from 2 Nov 2025 to 10 Oct 2026 (343 days). Skill is how much lower the loss is than the benchmark. Value is what picking a side earns per MWh, with p against the benchmark.

| Horizon | Direction skill | Spread skill | Value of picking a side |
|---|---|---|---|
| 0-1 h | 4.9% | 5.7% | 12.5 €/MWh (p < 0.001) |
| 1-2 h | 2.7% | 3.0% | 4.3 €/MWh (p = 0.001) |
| 2-3 h | 2.2% | 2.8% | 3.1 €/MWh (p = 0.044) |
| 3-4 h | 2.0% | 2.6% | 2.6 €/MWh (p = 0.06) |
| Day-ahead | 1.5% | 2.3% | 1.1 €/MWh (p = 0.28) |

The forecast has real value in the first hours. For tomorrow it still beats the benchmark, but the value isn't significant.

## Why these tests

- Log loss and pinball loss: is the forecast better than the benchmark?
- Diebold-Mariano with Newey-West: is the difference real or luck? Good and bad days come in runs, so the variance has to allow for that. It also shows the ensemble beats both models, which is why I use it.
- Calibration chart: when it says 40%, does it happen about 40% of the time?
- Value of picking a side: a lower loss doesn't say if the forecast is worth anything. So each quarter I settle 1 MWh at the imbalance price or the spot price, depending on the sign of the expected spread. The intraday price isn't public, so the spot price stands in for it.

Tested and not used: Energinet's newer wind and solar forecasts. They didn't help, probably because the market already trades on them.

## License

The code in this repository is released under the [MIT License](LICENSE).

The forecasts are produced for research and portfolio purposes only and come
with no guarantee of accuracy. They are not trading or investment advice.

### Data

Contains data used pursuant to "Conditions for use of Danish public-sector data"
from the Energi Data Service portal (www.energidataservice.dk), published by
Energinet. The data is not covered by the MIT License, and this project
is not endorsed by Energinet.

Weather data by [Open-Meteo.com](https://open-meteo.com), licensed under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
