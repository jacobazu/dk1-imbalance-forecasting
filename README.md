# DK1 imbalance forecasting

**Dashboard:** https://jacobazu.github.io/dk1-imbalance-forecasting

Forecasts of the imbalance in DK1 (western Denmark): the probability that Energinet regulates up, down
or not at all in each quarter-hour, and the spread between the imbalance price and the day-ahead price.
Every three hours for the next four hours, and once a day after the day-ahead auction for all of
tomorrow. The forecasts are saved before their quarter starts and never changed, and they are scored
once the imbalance data is published. It all runs on GitHub Actions.

## Models

- **Multinomial lasso**: one model for the three directions, lambda chosen by BIC.
- **XGBoost**: gradient boosted trees with a softmax over the three directions.
- **Ensemble**: the average of the two.
- **Benchmark**: how often each direction happened in that hour over the last 28 days. The backtest
  also has persistence (what followed the last known direction at that lead time).

Intraday there is one model per hour ahead. Inputs: the last known quarters (direction, size of the
imbalance, aFRR, spread), the calendar, the spot price of the quarter, wind and solar forecasts made 48
hours ahead (Open-Meteo), and for the first hour also the real-time state ten minutes before (aFRR
activation, wind and solar against Energinet's forecast, cross-border flows). The day-ahead model uses
the calendar, the spot price, the weather and the imbalances up to noon the day before. The models are
trained on the year before and refitted once a week.

The spread forecast mixes the direction probabilities with recent spreads: the 300 spreads of each
direction in the last 60 days with the closest spot price, since the size of the spread grows with the
spot price.

## Results

Backtest from 2 November 2025 to 10 October 2026 (343 days), all horizons on the same days. Skill is how
much lower the log loss is than the benchmark:

| Horizon | Ensemble | Lasso | XGBoost | Persistence |
|---|---|---|---|---|
| 0–1 h | 4.9% | 4.0% | 4.4% | 0.1% |
| 1–2 h | 2.7% | 2.1% | 2.1% | −0.5% |
| 2–3 h | 2.2% | 1.7% | 1.8% | −0.9% |
| 3–4 h | 2.0% | 1.5% | 1.6% | −1.1% |
| Day-ahead | 1.5% | 1.2% | 0.6% | |

The ensemble beats the benchmark and both single models at every horizon (Diebold-Mariano, p < 0.01).
The gains are small: imbalances are the errors in everyone's plans, so most of them can't be seen
coming from public data. Energinet's updated wind and solar forecasts for the hour ahead didn't help
either (tested without look-ahead: only used from when their last part is published), probably because
the market already trades on them.

The spread forecast beats its benchmark at every horizon: the pinball loss is 5.7% lower for the next
hour and 2.3% lower for tomorrow (p < 0.001). Picking the side of the spread with it (settling 1 MWh
at the imbalance price instead of the spot price when the expected spread is positive, and the other
way round when it is negative) earns 12.5 €/MWh on average for the next hour, against about zero for
the benchmark and about 52 €/MWh if the direction were known in advance. For tomorrow it is 1.1 €/MWh,
which is not significant. This measures how useful the forecast is for a decision; it isn't a strategy
that could be run as is.

The spread method was chosen on the months before March 2026 and checked on the months after. The
current numbers and the live track record are on the dashboard.

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
