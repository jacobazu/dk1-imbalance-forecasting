# Not shown (since 26 Nov 2026)

These still run every day and their forecasts are saved and scored as before, in the same files. They didn't add
enough, so they are off the dashboard.

**Day-ahead to imbalance** (`scripts/08_da_imbalance.R`, `forecasts/da_imbalance_log.csv`,
`results/da_imbalance_*.csv`). Failed the rule set before the backtest: +37 € a day with 1 MW, p = 0.13. Live
paper test 17 Oct to 25 Nov 2026: 40 days, +3,323 € in total (+83 € a day), too few days to tell from luck.

**Tomorrow's forecast** (`forecasts/dayahead_log.csv`). 1.5% lower log loss than the benchmark in the backtest, and
picking a side earned 1.1 €/MWh (p = 0.28).

**Hours 3-4** (`forecasts/intraday_log.csv`, buckets 3 and 4). About 2% lower log loss, close to tomorrow's
forecast, and 2.6 €/MWh (p = 0.06).

Live scores for all of them stay in `results/live_scored.csv`.

## Day-ahead to imbalance, the test

A friend of mine pointed out that a day-ahead position without physical power is settled at the imbalance price, and I took it from there. Buying a quarter on day-ahead then earns the imbalance price minus spot, and selling it earns spot minus the imbalance price. So the question is whether the spread can be forecast well enough before the noon auction to be worth a position.

- Every morning between 10:30 and 11:55 Danish time (`scripts/08_da_imbalance.R`): imbalances up to 09:00, the [price forecast](https://github.com/jacobazu/dk1-price-forecasting) in place of the spot price, which isn't known yet, and weather made 48 hours ahead. Long 1 MW in each quarter where the expected spread is above the fees, short where it is below minus the fees. The positions are saved before the auction and scored once the imbalance prices are out.
- Fees: Nord Pool's day-ahead fee, 0.055 €/MWh ([fee schedule 2026](https://www.nordpoolgroup.com/4a7ad8/globalassets/trading-and-services/nord-pool-fee-schedule-2026-nordic-and-baltic-market.pdf)), and eSett's imbalance fee for Denmark, 0.133 €/MWh ([eSett, Oct 2026](https://www.esett.com/app/uploads/2026/08/Imbalance-Settlement-Agreement-Appendix-1-1.10.2026-31.10.2026.pdf)). The yearly and weekly fees are left out.
- The rule was set and committed before the backtest was run (`scripts/06_dayahead_to_imbalance.R`): worth trading only if the mean daily P&L is above 0 with p < 0.05 and it beats the benchmark forecast, always long and always short with p < 0.05 (Holm).
- Backtest, 2 Nov 2025 to 10 Oct 2026: +37 € a day with 1 MW (p = 0.13), and the gains over the alternatives aren't significant either. So by that rule it isn't worth trading.
- A test, not trading. Energinet expects a balance responsible party to aim to be in balance ([Energinet, 2021](https://forsyningstilsynet.dk/Media/638215639738153946/2021_09_17_bilag1.pdf)), so whether this may be done on purpose depends on the market rules and the party's agreement. Closing a position on the intraday market isn't modelled, because intraday prices aren't public.

