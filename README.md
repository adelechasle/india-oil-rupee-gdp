# Oil shocks, the rupee and India's GDP growth

**Question:** does a depreciation of the Indian rupee driven by oil price shocks reduce India's GDP growth?

India imports most of its oil and pays for it in US dollars. When oil prices rise, the import bill grows, demand for dollars increases and the rupee can weaken. This makes all dollar-priced imports more expensive, which raises costs for firms and households. This project tests whether this channel shows up in India's growth data.

*Originally written as the final project for an econometrics course at Sciences Po (2024-25), then corrected and extended.*

## Method

- **Data:** quarterly time series for India. The main sample covers 1996–2019 and 2022–2023 (100 quarters).
- **Why the pandemic years are excluded:** the lockdown crash and rebound (about −25% and then +20% quarterly GDP growth in 2020) dominate the estimation and cannot be absorbed by a simple year dummy. The full sample is reported as a robustness check.
- **Problem:** the exchange rate is endogenous. A weak economy can cause depreciation, not only the reverse, so OLS may be biased.
- **Approach:** Two-Stage Least Squares (2SLS). The quarterly change in the Brent crude price instruments the change in the USD/INR rate.
- **Controls:** lagged GDP growth, CPI inflation, seasonal dummies, and a dummy for the 2008-09 crisis.
- **Checks:**
  - first-stage F-statistic and reduced form
  - Wu-Hausman endogeneity test
  - bootstrap confidence interval and permutation test (using the Frisch-Waugh-Lovell theorem)
  - US growth as an extra control
  - placebo test using next quarter's exchange rate
  - full sample including 2020-21

## Results

**Main sample (excluding 2020-21, 100 quarters):**

| | Estimate |
|---|---|
| 2SLS coefficient on Δ USD/INR | **−0.25** (SE 0.12, p = 0.04) |
| OLS coefficient on Δ USD/INR | −0.10 |
| First-stage coefficient on Δ Brent | −0.059 (F = 11.1) |
| Reduced form (Δ Brent on GDP growth) | +0.015 |
| Wu-Hausman p-value | 0.16 |
| Bootstrap 95% CI | [−0.55, 0.03] |
| Permutation p-value | 0.02 |

A 1 percentage point depreciation of the rupee, as predicted by oil price movements, is associated with about 0.25 points lower quarterly GDP growth. The effect is modest and only weakly significant: the conventional p-value (0.04) and the permutation test (0.02) are below 5%, but the bootstrap confidence interval includes zero. The result should therefore be read as suggestive rather than robust.

**How to read the sign.** The first stage is negative: a 10% rise in oil prices goes with a rupee *appreciation* of about 0.6%, and the reduced form shows it also goes with about 0.15 points *higher* GDP growth. The depreciations predicted by the instrument therefore occur when oil prices fall, and these are quarters of slightly weaker growth. This pattern fits the global cycle (strong world demand raises oil prices, weakens the dollar and lifts Indian growth) better than an oil supply shock transmitted through the rupee. See Limitation 1.

Wu-Hausman does not reject exogeneity (p = 0.16), so in this sample I cannot show that OLS is biased. The 2SLS and OLS estimates should therefore be read together.

**Sensitivity to the pandemic:** including 2020-21 (with a 2020 dummy), the coefficient rises to −1.15 (SE 0.35) and Wu-Hausman rejects exogeneity (p = 0.003). The difference comes almost entirely from the 2020 crash and rebound, when GDP and oil prices fell and recovered together. This shows how sensitive IV estimates can be to a few extreme observations.

## Limitations (and what I would do next)

1. **Sign of the first stage.** The mechanism predicts that higher oil prices weaken the rupee. In the data, oil price increases tend to coincide with rupee *appreciation*. A likely explanation is the global cycle: when the world economy is strong, oil prices rise, the dollar weakens and emerging-market currencies strengthen. The instrument may therefore capture global demand as well as oil shocks. *Next step:* control for a broad dollar index, or use oil supply shocks (e.g. Kilian 2009; Baumeister & Hamilton 2019) as the instrument.
2. **Exclusion restriction.** The method assumes oil prices affect Indian GDP only through the exchange rate. Oil very likely also affects growth directly, through production costs and global demand. Controlling for CPI is also debatable, since inflation is partly an outcome of the depreciation.
3. **Instrument strength.** A first-stage F of 11 is only just above the usual threshold of 10. Recent work suggests standard t-tests can be unreliable at this level, so the p-value should be treated with caution.
4. **Inference.** The bootstrap keeps the first stage fixed, so it does not include first-stage uncertainty. With time-series data, errors may also be autocorrelated. *Next step:* Newey-West standard errors and a block bootstrap that re-estimates both stages.
5. **Data frequency.** US growth is annual and repeated over four quarters.
6. **One country.** A panel of oil-importing emerging economies would allow cross-country comparison and fixed effects.

## How to run

1. Download the data and save it in the same folder as the script, with these names:

| File | Series | Source |
|---|---|---|
| `Data GDP quarterly.csv` | NAEXKP01INQ652S | [FRED](https://fred.stlouisfed.org) |
| `Data global oil price (brent crude).csv` | POILBREUSDQ | FRED |
| `India Currency conversion.csv` | CCUSMA02INM618N | FRED |
| `india_cpi_index_monthly.csv` | INDCPIALLMINMEI (CPI index, 2015=100) | FRED |
| `US real GDP growth.xls` | Real GDP growth, United States | [IMF World Economic Outlook](https://www.imf.org/external/datamapper/NGDP_RPCH@WEO) |

2. In RStudio, open `Final_Project_Adele_Chasle.R` and go to *Session → Set Working Directory → To Source File Location*, then run the whole script.

**R packages:** tidyverse, readxl, lubridate, broom, infer, jtools, car, AER.

## Author

Adèle Chasle, Sciences Po (exchange at LSE)
