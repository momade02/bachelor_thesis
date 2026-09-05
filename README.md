## Bachelor Thesis: Hedge Fund Replication in Times of Change

Bachelor thesis (B.Sc.) at Hochschule der Bayerischen Wirtschaft Munich.
Supervisor & examiner: Prof. Dr. Christian Schmitt.

Can a portfolio of cheap, liquid, exchange-traded risk factors reproduce hedge fund index
returns - including through the COVID-19 crash, the 2020/21 bull market, and the fastest
rate-hiking cycle in four decades?

Concise answer: it reproduces the shape well and the level poorly. Nine of ten cloned
strategies fall short of their index.

---

## Contents

- [Research questions](#research-questions)
- [Key findings](#key-findings)
- [Repository contents](#repository-contents)
- [Data](#data)
- [Method](#method)
- [Results](#results)
- [Running the code](#running-the-code)
- [Known issues and limitations](#known-issues-and-limitations)
- [References](#references)
- [Citation](#citation)

---

## Research questions

1. How are hedge fund factor exposures structured across investment strategies over
   2010–2023, given recent market developments?
2. Does the documented decline in returns attributable to manager skill (alpha) hold up,
   given that hedge funds are significantly exposed to systematic risk factors?
3. Can linear factor-based replication generate out-of-sample clones that perform
   comparably to their hedge fund index over an extended period?

## Key findings

**Hedge fund returns are largely systematic.** Across ten strategies, nine liquid risk
factors explain an average of 68% of return variance (R² up to 0.85 for Long/Short
Equities). Nine of ten strategies carry a highly significant S&P 500 beta, which is awkward for
investors buying hedge funds as portfolio diversifiers.

**Alpha declined, then reversed sharply.** Rolling 60-month intercepts fall steadily from
2014 to early 2020, consistent with Cherian, Kon & Li (2020). From mid-2020 to mid-2021
every strategy except Fixed Income at least doubled its alpha, and most held those levels
through September 2023. This supports Fung & Hsieh's (2004) hypothesis that alpha rises in
bull markets.

**Predicted returns track well; investable clones do not.** Out-of-sample predicted returns
(intercept included) correlate 0.70+ with realised returns for nine of ten strategies, and
no paired t-test rejects equality of means. Once the intercept is dropped due to manager
skill not being purchasable, clone returns run on average 0.12 percentage points per month
below the index, roughly 1.5pp annually. That is in line with Hartley (2019), who finds
liquid alternative mutual funds trailing hedge funds by 1–2% per year.

**The exception is Long/Short Equities**, the one clone that matches its index (0.42% vs
0.42% monthly, identical Sharpe ratio of 0.19). Probably because the strategy's dominant
exposure is the S&P 500, which is exactly what the factor set represents best.

## Repository contents

| File | Description |
| --- | --- |
| `Bachelor_Thesis_Moritz_Maidl.pdf` | Full thesis, 59 pages, including appendix with all regression tables and plots. |
| `Data.csv` | Monthly panel, Jan 2010 – Sep 2023 (165 observations, 23 columns). |
| `Skript.R` | Complete R analysis: data preparation, diagnostics, regressions, replications, clones. |

## Data

**Period:** January 2010 – September 2023, 165 monthly observations. Post-GFC: it spans one of the longest bull markets on record plus the COVID-19 drawdown, the war in Ukraine, high inflation, and the exit from near-zero rates. All series in USD.

**Format:** semicolon-delimited, comma as decimal separator (German Excel export). Read with
`readr::read_csv2()`.

### Dependent variables (Eurekahedge equal-weighted indices)

Monthly returns, net of fees, AUM flows excluded. Constituent counts as of download:

| Column | Strategy | Funds |
| --- | --- | --- |
| `EHIMainReturn` | Eurekahedge Hedge Fund Index (flagship) | 3,241 |
| `EHIArbitrageReturn` | Arbitrage | 90 |
| `EHIManagedFuturesReturn` | CTA / Managed Futures | 405 |
| `EHIDistressedDebtReturn` | Distressed Debt | 19 |
| `EHIEventDrivenReturn` | Event Driven | 127 |
| `EHIFixedIncomeReturn` | Fixed Income | 496 |
| `EHILSEquityReturn` | Long/Short Equities | 1,235 |
| `EHIMacroReturn` | Macro | 217 |
| `EHIMultiStrategyReturn` | Multi-Strategy | 355 |
| `EHIRelativeValueReturn` | Relative Value | 62 |

### Independent variables (risk factors)

Stored as levels (`*Value`); returns are computed in the script.

| Column | Proxy | Return transform |
| --- | --- | --- |
| `GB1MValue` | US 1-Month Treasury Bill (risk-free) | first difference |
| `BAA10YValue` | Moody's Baa yield − 10Y Treasury (credit spread) | first difference |
| `SPXValue` | S&P 500 (equities) | log difference |
| `IEFValue` | iShares 7-10Y Treasury Bond ETF (bonds) | log difference |
| `DXYValue` | U.S. Dollar Index (currency) | log difference |
| `VIXYValue` | ProShares VIX Short-Term Futures ETF (volatility) | log difference |
| `GSGValue` | iShares S&P GSCI Commodity-Indexed Trust (commodities) | log difference |
| `RSSCValue`, `RSLCValue` | Russell 2000 / Russell 1000 → `SIZE` | log difference, then spread |
| `RSVValue`, `RSGValue` | Russell 1000 Value / Growth → `VALUE` | log difference, then spread |
| `MTUMValue` | iShares MSCI USA Momentum Factor ETF (momentum) | log difference |
| `PHDGValue` | Invesco S&P 500 Downside Hedged ETF (OTM short put proxy) | log difference |

Sources: Eurekahedge (indices), Investing.com and Federal Reserve Bank of St. Louis (FRED)
for factors.

**Note on padded series.** Several ETFs did not trade for the whole window, so the script
zero-fills their leading months: `VIXY` (first 14), `RSLC` and `SIZE` (first 16), `PHDG`
(first 37), `MTUM` (first 41). Zeros are not missing-data indicators here — they enter the
regressions as genuine zero returns and bias early-window estimates toward zero. See
[Known issues](#known-issues-and-limitations).

## Method

### Stage 1 — Preparation and diagnostics

Returns are computed from levels; `SIZE` and `VALUE` are built as Russell spreads. Two
diagnostics follow:

- **Ljung-Box tests** on all ten indices at lags 1 and 5, checking for the return smoothing
  typical of illiquid holdings. Only Distressed Debt's lag-1 statistic (7.55) exceeds the
  5% critical value of 3.84, so no smoothing correction is applied.
- **Correlation matrix** of the risk factors, to screen for multicollinearity. `SIZE`
  correlates 0.83 with `SPX` and is dropped. `MTUM` correlates 0.77 but is retained as one
  of the two non-linear factors.

The risk-free rate is then subtracted from `SPX` and `IEF`, leaving **nine regressors**.

### Stage 2 — Full-sample regression

For each index *i*, over all 165 months:

```
R_it = α_i + β_i1·BAA10Y_t + β_i2·SPX_t + β_i3·IEF_t + β_i4·DXY_t + β_i5·VIXY_t
     + β_i6·GSG_t + β_i7·VALUE_t + β_i8·MTUM_t + β_i9·PHDG_t + ε_it
```

Estimated by OLS. The intercept is read as manager-specific alpha, the betas as systematic
exposures. Alpha dynamics are then traced with 60-month rolling windows, giving the first
estimate for December 2014.

### Stage 3 — Out-of-sample replication

Rolling 60-month windows via `zoo::rollapply`: coefficients are estimated on months
*(t−60)* to *(t−1)* and applied to month *t*'s factor realisations. Rolling windows rather
than fixed weights, because fixed weights impose look-ahead bias and cannot adapt to the
exposure drift documented in Stage 2. This yields **105 out-of-sample months** (months
61–165, Jan 2015 – Sep 2023).

Evaluated by correlation with realised returns and a paired t-test.

### Stage 4 — Investable clones

Same rolling machinery, three changes that make the output an actual portfolio:

1. **Intercept dropped** (`- 1` in the formula). Manager skill is not investable.
2. **Betas constrained to sum to one.** The residual `1 − Σβ` is allocated long or short to
   the risk-free asset, so the clone deploys the same capital as the index.
3. **`BAA10Y` dropped from the clone factor set** — eight factors, not nine. The spread
   attracts betas an order of magnitude larger than any other factor (−1 to −3.65), which
   forced extreme risk-free leverage. Once T-bill yields rose from mid-2022, that leverage
   dominated clone returns and produced an artefact rather than a replication (thesis
   Figures 11 vs 12).

Returns are then taken in excess of the risk-free rate, and clone returns are renormalised
by *σ(index) / σ(clone)* so both series carry identical volatility — following Hasanhodzic
& Lo (2006). This changes the clone's leverage, which matters for interpreting the Sharpe
ratios.

Evaluated on mean return, Sharpe ratio, RMSE, tracking error and Theil's inequality
coefficient. Fees are assumed negligible and are not modelled.

## Results

Full tables are in the thesis appendix (Tables 3–5). Condensed:

| Strategy | R² | Alpha (%/mo) | Clone corr. | Δ mean (pp/mo) | Sharpe: index / clone |
| --- | --- | --- | --- | --- | --- |
| EHI Main Index | 0.82 | 0.26 | 0.84 | −0.11 | 0.25 / 0.18 |
| Arbitrage | 0.67 | 0.31 | 0.68 | −0.21 | 0.45 / 0.18 |
| CTA / Managed Futures | 0.04 | 0.40 | 0.20 | −0.09 | 0.23 / 0.17 |
| Distressed Debt | 0.68 | 0.54 | 0.72 | −0.24 | 0.27 / 0.14 |
| Event Driven | 0.82 | 0.28 | 0.80 | −0.07 | 0.20 / 0.17 |
| Fixed Income | 0.76 | 0.30 | 0.60 | −0.05 | 0.23 / 0.18 |
| **Long/Short Equities** | **0.85** | 0.17 | **0.87** | **−0.01** | **0.19 / 0.19** |
| Macro | 0.58 | 0.27 | 0.70 | −0.15 | 0.29 / 0.15 |
| Multi-Strategy | 0.80 | 0.26 | 0.82 | −0.10 | 0.26 / 0.19 |
| Relative Value | 0.74 | 0.27 | 0.77 | −0.14 | 0.28 / 0.16 |

Two results deserve emphasis:

**CTA / Managed Futures is unreplicable here.** R² of 0.04, no significant exposure to any
of the nine factors, out-of-sample correlation of 0.16. Two readings: the strategy really is
market-neutral on average, or a linear model with these factors simply cannot decompose
returns generated by systematic, quantitatively driven programmes. The thesis does not
separate the two.

**The clones fail specifically at the 2020/21 bull market.** Every clone tracks its index
reasonably through the COVID-19 crash and rebound, then diverges. Shortening the window to
36 months did not help, which argues against a reactivity explanation and points to the
alpha surge instead: the clones are missing precisely the component that is not investable.

## Running the code

**Honest warning:** `Skript.R` is a single 3,138-line exploratory script written to be
stepped through line by line in RStudio. It is not a pipeline. It calls `view()` throughout,
writes no files, saves no plots, and produces no reproducible artefacts — results are read
off the console and the viewer pane. Sourcing it end-to-end will open dozens of viewer tabs
and print a lot of output. Run it in sections.

```r
# from the repository root
install.packages(c(
  "tidyverse", "data.table", "broom", "mosaic", "gridExtra", "ggfortify",
  "ggcorrplot", "qqplotr", "reshape2", "plotly", "corrplot", "viridisLite",
  "tseries", "forecast", "zoo", "moments", "PerformanceAnalytics", "TTR",
  "matrixTests", "lmtest", "car", "quantmod"
))
```

`Data.csv` is loaded by relative path, so the working directory must be the repository
root. Developed on R 4.3.x; no `renv` lockfile, so package versions are not pinned.

Section headers in the script follow the analysis order:

```
Packages → Loading Data → Autocorrelation → Risk Factor Returns → Fama-French Factors
→ Performance Visualization → Isolation of the Risk-Free Asset → Factor Correlation
→ Analysis of the Regression → <Strategy> Replication ×10 → <Strategy> Clone ×10
```

## Known issues and limitations

Recorded openly. This was written before I had much data science or software engineering
practice, and several things would be done differently now.

**Data bug — `RSLC` returns are wrong.** Line 183 reads
`RSLCReturn <- diff(log(DataAll$DXYValue))`; it should reference `RSLCValue`. The Russell
1000 series is therefore the dollar index, which propagates into `SIZEReturn`. `SIZE` never
enters any `lm()` call, so all regression, replication and clone results are unaffected —
but the reported SPX–SIZE correlation of 0.83, which motivated dropping `SIZE` in the first
place, is not trustworthy. Re-running the correlation with the correct series is the first
thing to fix.

**Zero-padding instead of shortening the window.** Four factor series are zero-filled for
their pre-inception months. Zero returns are not neutral: they shrink early-window
coefficients toward zero and understate those factors' variance. A shorter sample starting
in mid-2013, or explicit `NA` handling, would be cleaner.

**Extreme credit-spread betas.** `BAA10Y` betas run from −0.93 to −3.65 while other factors
sit near zero. The thesis offers two explanations — the yield-versus-price argument from
Cherian et al. (2020), and OLS levering a near-flat series to make it useful — and is
explicit that the second is a guess. Constraining betas to sum to one *inside* the
regression, rather than patching the residual into the risk-free asset afterwards, would
avoid the problem the clones ran into.

**No formal OLS diagnostics.** Seven Gauss-Markov assumptions are listed; only
autocorrelation is tested. Heteroscedasticity and residual normality are not, despite
`lmtest` and `car` being loaded. Newey-West or HAC standard errors would be the sensible
default for monthly financial returns.

**Massive code duplication.** The twenty replication and clone blocks are near-identical
copies differing only in the dependent variable. They should be one function over a vector
of index names — roughly 200 lines instead of 2,200. Same for the correlation and
performance tests.

**Hardcoded indices.** `DataAll[61:165, ]` and `betas[1:14]`-style slicing appear
throughout. Any change to the sample period breaks them silently.

**Fees are not modelled.** Index returns are net of fees; factor returns are gross. The
clones' cost advantage — a main argument for replication in the first place — is asserted
rather than quantified.

**Data licensing.** Eurekahedge index data is proprietary and its redistribution terms
should be verified before this file stays in a public repository. FRED and Investing.com
series are unrestricted. *(The Eurekahedge indices were discontinued in 2024, which makes
this both harder to check and harder to reproduce from source.)*

**Not addressed at all:** transaction costs and slippage on monthly rebalancing;
survivorship and backfill bias in the index constituents; parameter uncertainty in the
rolling betas.

### What a follow-up would change

More sophisticated estimation is the obvious direction — a Kalman filter for time-varying
betas, or regularised/machine-learning approaches that handle correlated factors better
than OLS. Constraining the betas during estimation, and modelling fees explicitly, would do
more for the credibility of the clone comparison than any additional factor.

## References

Principal sources; full bibliography in the thesis.

- Asness, C., Krail, R. & Liew, J. (2001). Do Hedge Funds Hedge? *Journal of Portfolio Management*, 28(1), 6–19.
- Cherian, J. A., Kon, C. & Li, Z. (2020). Replicas: Have Hedge Funds Re-Resurrected as Traditional Beta? SSRN.
- Fama, E. & French, K. (1993). Common Risk Factors in the Returns of Stocks and Bonds. *Journal of Financial Economics*, 33, 3–56.
- Fung, W. & Hsieh, D. (2004). Hedge Fund Benchmarks: A Risk-Based Approach. *Financial Analysts Journal*, 60(5), 65–80.
- Hartley, J. (2019). Liquid Alternative Mutual Funds versus Hedge Funds. *Journal of Alternative Investments*, 22(1), 37–56.
- Hasanhodzic, J. & Lo, A. (2006). Can Hedge-Fund Returns be Replicated?: The Linear Case. SSRN.
- Soerensen, M. W. & Hansen, E. (2020). Hedge Fund Replication Before, During and After the Financial Crisis. CBS Research Portal.
