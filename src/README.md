# Code

Three stages; each reads only what the one before wrote.

1. `data_prep/` turns the raw Kilts files into the analysis dataset. SQL in
   DuckDB, shell, some Python and R. Steps 02-09, run by `run_data_prep.sh`.
2. `analysis/estimate/` fits the models and saves the estimates to
   `results/<script>/`, one CSV per object.
3. `analysis/<section>/` reads `results/` and writes the tables, figures and
   in-text numbers for that section of the paper.

`run_all.sh` runs all three. `lib/` is shared code: `helpers.R` (data readers,
2SLS wrappers, wild bootstrap), `format.R` (results store, table and figure
helpers), `theme_print.R` (figure style), `paths.sh` (data paths).

Data prep steps:

    02  check the raw files, stage them as Parquet
    03  products, purchase merge, household-year panel
    04  nutrient profiles by barcode, food groups
    05  household x quarter outcomes
    06  price indices: exposure inflation, own prices, split samples
    07  controls and the estimation-sample flag
    08  public data (BLS, BEA, FRED, minimum wages)
    09  analysis dataset, instrument, calorie-price ladders, shelves

`FROM=` and `TO=` run a subset, `FORCE=1` rebuilds. The three R scripts under
`data_prep/` take their paths from `run_data_prep.sh`, so run them through it.

## What each estimation script does

| Script | What it estimates |
|---|---|
| `01_main` | Headline OLS and 2SLS, pairs bootstrap, robustness, OLS against 2SLS by basket size |
| `02_cells` | State-quarter cells, cell-level slopes, Figure 2 bins and diet levels by income, most against least exposed states |
| `03_descriptive` | Where the variation comes from; exposure against the CPI; Figure 1 data |
| `04_summary` | Summary statistics and the sample path |
| `05_identification` | Leads, pre-trends, balance, stronger fixed effects, neighbour instrument, same-product prices against composition |
| `06_inference` | Other clusterings, wild cluster bootstrap (Anderson-Rubin), randomization inference |
| `07_dynamics` | Pre-trends under stronger fixed effects, lagged changes, persistent against transient exposure |
| `08_mechanism` | The calorie-price ladder: savings on offer, tier inflation, decomposition, response by room to save |
| `09_heterogeneity` | By income, SNAP eligibility, and food budget share; the income gradient |
| `10_regime` | By national inflation regime, episode, and year; intensity; asymmetry |
| `11_leverage` | Leave one state or year out, trims, winsorising, influential state-quarters |
| `12_measurement` | Other outcome and instrument definitions; households recording random-weight items |
| `13_sample` | Thresholds, weights, controls, clustering, attrition, by year |
| `14_shiftshare` | An alternative shift-share design and its failed placebo (SI only) |
| `15_local_demand` | Balance on local shocks; local-demand controls |
| `16_bls_cpi` | The instrument from BLS price indices, with few-cluster inference |
| `17_other_prices` | Controls for other local prices |
| `18_methods_audit` | Minimum wages; leave-one-state-out jackknife |
| `19_revision` | Health-score components, shopping margins, equivalence tests, habit |
| `21_panel_shelf_check` | Validates panel-observed shelves against Retail Scanner shelves |
| `22_panel_reach` | Savings ladder on panel-observed shelves, by reach and income |
| `23_national` | National food inflation over time |
| `24_burden` | The dollar cost of the 2021-23 rise, by poverty status |
| `25_eggs` | The response to the 2022-23 egg price shock |
| `26_fixed_retailer` | The instrument from state prices at fixed retailer weights |
| `27_relative_prices` | Own prices against what other households in the state paid for the same products, under both rules for extreme price changes |
| `28_split_sample` | Exposure and the instrument priced from different random halves of households |
| `29_persistence_income` | Persistent and transitory price changes, by income |
| `30_density` | Responses by how densely a household's purchases are priced (SI log only) |
| `31_state_size` | Responses by state size, with few-cluster and permutation inference |
| `32_nonfood` | Non-food spending recorded by the panel |
| `33_own_price` | OLS and 2SLS on the inflation each household records from the prices it pays |
| `34_egg_episode` | The national egg price rise of 2022Q2-2023Q1 |
| `35_own_price_rules` | The own-price index under five rules for extreme price changes; coupon use |
| `36_beliefs` | Stated beliefs about price changes (PanelViews economic surveys, 2015-2019) |
| `37_price_gap_sources` | Where the gap between own and state prices sits: channel, retailer, store |
| `38_outlier_diagnostics` | Evidence behind the rule for extreme price changes (SI log only) |
| `39_income_benchmark` | Income-effect benchmark for trading down (SI log only) |

`run_analysis.sh` runs them in this order (there is no 20), then the output
scripts.
