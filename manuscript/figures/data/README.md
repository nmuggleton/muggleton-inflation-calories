# Figure data

The data plotted in the main-text figures, saved as CSV files so that anyone can redraw or refit them. They are aggregates and hold no household records.

Each figure script saves its CSVs first and then draws the figure. The scripts read only the committed estimates in `results/`, so they run from a plain checkout without the NielsenIQ data:

```bash
Rscript src/analysis/1_pay_same_calories/figure_2_budget_identity.R
Rscript src/analysis/2_not_local_demand/si_fig_S4_bls_instrument.R
Rscript src/analysis/4_cheaper_calories/figure_3_shelf_reach.R
```

Figure 1, the introduction schematic (`src/analysis/0_introduction/figure_1_schematic.R`), has no data file here. The scripts depend on data.table, ggplot2, patchwork and scales, and take the figure style from `src/lib/theme_print.R`.

## Files

| File | Figure | Contents |
|---|---|---|
| `fig2_cells.csv` | 2 | The 3,724 state-quarter cells: household-quarters `n`; mean exposure `x`; the outcome means `spend`, `kcal`, and `kpu` (log x 100, in percent) and `ed`, `hs`, and `cov` (in SDs of the level); and every variable net of quarter fixed effects (`*_net`). Use these to rebin or refit. |
| `fig2_bins.csv` | 2 | The equal-weight bins of `x_net` for each outcome, as plotted: the bin mean `value`, its `se`, and a 95% interval `lo`, `hi`. |
| `fig2_fit.csv` | 2 | The cell-level slope for each outcome (weighted by `n`, quarter fixed effects, clustered by state), as in the caption: the fitted line through the origin over the binned range, with a 95% band `lo`, `hi` from the slope's SE. |
| `figS4_iv.csv` | S4 | The 2SLS estimate `b` by instrument and outcome, with `se` and `lo`, `hi`. The interval method is named in `ci_method`: Wald for the main and combined instruments, and a pairs cluster bootstrap for the two BLS rows. `benchmark` is the reference line at -1 or +1. |
| `fig3_density.csv` | 3A | The density of savings available at fixed calories, as a percentage of base labelled spending, on a 512-point grid for savings up to 80 percent: a Gaussian kernel with bandwidth `bw` (bw.nrd0 x 1.2), as `geom_density` draws it. |
| `fig3_hist.csv` | 3A | The same savings in 1-point bins: counts and shares, and `mean_shock`, the mean yearly basket price rise marked on the figure. |
| `fig3_reach.csv` | 3B | Mean saving at the 25th percentile, as a percentage of common-base spending, for each rung: `ladder` (pooled or best retailer), `rung`, the plotted `level` and `series`, `mean` with a 95% interval `lo`, `hi` (SEs clustered by household), `median`, and household-quarters `n`. |
| `fig3_reach_income.csv` | 3C | The same for three rungs (retailer used, pooled state shelf, best retailer in the 3-digit zip code) by real household income in the base year (2019 dollars), with `se` and a 95% interval `lo`, `hi`. |

## Caveats

- **Bin intervals in `fig2_bins.csv`** treat the cells in a bin as independent of one another; the slopes in `fig2_fit.csv` are clustered by state. Use the slope intervals for inference and the bin intervals for display only.
- **Small cells.** Eleven state-quarter cells in `fig2_cells.csv` have fewer than 30 household-quarters. Double-check the NielsenIQ disclosure terms before sharing that file beyond the review.
