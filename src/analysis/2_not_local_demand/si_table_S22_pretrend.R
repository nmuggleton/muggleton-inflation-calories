# Table S22: pre-trend placebos and conditioning on lagged changes.

rm(list = ls())

source('src/lib/format.R')

dyn <- load_results('07_dynamics')
dyn$pre # placebo: last year's change on this year's exposure
dyn$cond # lag sample, with and without the household's own lagged change

est <- rbind(
    dyn$pre[, .(spec = paste('Pre-trend placebo,', spec), outcome, b, se)],
    dyn$cond[, .(spec, outcome, b, se)]
)

tab <- wide_bse(est, 'spec', names(ROB_SHORT), digits = c(d_hs = 5))
tab

# All five outcomes are too wide for the page, so two tables, one under the
# other
spend_cal <- c(
    'spec', 'b_d_lkpu', 'se_d_lkpu', 'b_d_lspend', 'se_d_lspend',
    'b_d_lkcal', 'se_d_lkcal'
)
diet <- c('spec', 'b_d_ed', 'se_d_ed', 'b_d_hs', 'se_d_hs')
write_tex_table(
    tab[, ..spend_cal],
    'manuscript/tables/pretrend.tex',
    header = c('', rep(c('b', 'SE'), 3)),
    spanners = setNames(rep(2, 3), ROB_SHORT[1:3]),
    title = 'A. Spending and calories'
)
write_tex_table(
    tab[, ..diet],
    'manuscript/tables/pretrend.tex',
    header = c('', rep(c('b', 'SE'), 2)),
    spanners = setNames(rep(2, 2), ROB_SHORT[4:5]),
    title = 'B. Diet quality',
    append = TRUE
)
