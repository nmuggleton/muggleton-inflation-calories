# Table S17: alternative fixed effects, controls and instruments.

rm(list = ls())

source('src/lib/format.R')

iden <- load_results('05_identification')

rows <- c(
    'Headline', iden$spec_names,
    'Household + state-quarter economic controls',
    'Neighbor-state instrument (leave-own-state division)',
    'Both instruments'
)
est <- iden$res[spec %in% rows][order(match(spec, rows))]
est

tab <- wide_bse(est, 'spec', names(ROB_SHORT), digits = c(d_hs = 5))
# First-stage F, from the calories-per-dollar models
f_stats <- est[outcome == 'd_lkpu'][match(tab$spec, spec), f1]
tab[, first_stage_f := fmt_num(f_stats, 0)]
tab

# All five outcomes are too wide for the page, so two tables, one under the
# other
spend_cal <- c(
    'spec', 'b_d_lkpu', 'se_d_lkpu', 'b_d_lspend', 'se_d_lspend',
    'b_d_lkcal', 'se_d_lkcal', 'first_stage_f'
)
diet <- c('spec', 'b_d_ed', 'se_d_ed', 'b_d_hs', 'se_d_hs')
write_tex_table(
    tab[, ..spend_cal],
    'manuscript/tables/spec.tex',
    header = c('Specification', rep(c('b', 'SE'), 3), 'First-stage F'),
    spanners = c(setNames(rep(2, 3), ROB_SHORT[1:3]), ' ' = 1),
    title = 'A. Spending and calories'
)
write_tex_table(
    tab[, ..diet],
    'manuscript/tables/spec.tex',
    header = c('Specification', rep(c('b', 'SE'), 2)),
    spanners = setNames(rep(2, 2), ROB_SHORT[4:5]),
    title = 'B. Diet quality',
    append = TRUE
)
