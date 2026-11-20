# Table S21: dynamics. Leads, pre-trends and lagged controls.

rm(list = ls())

source('src/lib/format.R')

iden <- load_results('05_identification')
# The pre-trend rows are saved in both objects, so keep one copy
est <- unique(rbind(iden$res, iden$pre))

# The rows of the table, in order, with their display labels
rows <- c(
    'Headline' = 'Headline 2SLS (T2)',
    'Lead zb(t+4)' = 'Lead: state T2 at t+4',
    'T2 | lead control' = 'T2, controlling for lead',
    'Lead zb(t+4) | lag control' = 'Lead, controlling for lag',
    "Placebo: last year's change on T2" = 'Pre-trend: change t-8 to t-4'
)
est <- est[spec %in% names(rows)][order(match(spec, names(rows)))]
est[, spec := rows[spec]]
est

tab <- wide_bse(est, 'spec', names(ROB_SHORT), digits = c(d_hs = 5))
tab

write_tex_table(
    tab,
    'manuscript/tables/placebo.tex',
    header = c('', rep(c('b', 'SE'), 5)),
    spanners = setNames(rep(2, 5), ROB_SHORT)
)
