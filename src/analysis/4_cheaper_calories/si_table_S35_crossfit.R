# Table S35: why the best-retailer rungs are cross-fitted. The plain minimum of
# retailer 25th percentiles over the household's retailers rises with the number
# of retailers it uses, since it is a minimum of noisy quantile estimates; the
# cross-fitted minimum does not climb past the pooled state shelf.

rm(list = ls())

source('src/lib/format.R')

sb <- load_results('22_panel_reach')$set_by_n
sb # mean p25 saving (%) by number of retailers used

stopifnot(identical(sb$n_ret_cat, c('1', '2', '3', '4-5', '6+')))
tab <- sb[, .(
    n_ret_cat,
    n = fmt_num(n, 0),
    retailer = fmt_num(retailer, 1),
    set_naive = fmt_num(set_naive, 1),
    set_crossfit = fmt_num(set_crossfit, 1),
    state = fmt_num(state, 1),
    best_st = fmt_num(best_st, 1)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/crossfit.tex',
    header = c(
        'Retailers used', 'N', 'Retailer used', 'Plain minimum',
        'Cross-fitted', 'All stores in state', 'Best in state'
    ),
    spanners = c(' ' = 2, 'Own retailers' = 2, 'State' = 2)
)
