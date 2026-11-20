# Table S48: responses by tercile of habit (the share of spend on the
# household's usual products).

rm(list = ls())

source('src/lib/format.R')

rev <- load_results('19_revision')
rev$rh # one row per habit tercile x outcome
rev$hm # median habit share in each tercile

outcomes <- c(
    d_lkpu = 'Calories per dollar', d_lspend = 'Real spend',
    d_lkcal = 'Calories'
)
tab <- wide_bse(rev$rh, 'group', names(outcomes))
tab <- tab[match(c('Low', 'Middle', 'High'), group)]
tab[, median := fmt_num(rev$hm[match(tab$group, hab3), med], 2)]
setcolorder(tab, c('group', 'median'))
tab

write_tex_table(
    tab,
    'manuscript/tables/rev_habit.tex',
    header = c(
        'Habit tercile', 'Median habit share',
        rep(c('b', 'SE'), 3)
    ),
    spanners = c(' ' = 1, setNames(rep(2, 3), outcomes))
)
