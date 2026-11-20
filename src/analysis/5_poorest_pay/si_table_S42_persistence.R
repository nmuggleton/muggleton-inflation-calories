# Table S42: persistent versus transitory exposure (the components of the
# instrument).

rm(list = ls())

source('src/lib/format.R')

per <- load_results('07_dynamics')$per
per # one row per instrument x outcome

tab <- wide_bse(per, 'spec', names(ROB_SHORT), digits = c(d_hs = 5))
# First-stage F, from the calories-per-dollar models
f_stats <- per[outcome == 'd_lkpu'][match(tab$spec, spec), f1]
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
    'manuscript/tables/persistence.tex',
    header = c('Instrument / FE', rep(c('b', 'SE'), 3), 'First-stage F'),
    spanners = c(setNames(rep(2, 3), ROB_SHORT[1:3]), ' ' = 1),
    title = 'A. Spending and calories'
)
write_tex_table(
    tab[, ..diet],
    'manuscript/tables/persistence.tex',
    header = c('Instrument / FE', rep(c('b', 'SE'), 2)),
    spanners = setNames(rep(2, 2), ROB_SHORT[4:5]),
    title = 'B. Diet quality',
    append = TRUE
)

# Panel C: the same split for low-income and other households. Estimates from
# estimate/29_persistence_income.R.
two <- load_results('29_persistence_income')$two_group
two # one row per outcome x instrument component

parts <- c(
    z_ann = 'annual state component',
    z_dev = 'within-state-year deviation'
)
shown <- c('d_lkpu', 'd_lspend', 'd_lkcal')
two <- two[outcome %in% shown]
two <- two[order(match(outcome, shown), match(part, names(parts)))]
stopifnot(nrow(two) == length(shown) * length(parts))
tab_c <- two[, .(
    row = paste0(ROB_SHORT[outcome], ': ', parts[part]),
    b_low = paste0(
        fmt_num(b_low, 3), stars(2 * pt(-abs(b_low / se_low), 48))
    ),
    se_low = parens(fmt_num(se_low, 3)),
    b_oth = paste0(
        fmt_num(b_oth, 3), stars(2 * pt(-abs(b_oth / se_oth), 48))
    ),
    se_oth = parens(fmt_num(se_oth, 3)),
    p = fmt_p(p)
)]
tab_c

write_tex_table(
    tab_c,
    'manuscript/tables/persistence.tex',
    header = c('Outcome: instrument', rep(c('b', 'SE'), 2), 'p'),
    spanners = c(
        `Up to 130% of poverty line` = 2, `Above` = 2,
        `Difference` = 1
    ),
    title = 'C. By household income',
    append = TRUE
)
