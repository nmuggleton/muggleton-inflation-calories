# Table S16: controlling for local labour markets, income and house prices. The
# controls are added in turn (cumulatively).

rm(list = ls())

source('src/lib/format.R')

est <- copy(load_results('15_local_demand')$est)
est # one row per specification x outcome

# Plain labels, in the order the controls are added
spec_label <- c(
    `f1 headline (full gate sample)` = 'Main estimate, full sample',
    `Baseline, control sample` = 'Main estimate, sample with local data',
    `+ state U, income, house prices` =
        '+ state unemployment, personal income, and house prices',
    `+ county U and employment` = '+ county unemployment and employment',
    `+ lagged changes of all` = "+ last year's changes in all of these",
    `+ division x quarter FE` =
        '+ census division \\texttimes{} quarter fixed effects'
)
est <- est[spec %in% names(spec_label)][order(match(spec, names(spec_label)))]
est[, spec := spec_label[spec]]

# Outcome names and order as in the main text (Table 1)
outcomes <- c(
    d_lspend = 'Real spend', d_lkcal = 'Calories bought',
    d_lkpu = 'Calories per dollar'
)
tab <- wide_bse(est, 'spec', names(outcomes))
# N for spending
tab[, N := fmt_num(est[outcome == 'd_lspend'][match(tab$spec, spec), n], 0)]
tab

write_tex_table(
    tab,
    'manuscript/tables/local_spec_grid.tex',
    header = c('Specification', rep(c('\\(b\\)', 'SE'), 3), '\\(N\\)'),
    spanners = c(setNames(rep(2, 3), outcomes), ' ' = 1)
)
