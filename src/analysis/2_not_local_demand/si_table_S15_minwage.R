# Table S15: minimum-wage changes.

rm(list = ls())

source('src/lib/format.R')

audit <- load_results('18_methods_audit')
mw <- copy(audit$mw)
mw # one row per specification x outcome

# Plain labels, in the order estimated
spec_label <- c(
    `Headline (gate)` = 'Main estimate',
    `+ minimum-wage change, current and lagged` =
        '+ change in the state minimum wage, this year and last',
    `Drop if state minimum wage changed t-4 to t` =
        paste0(
            'Excluding state-quarters where the ',
            'minimum wage changed in the past year'
        ),
    `Drop if it changed t-8 to t` =
        'Excluding state-quarters where it changed in the past two years'
)
stopifnot(all(mw$spec %in% names(spec_label)))
mw <- mw[order(match(spec, names(spec_label)))]
mw[, spec := spec_label[spec]]

# Outcome names and order as in the main text (Table 1)
outcomes <- c(
    d_lspend = 'Real spend', d_lkcal = 'Calories bought',
    d_lkpu = 'Calories per dollar'
)
tab <- wide_bse(mw, 'spec', names(outcomes))
# N for spending
tab[, N := fmt_num(mw[outcome == 'd_lspend'][match(tab$spec, spec), n], 0)]
tab

write_tex_table(
    tab,
    'manuscript/tables/methods_minwage.tex',
    header = c('Specification', rep(c('\\(b\\)', 'SE'), 3), '\\(N\\)'),
    spanners = c(setNames(rep(2, 3), outcomes), ' ' = 1)
)
