# Table S40: the income gradient (T2 x FPL rank) with and without households
# whose income is top-coded.

rm(list = ls())

source('src/lib/format.R')

tc <- load_results('09_heterogeneity')$tc
tc # one row per outcome: all households (...0) and no top-coded income (...1)

tab <- tc[, .(
    Outcome = HET_YS[outcome],
    # Stars: t, 48 df
    b0 = paste0(fmt_num(b0, 4), stars(2 * pt(-abs(b0 / se0), 48))),
    se0 = parens(fmt_num(se0, 4)),
    p0 = fmt_p(p0),
    b1 = paste0(fmt_num(b1, 4), stars(2 * pt(-abs(b1 / se1), 48))),
    se1 = parens(fmt_num(se1, 4)),
    p1 = fmt_p(p1)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/budget_topcode.tex',
    header = c('Outcome', 'b', 'SE', 'p', 'b', 'SE', 'p'),
    spanners = c('All households' = 3, 'No top-coded income' = 3)
)
