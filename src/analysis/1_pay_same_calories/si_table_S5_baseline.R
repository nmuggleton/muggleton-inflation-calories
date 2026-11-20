# Table S5: the income gradient (T2 x FPL rank), with and without a control for
# the household's base level of the outcome.

rm(list = ls())

source('src/lib/format.R')

gb <- load_results('09_heterogeneity')$gb
gb # one row per outcome: interaction without (...0) and with base-level control

tab <- gb[, .(
    Outcome = HET_YS[outcome],
    # Stars: t, 48 df
    b0 = paste0(fmt_num(b_int0, 4), stars(2 * pt(-abs(b_int0 / se_int0), 48))),
    se0 = parens(fmt_num(se_int0, 4)),
    p0 = fmt_p(p_int0),
    b1 = paste0(fmt_num(b_int, 4), stars(2 * pt(-abs(b_int / se_int), 48))),
    se1 = parens(fmt_num(se_int, 4)),
    p1 = fmt_p(p_int),
    b_base = paste0(
        fmt_num(b_base, 4),
        stars(2 * pt(-abs(b_base / se_base), 48))
    ),
    se_base = parens(fmt_num(se_base, 4))
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/budget_baseline.tex',
    header = c('Outcome', 'b', 'SE', 'p', 'b', 'SE', 'p', 'b', 'SE'),
    spanners = c(
        'T2 x FPL rank' = 3,
        'T2 x FPL rank, base-level control' = 3,
        'T2 x base level' = 2
    )
)
