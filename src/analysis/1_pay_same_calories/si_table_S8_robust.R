# Table S8: the headline calories-per-dollar estimate under other fixed effects,
# weights and samples.

rm(list = ls())

source('src/lib/format.R')

rob <- load_results('01_main')$robustness
rob # one row per specification: 2SLS b, SE, N, first-stage F

tab <- rob[, .(
    Specification = spec,
    # Stars: t test with 48 df (49 states)
    b = paste0(fmt_num(b, 3), stars(2 * pt(-abs(b / se), 48))),
    se = parens(fmt_num(se, 3)),
    offset = fmt_num(1 + b, 3),
    N = fmt_num(n, 0),
    F = fmt_num(f, 0)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/robust.tex',
    header = c(
        'Specification', '2SLS', 'SE', 'Offset share, 1 + b',
        'N', 'First-stage F'
    )
)
