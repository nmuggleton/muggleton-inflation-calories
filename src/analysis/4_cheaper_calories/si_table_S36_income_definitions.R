# Table S36: the income gradient of Figure 3C under three definitions of income:
# nominal brackets, real base-year income (2019 dollars, as in the figure), and
# the base-year ratio to the poverty line. Mean saving at the 25th percentile
# (%).

rm(list = ls())

source('src/lib/format.R')

inc <- load_results('22_panel_reach')$inc_def
inc # one row per definition x rung x group: mean, SE, N

defs <- c(
    nominal = 'A. Nominal income',
    real2019 = 'B. Real income, base year (2019 dollars)',
    poverty_line = 'C. Income relative to the poverty line, base year'
)
w <- dcast(inc, definition + group ~ series, value.var = c('mean', 'se'))
w[, ord := match(
    paste(definition, group),
    inc[series == 'retailer', paste(definition, group)]
)]
setorder(w, ord)
tab <- w[, .(
    group,
    ret = fmt_num(mean_retailer, 2),
    ret_se = parens(fmt_num(se_retailer, 3)),
    st = fmt_num(mean_state, 2),
    st_se = parens(fmt_num(se_state, 3)),
    bz3 = fmt_num(mean_best_zip3, 2),
    bz3_se = parens(fmt_num(se_best_zip3, 3)),
    n = fmt_num(
        inc[series == 'retailer'][match(
            paste(w$definition, w$group),
            paste(definition, group)
        ), n],
        0
    )
)]
first <- w[, .(row = .I[1]), by = definition]
tab

write_tex_table(
    tab,
    'manuscript/tables/income_definitions.tex',
    header = c('Income group', 'Mean', 'SE', 'Mean', 'SE', 'Mean', 'SE', 'N'),
    spanners = c(
        'Retailer used' = 2, 'All stores in state' = 2,
        'Best retailer in zip3' = 2, ' ' = 1
    ),
    panels = setNames(first$row, defs[first$definition]),
    indent = TRUE
)
