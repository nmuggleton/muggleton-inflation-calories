# Table S11: the headline 2SLS estimates under other definitions of the
# outcomes.

rm(list = ls())

source('src/lib/format.R')

oa <- copy(load_results('12_measurement')$oa)
oa # one row per outcome definition, grouped by the outcome it measures

# Bounded or small-unit outcomes are shown x100 so that 3 decimals carry
x100 <- c('d_hs', 'd_cov')
oa[outcome %in% x100, `:=`(
    b = 100 * b,
    se = 100 * se,
    label = paste(label, 'x100')
)]

tab <- oa[, .(
    Outcome = label,
    # Stars: t test with 48 df (49 states)
    b = paste0(fmt_num(b, 3), stars(2 * pt(-abs(b / se), 48))),
    se = parens(fmt_num(se, 3)),
    N = fmt_num(n, 0)
)]
tab

first_row <- oa[, .(row = .I[1]), by = group]
write_tex_table(
    tab,
    'manuscript/tables/outcomes.tex',
    header = c(
        'Outcome (change t vs t-4, x100 log)', '2SLS per pp',
        'SE', 'N'
    ),
    panels = setNames(first_row$row, first_row$group)
)
