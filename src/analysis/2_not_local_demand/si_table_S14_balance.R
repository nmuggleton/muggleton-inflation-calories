# Table S14: balance. Does the instrument predict local economic shocks?

rm(list = ls())

source('src/lib/format.R')

ld <- load_results('15_local_demand')
ld$bal # one row per local shock: coefficient on z, SE, p, and standardised

tab <- ld$bal[, .(
    Shock = ld$lab[var],
    b = paste0(fmt_num(b, 3), stars(p)),
    se = parens(fmt_num(se, 3)),
    p = fmt_p(p),
    b_std = fmt_num(b_std, 3),
    se_std = parens(fmt_num(se_std, 3))
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/local_balance.tex',
    header = c(
        'Change in local conditions',
        '\\(b\\)',
        'SE',
        '\\(p\\)',
        '\\(b\\)',
        'SE'
    ),
    spanners = c('Coefficient on \\(z\\)' = 3, 'Standardized' = 2)
)
