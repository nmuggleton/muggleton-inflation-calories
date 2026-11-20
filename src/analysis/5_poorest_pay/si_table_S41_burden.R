# SI table: extra food spending from exposure inflation, for households at or
# below the poverty line and above it, by year: mean exposure, extra spending a
# year (2019 dollars), and the median share of base income.

rm(list = ls())

source('src/lib/format.R')

r <- load_results('24_burden')
by <- r$by_year
cum <- r$cum
by # one row per group x period

per <- c('2017-19', '2021', '2022', '2023')
g <- function(p, v) by[poor == p][match(per, period)][[v]]
tab <- data.table(
    Period = c('2017--19 (mean)', '2021', '2022', '2023'),
    e1 = fmt_num(g(TRUE, 'expo'), 1),
    d1 = fmt_num(g(TRUE, 'paid'), 0),
    s1 = fmt_num(g(TRUE, 'sh_paid'), 2),
    e0 = fmt_num(g(FALSE, 'expo'), 1),
    d0 = fmt_num(g(FALSE, 'paid'), 0),
    s0 = fmt_num(g(FALSE, 'sh_paid'), 2)
)
tab <- rbind(
    tab,
    data.table(
        Period = '2021--23, a year by 2023',
        e1 = '{}',
        d1 = fmt_num(cum[poor == TRUE, paid], 0),
        s1 = fmt_num(cum[poor == TRUE, sh_paid], 2),
        e0 = '{}',
        d0 = fmt_num(cum[poor == FALSE, paid], 0),
        s0 = fmt_num(cum[poor == FALSE, sh_paid], 2)
    )
)
stopifnot(!anyNA(tab))
tab

write_tex_table(
    tab,
    'manuscript/tables/burden.tex',
    header = c(
        '', 'Exposure (\\%)', 'Extra spend (\\$)',
        'Share of income (\\%)', 'Exposure (\\%)',
        'Extra spend (\\$)', 'Share of income (\\%)'
    ),
    spanners = c('At or below poverty line' = 3, 'Above' = 3),
    rule_before = 5
)
