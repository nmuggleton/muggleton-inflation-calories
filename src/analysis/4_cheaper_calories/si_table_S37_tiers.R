# Table S37: inflation of the cheap and premium tiers under local exposure. One
# row per outcome: the 2SLS coefficient of tier-level price-per-calorie
# inflation in the household's modules and state on exposure (stars from
# fixest's p, 49 state clusters), SE, N and first-stage F.

rm(list = ls())

source('src/lib/format.R')

ivt <- load_results('08_mechanism')$ivt
ivt # one row per outcome: b, se, n, f, p

rows <- c(
    f_cheap = 'Cheap tier',
    f_all = 'All tiers',
    f_prem = 'Premium tier',
    f_rel = 'Cheap minus premium tier',
    e_own = "Household's own base products"
)
d <- ivt[match(names(rows), outcome)]
stopifnot(!anyNA(d$b))

tab <- d[, .(
    Outcome = sprintf('(%d) %s', seq_len(.N), rows[outcome]),
    b = paste0(fmt_num(b, 3), stars(p)),
    se = parens(fmt_num(se, 3)),
    N = fmt_num(n, 0),
    F = fmt_num(f, 0)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/ladder_iv.tex',
    header = c(
        'Price-per-calorie inflation (pp)', '\\(b\\)', 'SE', '\\(N\\)',
        'First-stage \\(F\\)'
    )
)
