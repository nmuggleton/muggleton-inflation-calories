# Table S25: the price paid per calorie decomposed along the within-module
# ladder. One row per outcome: the 2SLS coefficient on exposure (stars from
# fixest's p, 49 state clusters), SE, N and first-stage F.

rm(list = ls())

source('src/lib/format.R')

ivt <- load_results('08_mechanism')$ivt
ivt # one row per outcome: b, se, n, f, p

rows <- c(
    dec_t = 'Price per calorie paid (1 = 2 + 3 + 4)',
    dec_m = 'Module median price per calorie',
    dec_l = 'Own rung relative to the median',
    dec_mix = 'Module mix and entry',
    d_cheap = 'Cheap tier (cheapest quarter of calories)',
    d_prem = 'Premium tier (dearest quarter of calories)'
)
d <- ivt[match(names(rows), outcome)]
stopifnot(!anyNA(d$b))
# ~0
d[outcome %in% c(
    'dec_m',
    'dec_l',
    'dec_mix'
), sum(b)] - d[outcome == 'dec_t', b]

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
    'manuscript/tables/ladder_dec.tex',
    header = c('', '\\(b\\)', 'SE', '\\(N\\)', 'First-stage \\(F\\)'),
    panels = c(
        `A. Change in log price per labeled calorie (\\texttimes{}100)` = 1,
        `B. Change in share of calories (percentage points)` = 5
    )
)
