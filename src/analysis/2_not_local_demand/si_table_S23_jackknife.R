# Table S23: the leave-one-state-out jackknife.

rm(list = ls())

source('src/lib/format.R')

audit <- load_results('18_methods_audit')
jks <- audit$jks[match(names(audit$YS), outcome)]
jks # one row per outcome: 2SLS, cluster and jackknife SEs, CI, LOO range

tab <- jks[, .(
    Outcome = audit$YS[outcome],
    b = fmt_num(b, 3),
    se_cl = fmt_num(se_cl, 3),
    se_jk = fmt_num(se_jk, 3),
    lo = fmt_num(lo_jk, 3),
    hi = fmt_num(hi_jk, 3),
    min = fmt_num(b_min, 3),
    max = fmt_num(b_max, 3)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/methods_jackknife.tex',
    header = c(
        'Outcome', '2SLS', 'Cluster', 'Jackknife',
        'Lower', 'Upper', 'Min', 'Max'
    ),
    spanners = c(
        ' ' = 1, 'SE' = 2, 'Jackknife 95% CI (t, G-1)' = 2,
        'Leave-one-out range' = 2
    )
)
