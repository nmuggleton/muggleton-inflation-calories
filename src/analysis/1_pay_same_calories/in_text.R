# Numbers quoted in "Households pay for the same calories".

rm(list = ls())

source('src/lib/format.R')
main <- load_results('01_main')
cells <- load_results('02_cells')
summ <- load_results('04_summary')
smp <- load_results('13_sample')
rev <- load_results('19_revision')
headline <- main$headline
iv <- function(y) headline[outcome == y]

cat('== Paragraph 1: spending, calories and nutrition\n')
say(
    'Real food spending: %.3f percent per point (SE %.3f)',
    iv('d_lspend')$b_iv,
    iv('d_lspend')$se_iv
)
say(
    'Calories bought: %.3f percent (SE %.3f)',
    iv('d_lkcal')$b_iv,
    iv('d_lkcal')$se_iv
)
w <- load_results('06_inference')$wcr[cluster == 'state' & outcome == 'd_lkcal']
say(
    'Few-cluster (WCR-AR, states) 95%% CI for calories: [%.3f, %.3f]',
    w$lo,
    w$hi
)
kc <- summ$s[variable == 'kcal_ae_day', mean]
say(
    paste0(
        'Labeled calories per adult-equivalent ',
        'day, mean: %.0f; %.3f%% of it = %.2f kcal'
    ),
    kc,
    -w$lo,
    -w$lo / 100 * kc
)
say(
    'Energy density: %.3f kcal per 100 g (SE %.3f); mean level %.0f',
    iv('d_ed')$b_iv,
    iv('d_ed')$se_iv,
    summ$s[variable == 'kcal_100g_food', mean]
)
say('Health score: %.5f (SE %.5f)', iv('d_hs')$b_iv, iv('d_hs')$se_iv)
h <- rev$hd
for (y in c('d_lkcal', 'd_ed', 'd_hs')) {
    say(
        'TOST %s: margin %.4f, p = %s',
        y,
        h[outcome == y, margin],
        fmt_p(h[outcome == y, p_tost])
    )
}
say('Staple-price benchmark (H0: calories = -0.5): t = %.1f', rev$t_meta$t)
say(
    'Staple-price benchmark (H0: calories = -0.2): t = %.1f',
    (h[outcome == 'd_lkcal', b] + 0.2) / h[outcome == 'd_lkcal', se]
)
cat('Health-score components (2SLS x100, SE x100, p):\n')
print(
    rev$rc[, .(
        outcome,
        b100 = round(b100, 3),
        se100 = round(se100, 3),
        p = round(p, 3)
    )]
)

cat('\n== Figure 2 caption: cell-level slopes (SE)\n')
g <- cells$gs
for (v in c('spend', 'kcal', 'kpu')) {
    say('%s: %.3f (%.3f)', v, g[y == v, b], g[y == v, se])
}

cat('\n== Paragraph 2: the budget identity\n')
say(
    'Calories per dollar: %.3f (SE %.3f)',
    iv('d_lkpu')$b_iv,
    iv('d_lkpu')$se_iv
)
say(
    'Households pay %.0f%% and avoid %.0f%% of the increase',
    -100 * iv('d_lkpu')$b_iv,
    100 * (1 + iv('d_lkpu')$b_iv)
)
eq <- smp$rb[spec == 'Equal weight per state-quarter']
say(
    paste0(
        'Equal weight per state-quarter: calories ',
        'per dollar %.3f; calories %.3f (SE %.3f)'
    ),
    eq[outcome == 'd_lkpu', b],
    eq[outcome == 'd_lkcal', b],
    eq[outcome == 'd_lkcal', se]
)

cat('\n== Paragraph 3: OLS\n')
say(
    'OLS: spending %.3f, calories per dollar %.3f, calories %.3f',
    iv('d_lspend')$b_ols,
    iv('d_lkpu')$b_ols,
    iv('d_lkcal')$b_ols
)
say(
    '2SLS / OLS, calories per dollar: %.1f; spending: %.1f',
    iv('d_lkpu')$b_iv / iv('d_lkpu')$b_ols,
    iv('d_lspend')$b_iv / iv('d_lspend')$b_ols
)
cat('OLS and 2SLS by base-basket size (Fig. S3):\n')
print(main$reliability[, .(
    items,
    est_type,
    est = round(est, 3),
    se = round(se, 3)
)])
