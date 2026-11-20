# Numbers quoted in "The poorest households pay too".

rm(list = ls())

source('src/lib/format.R')
het <- load_results('09_heterogeneity')
dyn <- load_results('07_dynamics')
reg <- load_results('10_regime')

cat('== By income-to-poverty ratio (Table S39)\n')
r <- het$res[moderator == 'fpl_bin' & outcome %in% c('d_lspend', 'd_lkcal')]
print(r[, .(group, outcome, b = round(b, 3), se = round(se, 3), n)])
say(
    paste0(
        'At or below the poverty line: spending ',
        '%.3f (SE %.3f), calories %.3f (SE %.3f)'
    ),
    r[group == '<=100%' & outcome == 'd_lspend', b],
    r[group == '<=100%' & outcome == 'd_lspend', se],
    r[group == '<=100%' & outcome == 'd_lkcal', b],
    r[group == '<=100%' & outcome == 'd_lkcal', se]
)
say(
    'Above five times the poverty line: spending %.3f, calories %.3f',
    r[group == '>500%' & outcome == 'd_lspend', b],
    r[group == '>500%' & outcome == 'd_lkcal', b]
)
w <- het$wt[moderator == 'fpl_bin']
say(
    'Equal effects across income groups: calories p = %s, spending p = %s',
    fmt_p(w[outcome == 'd_lkcal', wald_p]),
    fmt_p(w[outcome == 'd_lspend', wald_p])
)
cat('SNAP proxy and food-budget burden:\n')
print(
    het$res[
        moderator != 'fpl_bin' & outcome %in% c('d_lkcal', 'd_lkpu'),
        .(
            moderator,
            group,
            outcome,
            b = round(b, 3),
            se = round(se, 3)
        )
    ]
)
print(het$wt[, .(moderator, outcome, wald_p = round(wald_p, 3))])

cat('\n== Persistent vs transient exposure (Table S42, Fig. S5)\n')
p <- dyn$per
say(
    'Persistent component, calories: %.3f (SE %.3f)',
    p[spec == 'Annual state component (z_ann)' & outcome == 'd_lkcal', b],
    p[spec == 'Annual state component (z_ann)' & outcome == 'd_lkcal', se]
)
say(
    'Within-year deviation, calories: %.3f (SE %.3f)',
    p[spec == 'Within-state-year deviation (z_dev)' & outcome == 'd_lkcal', b],
    p[spec == 'Within-state-year deviation (z_dev)' & outcome == 'd_lkcal', se]
)

cat('\n== By national food-inflation regime (Table S43)\n')
g <- reg$res[moderator == 'regime']
say(
    paste0(
        'Calories per dollar: low-inflation ',
        'quarters %.3f, high-inflation quarters %.3f'
    ),
    g[group == 'Low' & outcome == 'd_lkpu', b],
    g[group == 'High' & outcome == 'd_lkpu', b]
)
print(g[outcome == 'd_lkcal', .(group, b = round(b, 3), se = round(se, 3))])
