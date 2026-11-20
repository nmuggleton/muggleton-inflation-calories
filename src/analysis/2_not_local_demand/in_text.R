# Numbers quoted in "The response is not local demand or shared measurement".

rm(list = ls())

source('src/lib/format.R')
iden <- load_results('05_identification')
inf <- load_results('06_inference')
dyn <- load_results('07_dynamics')
lev <- load_results('11_leverage')
loc <- load_results('15_local_demand')
bls <- load_results('16_bls_cpi')
oth <- load_results('17_other_prices')
mwa <- load_results('18_methods_audit')
ss <- load_results('14_shiftshare')

cat('== Balance on local economic shocks (Table S14)\n')
print(loc$bal[, .(var, b = round(b, 3), se = round(se, 3), p = round(p, 3))])
say(
    'p-values excluding house prices: %.2f to %.2f; house prices p = %.3f',
    min(loc$bal[var != 'd_hpi', p]),
    max(loc$bal[var != 'd_hpi', p]),
    loc$bal[var == 'd_hpi', p]
)
say(
    'Joint Wald, household level: p = %.3f | state-quarter level: p = %.3f',
    loc$joint_hh$p,
    loc$joint_sq$p
)
say('Minimum wage on z: %.3f (SE %.3f)', mwa$mw_balance$b, mwa$mw_balance$se)

cat('\n== Controls and fixed effects (rows of Table 1)\n')
print(
    loc$est[
        outcome %in% c('d_lkpu', 'd_lkcal'),
        .(spec, outcome, b = round(b, 3), se = round(se, 3))
    ]
)
print(
    oth$est[
        outcome %in% c('d_lkpu', 'd_lkcal'),
        .(spec, outcome, b = round(b, 3), se = round(se, 3))
    ]
)
say(
    'Food away from home control, calories: %.3f (SE %.3f)',
    oth$est[spec == '+ food away from home' & outcome == 'd_lkcal', b],
    oth$est[spec == '+ food away from home' & outcome == 'd_lkcal', se]
)
print(
    iden$res[
        spec %in% c(
            'State FE + division x quarter FE',
            'Neighbor-state instrument (leave-own-state division)'
        ) &
            outcome %in% c('d_lkpu', 'd_lspend', 'd_lkcal'),
        .(spec, outcome, b = round(b, 3), se = round(se, 3))
    ]
)

cat('\n== BLS instrument (Fig. S4)\n')
e <- bls$est[model == 'BLS CPI, all areas']
say(
    'Calories per dollar %.3f (AR 95%% CI [%.3f, %.3f])',
    e[outcome == 'd_lkpu', b],
    e[outcome == 'd_lkpu', ar_lo],
    e[outcome == 'd_lkpu', ar_hi]
)
say(
    'Spending %.3f; calories %.3f (AR 95%% CI [%.3f, %.3f])',
    e[outcome == 'd_lspend', b],
    e[outcome == 'd_lkcal', b],
    e[outcome == 'd_lkcal', ar_lo],
    e[outcome == 'd_lkcal', ar_hi]
)

cat('\n== Dynamics: lead and lagged-change controls\n')
r <- iden$res[spec == 'T2 | lead control' & outcome == 'd_lkpu']
say('Calories per dollar controlling for the lead: %.3f', r$b)
cc <- dyn$cond[spec == 'Lag sample, + own lagged change + zb(t-4)']
say(
    paste0(
        'Calories per dollar controlling for own ',
        'lagged change: %.3f; calories %.3f (SE %.3f)'
    ),
    cc[outcome == 'd_lkpu', b],
    cc[outcome == 'd_lkcal', b],
    cc[outcome == 'd_lkcal', se]
)
print(
    dyn$pre[
        outcome %in% c('d_lkpu', 'd_lspend'),
        .(spec, outcome, b = round(b, 3), se = round(se, 3))
    ]
)

cat('\n== Few-cluster inference (Table S4)\n')
print(
    inf$wcr[, .(
        outcome,
        cluster,
        G,
        p0 = round(p0, 4),
        lo = round(lo, 3),
        hi = round(hi, 3)
    )]
)
print(
    inf$ri[, .(
        outcome,
        b0,
        p_path = round(p_path, 4),
        p_withinq = round(p_withinq, 4)
    )]
)
l <- lev$loo_sum[spec == 'Leave one state out' & outcome == 'd_lkpu']
say('Leave one state out, calories per dollar: [%.3f, %.3f]', l$min, l$max)

cat('\n== Shift-share alternative (SI Fig. S7)\n')
print(
    ss$dyn[
        outcome == 'd_lkcal',
        .(
            instrument,
            horizon,
            b_scaled = round(b_scaled, 3),
            se_scaled = round(se_scaled, 3),
            p = round(p, 4)
        )
    ]
)
