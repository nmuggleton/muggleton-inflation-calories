# Numbers quoted in "Where the avoided share goes".

rm(list = ls())

source('src/lib/format.R')
rules <- load_results('35_own_price_rules')
mech <- load_results('08_mechanism')
rev <- load_results('19_revision')

cat('== Own prices vs composition (T1-valid sample)\n')
# Price changes beyond tenfold before coupons are left out
tu <- rules$est[rule == 'gross10']
say(
    'Own prices on products bought in both years: %.3f (SE %.3f)',
    tu[outcome == 'own', b],
    tu[outcome == 'own', se]
)
say(
    'Composition remainder: %.3f (SE %.3f)',
    tu[outcome == 'comp', b],
    tu[outcome == 'comp', se]
)

cat('\n== Where the gap to state prices sits (Table S28)\n')
gs <- load_results('37_price_gap_sources')
con <- gs$contrib
gap_tot <- gs$total[outcome == 'agap']
say(
    'Gap, parts that add up: %.3f (SE %.3f); as Table S27: %.3f (SE %.3f)',
    gap_tot$b,
    gap_tot$se,
    gs$total[outcome == 'gap', b],
    gs$total[outcome == 'gap', se]
)
for (k in c('ch:Discount Store', 'ch:Other', 'ch:Grocery')) {
    say(
        '%s: %.0f%% of spending, %.0f%% of the gap (%.3f, SE %.3f)',
        k,
        100 * con[grp == k, sh_spend],
        100 * con[grp == k, share_of_gap],
        con[grp == k, b],
        con[grp == k, se]
    )
}
# The two contributions are summed before rounding
dd <- con[grp %in% c('ch:Discount Store', 'ch:Other')]
say(
    'Discount, dollar, drug, and other: %.0f%% of spending, %.0f%% of gap',
    100 * dd[, sum(sh_spend)],
    100 * dd[, sum(share_of_gap)]
)
say(
    'Bought by another household at the same retailer: %.3f (SE %.3f)',
    con[grp == 'cov:1', b],
    con[grp == 'cov:1', se]
)
say(
    '  gap on those products alone: %.3f (SE %.3f)',
    gs$own[grp == 'cov:1' & outcome == 'gap', b],
    gs$own[grp == 'cov:1' & outcome == 'gap', se]
)
say(
    'Discount retailers against the state: %.3f (SE %.3f)',
    gs$parts[grp == 'covch:Discount Store|1' & outcome == 'ret_step', b],
    gs$parts[grp == 'covch:Discount Store|1' & outcome == 'ret_step', se]
)
sw <- gs$switching[years == 'all']
say(
    paste(
        'Another store of the retailer: %.2f (SE %.2f);',
        'another retailer: %.2f (SE %.2f)'
    ),
    sw[outcome == 'sh_within', b],
    sw[outcome == 'sh_within', se],
    sw[outcome == 'sh_ret', b],
    sw[outcome == 'sh_ret', se]
)

cat('\n== The within-module ladder (Tables S25, S37)\n')
print(
    mech$ivt[, .(
        outcome,
        b = round(b, 3),
        se = round(se, 3),
        p = round(p, 3),
        n
    )]
)

cat('\n== Shopping margins (Table S26)\n')
print(
    rev$rm_[, .(
        outcome,
        b = round(b, 3),
        se = round(se, 3),
        p = round(p, 3)
    )]
)
