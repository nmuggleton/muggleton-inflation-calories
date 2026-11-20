# Numbers quoted in "Cheaper calories were available" and the SI section on shelves.

rm(list = ls())

source('src/lib/format.R')
mech <- load_results('08_mechanism')
chk <- load_results('21_panel_shelf_check')
pr <- load_results('22_panel_reach')
shelf <- mech$shelf
mean_shock <- mech$mean_shock
ivt <- mech$ivt
het <- mech$het
slack_t3_t1 <- mech$slack_t3_t1

cat('== Savings available on the shelf (Figure 3A, Table S29)\n')
say(
    'Saving at the state 25th percentile price per calorie: mean %.1f%%',
    shelf[grepl('p25', measure), mean]
)
say(
    'Saving at the state median price per calorie: mean %.1f%%',
    shelf[grepl('p50', measure), mean]
)
say('Mean annual basket price increase: %.1f%%', mean_shock)

cat('\n== Tier inflation under local exposure (Table S37)\n')
r <- function(y) ivt[outcome == y]
say(
    'Cheapest quarter: %.3f; dearest quarter: %.3f; difference %.3f (SE %.3f)',
    r('f_cheap')$b,
    r('f_prem')$b,
    r('f_rel')$b,
    r('f_rel')$se
)

cat('\n== By available savings (Table S38)\n')
print(het[, .(outcome, slack3, b = round(b, 3), se = round(se, 3))])
say(
    'Top minus bottom third, calories per dollar: %.3f (SE %.3f)',
    slack_t3_t1$b,
    slack_t3_t1$se
)

cat(
    '\n== Within reach',
    '(Figure 3B and C; SI tables reach, income definitions)\n'
)
p25 <- pr$reach[quantile == 25]
m <- function(l, r) p25[ladder == l & rung == r, mean]
say('Retailer where the calories were bought: %.1f%%', m('pooled', 'ret'))
say(
    paste0(
        'Pooled zip3 / county / state: %.1f%% / ',
        '%.1f%% / %.1f%% (adds %.1f to %.1f points)'
    ),
    m('pooled', 'zip3'),
    m('pooled', 'cty'),
    m('pooled', 'st'),
    min(
        m('pooled', 'zip3'),
        m('pooled', 'cty'),
        m('pooled', 'st')
    ) - m('pooled', 'ret'),
    max(
        m('pooled', 'zip3'),
        m('pooled', 'cty'),
        m('pooled', 'st')
    ) - m('pooled', 'ret')
)
say(
    paste0(
        'Best retailer in zip3: %.1f%%; in county ',
        '%.1f%%; in state %.1f%%; own retailers %.1f%%'
    ),
    m(
        'best retailer',
        'bz3'
    ),
    m('best retailer', 'bcty'),
    m('best retailer', 'bst'),
    m('best retailer', 'setx')
)
say(
    'Household-quarters on the common base: %s (of %s)',
    format(tail(pr$flow$hh_quarters, 1), big.mark = ','),
    format(pr$flow$hh_quarters[1], big.mark = ',')
)
inc <- pr$inc_def[definition == 'real2019']
say(
    'By real income, best in zip3: %.1f%% (lowest group) to %.1f%% (highest)',
    inc[series == 'best_zip3', mean][1],
    tail(
        inc[series == 'best_zip3', mean],
        1
    )
)

cat("\n== SI section \"Shelves within reach\"\n")
say(
    'RMS share of panel food spend: %s',
    paste(round(chk$by_kind[kind == 'rms', sh], 2), collapse = ', ')
)
d <- chk$dens[kind == 'coded_non_rms']
say(
    paste0(
        'Spend in a cell with >= 5 items (not in ',
        'RMS), single store: %s; retailer x state: %s'
    ),
    paste(
        round(d$store, 2),
        collapse = ', '
    ),
    paste(round(d$retailer_state, 2), collapse = ', ')
)
say(
    'National brands, panel p25 / RMS p25: %s',
    paste(round(chk$lad[v == 'nopl', gm_ratio_p25], 2), collapse = ', ')
)
sv <- chk$sav[source == 'full']
say(
    paste0(
        'Savings gap, panel minus RMS (points), ',
        'all purchases: %s; national brands: %s'
    ),
    paste(round(100 * sv[v == 'all', gap25], 1), collapse = ', '),
    paste(round(100 * sv[v == 'nopl', gap25], 1), collapse = ', ')
)
say(
    'Store brands, share of unmatched panel spend: %.2f',
    chk$match$sh_unmatched_pl
)
sb <- pr$set_by_n
say(
    'Plain minimum over own retailers: %.1f%% (1 retailer) to %.1f%% (6+)',
    sb[n_ret_cat == '1', set_naive],
    sb[n_ret_cat == '6+', set_naive]
)
say(
    'Median own share of the county shelf: %.3f',
    pr$own[sample == 'all', med_own_cty]
)
