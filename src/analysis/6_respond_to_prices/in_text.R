# Numbers quoted in "Households respond to prices, whatever they report believing".

rm(list = ls())

source('src/lib/format.R')
egg <- load_results('34_egg_episode')
bel <- load_results('36_beliefs')

cat('== Egg price rise, 2022Q2-2023Q1 (Tables S45, S46)\n')
print(
    egg$est[, .(
        outcome,
        ordinary = round(ordinary, 2),
        episode = round(episode, 2),
        diff = round(diff, 2),
        se = round(se, 2)
    )]
)
print(egg$iv[, .(period, b = round(b, 2), se = round(se, 2), n)])

cat('\n== Stated beliefs (Table S47)\n')
yrs <- bel$years
say(
    '%d waves, %d-%d: %d answers from %d households',
    yrs$waves, yrs$first_year, yrs$last_year, yrs$n, yrs$households
)
say(
    'food-at-home inflation in the survey quarters: %.1f to %.1f',
    yrs$cpi_fah_min, yrs$cpi_fah_max
)
say(
    'stated change: mean %.1f, median %.0f',
    yrs$belief_mean, yrs$belief_median
)
print(
    bel$tracking[
        design %in% c('All waves', 'Within household', 'Own price index'),
        .(design, b = round(b, 3), se = round(se, 3), n)
    ]
)
print(
    bel$behave[
        design == '2SLS with belief' & outcome == 'd_lspend',
        .(term, b = round(b, 2), se = round(se, 2), n)
    ]
)
print(bel$types[, .(type, share = round(100 * share, 1))])
