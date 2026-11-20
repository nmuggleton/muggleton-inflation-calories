# What food inflation cost households at or below the poverty line in dollars,
# 2021-23. For each household-quarter, the annual rate of base-quarter food
# spending (real spend per AE-day x AE x 365, in 2019 dollars) times exposure
# inflation gives how much more it would cost to buy the same basket at the new
# prices; multiplied by the 2SLS spending response, it gives how much more
# households actually spent.

rm(list = ls())
library(data.table)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)


hl <- load_results('01_main')$headline
b_all <- hl[outcome == 'd_lspend', b_iv]
res <- load_results('09_heterogeneity')$res
b_poor <- res[
    moderator == 'fpl_bin' & group == '<=100%' &
        outcome == 'd_lspend', b
]
stopifnot(length(b_all) == 1, length(b_poor) == 1)
cat(
    'spending response: pooled',
    round(b_all, 3),
    '| poor',
    round(b_poor, 3),
    '\n'
)

d <- het_load(c('spend_b', 'ae_b', 'inc_real_b', 'fpl_bin'))
d <- d[year %in% c(2017:2019, 2021:2023)]
cat('sample flow: gate 2017-19 and 2021-23', nrow(d), 'household-quarters\n')
d <- d[!is.na(spend_b) & !is.na(ae_b) & !is.na(fpl_bin)]
cat('  with base spending and income group', nrow(d), '\n')

d[, `:=`(
    spend_yr = spend_b * ae_b * 365,
    poor = fpl_bin == '<=100%'
)]
d[, e_norm := mean(infl_exposure[year %in% 2017:2019]), by = poor]
d[, `:=`(
    cost = infl_exposure / 100 * spend_yr,
    cost_x = (infl_exposure - e_norm) / 100 * spend_yr
)]
d[, `:=`(
    paid = b_all * cost,
    paid_x = b_all * cost_x,
    paid_g = fifelse(poor, b_poor, b_all) * cost
)]

# Means and medians by poverty status and year (2017-19 pooled)
by_year <- d[, .(
    n = .N,
    hh = uniqueN(household_code),
    expo = mean(infl_exposure),
    e_norm = e_norm[1],
    spend_yr = mean(spend_yr),
    inc = median(inc_real_b, na.rm = TRUE),
    cost = mean(cost),
    paid = mean(paid),
    paid_g = mean(paid_g),
    paid_x = mean(paid_x),
    sh_paid = median(paid / inc_real_b, na.rm = TRUE) * 100,
    sh_paid_x = median(paid_x / inc_real_b, na.rm = TRUE) * 100,
    sh_food = median(spend_yr / inc_real_b, na.rm = TRUE) * 100
),
keyby = .(
    poor,
    period = fifelse(
        year <= 2019,
        '2017-19',
        as.character(year)
    )
)
]
# The three years together: since each year's exposure is a year-on-year change,
# the sum of the annual amounts says how much more a year households were
# spending by 2023 than they would have at 2020 prices (ignoring compounding).
cum <- by_year[
    period %in% c('2021', '2022', '2023'),
    .(
        paid = sum(paid),
        paid_x = sum(paid_x),
        sh_paid = sum(sh_paid),
        sh_paid_x = sum(sh_paid_x)
    ),
    by = poor
]
print(by_year, digits = 3)
print(cum, digits = 3)

save_results(
    '24_burden',
    by_year = by_year,
    cum = cum,
    b = data.table(b_all = b_all, b_poor = b_poor)
)
