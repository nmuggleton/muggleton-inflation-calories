# How much trading down would the income effect of a food price rise predict? A
# one percent rise in the price of the basket lowers real income by the food
# share of income, in percent.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

d <- f1_gate(
    c(
        'd_lkcal', 'd_lkpu', 'd_linc', 'base_lkpu', 'base_lspend',
        'base_lkcal', 'inc_real_b', 'fpl_b', 'fpl_bin', 'hhsize_b',
        'age_b', 'college_b', 'kids_b', 'burden'
    )
)
x <- d[inc_real_b > 0 & is.finite(base_lkpu) & is.finite(burden)]
x[, `:=`(
    lppc = -base_lkpu, # log price per calorie
    linc = log(inc_real_b),
    lhh = log(hhsize_b)
)]
cat(
    'gate', nrow(d), '| with income, price per calorie and food share',
    nrow(x), '| households', uniqueN(x$household_code), '\n'
)

# OLS coefficient on `term`, with SEs clustered by state
fit <- function(formula, data, term, label) {
    m <- feols(formula, data, cluster = ~state, notes = FALSE)
    data.table(
        model = label,
        outcome = as.character(formula[[2]])[1],
        b = coef(m)[[term]],
        se = se(m)[[term]],
        n = nobs(m)
    )
}

# (1) across households --------------------------------------------------------
gradient <- rbind(
    fit(lppc ~ linc | yq, x, 'linc', 'quarter effects'),
    fit(lppc ~ linc + lhh | yq, x, 'linc', 'household size'),
    fit(
        lppc ~ linc + lhh + age_b + college_b + kids_b | yq + state,
        x,
        'linc',
        'size, age, college, children, state effects'
    ),
    fit(base_lkcal ~ linc + lhh | yq, x, 'linc', 'household size'),
    fit(base_lspend ~ linc + lhh | yq, x, 'linc', 'household size'),
    fit(
        lppc ~ linc + lhh | yq,
        x[fpl_b <= 1.85],
        'linc',
        'household size, at or below 185% of poverty line'
    )
)
cat('\n== (1) elasticity to income across households\n')
print(gradient, digits = 3)

# (2) within households (changes x100) -----------------------------------------
w <- d[is.finite(d_linc)]
w[, d_lppc := -d_lkpu]
within <- rbind(
    fit(d_lppc ~ d_linc | yq, w, 'd_linc', 'changes, quarter effects'),
    fit(d_lkcal ~ d_linc | yq, w, 'd_linc', 'changes, quarter effects')
)
cat('\n== (2) elasticity to income within households\n')
print(within, digits = 3)

# (3) food share of income -----------------------------------------------------
share <- rbind(
    x[, .(
        group = 'all',
        n = .N,
        mean = mean(burden),
        median = median(burden),
        p90 = quantile(burden, 0.9)
    )],
    x[, .(
        n = .N,
        mean = mean(burden),
        median = median(burden),
        p90 = quantile(burden, 0.9)
    ), keyby = .(group = fpl_bin)]
)
cat('\n== (3) food share of income\n')
print(share, digits = 3)

# (4) what the income effect predicts per point of basket inflation ------------
# Percent change = elasticity x food share, since a 1% rise in food prices
# lowers real income by the food share (in percent)
avoided <- 1 + load_results('01_main')$headline[outcome == 'd_lkpu', b_iv]
eta_price <- gradient[outcome == 'lppc' & model == 'household size', b]
eta_kcal <- gradient[outcome == 'base_lkcal', b]
eta_within <- within[outcome == 'd_lppc', b]
predicted <- share[group %in% c('all', '<=100%'), .(
    group,
    food_share = median,
    price_per_kcal = eta_price * median,
    price_per_kcal_within = eta_within * median,
    calories = eta_kcal * median,
    avoided = avoided,
    share_of_avoided = 100 * eta_price * median / avoided
)]
cat('\n== (4) predicted percent change per point, at the median food share\n')
print(predicted, digits = 3)

save_results(
    '39_income_benchmark',
    gradient = gradient,
    within = within,
    share = share,
    predicted = predicted
)
