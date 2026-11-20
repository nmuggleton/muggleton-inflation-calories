# Does the result hold for national food inflation, which the quarter fixed
# effects of the main design absorb? Quarter means of household year-on-year
# changes (households weighted equally, as in the headline estimator) are
# regressed across quarters on the national mean of exposure inflation and on
# CPI food-at-home inflation, with Newey-West standard errors, for all
# households and for households at or below the poverty line; and the 2021-23
# inflation peak is set against other quarters.

rm(list = ls())

library(data.table)
library(sandwich)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)

ys <- c('d_lkcal', 'd_lspend', 'd_lspend_nom', 'd_lkpu', 'd_ed', 'd_hs')
d <- het_load(
    c(
        'household_code', 'yq', 'year', 'state', 'infl_exposure',
        ys, 'cpi_fah_yoy', 'fpl_bin'
    )
)
cat(
    'sample flow: gate',
    nrow(d),
    'household-quarters |',
    uniqueN(d$yq),
    'quarters\n'
)
stopifnot(!anyNA(d$cpi_fah_yoy))

# CPI food-at-home inflation is one national value per quarter
stopifnot(d[, uniqueN(cpi_fah_yoy), by = yq][, all(V1 == 1)])
# Quarter means, for all households and for those at or below the poverty line:
# stack the two samples, then average by group and quarter
both <- rbind(
    copy(d)[, group := 'all'],
    d[fpl_bin == '<=100%'][, group := 'poor']
)
q <- both[
    ,
    c(
        list(n = .N, expo = mean(infl_exposure), cpi = cpi_fah_yoy[1]),
        lapply(.SD, mean, na.rm = TRUE)
    ),
    keyby = .(group, yq),
    .SDcols = ys
]
setcolorder(q, c('yq', 'n', 'expo', 'cpi', ys, 'group'))
setkey(q, NULL)
rm(both)
q[, t := as.integer(yq %/% 10) + (yq %% 10 - 1) / 4]
print(
    q[group == 'all', .(yq, n, expo, cpi, d_lkcal, d_lspend_nom, d_ed)],
    digits = 3
)

# Across quarters: the outcome on national exposure (or the CPI), with
# Newey-West standard errors (4 lags)
grid <- CJ(
    y = ys,
    x = c('expo', 'cpi'),
    group = c('all', 'poor'),
    trend = c(FALSE, TRUE),
    sorted = FALSE
)
ts <- rbindlist(
    Map(
        function(y, x, grp, trend) {
            z <- q[group == grp]
            f <- if (trend) reformulate(c(x, 't'), y) else reformulate(x, y)
            m <- lm(f, data = z)
            v <- NeweyWest(m, lag = 4, prewhite = FALSE)
            data.table(
                group = grp,
                y = y,
                x = x,
                trend = trend,
                n_q = nrow(z),
                b = coef(m)[[x]],
                se = sqrt(v[x, x]),
                r2 = summary(m)$r.squared
            )
        },
        grid$y, grid$x, grid$group, grid$trend
    )
)
ts[, `:=`(lo = b - 1.96 * se, hi = b + 1.96 * se)]
print(ts[order(group, x, trend, y)], digits = 3)

# The 2021-23 peak: quarters in which CPI food-at-home inflation was 5% or more
q[, peak := cpi >= 5]
pk <- q[, c(
    list(n_q = .N, cpi = mean(cpi), expo = mean(expo)),
    lapply(.SD, mean)
), keyby = .(group, peak), .SDcols = ys]
print(pk, digits = 3)
cat(
    'peak quarters:',
    q[group == 'all' & peak, paste(yq, collapse = ' ')],
    '\n'
)

# Calories, all households, with a linear trend, in two subsamples: excluding
# 2021 (its base quarters fall in 2020), and before 2020 only
subsamples <- list(
    'without 2021' = q[group == 'all' & yq %/% 10 != 2021],
    '2005-2019' = q[group == 'all' & yq %/% 10 < 2020]
)
sub <- rbindlist(
    lapply(
        names(subsamples),
        function(lab) {
            z <- subsamples[[lab]]
            m <- lm(d_lkcal ~ expo + t, data = z)
            v <- NeweyWest(m, lag = 4, prewhite = FALSE)
            data.table(
                sample = lab,
                n_q = nrow(z),
                b = coef(m)[['expo']],
                se = sqrt(v['expo', 'expo'])
            )
        }
    )
)
print(sub, digits = 3)

save_results('23_national', q = q, ts = ts, pk = pk, sub = sub)
