# Next, a check on the main result using state averages. The instrument is
# almost identical for every household in a state in a given quarter, so all the
# information it carries sits in one number per state-quarter.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)

# 1. Estimation sample ---------------------------------------------------------

d <- f1_sql(
    "SELECT household_code, yq, year, qtr, state, infl_exposure,
        d_lkpu, d_lspend, d_lkcal, d_ed, d_hs, d_cov, kcal_100g_food,
        health_score, cov_lab, fpl_b, hhsize_b
    FROM F1
    WHERE est_gate"
)
stopifnot(
    nrow(d) == F1_N,
    !anyNA(d$infl_exposure),
    !anyNA(d$state),
    all(d$yq == 10 * d$year + d$qtr),
    !anyNA(d$d_lkpu)
)

# Observations with a non-missing value for each outcome
say('sample: %d household-quarters', nrow(d))
for (v in c('d_lspend', 'd_lkcal', 'd_ed', 'd_hs', 'd_cov')) {
    say('  with %s: %d', v, sum(!is.na(d[[v]])))
}

# Energy density, the health score, and labelled coverage are measured in SD
# units (the SD of the level across household-quarters in the estimation sample)
sds <- c(
    ed = sd(d$kcal_100g_food, na.rm = TRUE),
    hs = sd(d$health_score, na.rm = TRUE),
    cov = sd(d$cov_lab, na.rm = TRUE)
)
print(sds)

# For reference, the household-level 2SLS estimates from 01_main, per 5 point
# increase in exposure, in the same SD units
headline <- load_results('01_main')$headline
for (y in c('ed', 'hs')) {
    est <- headline[outcome == paste0('d_', y)]
    say(
        '2SLS per 5 points, in SDs of the level, %s: %.4f (SE %.4f)',
        y,
        5 * est$b_iv / sds[[y]],
        5 * est$se_iv / sds[[y]]
    )
}

# 2. State-quarter cells and cell-level slopes ---------------------------------

# Data collapsed to state-quarters: n = number of households, x = average
# exposure, and the average change in each outcome
cl <- d[
    ,
    .(
        n = .N,
        x = mean(infl_exposure),
        spend = mean(d_lspend, na.rm = TRUE),
        kcal = mean(d_lkcal, na.rm = TRUE),
        kpu = mean(d_lkpu),
        ed = mean(d_ed, na.rm = TRUE) / sds[['ed']],
        hs = mean(d_hs, na.rm = TRUE) / sds[['hs']],
        cov = mean(d_cov, na.rm = TRUE) / sds[['cov']]
    ),
    keyby = .(state, year, qtr, yq)
]
stopifnot(
    nrow(cl) == expected_n('state_quarter'),
    sum(cl$n) == nrow(d),
    !anyNA(cl[, .(x, spend, kcal, kpu, ed, hs, cov)])
)
say(
    'cells: %d | households per cell: min %d, median %g, max %d',
    nrow(cl),
    min(cl$n),
    median(cl$n),
    max(cl$n)
)
say(
    'cells with fewer than 100 household-quarters: %d (%.4f of the sample)',
    sum(cl$n < 100),
    sum(cl[n < 100, n]) / nrow(d)
)

# Regressions of each outcome on x using the state-quarter averages, weighted by
# number of households, with quarter FE and SEs clustered at the state level
Y <- c(
    spend = 'Real spend',
    kcal = 'Calories bought',
    kpu = 'Calories per $',
    ed = 'Energy density',
    hs = 'Health score',
    cov = 'Labeled coverage'
)
cell_formula <- as.formula(
    sprintf('c(%s) ~ x | yq', paste(names(Y), collapse = ', '))
)
gm <- feols(cell_formula, cl, weights = ~n, cluster = ~state)
print(etable(gm, digits = 4))

gs <- rbindlist(
    lapply(
        seq_along(Y), function(k) {
            data.table(
                y = names(Y)[k],
                b = coef(gm[[k]])[['x']],
                se = se(gm[[k]])[['x']],
                n_cells = nobs(gm[[k]])
            )
        }
    )
)
gs[, lo := b - 1.96 * se]
gs[, hi := b + 1.96 * se]
print(gs)

# The budget identity at cell level: spend + calories per dollar = calories
b_of <- function(v) gs[y == v, b]
say(
    'identity: spend + kcal/$ = %.4f, against kcal = %.4f',
    b_of('spend') + b_of('kpu'),
    b_of('kcal')
)
say('share of the price rise offset (1 + b for kcal/$): %.4f', 1 + b_of('kpu'))
say(
    'coverage slope, in share units per point: %.5f (SE %.5f)',
    b_of('cov') * sds[['cov']],
    gs[y == 'cov', se] * sds[['cov']]
)
for (v in c('ed', 'hs', 'cov')) {
    say(
        '%s per 5 points, in SDs: %.4f (95%% CI %.4f to %.4f)',
        v,
        5 * b_of(v),
        5 * gs[y == v, lo],
        5 * gs[y == v, hi]
    )
}

# 3. Cells binned by exposure (Figure 2) ---------------------------------------

# Centre x and every outcome by quarter means (weighted by number of
# households), so the bins show variation within quarters
NB <- 20
vv <- c('x', names(Y))
r <- demean(X = as.matrix(cl[, ..vv]), f = cl[, .(yq)], weights = cl$n)
rr <- as.data.table(r)
setnames(rr, vv)
rr[, n := cl$n]

# Divide into 20 bins of equal household weight, ordered by x
setorder(rr, x)
rr[, bin := pmin(NB, ceiling(NB * cumsum(n) / sum(n)))]
bn <- rr[, lapply(.SD, weighted.mean, w = n), keyby = bin, .SDcols = vv]
bn[, n := rr[, sum(n), keyby = bin]$V1]
stopifnot(nrow(bn) == NB)
say(
    'binned exposure, net of quarter means: %.3f to %.3f points',
    min(bn$x),
    max(bn$x)
)
print(bn)

# The state-quarter cells, centred by quarter means, for the figure
net <- as.data.table(r)
setnames(net, paste0(vv, '_net'))
cells <- cbind(
    cl[, c('state', 'year', 'qtr', 'yq', 'n', vv), with = FALSE],
    net
)

# Averages within bins, with a weighted SE across the cells in a bin.
bl <- melt(
    rr,
    id.vars = c('bin', 'n', 'x'),
    measure.vars = names(Y),
    variable.name = 'outcome'
)
bins <- bl[
    ,
    {
        m <- weighted.mean(value, n)
        se <- sqrt(sum(n^2 * (value - m)^2)) / sum(n)
        .(
            x = weighted.mean(x, n),
            value = m,
            se = se,
            n_hhq = sum(n),
            n_cells = .N
        )
    },
    keyby = .(outcome, bin)
]
bins[, lo := value - 1.96 * se]
bins[, hi := value + 1.96 * se]

# Panel A: spending, calories and calories per dollar (log points); Panel B: the
# diet measures (SD units of the level)
in_panel_a <- bins$outcome %in% c('spend', 'kcal', 'kpu')
bins[, panel := fifelse(in_panel_a, 'A', 'B')]
bins[, label := Y[as.character(outcome)]]
bins[, units := fifelse(in_panel_a, '% (log x 100)', 'SD of level')]
bins[, outcome := factor(outcome, levels = names(Y))]
setorder(bins, outcome, bin)
stopifnot(all.equal(bins[outcome == 'spend', value], bn$spend))

# Plot each cell slope, with its 95% band, as a line through the origin across
# the binned range of x
xg <- seq(min(bn$x), max(bn$x), length.out = 50)
fit <- gs[
    ,
    .(
        x = xg,
        fit = b * xg,
        lo = b * xg - 1.96 * se * abs(xg),
        hi = b * xg + 1.96 * se * abs(xg),
        b = b,
        se = se,
        n_cells = n_cells
    ),
    by = .(outcome = y)
]
fit[, label := Y[outcome]]

# 4. Most and least exposed thirds of states, quarter by quarter (Fig. S2) -----

# Within each quarter, divide the state-quarters into terciles of
# household-quarters, ranked by exposure; a cell is assigned to the tercile that
# holds its midpoint
setorder(cl, yq, x)
cl[, cmid := (cumsum(n) - n / 2) / sum(n), by = yq]
cl[, ter := 'mid']
cl[cmid <= 1 / 3, ter := 'low']
cl[cmid > 2 / 3, ter := 'high']

# Averages in the high and low terciles, weighted by number of households, and
# their difference
tg <- cl[
    ter != 'mid',
    lapply(.SD, weighted.mean, w = n),
    by = .(yq, year, qtr, ter),
    .SDcols = c('x', 'spend', 'kcal', 'kpu')
]
tw <- dcast(
    tg,
    yq + year + qtr ~ ter,
    value.var = c('x', 'spend', 'kcal', 'kpu')
)
tw[, gx := x_high - x_low]
tw[, gspend := spend_high - spend_low]
tw[, gkcal := kcal_high - kcal_low]
tw[, gkpu := kpu_high - kpu_low]

# SEs for the confidence bands.
cse <- function(y, n) {
    w <- n / sum(n)
    G <- length(y)
    sqrt(G / (G - 1) * sum(w^2 * (y - sum(w * y))^2))
}
ts <- cl[
    ter != 'mid',
    .(
        se_x = cse(x, n),
        se_spend = cse(spend, n),
        se_kcal = cse(kcal, n),
        se_kpu = cse(kpu, n),
        G = .N
    ),
    by = .(yq, ter)
]
tsw <- dcast(
    ts,
    yq ~ ter,
    value.var = c('se_x', 'se_spend', 'se_kcal', 'se_kpu', 'G')
)
tsw <- tsw[, .(
    yq,
    se_gx = sqrt(se_x_high^2 + se_x_low^2),
    se_gspend = sqrt(se_spend_high^2 + se_spend_low^2),
    se_gkcal = sqrt(se_kcal_high^2 + se_kcal_low^2),
    se_gkpu = sqrt(se_kpu_high^2 + se_kpu_low^2),
    df = G_high + G_low - 2L
)]
n_tw <- nrow(tw)
tw <- merge(tw, tsw, by = 'yq')
stopifnot(
    nrow(tw) == n_tw,
    !anyNA(tw$se_gspend),
    all(tw$df >= 10),
    nrow(tw) == expected_n('quarter_sample'),
    all(tw$gx > 0)
)

say(
    'exposure gap between the thirds (points): mean %.3f, min %.3f, max %.3f',
    mean(tw$gx),
    min(tw$gx),
    max(tw$gx)
)
for (v in c('gspend', 'gkcal', 'gkpu')) {
    m <- lm(tw[[v]] ~ tw$gx)
    say(
        paste0(
            '%s: mean gap %.3f | corr with exposure ',
            'gap %.3f | slope %.3f | ratio of means %.3f'
        ),
        v,
        mean(tw[[v]]),
        cor(tw[[v]], tw$gx),
        coef(m)[[2]],
        mean(tw[[v]]) / mean(tw$gx)
    )
}
say(
    paste0(
        'quarters (of %d) with kcal/$ gap < 0: ',
        '%d | spend gap > 0: %d | kcal gap > 0: %d'
    ),
    nrow(tw),
    sum(tw$gkpu < 0),
    sum(tw$gspend > 0),
    sum(tw$gkcal > 0)
)
print(tw[, .(yq, gx, gspend, gkcal, gkpu)], digits = 2)

# 5. Nutrients behind the health score, in their own units (Figure 2) ----------

# Columns: nutrients in grams (sodium in mg) per 1,000 kcal, and three shares
# (fruit and veg, sweet drinks, snacks and sweets) in percentage points
NUT <- c(
    protein = 'protein_per_1000',
    fiber = 'fiber_per_1000',
    sugar = 'sugar_per_1000',
    satfat = 'satfat_per_1000',
    sodium = 'sodium_per_1000',
    fv = 'sh_fv_nomag',
    sweet = 'sh_kcal_sweet_drinks',
    snack = 'sh_kcal_snacks_sweets'
)
PCT <- c('fv', 'sweet', 'snack')

# Levels for every household-quarter
lv <- wp2_sql(
    sprintf(
        "SELECT household_code, CAST(yq AS INTEGER) AS yq, %s
    FROM read_parquet(
        '%s/hh_quarter_outcomes/*/*.parquet',
        hive_partitioning = true
    )",
        paste(NUT, collapse = ', '),
        wp2_paths()$final
    )
)

# Levels in the current quarter and in the same quarter one year earlier (the
# latter with suffix _b)
nd <- merge(
    d[, .(household_code, yq, year, state, fpl_b, hhsize_b, kcal_100g_food)],
    lv,
    by = c('household_code', 'yq')
)
lb <- copy(lv)
lb[, yq := yq + 10L]
setnames(lb, NUT, paste0(NUT, '_b'))
nd <- merge(nd, lb, by = c('household_code', 'yq'))
rm(lv, lb)
say('nutrients: %d estimation rows, %d with both quarters', nrow(d), nrow(nd))

nut_levels <- data.table(
    nutrient = names(NUT),
    mean = vapply(NUT, function(v) mean(nd[[v]], na.rm = TRUE), 0)
)
nut_levels[nutrient %in% PCT, mean := 100 * mean]

# Divide households into fifths of the base-year income-to-poverty ratio, ranked
# within year so that real income growth cannot move households between groups.
nd[
    ,
    fifth := {
        cuts <- quantile(fpl_b, c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE, type = 1)
        findInterval(fpl_b, cuts, left.open = TRUE) + 1L
    },
    by = year
]
fifth_shares <- nd[, .N, keyby = fifth]
fifth_shares[, share := round(N / sum(N), 3)]
print(fifth_shares)
INC <- c('1' = 'Bottom fifth', '3' = 'Middle fifth', '5' = 'Top fifth')

# The bottom, middle and top fifths; and, as a check, the same groups limited to
# two-person households, to make sure household size does not drive the gap (the
# top fifth is mostly small households: the top bracket, $100k or more, is set
# to $125k)
inc <- rbind(
    nd[fifth %in% c(1, 3, 5)][, sample := 'All households'],
    nd[fifth %in% c(
        1,
        3,
        5
    ) & hhsize_b == 2][, sample := 'Two-person households']
)

# Mean level by sample, income group and outcome, with SEs clustered at the
# household level since households show up in several quarters
inc_long <- melt(
    inc,
    id.vars = c('household_code', 'sample', 'fifth'),
    measure.vars = c('kcal_100g_food', NUT),
    variable.name = 'outcome',
    na.rm = TRUE
)
rm(inc)
short_name <- c(kcal_100g_food = 'ed', setNames(names(NUT), NUT))
inc_long[, outcome := short_name[as.character(outcome)]]
inc_long[outcome %in% PCT, value := 100 * value]

nut_by_income <- inc_long[
    ,
    {
        m <- mean(value)
        # residuals summed within each household
        s_h <- rowsum(value - m, household_code)
        G <- length(s_h)
        se <- sqrt(sum(s_h^2)) / .N * sqrt(G / (G - 1))
        .(mean = m, se = se, n_hhq = .N, n_hh = G)
    },
    keyby = .(sample, fifth, outcome)
]
nut_by_income[, lo := mean - 1.96 * se]
nut_by_income[, hi := mean + 1.96 * se]
nut_by_income[, income := factor(INC[as.character(fifth)], levels = INC)]
setorder(nut_by_income, sample, outcome, income)
rm(inc_long)
print(nut_by_income)

# Year-on-year change in each nutrient, with both levels winsorised at the 1st
# and 99th percentiles of the sample, as the health score does
for (k in names(NUT)) {
    q <- quantile(nd[[NUT[[k]]]], c(0.01, 0.99), na.rm = TRUE)
    now <- pmin(pmax(nd[[NUT[[k]]]], q[[1]]), q[[2]])
    base <- pmin(pmax(nd[[paste0(NUT[[k]], '_b')]], q[[1]]), q[[2]])
    nd[, (k) := now - base]
}
nd[, (PCT) := lapply(.SD, `*`, 100), .SDcols = PCT]

# Cell means, then centred by quarter means using the same household weights as
# the cells above, so they line up with cells$x_net
nc <- nd[
    ,
    lapply(.SD, mean, na.rm = TRUE),
    keyby = .(state, yq),
    .SDcols = names(NUT)
]
rm(nd)

nutrients <- merge(cells[, .(state, yq, n, x_net)], nc, by = c('state', 'yq'))
stopifnot(nrow(nutrients) == expected_n('state_quarter'), !anyNA(nutrients))
nn <- demean(
    X = as.matrix(nutrients[, names(NUT), with = FALSE]),
    f = nutrients[, .(yq)],
    weights = nutrients$n
)
nutrients[, (paste0(names(NUT), '_net')) := as.data.table(nn)]
nutrients[, (names(NUT)) := NULL]
print(nut_levels)

# Save -------------------------------------------------------------------------

save_results(
    '02_cells',
    sds = as.list(sds),
    gs = gs,
    bn = bn,
    bins = bins,
    fit = fit,
    cells = cells,
    tw = tw,
    nutrients = nutrients,
    nut_levels = nut_levels,
    nut_by_income = nut_by_income
)

print(sessionInfo())
