# Summary statistics of the estimation sample, and how I get from the full panel
# to it.

rm(list = ls())

library(data.table)

source('src/lib/helpers.R')

v <- c(
    spend_food_nomag_ae_day_real = 'Real food spend per AE-day (2019 dollars)',
    kcal_ae_day = 'Labeled kcal per AE-day',
    kcal_per_usd_real = 'kcal per real dollar',
    kcal_100g_food = 'Energy density, foods (kcal/100 g)',
    sugar = 'Sugar (g per 1,000 kcal)',
    satfat = 'Saturated fat (g per 1,000 kcal)',
    sweet = 'Sweet drinks (% of calories)',
    snack = 'Snacks and sweets (% of calories)',
    sodium = 'Sodium (mg per 1,000 kcal)',
    protein = 'Protein (g per 1,000 kcal)',
    fiber = 'Fiber (g per 1,000 kcal)',
    fv = 'Fruit and veg (% of spend)',
    health_score = 'Health score (item-SD units)',
    cov_lab = 'Labeled share of food spend',
    infl_exposure = 'Exposure inflation T2 (pp)',
    z = 'Instrument z (pp)',
    d_lspend = 'Change log real spend (x100)',
    d_lkcal = 'Change log kcal (x100)',
    d_lkpu = 'Change log kcal per dollar (x100)',
    d_ed = 'Change energy density',
    d_sugar = 'Change sugar',
    d_satfat = 'Change saturated fat',
    d_sweet = 'Change sweet drinks share (pp)',
    d_snack = 'Change snacks and sweets share (pp)',
    d_sodium = 'Change sodium',
    d_protein = 'Change protein',
    d_fiber = 'Change fiber',
    d_fv = 'Change fruit and veg share (pp)',
    d_hs = 'Change health score'
)
d <- f1_gate(
    c(
        'spend_food_nomag_ae_day_real', 'kcal_lab', 'aed_a',
        'kcal_per_usd_real', 'kcal_100g_food', 'health_score',
        'cov_lab', 'd_lspend', 'd_lkcal', 'd_lkpu', 'd_ed', 'd_hs'
    )
)
d[, kcal_ae_day := kcal_lab / aed_a]
cat('gate rows', nrow(d), '\n')

# The items in Figure 2: levels at t, and four-quarter changes (t-4 to t) with
# levels winsorised at the 1st and 99th percentiles of the estimation sample
# before differencing, as in estimate/02_cells.R.
NUT <- c(
    protein = 'protein_per_1000', fiber = 'fiber_per_1000',
    sugar = 'sugar_per_1000', satfat = 'satfat_per_1000',
    sodium = 'sodium_per_1000', fv = 'sh_fv_nomag',
    sweet = 'sh_kcal_sweet_drinks', snack = 'sh_kcal_snacks_sweets'
)
PCT <- c('fv', 'sweet', 'snack')
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
lb <- copy(lv)[, yq := yq + 10L] # the same quarter a year earlier
setnames(lb, NUT, paste0(NUT, '_b'))
nd <- merge(
    merge(d[, .(household_code, yq)], lv, by = c('household_code', 'yq')),
    lb,
    by = c('household_code', 'yq')
)
stopifnot(nrow(nd) == nrow(d))
rm(lv, lb)
for (k in names(NUT)) {
    # Winsorise both levels at the 1st and 99th percentiles before differencing
    cap <- quantile(nd[[NUT[[k]]]], c(0.01, 0.99), na.rm = TRUE)
    cap_lo <- cap[[1]]
    cap_hi <- cap[[2]]
    nd[, (paste0(
        'd_',
        k
    )) := pmin(pmax(get(NUT[[k]]), cap_lo), cap_hi) -
        pmin(pmax(get(paste0(NUT[[k]], '_b')), cap_lo), cap_hi)]
    setnames(nd, NUT[[k]], k)
}
nd[, (c(
    PCT,
    paste0('d_', PCT)
)) := lapply(.SD, `*`, 100), .SDcols = c(PCT, paste0('d_', PCT))]
d <- merge(
    d,
    nd[, c(
        'household_code',
        'yq',
        names(NUT),
        paste0('d_', names(NUT))
    ), with = FALSE],
    by = c('household_code', 'yq')
)
rm(nd)
s <- rbindlist(
    lapply(
        names(v),
        function(x) {
            y <- d[[x]]
            y <- y[is.finite(y)]
            q <- quantile(y, c(.1, .5, .9))
            k <- if (x %in% c(
                'kcal_ae_day',
                'kcal_per_usd_real',
                'kcal_100g_food',
                'sodium'
            )) {
                0L
            } else if (x %in% c('health_score', 'cov_lab', 'd_hs')) 3L else 2L
            data.table(
                variable = x,
                label = v[[x]],
                digits = k,
                n = length(y),
                mean = mean(y),
                sd = sd(y),
                p10 = q[[1]],
                p50 = q[[2]],
                p90 = q[[3]]
            )
        }
    )
)
print(s)

# The sample path from the universe of household-quarters to est_gate: the
# number of household-quarters remaining after each cumulative restriction.
u <- wp2_sql(
    sprintf(
        "SELECT count(*) n, count(DISTINCT household_code) hh
  FROM read_parquet(
      '%s/analysis_hh_quarter/*/*.parquet',
      hive_partitioning = true
  )",
        wp2_paths()$final
    )
)
fl <- f1_sql(
    "SELECT count(*) s0,
  sum((year <> 2020)::INT) s1,
  sum((year <> 2020 AND core_a AND core_b)::INT) s2,
  sum((year <> 2020 AND core_a AND core_b AND NOT partial_a
       AND NOT partial_b)::INT) s3,
  sum(est_gate::INT) s4 FROM F1"
)
cat('sample path: universe', u$n, 'household-quarters,', u$hh, 'households\n')
print(fl)
sq <- f1_sql(
    "SELECT median(n) med, quantile_cont(n, .1) p10, min(n) mn
  FROM (SELECT state, yq, count(*) n FROM F1 WHERE est_gate GROUP BY 1, 2)"
)
cat(
    'households per state-quarter: median',
    sq$med,
    '| p10',
    sq$p10,
    '| min',
    sq$mn,
    '\n'
)

save_results('04_summary', s = s, universe = u, flow = fl, cells = sq)
