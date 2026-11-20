# What does the conventional regression give? Outcomes regressed on the
# household's own paid-price inflation (its Laspeyres index over products bought
# in both years) by OLS, set against OLS and 2SLS on exposure in the same
# sample.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

outcomes <- c('d_lspend', 'd_lkcal', 'd_lkpu', 'd_ed')
d <- f1_gate(c(outcomes, 'infl_laspeyres', 'f_idx_valid', 'n_base_items'))
d[
    ,
    valid := f_idx_valid %in% TRUE & is.finite(infl_laspeyres) &
        infl_laspeyres > -100
]
cat('gate', nrow(d), '| valid own-price index', d[, sum(valid)], '\n')

# (1) OLS and 2SLS, full gate and own-price sample -----------------------------
fit <- function(data, y, rhs, spec, sample) {
    m <- feols(
        as.formula(sprintf('%s ~ %s', y, rhs)),
        data,
        cluster = ~state,
        notes = FALSE
    )
    data.table(
        sample = sample,
        spec = spec,
        outcome = y,
        b = coef(m)[[1]],
        se = se(m)[[1]],
        n = nobs(m)
    )
}
specs <- list(
    c('OLS on exposure', 'infl_exposure | yq'),
    c('2SLS on exposure', '1 | yq | infl_exposure ~ z'),
    c('OLS on own-price inflation', 'infl_laspeyres | yq'),
    c('Own-price inflation, instrumented', '1 | yq | infl_laspeyres ~ z')
)
v <- d[valid == TRUE]
est <- rbind(
    rbindlist(lapply(specs[1:2], function(s) rbindlist(lapply(
        outcomes,
        function(y) fit(d, y, s[2], s[1], 'gate')
    )))),
    rbindlist(lapply(specs, function(s) rbindlist(lapply(
        outcomes,
        function(y) fit(v, y, s[2], s[1], 'own-price sample')
    ))))
)
print(dcast(est, sample + spec ~ outcome, value.var = 'b'), digits = 3)
print(dcast(est, sample + spec ~ outcome, value.var = 'se'), digits = 3)

# Own-price inflation trimmed at p1/p99 within quarter, to limit noise from
# few-item indices
v[, c('lo', 'hi') := as.list(quantile(infl_laspeyres, c(.01, .99))), by = yq]
trim <- rbindlist(lapply(
    outcomes,
    function(y) {
        fit(
            v[infl_laspeyres >= lo & infl_laspeyres <= hi],
            y,
            'infl_laspeyres | yq',
            'OLS on own-price inflation, trimmed',
            'own-price sample'
        )
    }
))
print(trim, digits = 3)

# Moments: how the two measures relate
mom <- v[, .(
    sd_exposure = sd(infl_exposure),
    sd_own = sd(infl_laspeyres),
    cor = cor(infl_exposure, infl_laspeyres),
    mean_exposure = mean(infl_exposure),
    mean_own = mean(infl_laspeyres)
)]
print(mom, digits = 3)
own_on_exp <- feols(
    infl_laspeyres ~ infl_exposure | yq, v, cluster = ~state, notes = FALSE
)
print(coeftable(own_on_exp), digits = 3)

# (2) do bargain hunters record lower inflation? -------------------------------
hq <- sprintf(
    paste0(
        "read_parquet('%s/analysis_hh_quarter/*/*.parquet', ",
        'hive_partitioning = true)'
    ),
    wp2_paths()$final
)
vars <- c(
    'sh_deal', 'sh_pl', 'sh_ch_grocery', 'sh_ch_discount', 'sh_ch_club',
    'sh_ch_dollar', 'n_trips_food'
)
sel <- paste(vars, collapse = ', ')
ch <- wp2_sql(
    sprintf(
        "WITH h AS (SELECT household_code, yq, %s FROM %s)
   SELECT a.household_code, a.yq, %s, %s
   FROM h a
   JOIN h b
       ON a.household_code = b.household_code AND b.yq = a.yq - 10",
        sel,
        hq,
        paste(sprintf('a.%1$s - b.%1$s AS d_%1$s', vars), collapse = ', '),
        paste(sprintf('b.%1$s AS b_%1$s', vars), collapse = ', ')
    )
)
v <- merge(v, ch, by = c('household_code', 'yq'))
rm(ch)
v[, d_n_trips_food := as.numeric(d_n_trips_food)]
cat('own-price sample with margins:', nrow(v), '\n')
print(
    v[
        ,
        lapply(.SD, function(x) sd(x, na.rm = TRUE)),
        .SDcols = c(paste0('d_', vars), paste0('b_', vars))
    ], digits = 3
)
# The gap between own-price inflation and exposure (pp)
v[, gap := infl_laspeyres - infl_exposure]

margin <- function(y, rhs, spec) {
    m <- feols(
        as.formula(sprintf('%s ~ %s | yq', y, rhs)),
        v,
        cluster = ~state,
        notes = FALSE
    )
    ct <- coeftable(m)
    data.table(
        spec = spec,
        lhs = y,
        term = rownames(ct),
        b = ct[, 1],
        se = ct[, 2],
        p = ct[, 4],
        n = nobs(m)
    )
}
ch_all <- paste(paste0('d_', vars), collapse = ' + ')
bargain <- rbind(
    margin('infl_laspeyres', 'd_sh_deal', 'change in deal share alone'),
    margin('gap', 'd_sh_deal', 'change in deal share alone'),
    margin('infl_exposure', 'd_sh_deal', 'change in deal share alone'),
    margin('infl_laspeyres', ch_all, 'changes in all margins'),
    margin('gap', ch_all, 'changes in all margins'),
    margin('infl_exposure', ch_all, 'changes in all margins'),
    margin('infl_laspeyres', 'b_sh_deal', 'base-quarter deal share'),
    margin('gap', 'b_sh_deal', 'base-quarter deal share'),
    margin(
        'infl_laspeyres',
        paste('infl_exposure +', ch_all),
        'changes in all margins, given exposure'
    )
)
print(bargain, digits = 3)

# (3) does conditioning on the margins move the own-price OLS? -----------------
cond <- rbindlist(lapply(
    outcomes,
    function(y) {
        fit(
            v,
            y,
            sprintf('infl_laspeyres + %s | yq', ch_all),
            'OLS on own-price inflation, given margin changes',
            'own-price sample'
        )
    }
))
print(cond, digits = 3)

save_results(
    '33_own_price',
    est = rbind(est, trim, cond),
    moments = mom,
    bargain = bargain
)
