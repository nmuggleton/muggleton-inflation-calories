# Is the result driven by price noise shared by exposure and the instrument?
# Exposure prices each basket at state unit values built from other panel
# households' purchases, and the instrument is the mean exposure of other
# households in the state and quarter, built from the same purchases.

rm(list = ls())
library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

int <- Sys.getenv('WP2_INT')
stopifnot(nzchar(int), dir.exists(file.path(int, 'price_split')))

outcomes <- c('d_lkpu', 'd_lspend', 'd_lkcal')
gate <- f1_gate(c(outcomes, 'infl_laspeyres', 'f_idx_valid'))
cat('sample flow: gate', nrow(gate), 'household-quarters\n')

# Households' own prices on products bought in both years, where valid
gate[, t1_log := fifelse(
    f_idx_valid %in% TRUE & is.finite(infl_laspeyres) &
        infl_laspeyres > -100,
    100 * log1p(infl_laspeyres / 100),
    NA_real_
)]
outcomes <- c(outcomes, 't1_log')

splits <- sort(
    as.integer(
        sub('split=', '', list.files(file.path(int, 'price_split')))
    )
)
cat('  random splits:', length(splits), '\n')

fit <- function(data, y, x, inst, spec) {
    m <- feols(
        as.formula(sprintf('%s ~ 1 | half_q | %s ~ %s', y, x, inst)),
        data,
        cluster = ~state
    )
    data.table(
        spec = spec,
        outcome = y,
        b = coef(m)[[1]],
        se = se(m)[[1]],
        n = nobs(m),
        fs = coef(summary(m, stage = 1))[[inst]]
    )
}

by_split <- rbindlist(
    lapply(
        splits,
        function(k) {
            con <- wp2_con()
            half <- setDT(
                DBI::dbGetQuery(
                    con,
                    sprintf(
                        paste(
                            'SELECT household_code, yq, half, n_priced,',
                            'idx_half FROM read_parquet(',
                            "'%s/price_split/split=%d/year=*.parquet')"
                        ),
                        int,
                        k
                    )
                )
            )
            DBI::dbDisconnect(con, shutdown = TRUE)
            stopifnot(!anyDuplicated(half, by = c('household_code', 'yq')))
            d <- merge(gate, half, by = c('household_code', 'yq'))
            # Require at least 10 products priced, as in the estimation sample
            d <- d[n_priced >= 10 & is.finite(idx_half)]
            d[, x_half := 100 * (idx_half - 1)]

            # The leave-household-out mean of the own half, and the mean of the
            # other half, by state x quarter
            d[, `:=`(sum_h = sum(x_half), n_h = .N), by = .(state_q, half)]
            d[, `:=`(sum_sq = sum(x_half), n_sq = .N), by = state_q]
            d[, `:=`(
                z_same = (sum_h - x_half) / (n_h - 1),
                z_other = (sum_sq - sum_h) / (n_sq - n_h)
            )]
            d <- d[is.finite(z_same) & is.finite(z_other)]
            d[, half_q := paste(half, yq)]
            est <- rbindlist(
                lapply(
                    outcomes,
                    function(y) {
                        rbind(
                            fit(d, y, 'infl_exposure', 'z', 'main'),
                            fit(d, y, 'x_half', 'z_same', 'same half'),
                            fit(d, y, 'x_half', 'z_other', 'other half')
                        )
                    }
                )
            )
            est[, `:=`(
                split = k,
                r_full_half = d[, cor(infl_exposure, x_half)]
            )]
            cat('  split', k, ':', nrow(d), 'household-quarters\n')
            est
        }
    )
)

# Average over splits; sd_b is the standard deviation of the estimate across
# splits
est <- by_split[, .(
    b = mean(b),
    se = mean(se),
    sd_b = sd(b),
    b_min = min(b),
    b_max = max(b),
    n = mean(n),
    fs = mean(fs),
    splits = .N
), by = .(spec, outcome)]
print(dcast(est, spec ~ outcome, value.var = 'b'), digits = 3)
print(dcast(est, spec ~ outcome, value.var = 'se'), digits = 3)
print(dcast(est, spec ~ outcome, value.var = 'sd_b'), digits = 3)

save_results(
    '28_split_sample',
    est = est,
    by_split = by_split,
    flow = data.table(
        gate = F1_N,
        splits = length(splits),
        r_full_half = by_split[, mean(r_full_half)]
    )
)
