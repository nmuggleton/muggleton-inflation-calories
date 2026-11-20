# Can shelves seen only through panel purchases stand in for the shelves of
# stores the Retail Scanner does not cover (the largest mass merchandiser,
# warehouse clubs)? This compares panel-observed state ladders with Retail
# Scanner ladders of the same stores, and breaks the difference down into what
# panellists see (products, stores), how much they buy, and what they pay.

rm(list = ls())

library(data.table)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
int <- file.path(wp2_paths()$data, 'intermediate')
chk <- file.path(int, 'panel_shelf_check')
pq <- function(pattern) {
    wp2_sql(
        sprintf(
            "SELECT * FROM read_parquet('%s')",
            file.path(chk, pattern)
        )
    )
}


# 1. Where panel food spend is, and how densely the panel sees each shelf ------

cov <- pq('cov_*.parquet')
stopifnot(
    nrow(cov) > 0,
    !anyDuplicated(
        cov,
        by = c(
            'year', 'channel_type',
            'kind'
        )
    )
)
by_kind <- cov[, .(spend = sum(spend)), keyby = .(year, kind)]
by_kind[, sh := spend / sum(spend), by = year]
print(dcast(by_kind, year ~ kind, value.var = 'sh'), digits = 3)
by_chan <- cov[, .(
    sh_spend = sum(spend),
    sh_rms = sum(spend[kind == 'rms']),
    spend = sum(spend)
), keyby = .(year, channel_type)]
by_chan[
    ,
    `:=`(sh_spend = sh_spend / sum(sh_spend), sh_rms = sh_rms / spend),
    by = year
]
by_chan[, spend := NULL]
print(by_chan[sh_spend >= .005][order(year, -sh_spend)], digits = 3)
dens <- cov[
    kind != 'uncoded',
    .(
        store = sum(spend_ge5_store, na.rm = TRUE) / sum(spend),
        retailer_zip3 = sum(spend_ge5_zip3, na.rm = TRUE) / sum(spend),
        retailer_state = sum(spend_ge5_state, na.rm = TRUE) / sum(spend),
        retailer = sum(spend_ge5_ret, na.rm = TRUE) / sum(spend)
    ),
    keyby = .(year, kind)
]
stopifnot(
    dens[, all(
        store <= retailer_zip3 + 1e-9 &
            retailer_zip3 <= retailer + 1e-9
    )]
)
print(dens, digits = 3)


# 2. Panel vs RMS state ladders, and state-rung savings (06a) ------------------

lad <- pq('lad_*.parquet')
stopifnot(!anyDuplicated(lad, by = c('year', 'v')))
print(lad, digits = 3)
sav <- pq('sav_*.parquet')
stopifnot(!anyDuplicated(sav, by = c('year', 'v', 'source')))
# Every variant x source is evaluated on the same set of rows within a year, and
# spend is compared to the cent (floating-point summation order can move it by
# up to 2e-6 dollars on $26-33M; measured 2008-2018)
stopifnot(
    sav[
        ,
        uniqueN(n_rows) == 1 & uniqueN(n_hh) == 1 &
            uniqueN(round(spend, 2)) == 1,
        by = year
    ]$V1
)
sav[, gap25 := sav25_panel - sav25_rms]
print(sav[order(year, v, source)], digits = 3)


# 3. Decomposition (06b): RMS shelf -> panel shelf, one change at a time -------

dec <- pq('decomp/dec_*.parquet')
mat <- pq('decomp/mat_*.parquet')
stopifnot(
    !anyDuplicated(
        dec,
        by = c(
            'year', 'module', 'step', 'state',
            'yq'
        )
    )
)
dy <- unique(dec$year)
stopifnot(length(dy) == 1)
mods <- sort(unique(dec$module))
cat('decomposition year', dy, '|', length(mods), 'modules\n')
# 06_panel_shelf_check.sh decomposes the DECOMP_N (25) modules with the largest
# panel spend at RMS stores; a module left over from an earlier selection would
# make it more
stopifnot(length(mods) == as.integer(Sys.getenv('DECOMP_N', '25')))
match <- mat[, .(
    n_modules = .N,
    sh_cells_matched = sum(n_matched) / sum(n_panel_cells),
    sh_spend_matched = sum(panel_spend_matched) /
        sum(panel_spend),
    sh_spend_pl = sum(panel_spend_pl, na.rm = TRUE) /
        sum(panel_spend),
    sh_unmatched_pl = sum(panel_spend_unmatched_pl, na.rm = TRUE) /
        (sum(panel_spend) - sum(panel_spend_matched))
)]
print(match, digits = 3)

# r0 should match the 05a state ladder of the same module and year (except for
# the trim-boundary nondeterminism)
rms <- wp2_sql(
    sprintf(
        "SELECT key AS state, yq, module, valid, p25, p50
   FROM read_parquet('%s') WHERE reach = 'state' AND module IN (%s)",
        file.path(int, 'rms_shelf', 'lad', sprintf('year=%d', dy), '*.parquet'),
        paste(mods, collapse = ', ')
    )
)
rep0 <- merge(
    dec[step == 'r0'],
    rms,
    by = c('state', 'yq', 'module'),
    suffixes = c('', '_05a')
)
repl <- rep0[, .(
    n = .N,
    sh_valid_same = mean(valid == valid_05a),
    sh_p25_same = mean(
        abs(p25 / p25_05a - 1) < 1e-6,
        na.rm = TRUE
    ),
    med_abs_dev_p25 = median(
        abs(p25 / p25_05a - 1),
        na.rm = TRUE
    )
)]
print(repl, digits = 4)

# The ladder level of each step relative to r0, as a calorie-weighted geometric
# mean
steps <- c('r0', 'r1', 'r2', 'r3', 'pm', 'p')
w <- dcast(
    dec[step %in% steps],
    state + yq + module ~ step,
    value.var = c('p25', 'p50', 'valid', 'kcal_tot')
)
w <- w[Reduce(
    `&`,
    lapply(paste0('valid_', steps), function(v) w[[v]] %in% TRUE)
)]
dec_lad <- rbindlist(
    lapply(
        steps,
        function(s) {
            data.table(
                step = s,
                n = nrow(w),
                gm_ratio_p25 = exp(
                    sum(
                        w$kcal_tot_r0 *
                            log(w[[paste0('p25_', s)]] / w$p25_r0)
                    ) /
                        sum(w$kcal_tot_r0)
                ),
                gm_ratio_p50 = exp(
                    sum(
                        w$kcal_tot_r0 *
                            log(w[[paste0('p50_', s)]] / w$p50_r0)
                    ) /
                        sum(w$kcal_tot_r0)
                )
            )
        }
    )
)
print(dec_lad, digits = 3)

# State-rung savings on target-year (dy + 1) base baskets bought at RMS stores
# in these modules, using only rows where all six ladders are valid
base <- wp2_sql(
    sprintf(
        "WITH gk AS (
            SELECT household_code, yq - 10 AS byq, state::INTEGER AS state
               FROM read_parquet('%s', hive_partitioning = true)
               WHERE est_gate AND yq BETWEEN %d AND %d)
   SELECT h.household_code, h.yq, h.module, g.state,
          sum(h.spend) AS spend, sum(h.kcal) AS kcal
   FROM read_parquet('%s') h
   JOIN gk g ON g.household_code = h.household_code AND g.byq = h.yq
   SEMI JOIN read_parquet('%s') s ON s.store_code_uc::INTEGER = h.store_code_uc
   WHERE h.kcal > 0 AND h.spend > 0 AND h.module IN (%s)
   GROUP BY ALL",
        file.path(int, 'desc_panel', '*', '*.parquet'),
        (dy + 1) * 10 + 1,
        (dy + 1) * 10 + 4,
        file.path(
            int,
            'rms_shelf',
            'hh_store',
            sprintf('panel_year=%d.parquet', dy)
        ),
        file.path(
            int,
            'rms',
            'stores',
            sprintf('panel_year=%d', dy),
            'data.parquet'
        ),
        paste(mods, collapse = ', ')
    )
)
stopifnot(!anyDuplicated(base, by = c('household_code', 'yq', 'module')))
n_base <- nrow(base)
base <- merge(base, w, by = c('state', 'yq', 'module'))
cat(
    'decomposition base rows:',
    n_base,
    '| with all six ladders valid:',
    nrow(base),
    '| households:',
    uniqueN(base$household_code),
    '\n'
)
dec_sav <- rbindlist(
    lapply(
        steps,
        function(s) {
            data.table(
                step = s,
                n_rows = nrow(base),
                sav25 = sum(pmax(
                    base$spend - base$kcal * base[[paste0('p25_', s)]],
                    0
                )) /
                    sum(base$spend),
                sav50 = sum(pmax(
                    base$spend - base$kcal * base[[paste0('p50_', s)]],
                    0
                )) /
                    sum(base$spend)
            )
        }
    )
)
dec_sav[, step_change25 := sav25 - shift(sav25)]
dec_sav[, label := c(
    'RMS shelf (all items, all stores)',
    'RMS cells of items the panel bought in the state',
    'RMS cells of item x store pairs the panel bought',
    'RMS prices, panel calories as weights',
    'Panel prices and calories, same matched cells',
    'Panel prices and calories, all panel cells'
)]
print(dec_sav, digits = 3)

save_results(
    '21_panel_shelf_check',
    by_kind = by_kind,
    by_chan = by_chan,
    dens = dens,
    lad = lad,
    sav = sav,
    match = match,
    repl = repl,
    dec_lad = dec_lad,
    dec_sav = dec_sav
)
print(sessionInfo())
