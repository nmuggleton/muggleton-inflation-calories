# Combine the per-panel-year additive components in intermediate/hhq/ into one
# household x calendar-quarter outcome panel and derive the RQ4 outcome
# variables. data.table throughout. Output:
# final/hh_quarter_outcomes/year=YYYY/*.parquet (key: household_code, yq) Usage:
# Rscript src/data_prep/05_household_quarter/02_combine.R <WP2_DATA>
# <PROJECT_ROOT>

rm(list = ls())

library(data.table)
library(DBI)
library(duckdb)

args <- commandArgs(trailingOnly = TRUE)
dat <- args[1]
root <- args[2]
fin <- file.path(dat, 'final')
int <- file.path(dat, 'intermediate')
con <- dbConnect(duckdb(shared_home = FALSE))
q <- function(s) as.data.table(dbGetQuery(con, s))

# 1. sum components across panel-year files; the quarter's panel year is the
# file holding most of its spend (late-December fragments belong to the next
# panel year)
hq <- q(
    sprintf(
        "
  WITH a AS (SELECT * FROM read_parquet('%s/hhq/*.parquet')),
  py AS (
      SELECT household_code, yq,
          arg_max(panel_year, spend_all) AS panel_year,
                count(*) AS n_files
         FROM a GROUP BY 1, 2)
  SELECT py.panel_year, py.n_files, s.*
  FROM (SELECT household_code, yq, %s FROM a GROUP BY 1, 2) s
  JOIN py USING (household_code, yq)",
        int,
        paste(
            sprintf(
                'sum(%1$s) AS %1$s',
                setdiff(
                    q(
                        sprintf(
                            paste0(
                                'DESCRIBE SELECT * FROM ',
                                "read_parquet('%s/hhq/*.parquet')"
                            ),
                            int
                        )
                    )$column_name,
                    c('household_code', 'panel_year', 'yq')
                )
            ),
            collapse = ', '
        )
    )
)
setkey(hq, household_code, yq)
# SQL sum() over an empty FILTER returns NULL, not 0.
for (v in setdiff(
    names(hq),
    c(
        'household_code', 'yq', 'panel_year',
        'n_files'
    )
)) {
    i <- which(is.na(hq[[v]]))
    if (length(i)) set(hq, i, v, if (is.integer(hq[[v]])) 0L else 0)
}
stopifnot(!anyNA(hq$spend_food), !anyNA(hq$spend_ch_dollar))

# 2. calendar days in the quarter and fragment flag ----------------------------
hq[, `:=`(year = yq %/% 10L, qtr = yq %% 10L)]
hq[, qstart := as.IDate(sprintf('%d-%02d-01', year, 3L * qtr - 2L))]
hq[, qend := as.IDate(
    ifelse(
        qtr == 4L,
        sprintf('%d-01-01', year + 1L),
        sprintf('%d-%02d-01', year, 3L * qtr + 1L)
    )
) - 1L]
hq[, days_q := as.integer(qend - qstart) + 1L]
hy <- q(
    sprintf(
        "SELECT household_code, panel_year, first_trip_date,
                 last_trip_date FROM '%s/household_year.parquet'",
        fin
    )
)
hq <- merge(hq, hy, by = c('household_code', 'panel_year'), all.x = TRUE)
# Number of days of the quarter that fall inside the household's observed
# reporting window
hq[, days_obs := pmax(
    0L,
    as.integer(
        pmin(qend, as.IDate(last_trip_date)) -
            pmax(qstart, as.IDate(first_trip_date))
    ) + 1L
)]
hq[, f_partial_q := days_obs < 0.75 * days_q]

# 3. outcome variables ---------------------------------------------------------
sdiv <- function(a, b) fifelse(is.na(b) | b <= 0, NA_real_, a / b)
hq[, `:=`(
    cov_lab = sdiv(spend_lab, spend_food_nomag),
    cov_lab_pl = sdiv(spend_lab_pl, spend_pl),
    cov_fb = sdiv(spend_fb, spend_food_nomag),
    kcal_per_usd = sdiv(kcal_lab, spend_lab),
    kcal_100g_food = 100 * sdiv(kcal_lab_g, grams_lab),
    kcal_100ml_drink = 100 * sdiv(kcal_lab_ml, ml_lab),
    sugar_per_1000 = 1000 * sdiv(sugar_g, kcal_w_sugar),
    satfat_per_1000 = 1000 * sdiv(satfat_g, kcal_w_satfat),
    sodium_per_1000 = 1000 * sdiv(sodium_mg, kcal_w_sodium),
    fiber_per_1000 = 1000 * sdiv(fiber_g, kcal_w_fiber),
    protein_per_1000 = 1000 * sdiv(protein_g, kcal_w_protein),
    sh_pl = sdiv(spend_pl, spend_food_nomag),
    sh_deal = sdiv(spend_deal, spend_food),
    sh_coupon = sdiv(coupon_value, spend_food + coupon_value),
    sh_food_in_all = sdiv(spend_food, spend_all),
    sh_ch_grocery = sdiv(spend_ch_grocery, spend_food),
    sh_ch_discount = sdiv(spend_ch_discount, spend_food),
    sh_ch_club = sdiv(spend_ch_club, spend_food),
    sh_ch_dollar = sdiv(spend_ch_dollar, spend_food),
    sh_ch_convenience = sdiv(spend_ch_convenience, spend_food),
    sh_ch_online = sdiv(spend_ch_online, spend_food),
    sh_fv_nomag = sdiv(spend_fg_fv_nomag, spend_food_nomag),
    sh_kcal_fv = sdiv(kcal_fg_fv, kcal_fb),
    sh_kcal_sweet_drinks = sdiv(kcal_fg_drinks_sweet, kcal_fb),
    sh_kcal_snacks_sweets = sdiv(kcal_fg_snacks_sweets, kcal_fb)
)]
fg <- grep('^spend_fg_', names(hq), value = TRUE)
fg <- setdiff(fg, 'spend_fg_fv_nomag')
for (v in fg) {
    set(
        hq,
        j = sub('^spend_fg_', 'sh_fg_', v),
        value = hq[, sdiv(get(v), spend_food)]
    )
}
hq[, sh_takeout := sdiv(spend_fg_takeout, spend_food)]

# 3b.
wins <- function(x, ok) {
    lo <- quantile(x[ok], 0.01, na.rm = TRUE)
    hi <- quantile(
        x[ok],
        0.99,
        na.rm = TRUE
    )
    pmin(pmax(x, lo), hi)
}
for (v in c(
    'kcal_per_usd', 'sodium_per_1000', 'sugar_per_1000',
    'kcal_100ml_drink'
)) {
    hq[, (paste0(v, '_w')) := wins(get(v), !f_partial_q), by = year]
}

# 4. health score: z-scores against the pooled 2019 household-quarter
#    distribution (fixed reference), complete quarters with >= 50% coverage
comp <- c(
    fiber = 1, protein = 1, fv = 1, sugar = -1, satfat = -1,
    sodium = -1
)
hq[, fv_comp := sh_fv_nomag]
src <- c(
    fiber = 'fiber_per_1000', protein = 'protein_per_1000',
    fv = 'fv_comp', sugar = 'sugar_per_1000',
    satfat = 'satfat_per_1000', sodium = 'sodium_per_1000'
)
# Shares are rounded before the cut-offs, to avoid floating-point noise
# (03_sample_panel.sql)
ref <- hq[year == 2019 & !f_partial_q & round(cov_lab, 9) >= 0.5]
zs <- lapply(
    names(comp),
    function(k) {
        x <- hq[[src[k]]]
        r <- ref[[src[k]]]
        # Winsorise at the reference 1st/99th percentiles before standardising
        lo <- quantile(r, 0.01, na.rm = TRUE)
        hi <- quantile(r, 0.99, na.rm = TRUE)
        comp[k] * (pmin(
            pmax(x, lo),
            hi
        ) - mean(pmin(pmax(r, lo), hi), na.rm = TRUE)) /
            sd(pmin(pmax(r, lo), hi), na.rm = TRUE)
    }
)
names(zs) <- paste0('z_', names(comp))
hq[, (names(zs)) := zs]
hq[, health_score := rowMeans(.SD), .SDcols = names(zs)]
hq[, fv_comp := NULL]

dbWriteTable(con, 'hq', hq, overwrite = TRUE)
# One file per calendar year, since WebDAV rejects single files above 2 GB
out <- file.path(fin, 'hh_quarter_outcomes')
unlink(out, recursive = TRUE)
dbExecute(
    con,
    sprintf(
        "COPY (SELECT * REPLACE (CAST(year AS INTEGER) AS year)
                        FROM hq) TO '%s' (FORMAT parquet, COMPRESSION zstd,
                        PARTITION_BY (year))",
        out
    )
)
cat(
    sprintf(
        'hh_quarter_outcomes: %d rows, %d households, yq %d-%d\n',
        nrow(hq),
        uniqueN(hq$household_code),
        min(hq$yq),
        max(hq$yq)
    )
)
dbDisconnect(con, shutdown = TRUE)
