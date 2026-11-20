# Controls, moderators, fixed-effect groupings and treatments merged onto the
# household-quarter outcome panel. Inputs : final/hh_quarter_outcomes/
# (src/data_prep/05_household_quarter/02_combine.R)
# intermediate/price_idx/*.parquet (src/data_prep/06_prices/01_price_indices.sh)
# final/household_year.parquet, final/sample_flags.parquet
# data/raw/external/{CPIAUCNS,CUUR0000SAF11,CUUR0000SETB01}.csv,
# data/raw/external/hhs_poverty_guidelines_48states.csv Outputs:
# final/hh_quarter_treatments.parquet  (household, yq: T1-T4)
# final/analysis_hh_quarter/year=YYYY/ (everything) Usage: Rscript
# src/data_prep/07_controls/01_controls.R <WP2_DATA> <PROJECT_ROOT>

rm(list = ls())

library(data.table)
library(DBI)
library(duckdb)

args <- commandArgs(trailingOnly = TRUE)
dat <- args[1]
root <- args[2]
fin <- file.path(dat, 'final')
int <- file.path(dat, 'intermediate')
ext <- file.path(root, 'data', 'raw', 'external')
con <- dbConnect(duckdb(shared_home = FALSE))
q <- function(s) as.data.table(dbGetQuery(con, s))

hq <- q(
    sprintf(
        "SELECT * FROM read_parquet('%s/hh_quarter_outcomes/*/*.parquet',
                 hive_partitioning = true)",
        fin
    )
)
hq[, year := as.integer(year)]
n0 <- nrow(hq)


# A. Controls from household_year (merged on the quarter's panel_year) ---------

hy <- q(sprintf("SELECT * FROM '%s/household_year.parquet'", fin))
# Income code -> bracket midpoint (nominal dollars, for the calendar year
# panel_year - 2).
inc <- data.table(
    household_income = c(
        3, 4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23,
        26, 27, 28, 29, 30
    ),
    inc_lo = c(
        0, 5000, 8000, 10000, 12000, 15000, 20000, 25000, 30000, 35000,
        40000, 45000, 50000, 60000, 70000, 100000, 125000, 150000,
        200000
    ),
    inc_hi = c(
        4999, 7999, 9999, 11999, 14999, 19999, 24999, 29999, 34999,
        39999, 44999, 49999, 59999, 69999, 99999, NA, 149999, 199999,
        NA
    )
)
hy <- merge(hy, inc, by = 'household_income', all.x = TRUE)
hy[, top6_09 := panel_year %between% c(2006, 2009)]
hy[, income_mid := fcase(
    household_income == 27 & !top6_09,
    125000,
    household_income == 27 & top6_09,
    112500,
    household_income == 30,
    250000,
    default = (inc_lo + inc_hi) / 2
)]
hy[
    ,
    f_income_topcoded := household_income == 27 & !top6_09 |
        household_income == 30
]
# Harmonised top bracket, $100k+, so that categories are comparable across years
hy[, income_cat := fcase(
    inc_lo < 25000,
    '<25k',
    inc_lo < 50000,
    '25-50k',
    inc_lo < 70000,
    '50-70k',
    inc_lo < 100000,
    '70-100k',
    default = '100k+'
)]

# CPI-U annual averages for deflating (income year = panel_year - 2)
cpi <- fread(file.path(ext, 'CPIAUCNS.csv'))
setnames(cpi, c('date', 'cpi'))
cpi[, year := as.integer(substr(date, 1, 4))]
cpia <- cpi[, .(cpi = mean(cpi)), by = year]
base <- cpia[year == 2019, cpi]
hy[, income_year := panel_year - 2L]
hy <- merge(
    hy,
    cpia[, .(income_year = year, cpi_inc = cpi)],
    by = 'income_year',
    all.x = TRUE
)
hy[, income_real2019 := income_mid * base / cpi_inc]

# Household members and ages (heads plus members 1-7; births are YYYY text)
births <- c(
    'male_head_birth', 'female_head_birth',
    paste0('member_', 1:7, '_birth')
)
# Birth years are stored as text; anything that is not a number becomes NA
ages <- hy[
    ,
    lapply(.SD, function(b) panel_year - suppressWarnings(as.integer(b))),
    .SDcols = births
]
hy[, n_persons_listed := rowSums(!is.na(ages))]
hy[, n_children_u14 := rowSums(ages < 14, na.rm = TRUE)]
hy[, n_adults_14p := rowSums(ages >= 14, na.rm = TRUE)]
# OECD-modified scale: 1 for the first adult, plus 0.5 per extra person aged 14+
# and 0.3 per child under 14.
hy[, gap := pmax(0L, pmin(household_size, 9L) - n_persons_listed)]
hy[
    ,
    ae_oecd := 1 + 0.5 * pmax(0, n_adults_14p + gap - 1) +
        0.3 * n_children_u14
]
hy[n_adults_14p + gap == 0, ae_oecd := 1 + 0.3 * pmax(0, n_children_u14 - 1)]
hy[, f_ae_imputed := gap > 0]

# Ratio of income to the poverty line, and a SNAP gross-income eligibility proxy
# (income under 130% of the FPL).
pov <- fread(file.path(ext, 'hhs_poverty_guidelines_48states.csv'))
povl <- melt(
    pov,
    id.vars = c('year', 'add'),
    variable.name = 'sz',
    value.name = 'fpl'
)
povl[, sz := as.integer(sub('p', '', sz))]
hy[, sz := pmin(household_size, 8L)]
hy <- merge(
    hy,
    povl[, .(income_year = year, sz, fpl, add)],
    by = c('income_year', 'sz'),
    all.x = TRUE
)
hy[household_size > 8, fpl := fpl + add * (household_size - 8)]
hy[, fpl_ratio := income_mid / fpl]
hy[, snap_elig_proxy := fpl_ratio <= 1.3]
hy[, `:=`(gap = NULL, sz = NULL, add = NULL)]

# Heads: age band (the oldest head), education (the highest), and employment
agemid <- c(
    `1` = 22, `2` = 27, `3` = 32, `4` = 37, `5` = 42, `6` = 47,
    `7` = 52, `8` = 60, `9` = 70
)
hy[, head_age := pmax(
    agemid[as.character(male_head_age)],
    agemid[as.character(female_head_age)],
    na.rm = TRUE
)]
hy[, head_educ := pmax(
    male_head_education,
    female_head_education,
    na.rm = TRUE
)]
hy[head_educ == 0, head_educ := NA]
hy[, head_college := head_educ >= 5]
hy[, n_heads := (male_head_age > 0) + (female_head_age > 0)]
# Employment codes 1-3 mean in work
hy[
    ,
    any_head_employed := male_head_employment %in% 1:3 |
        female_head_employment %in% 1:3
]
hy[, all_heads_not_employed := !any_head_employed & n_heads > 0]
hy[, children_present := age_and_presence_of_children != 9]
hy[, `:=`(
    race_f = factor(race, 1:4, c('white', 'black', 'asian', 'other')),
    hispanic = hispanic_origin == 1
)]

ctrl <- hy[, .(
    household_code,
    panel_year,
    projection_factor,
    projection_factor_magnet,
    household_size,
    ae_oecd,
    f_ae_imputed,
    n_children_u14,
    household_income,
    income_cat,
    income_mid,
    income_real2019,
    f_income_topcoded,
    income_year,
    fpl_ratio,
    snap_elig_proxy,
    head_age,
    head_educ,
    head_college,
    n_heads,
    any_head_employed,
    all_heads_not_employed,
    children_present,
    age_and_presence_of_children,
    household_composition,
    marital_status,
    race_f,
    hispanic,
    region_code,
    fips_state_code,
    fips_county_code,
    scantrack_market_code,
    dma_code,
    panelist_zip_code,
    n_years_in_panel,
    first_panel_year,
    last_panel_year,
    in_prev_year,
    in_next_year,
    in_health_survey,
    wic_indicator_current
)]

sf <- q(sprintf("SELECT * FROM '%s/sample_flags.parquet'", fin))
ctrl <- merge(ctrl, sf, by = c('household_code', 'panel_year'), all.x = TRUE)

an <- merge(hq, ctrl, by = c('household_code', 'panel_year'), all.x = TRUE)
# The panel is about 6 GB per copy, so free what has been merged, and add later
# columns by reference (update joins) rather than with merge(), which copies the
# panel
rm(hq, hy, ctrl)
invisible(gc())
mr <- data.table(
    step = 'outcomes -> household_year controls',
    n_rows = nrow(an),
    matched = an[, sum(!is.na(ae_oecd))]
)

# per adult-equivalent per day outcomes
an[, `:=`(
    spend_food_ae_day = spend_food / (ae_oecd * pmax(days_obs, 1)),
    kcal_lab_ae_day = kcal_lab / (ae_oecd * pmax(days_obs, 1)),
    kcal_fb_ae_day = kcal_fb / (ae_oecd * pmax(days_obs, 1)),
    # Labelled kcal scaled up to all non-Magnet food spend, which assumes the
    # unlabelled spend buys the same kcal per dollar.
    kcal_scaled_ae_day = fifelse(
        round(cov_lab, 9) >= 0.5,
        kcal_lab / cov_lab /
            (ae_oecd * pmax(days_obs, 1)),
        NA_real_
    )
)]


# B. Treatments ----------------------------------------------------------------

pi <- q(sprintf("SELECT * FROM read_parquet('%s/price_idx/*.parquet')", int))
pi[, `:=`(
    infl_laspeyres = 100 * (idx_laspeyres - 1),
    infl_fisher    = 100 * (idx_fisher - 1),
    infl_exposure  = 100 * (idx_exposure - 1),
    f_idx_valid    = n_matched >= 5,
    f_exp_valid    = n_base_priced >= 10
)]
# Aggregate CPI for food at home and for gasoline, change on the same quarter a
# year earlier
qcpi <- function(f, nm) {
    d <- fread(file.path(ext, f))
    setnames(d, c('date', 'v'))
    d[, `:=`(
        year = as.integer(substr(date, 1, 4)),
        qtr = (as.integer(substr(date, 6, 7)) - 1L) %/% 3L + 1L
    )]
    d <- d[, .(v = mean(v, na.rm = TRUE)), by = .(year, qtr)]
    d[, yq := year * 10L + qtr]
    setkey(d, yq)
    d[, chg := 100 * (v / shift(v, 4) - 1)]
    setnames(d, c('v', 'chg'), paste0(nm, c('_index', '_yoy')))
    d[, .SD, .SDcols = c('yq', paste0(nm, c('_index', '_yoy')))]
}
agg <- Reduce(
    function(a, b) merge(a, b, by = 'yq'),
    list(
        qcpi('CUUR0000SAF11.csv', 'cpi_fah'),
        qcpi('CPIAUCNS.csv', 'cpi_all'),
        qcpi('CUUR0000SETB01.csv', 'cpi_gas')
    )
)
# Gasoline exposure (Magnet households, 2012 on): the base-quarter share of
# total recorded spend on the gas reference card, times the national change in
# the gas price
setkey(an, household_code, yq)
an[, gas_share := fifelse(
    spend_all > 0 & f_magnet_hh %in% TRUE &
        year >= 2012,
    spend_gas_card / spend_all,
    NA_real_
)]
# base = the same quarter one year earlier (as for the price indices)
gb <- an[!is.na(gas_share), .(
    household_code,
    yq = yq + 10L,
    gas_share_base = gas_share
)]
an[gb, gas_share_base := i.gas_share_base, on = .(household_code, yq)]
rm(gb)
tr <- merge(
    an[, .(household_code, yq, gas_share_base)],
    pi,
    by = c('household_code', 'yq'),
    all.x = TRUE
)
tr <- merge(tr, agg, by = 'yq', all.x = TRUE)
tr[, gas_exposure := gas_share_base * cpi_gas_yoy]
tr[, episode := fcase(
    yq %between% c(20074, 20084),
    '2007-08 food spike',
    yq %between% c(20111, 20114),
    '2011 spike',
    # Food-at-home CPI above about 3% a year from 2021Q2 to 2023Q3 (the episode
    # is the same when the end year is 2021)
    yq %between% c(20212, 20233),
    '2021-23 surge',
    default = 'none'
)]
dbWriteTable(con, 'tr', tr, overwrite = TRUE)
dbExecute(
    con,
    sprintf(
        "COPY tr TO '%s' (FORMAT parquet)",
        file.path(fin, 'hh_quarter_treatments.parquet')
    )
)
# Left join by reference (the keys are unique in tr, and tr and an share no
# column names)
trc <- setdiff(names(tr), c('household_code', 'yq', 'gas_share_base'))
stopifnot(
    !anyDuplicated(tr, by = c('household_code', 'yq')),
    !length(intersect(trc, names(an)))
)
# row of tr matching each row of an (NA if none), then one column at a time:
# mget() inside an update join would copy every column of an
idx <- tr[an[, .(household_code, yq)], on = .(household_code, yq), which = TRUE]
for (v in trc) set(an, j = v, value = tr[[v]][idx])
rm(idx)
rm(tr, pi)
invisible(gc())
mr <- rbind(
    mr,
    data.table(
        step = 'outcomes -> price indices (T1 valid)',
        n_rows = nrow(an),
        matched = an[, sum(f_idx_valid %in% TRUE)]
    ),
    data.table(
        step = 'outcomes -> exposure index (T2 valid)',
        n_rows = nrow(an),
        matched = an[, sum(f_exp_valid %in% TRUE)]
    )
)

# Real calorie price: kcal per 2019 dollar, deflated by the quarterly CPI for
# food at home
fah19 <- agg[yq %/% 10 == 2019, mean(cpi_fah_index)]
an[, kcal_per_usd_real := kcal_per_usd_w * cpi_fah_index / fah19]
an[, spend_food_nomag_ae_day := spend_food_nomag / (
    ae_oecd * pmax(
        days_obs,
        1
    ))]
# Real food spending, in 2019 dollars
an[
    ,
    spend_food_nomag_ae_day_real :=
        spend_food_nomag_ae_day * fah19 / cpi_fah_index
]

# FE groupings
an[, `:=`(
    fe_state_q = paste(fips_state_code, yq, sep = '_'),
    fe_market_q = paste(scantrack_market_code, yq, sep = '_'),
    fe_inc_q = paste(income_cat, yq, sep = '_')
)]
stopifnot(nrow(an) == n0, !anyDuplicated(an, by = c('household_code', 'yq')))

duckdb::duckdb_register(con, 'an', an) # zero-copy view, not a second copy
out <- file.path(fin, 'analysis_hh_quarter')
unlink(out, recursive = TRUE)
dbExecute(
    con,
    sprintf(
        "COPY an TO '%s' (FORMAT parquet, COMPRESSION zstd,
                        PARTITION_BY (year))",
        out
    )
)
cat(sprintf('analysis_hh_quarter: %d rows x %d cols\n', nrow(an), ncol(an)))
print(mr)
dbDisconnect(con, shutdown = TRUE)
