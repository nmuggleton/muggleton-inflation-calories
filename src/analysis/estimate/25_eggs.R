# Do households respond to a large, salient price rise? Egg prices rose by more
# than half in 2022-23 (avian flu), far more than the small, diffuse basket
# price rises behind the main design.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)


# Find the fresh-egg products --------------------------------------------------

int <- Sys.getenv('WP2_INT')
fin <- wp2_paths()$final
stopifnot(nzchar(int), dir.exists(file.path(int, 'hh_upc_q')))

con <- wp2_con()

# Prior to 2021, fresh eggs are product module 4100, sold by the count (CT). The
# number of eggs in a pack is the pack size times the number of packs.
DBI::dbExecute(
    con,
    sprintf(
        "
    CREATE TEMP TABLE egg_products_to_2020 AS
    SELECT
        upc,
        upc_ver_uc,
        size1_amount * coalesce(multi, 1) AS eggs_per_pack
    FROM read_parquet('%s/dim_products_b1.parquet')
    WHERE product_module_code = 4100
        AND size1_units = 'CT'
        AND size1_amount > 0
    ",
        fin
    )
)

# From 2021, fresh eggs are the category CHICKEN EGGS, and the egg count is
# listed in the size text (e.g. '12 COUNT').
DBI::dbExecute(
    con,
    sprintf(
        "
    CREATE TEMP TABLE egg_products_from_2021 AS
    SELECT
        upc,
        panel_year,
        TRY_CAST(regexp_extract(total_size, '^([0-9.]+) COUNT$', 1) AS DOUBLE)
            * coalesce(multi, 1) AS eggs_per_pack
    FROM read_parquet('%s/dim_products_b2.parquet')
    WHERE super_category = 'EGGS'
        AND category = 'CHICKEN EGGS'
    ",
        fin
    )
)

# Exclude products for which the number of eggs cannot be identified
DBI::dbExecute(
    con,
    'DELETE FROM egg_products_from_2021
     WHERE eggs_per_pack IS NULL OR eggs_per_pack <= 0'
)

n_to_2020 <- DBI::dbGetQuery(
    con,
    'SELECT count(*) FROM egg_products_to_2020'
)[[1]]
n_from_2021 <- DBI::dbGetQuery(
    con,
    'SELECT count(*) FROM egg_products_from_2021'
)[[1]]
cat('egg products, 2004-2020:', n_to_2020, '\n')
cat('egg product-years, 2021 on:', n_from_2021, '\n')


# Egg purchases by household and quarter ---------------------------------------

# Keep only the purchase lines of fresh eggs. Joining to the small egg tables
# first lets DuckDB filter the purchase file before it reads the rest.
DBI::dbExecute(
    con,
    sprintf(
        "
    CREATE TEMP VIEW household_egg_lines AS
    SELECT x.*, a.eggs_per_pack
    FROM read_parquet('%1$s/hh_upc_q/*.parquet', hive_partitioning = true) x
    JOIN egg_products_to_2020 a
        ON a.upc = x.upc AND a.upc_ver_uc = x.upc_ver_uc
    WHERE x.panel_year <= 2020
    UNION ALL
    SELECT x.*, b.eggs_per_pack
    FROM read_parquet('%1$s/hh_upc_q/*.parquet', hive_partitioning = true) x
    JOIN egg_products_from_2021 b
        ON b.upc = x.upc AND b.panel_year = x.panel_year
    WHERE x.panel_year >= 2021
    ",
        int
    )
)

hh <- setDT(
    DBI::dbGetQuery(
        con,
        '
    SELECT
        household_code,
        yq,
        sum(qty * eggs_per_pack) AS eggs,
        sum(spend) AS spend
    FROM household_egg_lines
    GROUP BY household_code, yq
    '
    )
)


# State egg price index --------------------------------------------------------

# The same egg purchase lines, from the state totals
DBI::dbExecute(
    con,
    sprintf(
        "
    CREATE TEMP VIEW state_egg_lines AS
    SELECT x.*, a.eggs_per_pack
    FROM read_parquet('%1$s/state_upc_q/*.parquet', hive_partitioning = true) x
    JOIN egg_products_to_2020 a
        ON a.upc = x.upc AND a.upc_ver_uc = x.upc_ver_uc
    WHERE x.panel_year <= 2020
    UNION ALL
    SELECT x.*, b.eggs_per_pack
    FROM read_parquet('%1$s/state_upc_q/*.parquet', hive_partitioning = true) x
    JOIN egg_products_from_2021 b
        ON b.upc = x.upc AND b.panel_year = x.panel_year
    WHERE x.panel_year >= 2021
    ",
        int
    )
)

# Laspeyres index: each product's price change relative to the same quarter one
# year earlier, using its spending in that earlier quarter as the weight.
st <- setDT(
    DBI::dbGetQuery(
        con,
        '
    WITH prices AS (
        SELECT
            fips_state_code AS state,
            yq,
            upc,
            coalesce(upc_ver_uc, 0) AS ver,
            panel_year >= 2021 AS new_coding,
            sum(spend) AS spend,
            sum(spend) / sum(qty) AS price
        FROM state_egg_lines
        WHERE qty > 0 AND spend > 0
        GROUP BY ALL
    )
    SELECT
        now.state,
        now.yq,
        count(*) AS n_upc,
        100 * ln(
            sum(base.spend * now.price / base.price) / sum(base.spend)
        ) AS infl_egg
    FROM prices now
    JOIN prices base
        ON base.state = now.state
        AND base.upc = now.upc
        AND base.ver = now.ver
        AND base.new_coding = now.new_coding
        AND base.yq = now.yq - 10
    GROUP BY now.state, now.yq
    ORDER BY now.state, now.yq
    '
    )
)

DBI::dbDisconnect(con, shutdown = TRUE)

cat('state-quarters with an egg index:', nrow(st), '\n')
cat('median products per index:', median(st$n_upc), '\n')

# Exclude state-quarters whose index rests on fewer than five products
st <- st[n_upc >= 5]
cat('  with at least 5 products:', nrow(st), '\n')


# Estimation sample ------------------------------------------------------------

d <- f1_gate(c('household_code', 'yq', 'year', 'state'))
cat('sample flow: gate', nrow(d), 'household-quarters\n')

# The egg index cannot be matched in 2020 (outside the estimation sample) or in
# 2021 (the quarter one year earlier uses the earlier product coding)
d <- d[year %in% c(2005:2019, 2022:2024)]
cat('  2005-2019 and 2022-2024:', nrow(d), '\n')

# Merge eggs bought this quarter (1) and one year earlier (0), and the index
d[, yq0 := yq - 10L]
d[hh, on = .(household_code, yq), `:=`(e1 = i.eggs, s1 = i.spend)]
d[hh, on = .(household_code, yq0 = yq), `:=`(e0 = i.eggs, s0 = i.spend)]
d[st, on = .(state, yq), infl_egg := i.infl_egg]

# Keep only households that bought eggs in the same quarter one year earlier, in
# states where an index can be built
d <- d[!is.na(infl_egg) & !is.na(e0) & e0 > 0]
cat('  base-quarter egg buyers in states with an index:', nrow(d), '\n')

# Outcomes: an indicator for whether the household still buys eggs (in percent),
# and for those that do, the log change in the number of eggs bought and in the
# price paid per egg
d[, buy := 100 * as.numeric(!is.na(e1) & e1 > 0)]
d[buy > 0, d_leggs := 100 * log(e1 / e0)]
d[buy > 0, d_lpegg := 100 * log((s1 / e1) / (s0 / e0))]


# Ordinary quarters and egg price shocks ---------------------------------------

# A shock quarter is one in which the mean state egg index is at least 30
# percent
nat <- d[, .(nat_egg = mean(infl_egg)), keyby = yq]
nat[, shock := nat_egg >= 30]
print(nat[shock == TRUE])

d[nat, on = 'yq', shock := i.shock]

# Separate slopes on state egg inflation in ordinary quarters and in shock
# quarters
d[, x_norm := infl_egg * !shock]
d[, x_shock := infl_egg * shock]

# How much state egg inflation varies within a quarter, in each kind of quarter
d[, infl_egg_within := infl_egg - ave(infl_egg, yq)]
cat(
    'SD of egg inflation across states within quarter: ordinary',
    round(d[shock == FALSE, sd(infl_egg_within)], 1),
    '| shock',
    round(d[shock == TRUE, sd(infl_egg_within)], 1),
    '\n'
)


# Reduced form and first stage -------------------------------------------------

# Each outcome on state egg inflation, with quarter fixed effects and standard
# errors clustered at the state level
rf <- rbindlist(
    lapply(
        c('d_lpegg', 'd_leggs', 'buy'),
        function(y) {
            m <- feols(
                as.formula(paste(y, '~ x_norm + x_shock | yq')),
                d,
                cluster = ~state
            )
            ct <- coeftable(m)
            data.table(
                outcome = y,
                term = rownames(ct),
                b = ct[, 1],
                se = ct[, 2],
                n = nobs(m)
            )
        }
    )
)
print(rf, digits = 3)

# Does the response of eggs bought differ in shock quarters? The shock slope
# minus the ordinary slope, from the same regression
m_eggs <- feols(d_leggs ~ x_norm + x_shock | yq, d, cluster = ~state)
v <- vcov(m_eggs)

dq <- data.table(
    diff = coef(m_eggs)[['x_shock']] - coef(m_eggs)[['x_norm']],
    se = sqrt(
        v['x_shock', 'x_shock'] + v['x_norm', 'x_norm'] -
            2 * v['x_shock', 'x_norm']
    )
)
dq[, p := 2 * pt(-abs(diff / se), df = 48)]
print(dq, digits = 3)


# Elasticities of eggs bought with respect to the price paid -------------------

# From the reduced forms: eggs bought over price paid, in each kind of quarter
el <- dcast(
    rf[outcome %in% c('d_lpegg', 'd_leggs')],
    term ~ outcome,
    value.var = 'b'
)
el[, elast := d_leggs / d_lpegg]
print(el, digits = 3)

# By 2SLS: the log number of eggs bought on the log price paid, instrumented by
# state egg inflation, separately in ordinary and shock quarters
iv <- rbindlist(
    lapply(
        c(FALSE, TRUE),
        function(s) {
            m <- feols(
                d_leggs ~ 1 | yq | d_lpegg ~ infl_egg,
                d[shock == s],
                cluster = ~state
            )
            data.table(
                shock = s,
                b = coef(m)[[1]],
                se = se(m)[[1]],
                n = nobs(m),
                f1 = fitstat(m, 'ivf')[[1]]$stat
            )
        }
    )
)
print(iv, digits = 3)


# Save -------------------------------------------------------------------------

save_results(
    '25_eggs',
    rf = rf,
    iv = iv,
    nat = nat,
    dq = dq,
    st = st[, .(state, yq, n_upc, infl_egg)]
)
