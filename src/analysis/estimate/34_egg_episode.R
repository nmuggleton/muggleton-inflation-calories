# What do households do when a price rise is national and impossible to miss?
# Egg prices rose by more than half between 2022Q2 and 2023Q1 (avian flu).
# 25_eggs.R compares states within a quarter; this script takes the episode
# itself, a single national shock, and compares it with ordinary quarters.

rm(list = ls())

library(data.table)
library(fixest)
library(sandwich)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

int <- Sys.getenv('WP2_INT')
fin <- wp2_paths()$final
stopifnot(nzchar(int), dir.exists(file.path(int, 'hh_upc_q')))
hh_upc <- sprintf(
    "read_parquet('%s/hh_upc_q/*.parquet', hive_partitioning = true)",
    int
)
episode <- c(20222L, 20223L, 20224L, 20231L)

con <- wp2_con()

# (1) fresh-egg purchase lines -------------------------------------------------
# Before 2021, fresh eggs are product module 4100, sold by the count; from 2021
# they are the category CHICKEN EGGS, with the count in the size text.
DBI::dbExecute(
    con,
    sprintf(
        "CREATE TEMP TABLE egg_lines AS
   WITH b1 AS (
       SELECT upc,
              upc_ver_uc,
              size1_amount * coalesce(multi, 1) AS eggs_per_pack,
              is_private_label AS store_brand
       FROM read_parquet('%2$s/dim_products_b1.parquet')
       WHERE product_module_code = 4100
           AND size1_units = 'CT'
           AND size1_amount > 0
   ),
   b2 AS (
       SELECT upc,
              panel_year,
              TRY_CAST(
                  regexp_extract(total_size, '^([0-9.]+) COUNT$', 1) AS DOUBLE
              ) * coalesce(multi, 1) AS eggs_per_pack,
              is_private_label AS store_brand
       FROM read_parquet('%2$s/dim_products_b2.parquet')
       WHERE super_category = 'EGGS' AND category = 'CHICKEN EGGS'
   )
   SELECT x.household_code,
          x.yq,
          x.upc,
          coalesce(x.upc_ver_uc, 0) AS ver,
          FALSE AS new_coding,
          x.spend,
          x.qty * b1.eggs_per_pack AS eggs,
          b1.store_brand
   FROM %1$s x
   JOIN b1 ON b1.upc = x.upc AND b1.upc_ver_uc = x.upc_ver_uc
   WHERE x.panel_year <= 2020 AND x.qty > 0 AND x.spend > 0
   UNION ALL
   SELECT x.household_code,
          x.yq,
          x.upc,
          0 AS ver,
          TRUE AS new_coding,
          x.spend,
          x.qty * b2.eggs_per_pack AS eggs,
          b2.store_brand
   FROM %1$s x
   JOIN b2 ON b2.upc = x.upc AND b2.panel_year = x.panel_year
   WHERE x.panel_year >= 2021
       AND x.qty > 0
       AND x.spend > 0
       AND b2.eggs_per_pack > 0",
        hh_upc,
        fin
    )
)

# (2) household-quarter eggs, store brands and price tier ----------------------
# Price tier: the household's eggs valued at each product's national price per
# egg in the base quarter. tier_own values the eggs bought in quarter t at the
# prices of t; tier_next values the eggs bought in t + 4 at the prices of t
# (products sold in both quarters; covered is their share of the eggs).
hh <- setDT(
    DBI::dbGetQuery(
        con,
        'WITH ref AS (
       SELECT upc, ver, new_coding, yq, sum(spend) / sum(eggs) AS price
       FROM egg_lines
       GROUP BY ALL
   ),
   own AS (
       SELECT l.household_code,
              l.yq,
              sum(l.eggs) AS eggs,
              sum(l.spend) AS spend,
              sum(l.eggs * l.store_brand::INT) AS eggs_store_brand,
              sum(l.eggs * r.price) / sum(l.eggs) AS tier_own
       FROM egg_lines l
       JOIN ref r USING (upc, ver, new_coding, yq)
       GROUP BY ALL
   ),
   nxt AS (
       SELECT l.household_code,
              l.yq,
              sum(l.eggs * r.price) / sum(l.eggs) AS tier_next,
              sum(l.eggs) AS eggs_covered
       FROM egg_lines l
       JOIN ref r
           ON r.upc = l.upc
           AND r.ver = l.ver
           AND r.new_coding = l.new_coding
           AND r.yq = l.yq - 10
       GROUP BY ALL
   )
   SELECT own.*, nxt.tier_next, nxt.eggs_covered / own.eggs AS covered
   FROM own
   LEFT JOIN nxt USING (household_code, yq)'
    )
)

# (3) other protein foods and egg substitutes ----------------------------------
# Protein: the protein food group less fresh eggs and modules 449, 455, 463, 464
# and 465. Substitutes: liquid and dried egg products.
other <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            "WITH b1 AS (
       SELECT p.upc,
              p.upc_ver_uc,
              p.product_module_code IN (1342, 1673)
                  OR p.product_module_code = 4100 AS substitute
       FROM read_parquet('%2$s/dim_products_b1.parquet') p
       JOIN read_parquet('%2$s/xwalk_module_foodgroup.parquet') w
           USING (product_module_code)
       WHERE (
               w.food_group = 'protein'
               AND p.product_module_code
                   NOT IN (4100, 449, 455, 463, 464, 465)
           )
           OR p.product_module_code IN (1342, 1673)
           OR (p.product_module_code = 4100 AND p.size1_units <> 'CT')
   ),
   b2 AS (
       SELECT p.upc,
              p.panel_year,
              p.super_category = 'EGGS' AS substitute
       FROM read_parquet('%2$s/dim_products_b2.parquet') p
       LEFT JOIN read_parquet('%2$s/xwalk_module_foodgroup.parquet') w
           USING (product_module_code)
       WHERE (
               p.super_category = 'EGGS'
               AND p.category IN ('EGG SUBSTITUTES', 'DRY EGGS')
           )
           OR (
               w.food_group = 'protein'
               AND p.super_category <> 'EGGS'
               AND p.product_module_code
                   NOT IN (4100, 449, 455, 463, 464, 465)
           )
   )
   SELECT household_code,
          yq,
          sum(CASE WHEN NOT substitute THEN qty END) AS protein_packs,
          sum(CASE WHEN substitute THEN spend END) AS substitute_spend
   FROM (
       SELECT x.household_code, x.yq, x.qty, x.spend, b1.substitute
       FROM %1$s x
       JOIN b1 ON b1.upc = x.upc AND b1.upc_ver_uc = x.upc_ver_uc
       WHERE x.panel_year <= 2020 AND x.qty > 0 AND x.spend > 0
       UNION ALL
       SELECT x.household_code, x.yq, x.qty, x.spend, b2.substitute
       FROM %1$s x
       JOIN b2 ON b2.upc = x.upc AND b2.panel_year = x.panel_year
       WHERE x.panel_year >= 2021 AND x.qty > 0 AND x.spend > 0
   )
   GROUP BY ALL",
            hh_upc,
            fin
        )
    )
)
DBI::dbDisconnect(con, shutdown = TRUE)

# (4) year-on-year changes on the estimation sample ----------------------------
# Quarters and the state egg index as in 25_eggs.R: 2020 is outside the
# estimation sample, and in 2021 the base quarter is in the earlier product
# coding.
st <- load_results('25_eggs')$st[n_upc >= 5]
d <- f1_gate('fpl_bin')
d <- d[year %in% c(2005:2019, 2022:2024)]
d[, yq0 := yq - 10L]
d[hh, on = .(household_code, yq), `:=`(
    e1 = i.eggs,
    s1 = i.spend,
    sb1 = i.eggs_store_brand,
    tier1 = i.tier_next,
    covered = i.covered
)]
d[hh, on = .(household_code, yq0 = yq), `:=`(
    e0 = i.eggs,
    s0 = i.spend,
    sb0 = i.eggs_store_brand,
    tier0 = i.tier_own
)]
d[other, on = .(household_code, yq), `:=`(
    prot1 = i.protein_packs,
    sub1 = i.substitute_spend
)]
d[other, on = .(household_code, yq0 = yq), `:=`(
    prot0 = i.protein_packs,
    sub0 = i.substitute_spend
)]
d[st, on = .(state, yq), infl_egg := i.infl_egg]
d <- d[!is.na(infl_egg) & !is.na(e0) & e0 > 0]
cat(
    'base-quarter egg buyers in states with an index:', nrow(d),
    '| households', uniqueN(d$household_code), '\n'
)

# Outcomes.
d[, buy := 100 * as.numeric(!is.na(e1) & e1 > 0)]
d[, d_arc := 200 * (nafill(e1, fill = 0) - e0) / (nafill(e1, fill = 0) + e0)]
d[buy > 0, `:=`(
    d_lprice = 100 * log((s1 / e1) / (s0 / e0)),
    d_leggs = 100 * log(e1 / e0),
    d_store_brand = 100 * (sb1 / e1 - sb0 / e0)
)]
d[buy > 0 & covered >= 0.5, d_ltier := 100 * log(tier1 / tier0)]
d[prot1 > 0 & prot0 > 0, d_lprotein := 100 * log(prot1 / prot0)]
d[, d_substitute := 100 * (as.numeric(!is.na(sub1)) - as.numeric(!is.na(sub0)))]
outcomes <- c(
    'd_lprice', 'd_leggs', 'buy', 'd_arc', 'd_store_brand', 'd_ltier',
    'd_lprotein', 'd_substitute'
)

nat <- d[, .(nat_egg = mean(infl_egg)), keyby = yq]
nat[, period := fifelse(
    yq %in% episode,
    'episode',
    fifelse(nat_egg >= 30, 'other shock', 'ordinary')
)]
d[nat, on = 'yq', period := i.period]
print(
    d[
        ,
        .(.N, quarters = uniqueN(yq), egg_inflation = mean(infl_egg)),
        keyby = period
    ], digits = 3
)

# (5) quarter means; the episode against ordinary quarters ---------------------
qm <- d[, c(
    list(n = .N, nat_egg = mean(infl_egg), period = period[1]),
    lapply(.SD, mean, na.rm = TRUE)
), keyby = yq, .SDcols = outcomes]
cat('\n== quarter means, 2022-2024\n')
print(qm[yq >= 20221], digits = 3)

qm[, `:=`(
    in_episode = as.numeric(period == 'episode'),
    in_other = as.numeric(period == 'other shock')
)]
est <- rbindlist(
    lapply(
        outcomes,
        function(y) {
            m <- lm(as.formula(paste(y, '~ in_episode + in_other')), qm)
            v <- NeweyWest(m, lag = 4, prewhite = FALSE)
            data.table(
                outcome = y,
                ordinary = coef(m)[[1]],
                episode = coef(m)[[1]] + coef(m)[[2]],
                diff = coef(m)[[2]],
                se = sqrt(v[2, 2]),
                p = 2 * pt(
                    abs(coef(m)[[2]]) / sqrt(v[2, 2]),
                    df.residual(m),
                    lower.tail = FALSE
                ),
                quarters = nobs(m),
                n_ordinary = d[period == 'ordinary' & !is.na(get(y)), .N],
                n_episode = d[period == 'episode' & !is.na(get(y)), .N]
            )
        }
    )
)
cat('\n== episode against ordinary quarters (quarter means, Newey-West)\n')
print(est, digits = 3)

# (6) low-income households, episode against ordinary quarters -----------------
h <- d[period != 'other shock' & !is.na(fpl_bin)]
h[, low := as.numeric(fpl_bin %in% c('<=100%', '100-130%'))]
h[, low_episode := low * (period == 'episode')]
low <- rbindlist(
    lapply(
        outcomes,
        function(y) {
            m <- feols(
                as.formula(sprintf('%s ~ low_episode + low | yq', y)),
                h,
                cluster = ~state
            )
            data.table(
                outcome = y,
                b = coef(m)[['low_episode']],
                se = se(m)[['low_episode']],
                p = 2 * pt(
                    abs(tstat(m)[['low_episode']]),
                    48,
                    lower.tail = FALSE
                ),
                n = nobs(m),
                n_low_episode = h[
                    low_episode == 1 & !is.na(get(y)),
                    .N
                ]
            )
        }
    )
)
cat('\n== low income x episode (quarter effects, state clusters)\n')
print(low, digits = 3)
cat('\n== means by income, episode and ordinary quarters\n')
print(
    h[
        ,
        lapply(.SD, mean, na.rm = TRUE),
        keyby = .(period, low),
        .SDcols = outcomes
    ], digits = 3
)

# (7) within-quarter elasticity in the episode alone ---------------------------
# As in 25_eggs.R (eggs bought on price paid, instrumented by state egg
# inflation, with quarter effects), which pools all shock quarters.
iv <- rbindlist(
    lapply(
        c('ordinary', 'episode', 'other shock'),
        function(k) {
            m <- feols(
                d_leggs ~ 1 | yq | d_lprice ~ infl_egg,
                d[period == k],
                cluster = ~state
            )
            data.table(
                period = k, b = coef(m)[[1]], se = se(m)[[1]],
                n = nobs(m)
            )
        }
    )
)
cat('\n== 2SLS elasticity of eggs bought, by period\n')
print(iv, digits = 3)

save_results(
    '34_egg_episode',
    est = est,
    iv = iv,
    low = low,
    quarter_means = qm,
    periods = nat
)
