-- Can a shelf seen only through Homescan purchases stand in for a store's true
-- shelf? For one year, compare state ladders built from panel purchases at RMS
-- stores with the RMS state ladders of the same stores (05a), and diagnose the
-- gap.
CREATE TEMP TABLE uk AS
SELECT
    upc,
    upc_ver_uc,
    item,
    kcal_unit
FROM read_parquet('${UK}');
CREATE TEMP TABLE im AS
SELECT
    item,
    min(module) AS module
FROM read_parquet('${UK}')
GROUP BY item;
CREATE TEMP TABLE sto AS
SELECT
    store_code_uc::INTEGER AS store_code_uc,
    any_value(fips_state_code)::INTEGER AS state,
    count(*) AS n
FROM read_parquet('${RMS_STO}')
GROUP BY 1;

-- A few stores have no state in the raw stores file (3 in 2018, 38 in 2012); as
-- in 05a they are on no state ladder, but here they are counted
SELECT
    count(*) FILTER (state IS NULL) AS n_store_no_state,
    CASE
        WHEN max(n) > 1
            THEN error('panel shelf check ${YEAR}: RMS store not unique')
    END AS chk_sto
FROM sto;

CREATE TEMP TABLE p AS
SELECT
    p.household_code,
    p.household_code % 2 AS half,
    p.store_code_uc,
    s.state,
    year(p.purchase_date) * 10 + quarter(p.purchase_date) AS yq,
    u.item,
    im.module,
    p.total_price_paid AS spend,
    p.quantity * u.kcal_unit AS kcal,
    coalesce(p.deal_flag_uc, 0) = 1 OR coalesce(p.coupon_value, 0) > 0 AS deal,
    coalesce(p.is_private_label, false) AS pl
FROM read_parquet('${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet') AS p
JOIN sto AS s ON s.store_code_uc = p.store_code_uc
JOIN read_parquet('${WP2_FINAL}/xwalk_module_foodgroup.parquet') AS fg
    USING (product_module_code)
JOIN uk AS u
    ON
        u.upc = p.upc
        AND u.upc_ver_uc = coalesce(p.upc_ver_uc, 0)::SMALLINT
JOIN im ON im.item = u.item
WHERE
    fg.is_human_food AND NOT p.is_magnet_product AND p.quantity > 0
    AND p.total_price_paid > 0
    -- A panel year runs a few days past 31 December; RMS ${YEAR} does not
    AND year(p.purchase_date) = ${YEAR} AND s.state IS NOT NULL;

-- Cells per variant x source half ('a' = all households)
CREATE TEMP TABLE c AS
WITH v AS (
    SELECT
        'all' AS v,
        *
    FROM p
    UNION ALL
    SELECT
        'nodeal',
        *
    FROM p
    WHERE NOT deal
    UNION ALL
    SELECT
        'nopl',
        *
    FROM p
    WHERE NOT pl
),

h AS (
    SELECT
        'a' AS src,
        *
    FROM v
    UNION ALL
    SELECT
        half::VARCHAR,
        *
    FROM v
)

SELECT
    v,
    src,
    store_code_uc,
    state,
    yq,
    item,
    module,
    sum(spend) AS spend,
    sum(kcal) AS kcal,
    sum(spend) / sum(kcal) AS dpk
FROM h
GROUP BY ALL
HAVING sum(kcal) > 0;

CREATE TEMP TABLE trm AS
SELECT
    v,
    src,
    yq,
    module,
    quantile_cont(dpk, 0.01) AS lo,
    quantile_cont(dpk, 0.99) AS hi
FROM c
GROUP BY ALL;

CREATE TEMP TABLE pl AS
WITH f AS (
    SELECT c.* FROM c JOIN trm USING (v, src, yq, module)
    WHERE c.dpk BETWEEN trm.lo AND trm.hi
),

w AS (
    SELECT
        *,
        sum(kcal)
            OVER (
                PARTITION BY v, src, state, yq, module
                ORDER BY dpk, item, store_code_uc
                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            )
        / sum(kcal) OVER (PARTITION BY v, src, state, yq, module) AS cum_hi
    FROM f
)

SELECT
    v,
    src,
    state,
    yq,
    module,
    count(DISTINCT item) AS n_items,
    count(DISTINCT item) >= 5 AS valid,
    min(dpk) FILTER (cum_hi >= 0.25) AS p25,
    min(dpk) FILTER (cum_hi >= 0.50) AS p50
FROM w
GROUP BY ALL;

CREATE TEMP TABLE rl AS
SELECT
    key AS state,
    yq,
    module,
    n_items,
    valid,
    p25,
    p50,
    kcal_tot
FROM read_parquet('${LAD}')
WHERE reach = 'state';

SELECT CASE
    WHEN
        count(*) <> count(DISTINCT(state, yq, module))
        OR (
            SELECT count(*) <> count(DISTINCT(v, src, state, yq, module))
            FROM pl
        )
        THEN error('panel shelf check ${YEAR}: duplicate ladder key')
END AS chk_lad FROM rl;

-- 1. ladders: coverage and price level, full panel vs RMS
COPY (
    SELECT
        ${YEAR}::SMALLINT AS year,
        vv.v,
        count(*) AS n_rms_valid,
        avg((p.valid IS TRUE)::INTEGER) AS sh_panel_valid,
        median(p.n_items / r.n_items) AS med_item_share,
        count(*) FILTER (p.valid) AS n_both,
        exp(
            sum(r.kcal_tot * ln(p.p25 / r.p25)) FILTER (p.valid)
            / sum(r.kcal_tot) FILTER (p.valid)
        ) AS gm_ratio_p25,
        exp(
            sum(r.kcal_tot * ln(p.p50 / r.p50)) FILTER (p.valid)
            / sum(r.kcal_tot) FILTER (p.valid)
        ) AS gm_ratio_p50,
        quantile_cont(p.p25 / r.p25, 0.1) FILTER (p.valid) AS p10_ratio_p25,
        median(p.p25 / r.p25) FILTER (p.valid) AS p50_ratio_p25,
        quantile_cont(p.p25 / r.p25, 0.9) FILTER (p.valid) AS p90_ratio_p25
    FROM rl AS r
    JOIN (SELECT DISTINCT v FROM pl) AS vv ON true
    LEFT JOIN pl AS p
        ON
            p.v = vv.v AND p.src = 'a' AND p.state = r.state
            AND p.yq = r.yq AND p.module = r.module
    WHERE r.valid
    GROUP BY ALL
) TO '${OUT_LAD}' (FORMAT parquet);

-- 2. savings on target-year base baskets bought at RMS stores, on one
-- common set of rows: RMS ladder valid and every panel ladder valid
CREATE TEMP TABLE gk AS
SELECT
    household_code,
    yq - 10 AS byq,
    state::INTEGER AS hstate
FROM read_parquet('${DP}', hive_partitioning = true)
WHERE est_gate AND yq BETWEEN (${YEAR} + 1) * 10 + 1 AND (${YEAR} + 1) * 10 + 4;

CREATE TEMP TABLE b AS
SELECT
    h.household_code,
    h.household_code % 2 AS half,
    h.yq,
    h.module,
    g.hstate AS state,
    sum(h.spend) AS spend,
    sum(h.kcal) AS kcal
FROM read_parquet('${HS}') AS h
JOIN gk AS g ON g.household_code = h.household_code AND g.byq = h.yq
SEMI JOIN sto ON sto.store_code_uc = h.store_code_uc
WHERE h.kcal > 0 AND h.spend > 0
GROUP BY ALL;

CREATE TEMP TABLE bl AS
SELECT
    b.*,
    r.p25 AS r25,
    r.p50 AS r50,
    l.v,
    l.src,
    l.p25,
    l.p50,
    l.valid,
    CASE
        WHEN l.src = 'a' THEN 'full'
        WHEN l.src = b.half::VARCHAR THEN 'own' ELSE 'other'
    END AS source
FROM b
JOIN rl AS r
    ON
        r.state = b.state AND r.yq = b.yq AND r.module = b.module
        AND r.valid
JOIN pl AS l ON l.state = b.state AND l.yq = b.yq AND l.module = b.module;

CREATE TEMP TABLE common AS
SELECT
    household_code,
    yq,
    module
FROM bl
GROUP BY ALL
HAVING count(*) FILTER (valid) = 9;   -- 3 variants x 3 sources, all valid

COPY (
    SELECT
        ${YEAR}::SMALLINT AS year,
        v,
        source,
        count(*) AS n_rows,
        count(DISTINCT household_code) AS n_hh,
        sum(spend) AS spend,
        sum(greatest(spend - kcal * r25, 0)) / sum(spend) AS sav25_rms,
        sum(greatest(spend - kcal * p25, 0)) / sum(spend) AS sav25_panel,
        sum(greatest(spend - kcal * r50, 0)) / sum(spend) AS sav50_rms,
        sum(greatest(spend - kcal * p50, 0)) / sum(spend) AS sav50_panel,
        corr(
            greatest(spend - kcal * r25, 0) / spend,
            greatest(spend - kcal * p25, 0) / spend
        ) AS corr_row_p25
    FROM bl SEMI JOIN common USING (household_code, yq, module)
    GROUP BY ALL
) TO '${OUT_SAV}' (FORMAT parquet);

-- 3. where panel food spend is, and how densely the panel sees each shelf
CREATE TEMP TABLE pa AS
SELECT
    p.retailer_code,
    p.channel_type,
    p.store_zip3,
    hy.fips_state_code AS hstate,
    coalesce(p.store_code_uc, 0) AS store_code_uc,
    CASE
        WHEN coalesce(p.store_code_uc, 0) = 0 THEN 'uncoded'
        WHEN p.store_code_uc IN (SELECT store_code_uc FROM sto) THEN 'rms'
        ELSE 'coded_non_rms'
    END AS kind,
    year(p.purchase_date) * 10 + quarter(p.purchase_date) AS yq,
    u.item,
    im.module,
    p.net_price_paid AS spend
FROM read_parquet('${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet') AS p
JOIN read_parquet('${WP2_FINAL}/xwalk_module_foodgroup.parquet') AS fg
    USING (product_module_code)
JOIN uk AS u
    ON
        u.upc = p.upc
        AND u.upc_ver_uc = coalesce(p.upc_ver_uc, 0)::SMALLINT
JOIN im ON im.item = u.item
LEFT JOIN read_parquet('${WP2_FINAL}/household_year.parquet') AS hy
    ON hy.household_code = p.household_code AND hy.panel_year = ${YEAR}
WHERE
    fg.is_human_food AND NOT p.is_magnet_product AND p.quantity > 0
    AND p.net_price_paid > 0;

COPY (
    WITH d AS (
        SELECT
            *,
            count(DISTINCT item)
                OVER (PARTITION BY store_code_uc, yq, module)
                AS n_store,
            count(DISTINCT item)
                OVER (PARTITION BY retailer_code, store_zip3, yq, module)
                AS n_zip3,
            count(DISTINCT item)
                OVER (PARTITION BY retailer_code, hstate, yq, module)
                AS n_state,
            count(DISTINCT item)
                OVER (PARTITION BY retailer_code, yq, module)
                AS n_ret
        FROM pa
    )

    SELECT
        ${YEAR}::SMALLINT AS year,
        channel_type,
        kind,
        sum(spend) AS spend,
        count(DISTINCT store_code_uc) AS n_stores,
        count(DISTINCT retailer_code) AS n_retailers,
        -- The store-level pool needs a store code
        sum(spend) FILTER (n_store >= 5 AND kind <> 'uncoded')
            AS spend_ge5_store,
        sum(spend) FILTER (n_zip3 >= 5) AS spend_ge5_zip3,
        sum(spend) FILTER (n_state >= 5) AS spend_ge5_state,
        sum(spend) FILTER (n_ret >= 5) AS spend_ge5_ret
    FROM d
    GROUP BY ALL
) TO '${OUT_COV}' (FORMAT parquet);
