-- The ladder of reach from panel-observed shelves: for one calendar year,
-- calorie-price ladders built from what Homescan panellists bought, at five
-- reaches, plus each household's labelled spend by quarter x retailer x module
-- (the base baskets the rungs re-price). Run per year by
-- src/data_prep/09_analysis_dataset/07_panel_shelf.sh.
--
-- Definitions (05a / 01b rules unless stated)
--   purchase     non-Magnet human food, labelled UPC version, quantity
--                > 0, shelf price total_price_paid > 0 (before coupons;
--                ladders price the shelf, not the coupon), purchase date
--                in ${YEAR}
--   cell         store x quarter x item; a purchase with no store code is
--                its retailer's pseudo-store (-retailer_code)
--   residence    household state, county and zip3 in the purchase's
--                panel year (household_year)
--   reach        retailer  one retailer's cells bought by panellists
--                          living in the state (key retailer_code,
--                          state column set)
--                zip3      cells bought by panellists living in the zip3
--                county    ... in the county (state x 1000 + county)
--                state     ... in the state
--                retailer_h0, retailer_h1  the retailer shelf built from
--                          one half of the panel (household_code % 2), for
--                          the cross-fitted own-retailer rung in 07b
--   trim         national module x quarter 1st-99th percentile of $/kcal
--                over all cells
--   ladder       within reach key x quarter x module, cells sorted by
--                $/kcal and weighted by kcal bought; valid >= 5 items;
--                p25, p50 calorie-weighted quantiles; n_hh panellists
--                behind the ladder (a household's own purchases are part
--                of its zip3 and county shelves: 07b records their share)
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

CREATE TEMP TABLE hy AS
SELECT
    household_code,
    panel_year::INTEGER AS panel_year,
    fips_state_code::INTEGER AS state,
    fips_state_code::INTEGER * 1000 + fips_county_code::INTEGER AS county,
    substr(panelist_zip_code, 1, 3)::INTEGER AS zip3,
    count(*) AS n
FROM read_parquet('${WP2_FINAL}/household_year.parquet')
WHERE panel_year IN (${YEAR}, ${YEAR} + 1)
GROUP BY ALL;

SELECT CASE
    WHEN count(*) <> count(DISTINCT(household_code, panel_year))
        THEN error('panel shelf ${YEAR}: household_year not unique')
END AS chk_hy FROM hy;

CREATE TEMP TABLE p AS
SELECT
    p.household_code,
    p.panel_year::INTEGER AS panel_year,
    p.retailer_code,
    CASE
        WHEN coalesce(p.store_code_uc, 0) > 0 THEN p.store_code_uc
        ELSE -p.retailer_code
    END AS store,
    year(p.purchase_date) * 10 + quarter(p.purchase_date) AS yq,
    u.item,
    im.module,
    p.total_price_paid AS shelf,
    p.net_price_paid AS paid,
    p.quantity * u.kcal_unit AS kcal
FROM read_parquet([${P_FILES}]) AS p
JOIN read_parquet('${WP2_FINAL}/xwalk_module_foodgroup.parquet') AS fg
    USING (product_module_code)
JOIN uk AS u
    ON
        u.upc = p.upc
        AND u.upc_ver_uc = coalesce(p.upc_ver_uc, 0)::SMALLINT
JOIN im ON im.item = u.item
WHERE
    fg.is_human_food AND NOT p.is_magnet_product AND p.quantity > 0
    AND year(p.purchase_date) = ${YEAR};

-- Residence: every purchasing household must have a zip3, and every purchase a
-- retailer.
CREATE TEMP TABLE pr AS
SELECT
    p.*,
    h.state,
    h.county,
    h.zip3
FROM p LEFT JOIN hy AS h USING (household_code, panel_year);

SELECT
    count(*) AS n_purch,
    count(*) FILTER (state IS NULL) AS n_no_state,
    count(*) FILTER (county IS NULL) AS n_no_county,
    count(*) FILTER (retailer_code IS NULL) AS n_no_retailer,
    count(DISTINCT household_code) FILTER (state IS NULL) AS n_hh_no_state,
    CASE
        WHEN count(*) FILTER (zip3 IS NULL OR retailer_code IS NULL) > 0
            THEN
                error(
                    'panel shelf ${YEAR}: purchase '
                    || 'with no residence or retailer'
                )
    END AS chk_pr
FROM pr;

-- Base baskets: what households paid, every labelled purchase with a positive
-- amount paid (05b rules)
COPY (
    SELECT
        household_code,
        yq,
        retailer_code,
        module,
        sum(paid::DECIMAL(38, 9))::DOUBLE AS spend,
        sum(kcal::DECIMAL(38, 9))::DOUBLE AS kcal
    FROM pr
    WHERE paid > 0 AND kcal > 0
    GROUP BY ALL
) TO '${OUT_BASE}' (FORMAT parquet, COMPRESSION zstd);

-- Which retailers the residents of an area use (for the best-retailer ladder in
-- 07b): stores are not geocoded outside RMS, so a retailer is "in" a zip3 or
-- county when its residents buy there
COPY (
    WITH s AS (
        SELECT * FROM pr
        WHERE shelf > 0 AND kcal > 0 AND state IS NOT NULL
    )

    SELECT
        'zip3' AS level,
        state,
        zip3 AS key,
        retailer_code,
        yq,
        count(DISTINCT household_code) AS n_hh
    FROM s
    GROUP BY ALL
    UNION ALL
    SELECT
        'county',
        state,
        county,
        retailer_code,
        yq,
        count(DISTINCT household_code)
    FROM s
    WHERE county IS NOT NULL
    GROUP BY ALL
) TO '${OUT_PRES}' (FORMAT parquet, COMPRESSION zstd);

-- cells per reach
CREATE TEMP TABLE c AS
WITH s AS (
    SELECT * FROM pr
    WHERE shelf > 0 AND kcal > 0
),

r AS (
    SELECT
        'retailer' AS reach,
        state,
        retailer_code AS key,
        * EXCLUDE (state, county, zip3)
    FROM s
    WHERE state IS NOT NULL
    UNION ALL
    SELECT
        'retailer_h' || (household_code % 2),
        state,
        retailer_code,
        * EXCLUDE (state, county, zip3)
    FROM s
    WHERE state IS NOT NULL
    UNION ALL
    SELECT
        'zip3',
        NULL,
        zip3,
        * EXCLUDE (state, county, zip3)
    FROM s
    UNION ALL
    SELECT
        'county',
        NULL,
        county,
        * EXCLUDE (state, county, zip3)
    FROM s
    WHERE county IS NOT NULL
    UNION ALL
    SELECT
        'state',
        NULL,
        state,
        * EXCLUDE (state, county, zip3)
    FROM s
    WHERE state IS NOT NULL
)

SELECT
    reach,
    state,
    key,
    store,
    yq,
    item,
    module,
    -- Exact fixed-point sums, the same in any order, so that cells do not cross
    -- the trim or the calorie cut-offs from one build to the next
    sum(shelf::DECIMAL(38, 9))::DOUBLE AS spend,
    sum(kcal::DECIMAL(38, 9))::DOUBLE AS kcal,
    (sum(shelf::DECIMAL(38, 9)) / sum(kcal::DECIMAL(38, 9)))::DOUBLE AS dpk
FROM r
GROUP BY ALL;

-- National trim over all cells (the zip3 reach holds every purchase once)
CREATE TEMP TABLE trm AS
SELECT
    yq,
    module,
    quantile_cont(dpk, 0.01) AS lo,
    quantile_cont(dpk, 0.99) AS hi
FROM c
WHERE reach = 'zip3'
GROUP BY ALL;

CREATE TEMP TABLE nh AS
WITH s AS (
    SELECT * FROM pr
    WHERE shelf > 0 AND kcal > 0
)

SELECT
    'retailer' AS reach,
    state,
    retailer_code AS key,
    yq,
    module,
    count(DISTINCT household_code) AS n_hh
FROM s
WHERE state IS NOT NULL
GROUP BY ALL
UNION ALL
SELECT
    'retailer_h' || (household_code % 2),
    state,
    retailer_code,
    yq,
    module,
    count(DISTINCT household_code)
FROM s
WHERE state IS NOT NULL
GROUP BY ALL
UNION ALL
SELECT
    'zip3',
    NULL,
    zip3,
    yq,
    module,
    count(DISTINCT household_code)
FROM s
GROUP BY ALL
UNION ALL
SELECT
    'county',
    NULL,
    county,
    yq,
    module,
    count(DISTINCT household_code)
FROM s
WHERE county IS NOT NULL
GROUP BY ALL
UNION ALL
SELECT
    'state',
    NULL,
    state,
    yq,
    module,
    count(DISTINCT household_code)
FROM s
WHERE state IS NOT NULL
GROUP BY ALL;

COPY (
    WITH f AS (
        SELECT c.* FROM c JOIN trm USING (yq, module)
        WHERE c.dpk BETWEEN trm.lo AND trm.hi
    ),

w AS (
        SELECT
            *,
            sum(kcal::DECIMAL(38, 9))
                OVER (
                    PARTITION BY reach, state, key, yq, module
                    ORDER BY dpk, item, store
                    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
                )
            / sum(kcal::DECIMAL(38, 9))
                OVER (PARTITION BY reach, state, key, yq, module)
                AS cum_hi
        FROM f
    )

    SELECT
        w.reach,
        w.state,
        w.key::INTEGER AS key,
        w.yq,
        w.module,
        count(DISTINCT item) AS n_items,
        count(DISTINCT item) >= 5 AS valid,
        any_value(nh.n_hh) AS n_hh,
        sum(kcal::DECIMAL(38, 9))::DOUBLE AS kcal_tot,
        min(dpk) FILTER (cum_hi >= 0.25) AS p25,
        min(dpk) FILTER (cum_hi >= 0.50) AS p50
    FROM w LEFT JOIN nh
        ON
            nh.reach = w.reach AND nh.key = w.key
            AND nh.yq = w.yq AND nh.module = w.module
            AND nh.state IS NOT DISTINCT FROM w.state
    GROUP BY ALL
) TO '${OUT_LAD}' (FORMAT parquet, COMPRESSION zstd);
