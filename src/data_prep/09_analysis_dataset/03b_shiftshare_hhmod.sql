-- Household x base quarter x product-module food spend (the base-basket shares
-- of the ext1 shift-share instrument), held as DuckDB temp table hm${TAG} for
-- the households in temp table hq. Not written to disk (disk-space limit, 2 Oct
-- 2026): rebuilt in memory per target year by
-- src/data_prep/09_analysis_dataset/03_shiftshare.sh.

CREATE OR REPLACE TEMP TABLE hm${TAG} AS
SELECT
    h.household_code,
    h.yq + ${LAG} AS yq,
    m.product_module_code AS module,
    sum(h.spend) AS spend
FROM read_parquet([${HU_FILES}]) AS h
JOIN (SELECT DISTINCT household_code FROM hq) AS k USING (household_code)
LEFT JOIN (
    SELECT
        upc,
        upc_ver_uc,
        product_module_code
    FROM read_parquet('${WP2_FINAL}/dim_products_b1.parquet')
    UNION ALL
    -- 2021+ products, version 0: lowest module over years (deterministic)
    SELECT
        upc,
        0::SMALLINT,
        min(product_module_code)
    FROM read_parquet('${WP2_FINAL}/dim_products_b2.parquet')
    WHERE product_module_code IS NOT NULL
    GROUP BY upc
) AS m ON m.upc = h.upc AND m.upc_ver_uc = h.upc_ver_uc
WHERE h.yq BETWEEN ${BASEYEAR} * 10 + 1 AND ${BASEYEAR} * 10 + 4
GROUP BY ALL;
