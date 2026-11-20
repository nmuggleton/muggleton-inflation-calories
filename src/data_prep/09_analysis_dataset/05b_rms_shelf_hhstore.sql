-- One panel year of purchases -> labelled spend and kcal by household x quarter
-- x store x module, so that each household's base calories can be re-priced on
-- the shelf of the store it bought them in. Run per panel year by
-- src/data_prep/09_analysis_dataset/05_rms_shelf.sh.
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

COPY (
    SELECT
        p.household_code,
        CAST(${YEAR} AS SMALLINT) AS panel_year,
        (year(p.purchase_date) * 10 + quarter(p.purchase_date)) AS yq,
        coalesce(p.store_code_uc, 0) AS store_code_uc,
        im.module,
        sum(p.net_price_paid) AS spend,
        sum(p.quantity * u.kcal_unit) AS kcal
    FROM
        read_parquet('${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet')
            AS p
    JOIN read_parquet('${WP2_FINAL}/xwalk_module_foodgroup.parquet') AS fg
        USING (product_module_code)
    JOIN uk AS u
        ON
            u.upc = p.upc
            AND u.upc_ver_uc = coalesce(p.upc_ver_uc, 0)::SMALLINT
    JOIN im ON im.item = u.item
    WHERE
        fg.is_human_food AND NOT p.is_magnet_product AND p.quantity > 0
        AND p.net_price_paid > 0
    GROUP BY ALL
) TO '${OUT_HS}' (FORMAT parquet, COMPRESSION zstd);
