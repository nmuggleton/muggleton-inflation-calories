-- One panel year of purchases -> state x retailer x item x calendar-quarter
-- totals, for state prices at fixed retailer weights (robustness: does the
-- instrument move because other households changed where they shop?). Same line
-- rules as state_upc_q (05_household_quarter/01a_hh_quarter.sql): human food,
-- non-Magnet, quantity > 0, net price paid > 0.

COPY (
    SELECT
        hy.fips_state_code,
        p.retailer_code,
        year(p.purchase_date) * 10 + quarter(p.purchase_date) AS yq,
        coalesce(CASE WHEN ${YEAR} <= 2020 THEN x.upc_2021_masked END, p.upc)
            AS item,
        sum(p.quantity) AS qty,
        sum(p.net_price_paid) AS spend
    FROM
        read_parquet('${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet')
            AS p
    JOIN read_parquet('${WP2_FINAL}/xwalk_module_foodgroup.parquet') AS fg
        USING (product_module_code)
    JOIN read_parquet('${WP2_FINAL}/household_year.parquet') AS hy
        ON hy.household_code = p.household_code AND hy.panel_year = ${YEAR}
    LEFT JOIN read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet') AS x
        ON p.upc = x.upc_2020
    WHERE
        fg.is_human_food AND NOT p.is_magnet_product AND p.quantity > 0
        AND p.net_price_paid > 0
    GROUP BY ALL
) TO '${WP2_INT}/state_ret_upc_q/panel_year=${YEAR}.parquet'
(FORMAT parquet, COMPRESSION zstd);
