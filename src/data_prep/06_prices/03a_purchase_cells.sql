-- One panel year of purchases -> household x state x retailer x item x month x
-- deal-flag totals, the finest grain at which a household's price can be set
-- against what other households paid (03b). Same line rules as hh_upc_q
-- (05_household_quarter/01a_hh_quarter.sql): human food, non-Magnet, quantity >
-- 0, net price paid > 0.

COPY (
    SELECT
        p.household_code,
        hy.fips_state_code AS st,
        p.retailer_code AS ret,
        coalesce(CASE WHEN ${YEAR} <= 2020 THEN x.upc_2021_masked END, p.upc)
            AS item,
        (year(p.purchase_date) * 100 + month(p.purchase_date))::INTEGER AS ym,
        (coalesce(p.deal_flag_uc, 0) = 1) AS deal,
        sum(p.quantity)::DOUBLE AS q,
        sum(p.net_price_paid) AS s,          -- net of coupons
        sum(p.total_price_paid) AS g         -- before coupons
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
) TO '${WP2_INT}/purchase_cells/panel_year=${YEAR}.parquet'
(FORMAT parquet, COMPRESSION zstd);
