-- Purchase-level merge for ONE panel year, 2004-2020 ("Batch 1" layout). Run by
-- src/data_prep/03_homescan/02_merge.sh with ${YEAR} set.
--
-- Merge chain (all LEFT joins from the purchase line, so no purchase is lost):
--   purchases (m) : trips (1)        on trip_code_uc          [within year]
--   trips     (m) : panelists (1)    on household_code, panel_year
--   purchases (m) : products (1)     on upc, upc_ver_uc       [Batch 1 dim]
--   trips     (m) : retailers (1)    on retailer_code
-- trip_code_uc is only unique WITHIN a panel year (codes restart in 2021),
-- so every join is restricted to the same panel_year.
-- Output: final/purchases/panel_year=${YEAR}/data.parquet (one row per raw
-- purchase line)

CREATE OR REPLACE TEMP VIEW p AS
SELECT *
FROM read_parquet('${WP2_INT}/panel/purchases/panel_year=${YEAR}/data.parquet');
CREATE OR REPLACE TEMP VIEW t AS
SELECT *
FROM read_parquet('${WP2_INT}/panel/trips/panel_year=${YEAR}/data.parquet');
CREATE OR REPLACE TEMP VIEW h AS
SELECT *
FROM read_parquet('${WP2_INT}/panel/panelists/panel_year=${YEAR}/data.parquet');
CREATE OR REPLACE TEMP VIEW d AS
SELECT * FROM read_parquet('${WP2_FINAL}/dim_products_b1.parquet');
CREATE OR REPLACE TEMP VIEW r AS
SELECT * FROM read_parquet('${WP2_INT}/panel/retailers_master/data.parquet');

COPY (
    SELECT
        CAST(${YEAR} AS SMALLINT) AS panel_year,
        t.household_code,
        p.trip_code_uc,
        t.purchase_date,
        t.retailer_code,
        r.channel_type,
        t.store_code_uc,
        t.store_zip3,
        t.method_of_payment_cd,
        t.total_spent AS trip_total_spent,
        p.upc,
        p.upc_ver_uc,
        p.quantity,
        p.total_price_paid,
        p.coupon_value,
        -- Nielsen manual: final price = total_price_paid - coupon_value
        p.total_price_paid - coalesce(p.coupon_value, 0) AS net_price_paid,
        p.deal_flag_uc,
        d.product_module_code,
        d.product_group_code,
        d.department_code,
        d.brand_code_uc,
        d.multi,
        d.size1_amount,
        d.size1_units,
        d.is_private_label,
        d.is_magnet_product,
        d.is_food_department,
        -- Match flags, kept so that it is possible to see exactly what failed
        -- to merge
        (t.trip_code_uc IS NOT NULL) AS m_trip,
        (h.household_code IS NOT NULL) AS m_panelist,
        (d.upc IS NOT NULL) AS m_product,
        (r.retailer_code IS NOT NULL) AS m_retailer,
        'b1' AS product_batch
    FROM p
    LEFT JOIN t ON p.trip_code_uc = t.trip_code_uc
    LEFT JOIN h ON t.household_code = h.household_code
    LEFT JOIN d ON p.upc = d.upc AND p.upc_ver_uc = d.upc_ver_uc
    LEFT JOIN r ON t.retailer_code = r.retailer_code
) TO '${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet.tmp'
(FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE 1000000);
