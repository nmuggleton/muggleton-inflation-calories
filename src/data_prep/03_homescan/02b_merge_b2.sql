-- Purchase-level merge for ONE panel year, 2021+ ("Batch 2" layout). Run by
-- src/data_prep/03_homescan/02_merge.sh with ${YEAR} set.
--
-- Differences from Batch 1:
--   * purchases have NO upc_ver_uc; products are keyed on (upc, panel_year)
--   * purchases carry legacy product_module_code_hms (as-of-purchase), which
--     is used for module/group/department when the product dim has no module
--   * private-label UPCs are masked from 2021 (crosswalk in reference docs)
--   * retailers come from the annual retailer file
-- Output columns are identical to src/data_prep/03_homescan/02a_merge_b1.sql so
-- years stack.

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
SELECT * FROM read_parquet('${WP2_FINAL}/dim_products_b2.parquet')
WHERE panel_year = ${YEAR};
CREATE OR REPLACE TEMP VIEW x AS
SELECT * FROM read_parquet('${WP2_FINAL}/xwalk_module.parquet');
CREATE OR REPLACE TEMP VIEW r AS
SELECT *
FROM read_parquet('${WP2_INT}/panel/retailers/panel_year=${YEAR}/data.parquet');

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
        CAST(NULL AS SMALLINT) AS upc_ver_uc,
        p.quantity,
        p.total_price_paid,
        p.coupon_value,
        p.total_price_paid - coalesce(p.coupon_value, 0) AS net_price_paid,
        p.deal_flag_uc,
        coalesce(d.product_module_code, p.product_module_code_hms)
            AS product_module_code,
        x.product_group_code,
        x.department_code,
        d.brand_code_uc,
        coalesce(d.multi, p.multi_hms) AS multi,
        -- As-of-purchase size: size1_amount_hms has 3 implied decimals
        -- ('0030000' means 30.000; checked against the Batch-1 master for the
        -- same UPC)
        TRY_CAST(p.size1_amount_hms AS DOUBLE) / 1000 AS size1_amount,
        NULLIF(trim(p.size1_unit_hms), '') AS size1_units,
        d.is_private_label,
        coalesce(x.is_magnet_product, false) AS is_magnet_product,
        x.is_food_department,
        (t.trip_code_uc IS NOT NULL) AS m_trip,
        (h.household_code IS NOT NULL) AS m_panelist,
        (d.upc IS NOT NULL) AS m_product,
        (r.retailer_code IS NOT NULL) AS m_retailer,
        'b2' AS product_batch
    FROM p
    LEFT JOIN t ON p.trip_code_uc = t.trip_code_uc
    LEFT JOIN h ON t.household_code = h.household_code
    LEFT JOIN d ON p.upc = d.upc
    LEFT JOIN x
        ON
            coalesce(d.product_module_code, p.product_module_code_hms)
            = x.product_module_code
    LEFT JOIN r ON t.retailer_code = r.retailer_code
) TO '${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet.tmp'
(FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE 1000000);
