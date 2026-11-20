-- Calories and nutrients per purchased package for every labelled non-Magnet
-- food UPC version, so that the household x UPC x quarter and state x UPC x
-- quarter tables (step 05) can be priced per kcal. Run by
-- src/data_prep/09_analysis_dataset/01_ladder.sh (placeholders ${...}
-- substituted there).
--
-- Rules copied from src/data_prep/05_household_quarter/01a_hh_quarter.sql so
-- that kcal here equal the kcal behind kcal_lab_ae_day:
--   label UPC  2004-2020 private label -> 2021 masked code (crosswalk)
--   label used only if kcal_100 and basis are known and not implausible
--   amount     size1_amount x unit factor in the label basis (OZ read as
--              weight or fluid ounces by label basis), fallback label
--              unit_size, then servings x serving size, times multi
--   item       the label UPC: the cross-year item key of 06_prices/01a_price_indices.sql
CREATE TEMP TABLE nut AS
SELECT * FROM read_parquet('${WP2_FINAL}/upc_nutrients.parquet')
WHERE
    kcal_100 IS NOT NULL AND basis IS NOT NULL
    AND NOT coalesce(f_kcal_implausible, false)
QUALIFY row_number() OVER (PARTITION BY upc ORDER BY label_year DESC) = 1;

CREATE TEMP TABLE xw AS
SELECT
    upc_2020,
    any_value(upc_2021_masked) AS upc_2021_masked
FROM read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet')
GROUP BY upc_2020;

COPY (
    WITH p AS (
        SELECT
            d.upc,
            d.upc_ver_uc,
            d.product_module_code AS module,
            d.multi,
            d.size1_amount,
            upper(trim(d.size1_units)) AS su,
            coalesce(d.is_private_label, false) AS is_pl,
            CASE
                WHEN d.is_private_label THEN coalesce(x.upc_2021_masked, d.upc)
                ELSE d.upc
            END AS item
        FROM read_parquet('${WP2_FINAL}/dim_products_b1.parquet') AS d
        LEFT JOIN xw AS x ON x.upc_2020 = d.upc
        WHERE
            coalesce(d.is_food_department, false)
            AND NOT coalesce(d.is_magnet_product, false)
        UNION ALL
        -- 2021+ products (dim_products_b2, one row per upc x panel year): the
        -- purchase files carry no UPC version from 2021, so hh_upc_q and
        -- state_upc_q key them as version 0, which no 2004-2020 product uses.
        SELECT
            d.upc,
            0::SMALLINT AS upc_ver_uc,
            d.product_module_code AS module,
            d.multi,
            CAST(NULL AS DOUBLE) AS size1_amount,
            CAST(NULL AS VARCHAR) AS su,
            coalesce(d.is_private_label, false) AS is_pl,
            d.upc AS item
        FROM read_parquet('${WP2_FINAL}/dim_products_b2.parquet') AS d
        JOIN read_parquet('${WP2_FINAL}/xwalk_module.parquet') AS m
            ON m.product_module_code = d.product_module_code
        WHERE m.is_food_department AND NOT coalesce(m.is_magnet_product, false)
        QUALIFY row_number() OVER (PARTITION BY d.upc ORDER BY d.panel_year) = 1
    ),

j AS (
        SELECT
            p.*,
            n.basis,
            n.kcal_100,
            n.sugar_100,
            n.satfat_100,
            n.sodium_mg_100,
            n.fiber_100,
            n.protein_100,
            n.unit_size,
            n.pack_size_srv,
            CASE p.su
                WHEN 'OZ'
                    THEN
                        p.size1_amount * CASE n.basis
                            WHEN 'g' THEN 28.3495 WHEN 'ml' THEN 29.5735
                        END
                WHEN 'PO'
                    THEN
                        CASE
                            WHEN n.basis = 'g'
                                THEN p.size1_amount * 453.592
                        END
                WHEN 'QT'
                    THEN
                        CASE
                            WHEN n.basis = 'ml'
                                THEN p.size1_amount * 946.353
                        END
                WHEN 'LI'
                    THEN
                        CASE
                            WHEN n.basis = 'ml'
                                THEN p.size1_amount * 1000
                        END
                WHEN 'ML' THEN CASE WHEN n.basis = 'ml' THEN p.size1_amount END
                WHEN 'GA' THEN CASE
                    WHEN n.basis = 'ml'
                        THEN p.size1_amount * 3785.41
                END
            END AS size_line
        FROM p JOIN nut AS n ON n.upc = p.item
    ),

a AS (
        SELECT
            *,
            coalesce(size_line, unit_size, pack_size_srv)
            * coalesce(nullif(multi, 0), 1) AS amt_unit
        FROM j
    )

    SELECT
        upc,
        coalesce(upc_ver_uc, 0)::SMALLINT AS upc_ver_uc,
        item,
        module,
        is_pl,
        basis,
        amt_unit,
        kcal_100 * amt_unit / 100 AS kcal_unit,
        CASE WHEN basis = 'g' THEN amt_unit ELSE 0 END AS g_unit,
        coalesce(sugar_100, 0) * amt_unit / 100 AS sugar_unit,
        coalesce(satfat_100, 0) * amt_unit / 100 AS satfat_unit,
        coalesce(sodium_mg_100, 0) * amt_unit / 100 AS sodium_unit,
        coalesce(fiber_100, 0) * amt_unit / 100 AS fiber_unit,
        coalesce(protein_100, 0) * amt_unit / 100 AS protein_unit
    FROM a
    WHERE amt_unit > 0 AND kcal_100 > 0
) TO '${OUT_UK}' (FORMAT parquet, COMPRESSION zstd);
