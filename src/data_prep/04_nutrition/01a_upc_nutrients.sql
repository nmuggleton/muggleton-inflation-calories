-- UPC-level nutrient profile from the 2021 (panel) and 2022 (scanner annual
-- files) NielsenIQ label attributes, plus the private-label (PL) crosswalk that
-- lets 2004-2020 PL purchases borrow the label of their 2021 masked code. Run
-- by src/data_prep/04_nutrition/01_upc_nutrients.sh.
--
-- Definitions (all per 100 g for foods, per 100 ml for drinks; "basis"):
--   basis       'g' if the label serving size is in grams (or mg),
--               'ml' if in millilitres/cc. Mixed units are never pooled.
--   kcal_100    kcal per serving / serving size x 100
--   <nut>_100   grams (sodium: mg) per 100 g|ml, same scaling
--   unit_size   size of ONE item (product_size), converted to g or ml in
--               the label's basis. Pack mass on a purchase line =
--               unit_size x multi x quantity (multi from the purchase).
--   kcal_pkg_srv  kcal per serving x servings per container (cross-check)
--   pack_size_srv servings x serving size (fallback pack size, g or ml)
-- Flags (kept as columns, never dropped):
--   f_kcal_implausible  kcal_100 > 900 on a g basis (above pure fat) or
--                       > 900 on ml (oils labelled in ml), or kcal per
--                       serving present but serving size missing
--   f_srv_mismatch      |kcal_pkg_srv / (kcal_100 x unit_size/100) - 1| > 0.25
--                       (servings per container disagrees with pack size)
--   f_nutrient_cleaned  a macronutrient > 100 g per 100 g|ml (set NULL) or
--                       sodium above pure salt (GRAM->mg undone, else NULL)
--   f_macro_mismatch    |4 x carb + 4 x protein + 9 x fat - kcal| / kcal > 0.30
--                       with kcal >= 20 per serving (Atwater check)

CREATE MACRO g100(v, srv) AS
CASE WHEN 100 * v / nullif(srv, 0) <= 100 THEN 100 * v / nullif(srv, 0) END;

-- 1. PL crosswalk (tab-delimited, header row, UTF-8 BOM) ------------------
COPY (
    SELECT
        lpad(trim(column0), 12, '0') AS upc_2020,
        lpad(trim(column1), 12, '0') AS upc_2021_masked
    FROM
        read_csv(
            '${PL_XWALK}', delim = '\t', header = false, skip = 1,
            all_varchar = true
        )
    WHERE trim(column0) <> ''
) TO '${WP2_INT}/ref/pl_crosswalk/data.parquet' (FORMAT parquet);

-- 2. One label per UPC: prefer 2021 (panel) over 2022 (scanner files) -----
CREATE TEMP TABLE lab AS
SELECT * EXCLUDE (rk) FROM (
    SELECT
        n.*,
        row_number() OVER (
            PARTITION BY upc
            ORDER BY (kcal_val IS NULL), panel_year
        ) AS rk
    FROM read_parquet('${WP2_FINAL}/nutrition_b2.parquet') AS n
)
WHERE rk = 1;

CREATE TEMP TABLE b AS
SELECT
    l.upc,
    l.panel_year AS label_year,
    l.nutrition_status,
    l.kcal_val AS kcal_srv,
    CASE
        WHEN l.serving_metric_unit IN ('GRAM', 'GRAM MIX') THEN 'g'
        WHEN l.serving_metric_unit = 'MILLIGRAM' THEN 'g'
        WHEN l.serving_metric_unit IN ('MILLILITER', 'CUBIC CENTIMETER')
            THEN 'ml'
    END AS basis,
    CASE
        WHEN l.serving_metric_unit = 'MILLIGRAM'
            THEN l.serving_metric_val / 1000
        ELSE l.serving_metric_val
    END AS srv_size,
    l.servings_val AS servings,
    -- Nutrients per serving, in g (sodium and cholesterol in mg)
    CASE
        WHEN l.fat_unit = 'MILLIGRAM' THEN l.fat_val / 1000
        ELSE l.fat_val
    END AS fat_srv,
    CASE
        WHEN l.satfat_unit = 'MILLIGRAM' THEN l.satfat_val / 1000
        ELSE l.satfat_val
    END AS satfat_srv,
    CASE
        WHEN l.sugar_unit = 'MILLIGRAM' THEN l.sugar_val / 1000
        ELSE l.sugar_val
    END AS sugar_srv,
    CASE
        WHEN l.carb_unit = 'MILLIGRAM' THEN l.carb_val / 1000
        ELSE l.carb_val
    END AS carb_srv,
    CASE
        WHEN l.fiber_unit = 'MILLIGRAM' THEN l.fiber_val / 1000
        ELSE l.fiber_val
    END AS fiber_srv,
    CASE
        WHEN l.protein_unit = 'MILLIGRAM' THEN l.protein_val / 1000
        ELSE l.protein_val
    END AS protein_srv,
    CASE
        WHEN l.sodium_unit = 'GRAM' THEN l.sodium_val * 1000
        ELSE l.sodium_val
    END AS sodium_srv_mg,
    (l.sodium_unit = 'GRAM') AS sodium_unit_gram,
    -- The size of one item, from product_size, e.g. "12 FLUID OUNCE", "1.5
    -- POUND"
    TRY_CAST(
        regexp_extract(l.product_size, '^\s*([0-9]*\.?[0-9]+)', 1)
        AS DOUBLE
    ) AS size_num,
    trim(regexp_extract(l.product_size, '[A-Z][A-Z ]*$')) AS size_unit_b2,
    l.product_size
FROM lab AS l;

CREATE TEMP TABLE c AS
SELECT
    b.*,
    CASE size_unit_b2
        WHEN 'OUNCE' THEN size_num * 28.3495 WHEN
            'POUND'
            THEN size_num * 453.592
        WHEN 'GRAM' THEN size_num WHEN 'KILOGRAM' THEN size_num * 1000
        WHEN 'FLUID OUNCE' THEN size_num * 29.5735
        WHEN 'GALLON' THEN size_num * 3785.41 WHEN
            'QUART'
            THEN size_num * 946.353
        WHEN 'PINT' THEN size_num * 473.176 WHEN 'LITER' THEN size_num * 1000
        WHEN 'MILLILITER' THEN size_num
    END AS unit_size_raw,
    CASE
        WHEN size_unit_b2 IN ('OUNCE', 'POUND', 'GRAM', 'KILOGRAM') THEN 'g'
        WHEN size_unit_b2 IN (
            'FLUID OUNCE', 'GALLON', 'QUART', 'PINT',
            'LITER', 'MILLILITER'
        ) THEN 'ml'
        WHEN size_unit_b2 IN ('COUNT', 'DOZEN') THEN 'count' END
        AS size_basis
FROM b;

-- 3. Final UPC table --------------------------------------------------------
COPY (
    SELECT
        c.upc,
        c.label_year,
        c.nutrition_status,
        c.basis,
        c.srv_size,
        c.servings,
        c.kcal_srv,
        c.product_size,
        c.size_unit_b2,
        c.size_basis,
        -- The size of one item, used only when its basis matches the label
        -- basis (1 g ~ 1 ml is NOT assumed)
        CASE WHEN c.size_basis = c.basis THEN c.unit_size_raw END AS unit_size,
        100 * c.kcal_srv / nullif(c.srv_size, 0) AS kcal_100,
        -- Macronutrients: a density above 100 g per 100 g|ml is impossible, so
        -- it is set to NULL (label entry errors; flagged f_nutrient_cleaned)
        g100(c.fat_srv, c.srv_size) AS fat_100,
        g100(c.satfat_srv, c.srv_size) AS satfat_100,
        g100(c.sugar_srv, c.srv_size) AS sugar_100,
        g100(c.carb_srv, c.srv_size) AS carb_100,
        g100(c.fiber_srv, c.srv_size) AS fiber_100,
        g100(c.protein_srv, c.srv_size) AS protein_100,
        -- Sodium: anything above pure salt (38,758 mg/100 g; cap 40,000) is an
        -- error.
        CASE
            WHEN 100 * c.sodium_srv_mg / nullif(c.srv_size, 0) <= 40000
                THEN 100 * c.sodium_srv_mg / nullif(c.srv_size, 0)
            WHEN
                c.sodium_unit_gram
                AND 100 * c.sodium_srv_mg / nullif(c.srv_size, 0) / 1000
                <= 40000
                THEN 100 * c.sodium_srv_mg / nullif(c.srv_size, 0) / 1000
        END AS sodium_mg_100,
        (greatest(
            c.fat_srv, c.satfat_srv, c.sugar_srv, c.carb_srv, c.fiber_srv,
            c.protein_srv
        ) / nullif(c.srv_size, 0
        ) > 1
        OR 100 * c.sodium_srv_mg / nullif(c.srv_size, 0) > 40000)
            AS f_nutrient_cleaned,
        c.kcal_srv * c.servings AS kcal_pkg_srv,
        -- Pack size implied by the label (servings x serving size), used only
        -- when product_size is a COUNT or in the other basis
        c.servings * c.srv_size AS pack_size_srv,
        -- flags
        (c.kcal_srv IS NOT NULL AND (
            c.srv_size IS NULL OR c.srv_size <= 0
            OR 100 * c.kcal_srv / c.srv_size > 900
        )) AS f_kcal_implausible,
        (
            c.size_basis = c.basis AND c.servings > 0 AND c.kcal_srv > 0
            AND c.srv_size > 0 AND abs(
                (c.kcal_srv * c.servings)
                / (c.kcal_srv / c.srv_size * c.unit_size_raw) - 1
            ) > 0.25
        )
            AS f_srv_mismatch,
        (c.kcal_srv >= 20 AND abs(
            4 * coalesce(c.carb_srv, 0)
            + 4 * coalesce(c.protein_srv, 0) + 9 * coalesce(c.fat_srv, 0)
            - c.kcal_srv
        ) / c.kcal_srv > 0.30) AS f_macro_mismatch,
        d.product_module_code,
        d.is_private_label AS is_pl_masked_b2
    FROM c
-- 139k UPCs carry a different module in 2021 and 2022: take the earliest
    -- year's (the panel's own 2021 hierarchy), not an arbitrary one
    LEFT JOIN (SELECT
        upc,
        arg_min(product_module_code, panel_year)
        FILTER (product_module_code IS NOT NULL)
            AS product_module_code,
        bool_or(is_private_label) AS is_private_label
    FROM read_parquet('${WP2_FINAL}/dim_products_b2.parquet')
    GROUP BY upc) AS d USING (upc)
) TO '${WP2_FINAL}/upc_nutrients.parquet' (FORMAT parquet, COMPRESSION zstd);

-- 4. Module-level fallback: median densities among clean labelled UPCs ----
COPY (
    SELECT
        product_module_code,
        basis,
        count(*) AS n_upc,
        median(kcal_100) AS kcal_100,
        median(sugar_100) AS sugar_100,
        median(satfat_100) AS satfat_100,
        median(sodium_mg_100) AS sodium_mg_100,
        median(fiber_100) AS fiber_100,
        median(protein_100) AS protein_100,
        median(fat_100) AS fat_100,
        median(carb_100) AS carb_100
    FROM read_parquet('${WP2_FINAL}/upc_nutrients.parquet')
    WHERE
        kcal_100 IS NOT NULL AND NOT f_kcal_implausible
        AND basis IS NOT NULL AND product_module_code IS NOT NULL
    GROUP BY ALL
    HAVING count(*) >= 5
) TO '${WP2_FINAL}/module_nutrient_fallback.parquet' (FORMAT parquet);
