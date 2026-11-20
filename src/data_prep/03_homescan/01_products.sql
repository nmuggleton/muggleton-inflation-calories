-- Product dimensions, module crosswalk, 2021+ nutrition. Run by
-- src/data_prep/03_homescan/01_products.sh.

-- 1.
COPY (
    WITH x AS (
        SELECT
            product_module_code,
            any_value(product_module_descr) AS product_module_descr,
            any_value(product_group_code) AS product_group_code,
            any_value(product_group_descr) AS product_group_descr,
            any_value(department_code) AS department_code,
            any_value(department_descr) AS department_descr,
            count(DISTINCT product_group_code) AS n_groups_check,
            count(*) AS n_upc_versions,
            coalesce(any_value(department_code) IN (1, 2, 3, 4, 5, 6), false)
                AS is_food_department,
            (
                any_value(department_code) = 99 OR product_module_code = 750
                OR product_module_code BETWEEN 445 AND 469)
                AS is_magnet_product
        FROM read_parquet('${WP2_INT}/panel/products_master/data.parquet')
        GROUP BY product_module_code
    )

    SELECT
        x.*,
        coalesce(m.is_deferred_module, false) AS is_deferred_module,
        m.hms_only_note,
        m.is_rms_only_module,
        m.rms_years_no_data,
        (m.product_module_code IS NOT NULL) AS in_hierarchy_doc
    FROM x
    LEFT JOIN read_parquet('${WP2_INT}/ref/module_flags/data.parquet') AS m
        USING (product_module_code)
) TO '${WP2_FINAL}/xwalk_module.parquet' (FORMAT parquet, COMPRESSION zstd);

-- 2. Batch-1 product dimension
COPY (
    SELECT
        p.upc,
        p.upc_ver_uc,
        p.upc_descr,
        p.product_module_code,
        p.product_module_descr,
        p.product_group_code,
        p.product_group_descr,
        p.department_code,
        p.department_descr,
        p.brand_code_uc,
        p.brand_descr,
        p.multi,
        p.size1_code_uc,
        p.size1_amount,
        trim(p.size1_units) AS size1_units,
        p.dataset_found_uc,
        p.size1_change_flag_uc,
        (p.brand_code_uc = 536746) AS is_private_label,
        coalesce(x.is_magnet_product, false) AS is_magnet_product,
        coalesce(x.is_food_department, false) AS is_food_department
    FROM read_parquet('${WP2_INT}/panel/products_master/data.parquet') AS p
    LEFT JOIN read_parquet('${WP2_FINAL}/xwalk_module.parquet') AS x
        USING (product_module_code)
) TO '${WP2_FINAL}/dim_products_b1.parquet' (FORMAT parquet, COMPRESSION zstd);

-- 3.
CREATE TEMP VIEW pdesc AS
SELECT *
FROM
    read_parquet(
        '${WP2_INT}/panel/productdesc/*/data.parquet',
        union_by_name = true,
        hive_partitioning = false
    )
UNION ALL BY NAME
SELECT o.*
FROM
    read_parquet(
        '${WP2_INT}/rms_b2/productdesc/*/data.parquet',
        union_by_name = true,
        hive_partitioning = false
    ) AS o
ANTI JOIN
    (
        SELECT
            upc,
            year
        FROM
            read_parquet(
                '${WP2_INT}/panel/productdesc/*/data.parquet',
                union_by_name = true,
                hive_partitioning = false
            )
    ) AS f
    ON f.upc = o.upc AND f.year = o.year;
CREATE TEMP VIEW phier AS
SELECT *
FROM
    read_parquet(
        '${WP2_INT}/panel/producthierarchy/*/data.parquet',
        union_by_name = true,
        hive_partitioning = false
    )
UNION ALL BY NAME
SELECT o.*
FROM
    read_parquet(
        '${WP2_INT}/rms_b2/producthierarchy/*/data.parquet',
        union_by_name = true,
        hive_partitioning = false
    ) AS o
ANTI JOIN
    (
        SELECT
            upc,
            year
        FROM
            read_parquet(
                '${WP2_INT}/panel/producthierarchy/*/data.parquet',
                union_by_name = true,
                hive_partitioning = false
            )
    ) AS f
    ON f.upc = o.upc AND f.year = o.year;
CREATE TEMP VIEW pattr AS
SELECT *
FROM
    read_parquet(
        '${WP2_INT}/panel/productattributes/*/data.parquet',
        union_by_name = true,
        hive_partitioning = false
    )
UNION ALL BY NAME
SELECT o.*
FROM
    read_parquet(
        '${WP2_INT}/rms_b2/productattributes/*/data.parquet',
        union_by_name = true,
        hive_partitioning = false
    ) AS o
ANTI JOIN
    (
        SELECT
            upc,
            year
        FROM
            read_parquet(
                '${WP2_INT}/panel/productattributes/*/data.parquet',
                union_by_name = true,
                hive_partitioning = false
            )
    ) AS f
    ON
        f.upc = o.upc
        AND TRY_CAST(f.year AS SMALLINT) = TRY_CAST(o.year AS SMALLINT);
-- Nutrition: the scanner file takes precedence (see above)
CREATE TEMP VIEW pattr_nut AS
SELECT *
FROM
    read_parquet(
        '${WP2_INT}/rms_b2/productattributes/*/data.parquet',
        union_by_name = true,
        hive_partitioning = false
    )
UNION ALL BY NAME
SELECT o.*
FROM
    read_parquet(
        '${WP2_INT}/panel/productattributes/*/data.parquet',
        union_by_name = true,
        hive_partitioning = false
    ) AS o
ANTI JOIN
    (
        SELECT
            upc,
            year
        FROM
            read_parquet(
                '${WP2_INT}/rms_b2/productattributes/*/data.parquet',
                union_by_name = true,
                hive_partitioning = false
            )
    ) AS f
    ON
        f.upc = o.upc
        AND TRY_CAST(f.year AS SMALLINT) = TRY_CAST(o.year AS SMALLINT);
COPY (
    SELECT
        CAST(d.year AS SMALLINT) AS panel_year,
        d.upc,
        d.product_descr,
        d.product_module_code,
        d.product_module_descr,
        d.multi,
        h.department,
        h.department_cd,
        h.super_category,
        h.super_category_cd,
        h.category,
        h.category_cd,
        h.sub_category,
        h.sub_category_cd,
        h.segment,
        h.segment_cd,
        a.brand AS brand_descr,
        TRY_CAST(a.brand_cd AS BIGINT) AS brand_code_uc,
        a.product_size,
        a.base_size,
        a.total_size,
        CAST(NULL AS DOUBLE) AS size1_amount,   -- use purchase _hms
        CAST(NULL AS VARCHAR) AS size1_units,    -- fields instead
        (a.brand = 'MASKED') AS is_private_label,
        (h.upc IS NOT NULL) AS m_hierarchy,
        (a.upc IS NOT NULL) AS m_attributes
    FROM pdesc AS d
    LEFT JOIN phier AS h
        ON d.upc = h.upc AND d.year = h.year
    LEFT JOIN pattr AS a
        ON d.upc = a.upc AND d.year = TRY_CAST(a.year AS SMALLINT)
) TO '${WP2_FINAL}/dim_products_b2.parquet' (FORMAT parquet, COMPRESSION zstd);

-- 4.
CREATE OR REPLACE MACRO nnum(v) AS
TRY_CAST(regexp_extract(v, '^\s*([0-9]*\.?[0-9]+)', 1) AS DOUBLE);
CREATE OR REPLACE MACRO nhi(v) AS
TRY_CAST(regexp_extract(v, 'TO\s+([0-9]*\.?[0-9]+)', 1) AS DOUBLE);
CREATE OR REPLACE MACRO nunit(v) AS
NULLIF(trim(regexp_extract(v, '[0-9.]+\s*([A-Z][A-Z ]*)$', 1)), '');
CREATE OR REPLACE MACRO nstat(v) AS CASE
    WHEN v IS NULL THEN 'missing'
    WHEN v IN ('NOT STATED', 'NOT COLLECTED', 'NOT APPLICABLE') THEN lower(v)
    WHEN nnum(v) IS NOT NULL THEN 'value' ELSE 'other'
END;

COPY (
    SELECT
        TRY_CAST(year AS SMALLINT) AS panel_year,
        upc,
        nstat(calories_per_serving_size) AS nutrition_status,
        calories_per_serving_size AS kcal_txt,
        nnum(calories_per_serving_size) AS kcal_val,
        nunit(calories_per_serving_size) AS kcal_unit,
        fat_calorie_per_serving_size AS fat_kcal_txt,
        nnum(fat_calorie_per_serving_size) AS fat_kcal_val,
        total_fat_gram AS fat_txt,
        nnum(total_fat_gram) AS fat_val,
        nhi(total_fat_gram) AS fat_hi,
        nunit(total_fat_gram) AS fat_unit,
        saturated_fat_gram AS satfat_txt,
        nnum(saturated_fat_gram) AS satfat_val,
        nhi(saturated_fat_gram) AS satfat_hi,
        nunit(saturated_fat_gram) AS satfat_unit,
        trans_fat_gram AS transfat_txt,
        nnum(trans_fat_gram) AS transfat_val,
        nhi(trans_fat_gram) AS transfat_hi,
        nunit(trans_fat_gram) AS transfat_unit,
        cholesterol AS chol_txt,
        nnum(cholesterol) AS chol_val,
        nhi(cholesterol) AS chol_hi,
        nunit(cholesterol) AS chol_unit,
        sodium AS sodium_txt,
        nnum(sodium) AS sodium_val,
        nhi(sodium) AS sodium_hi,
        nunit(sodium) AS sodium_unit,
        total_carbohydrate_gram AS carb_txt,
        nnum(total_carbohydrate_gram) AS carb_val,
        nhi(total_carbohydrate_gram) AS carb_hi,
        nunit(total_carbohydrate_gram) AS carb_unit,
        dietary_fiber AS fiber_txt,
        nnum(dietary_fiber) AS fiber_val,
        nhi(dietary_fiber) AS fiber_hi,
        nunit(dietary_fiber) AS fiber_unit,
        sugar_gram AS sugar_txt,
        nnum(sugar_gram) AS sugar_val,
        nhi(sugar_gram) AS sugar_hi,
        nunit(sugar_gram) AS sugar_unit,
        protein_gram AS protein_txt,
        nnum(protein_gram) AS protein_val,
        nhi(protein_gram) AS protein_hi,
        nunit(protein_gram) AS protein_unit,
        serving_size_metric AS serving_metric_txt,
        nnum(serving_size_metric) AS serving_metric_val,
        nunit(serving_size_metric) AS serving_metric_unit,
        serving_size_household AS serving_household_txt,
        serving_per_container AS servings_txt,
        nnum(serving_per_container) AS servings_val,
        nunit(serving_per_container) AS servings_unit,
        product_size,
        base_size,
        total_size
    FROM pattr_nut
) TO '${WP2_FINAL}/nutrition_b2.parquet' (FORMAT parquet, COMPRESSION zstd);
