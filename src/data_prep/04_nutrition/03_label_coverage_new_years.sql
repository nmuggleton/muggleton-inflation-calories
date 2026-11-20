-- Before adding panel years 2022-2024: how much would their label files change
-- calorie coverage, and are their UPCs continuous with earlier years? A one-off
-- check for.

CREATE MACRO nnum(v) AS
TRY_CAST(regexp_extract(v, '^\s*([0-9]*\.?[0-9]+)', 1) AS DOUBLE);

CREATE TEMP TABLE cur AS
SELECT upc FROM read_parquet('${WP2_FINAL}/upc_nutrients.parquet')
WHERE kcal_100 IS NOT NULL;

CREATE TEMP TABLE new_lab AS
SELECT DISTINCT
    a.upc,
    min(TRY_CAST(a.year AS INT)) AS first_year
FROM
    read_parquet(
        '${WP2_INT}/panel/productattributes/panel_year=202[234]/data.parquet'
    ) AS a
WHERE
    nnum(a.calories_per_serving_size) IS NOT NULL
    AND nnum(a.serving_size_metric) > 0
    AND a.upc NOT IN (SELECT upc FROM cur)
GROUP BY a.upc;

SELECT
    'UPCs labelled: current pool' AS what,
    count(*) AS n
FROM cur
UNION ALL
SELECT
    'UPCs added by 2022-2024 panel labels, first labelled ' || first_year,
    count(*)
FROM new_lab
GROUP BY first_year
ORDER BY what;

-- Food purchase lines, all years, with the label UPC used in step 05
CREATE TEMP VIEW food AS
SELECT
    p.panel_year,
    p.upc,
    CASE
        WHEN p.is_private_label AND p.panel_year <= 2020
            THEN coalesce(x.upc_2021_masked, p.upc)
        ELSE p.upc
    END AS label_upc,
    p.net_price_paid AS spend
FROM
    read_parquet(
        '${WP2_FINAL}/purchases/*/*.parquet', hive_partitioning = false
    )
        AS p
LEFT JOIN read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet') AS x
    ON p.upc = x.upc_2020
WHERE p.is_food_department
UNION ALL
SELECT
    TRY_CAST(p.panel_year AS INT),
    p.upc,
    p.upc,
    p.total_price_paid - coalesce(p.coupon_value, 0)
FROM
    read_parquet(
        '${WP2_INT}/panel/purchases/panel_year=202[234]/data.parquet',
        hive_partitioning = false
    ) AS p
JOIN read_parquet('${WP2_FINAL}/xwalk_module.parquet') AS m
    ON TRY_CAST(p.product_module_code_hms AS INT) = m.product_module_code
WHERE m.is_food_department;

CREATE TEMP TABLE cov AS
SELECT
    f.panel_year,
    sum(f.spend) AS food_spend,
    sum(CASE WHEN c.upc IS NOT NULL THEN f.spend END)
    / sum(f.spend) AS cov_current,
    sum(CASE WHEN c.upc IS NOT NULL OR n.upc IS NOT NULL THEN f.spend END)
    / sum(f.spend) AS cov_with_new
FROM food AS f
LEFT JOIN cur AS c ON f.label_upc = c.upc
LEFT JOIN new_lab AS n ON f.label_upc = n.upc
GROUP BY ALL;

SELECT
    panel_year,
    round(food_spend / 1e6, 1) AS food_spend_m,
    round(100 * cov_current, 1) AS pct_labelled_now,
    round(100 * cov_with_new, 1) AS pct_with_2022_24,
    round(100 * (cov_with_new - cov_current), 2) AS added_pts
FROM cov
ORDER BY panel_year;

-- Continuity: the share of year-y food spend on UPCs also bought in y-1 (raw
-- UPC; 2021 dips because private-label UPCs were masked from 2021)
CREATE TEMP TABLE yu AS
SELECT DISTINCT
    panel_year,
    upc
FROM food
WHERE panel_year >= 2017;
SELECT
    f.panel_year,
    round(
        100 * sum(CASE WHEN y.upc IS NOT NULL THEN f.spend END)
        / sum(f.spend), 1
    ) AS pct_spend_on_upcs_bought_year_before
FROM food AS f
LEFT JOIN yu AS y ON y.upc = f.upc AND y.panel_year = f.panel_year - 1
WHERE f.panel_year >= 2018
GROUP BY ALL
ORDER BY 1;

-- Module codes on 2022+ purchase lines (needed for the food flag)
SELECT
    TRY_CAST(panel_year AS INT) AS panel_year,
    count(*) AS lines,
    round(100 * avg((m.product_module_code IS NOT NULL)::INT), 2)
        AS pct_module_known
FROM
    read_parquet(
        '${WP2_INT}/panel/purchases/panel_year=202[234]/data.parquet',
        hive_partitioning = false
    ) AS p
LEFT JOIN read_parquet('${WP2_FINAL}/xwalk_module.parquet') AS m
    ON TRY_CAST(p.product_module_code_hms AS INT) = m.product_module_code
GROUP BY ALL
ORDER BY 1;
