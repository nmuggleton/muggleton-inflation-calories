-- Per-year household reporting-coverage aggregates. Computed right after each
-- year's merge so the household-year panel can be built without re-reading
-- every purchase file (lets the build stream year by year when raw data are
-- fetched remotely).

CREATE OR REPLACE TEMP VIEW t AS
SELECT *
FROM read_parquet('${WP2_INT}/panel/trips/panel_year=${YEAR}/data.parquet');
CREATE OR REPLACE TEMP VIEW m AS
SELECT *
FROM read_parquet('${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet');

CREATE OR REPLACE TEMP TABLE trip_agg AS
SELECT
    household_code,
    count(*) AS n_trips,
    count(DISTINCT purchase_date) AS n_shopping_days,
    count(DISTINCT date_trunc('month', purchase_date)) AS n_months_with_trips,
    min(purchase_date) AS first_trip_date,
    max(purchase_date) AS last_trip_date,
    sum(total_spent) AS trip_total_spent,
    avg((store_code_uc > 0)::INT) AS sh_trips_store_identified,
    count(DISTINCT retailer_code) AS n_retailers
FROM t
GROUP BY ALL;

CREATE OR REPLACE TEMP TABLE line_agg AS
SELECT
    household_code,
    count(*) AS n_purchase_lines,
    sum(net_price_paid) AS spend_all,
    sum(CASE WHEN is_food_department THEN net_price_paid END) AS spend_food,
    sum(CASE WHEN is_magnet_product THEN net_price_paid END) AS spend_magnet,
    sum(CASE WHEN is_private_label THEN net_price_paid END)
        AS spend_private_label,
    sum(CASE WHEN store_code_uc > 0 THEN net_price_paid END)
        AS spend_store_identified,
    count(DISTINCT upc) AS n_distinct_upcs
FROM m
GROUP BY ALL;

-- Nutrition coverage (2021+): the share of food spend on UPCs with parsed kcal.
CREATE OR REPLACE TEMP TABLE nut_agg AS
SELECT
    m.household_code,
    sum(CASE WHEN n.kcal_val IS NOT NULL THEN m.net_price_paid END)
    / nullif(sum(m.net_price_paid), 0) AS sh_food_spend_with_kcal
FROM m
LEFT JOIN (
    SELECT
        upc,
        kcal_val
    FROM read_parquet('${WP2_FINAL}/nutrition_b2.parquet')
    WHERE panel_year = ${YEAR}
) AS n
    USING (upc)
WHERE m.is_food_department AND ${YEAR} >= 2021
GROUP BY ALL;

COPY (
    SELECT
        CAST(${YEAR} AS SMALLINT) AS panel_year,
        *
    FROM trip_agg
    FULL JOIN line_agg USING (household_code)
    LEFT JOIN nut_agg USING (household_code)
) TO '${WP2_FINAL}/_agg/hh_year/panel_year=${YEAR}.parquet' (FORMAT parquet);
