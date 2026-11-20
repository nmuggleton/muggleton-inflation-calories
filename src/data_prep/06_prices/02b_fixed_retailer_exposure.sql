-- Exposure inflation at two kinds of state price, for target calendar year
-- ${YEAR} (t = ${YEAR}Q1..Q4, base t-4). Robustness for the instrument: the
-- main T2 prices each item at the state unit value (all other households' spend
-- over packages), which moves if households switch retailers even when no
-- retailer changes its price.

CREATE TEMP TABLE xw AS
SELECT
    upc_2020,
    upc_2021_masked
FROM read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet');

-- The household's t-4 basket, as in 01a_price_indices.sql
CREATE TEMP TABLE b AS
SELECT
    h.household_code,
    h.yq + 10 AS yq,
    coalesce(
        CASE WHEN h.panel_year <= 2020 THEN x.upc_2021_masked END,
        h.upc
    ) AS item,
    sum(h.qty) AS q0
FROM read_parquet([${HU_FILES}]) AS h
LEFT JOIN xw AS x ON h.upc = x.upc_2020
WHERE h.yq BETWEEN (${YEAR} - 1) * 10 + 1 AND (${YEAR} - 1) * 10 + 4
GROUP BY ALL;

-- State x retailer x item x quarter, with quarters summed across panel years
CREATE TEMP TABLE sr AS
SELECT
    fips_state_code,
    retailer_code,
    yq,
    item,
    sum(qty) AS qty,
    sum(spend) AS spend
FROM read_parquet([${SR_FILES}])
WHERE yq BETWEEN (${YEAR} - 1) * 10 + 1 AND ${YEAR} * 10 + 4
GROUP BY ALL;

-- State prices for each item, t and t-4
CREATE TEMP TABLE sp AS
WITH uv AS (
    SELECT
        a.fips_state_code,
        a.item,
        a.yq,
        a.spend / a.qty AS uv_t,
        b.spend / b.qty AS uv_0
    FROM (
        SELECT
            fips_state_code,
            item,
            yq,
            sum(qty) AS qty,
            sum(spend) AS spend
        FROM sr
        GROUP BY ALL
    ) AS a
    JOIN (
        SELECT
            fips_state_code,
            item,
            yq,
            sum(qty) AS qty,
            sum(spend) AS spend
        FROM sr
        GROUP BY ALL
    ) AS b
        ON
            b.fips_state_code = a.fips_state_code AND b.item = a.item
            AND b.yq = a.yq - 10
    WHERE a.yq >= ${YEAR} * 10 + 1
),

fw AS (     -- retailers selling the item in the state in both quarters
    SELECT
        a.fips_state_code,
        a.item,
        a.yq,
        sum(b.qty * a.spend / a.qty) / sum(b.qty) AS fw_t,
        sum(b.spend) / sum(b.qty) AS fw_0,
        count(*) AS n_ret
    FROM sr AS a
    JOIN sr AS b
        ON
            b.fips_state_code = a.fips_state_code AND b.item = a.item
            AND b.retailer_code = a.retailer_code AND b.yq = a.yq - 10
    WHERE a.yq >= ${YEAR} * 10 + 1
    GROUP BY ALL
)

SELECT
    uv.*,
    fw.fw_t,
    fw.fw_0,
    fw.n_ret
FROM uv JOIN fw USING (fips_state_code, item, yq);

CREATE TEMP TABLE hs AS
SELECT
    household_code,
    panel_year,
    fips_state_code
FROM read_parquet('${WP2_FINAL}/household_year.parquet')
WHERE panel_year = ${YEAR} - 1;

COPY (
    SELECT
        b.household_code,
        b.yq,
        count(*) AS n_priced_fw,
        100 * (sum(b.q0 * sp.uv_t) / sum(b.q0 * sp.uv_0) - 1) AS t2_uv,
        100 * (sum(b.q0 * sp.fw_t) / sum(b.q0 * sp.fw_0) - 1) AS t2_fw,
        sum(b.q0 * sp.uv_0) AS base_cost
    FROM b
    JOIN hs ON hs.household_code = b.household_code
    JOIN sp
        ON
            sp.fips_state_code = hs.fips_state_code AND sp.item = b.item
            AND sp.yq = b.yq
    WHERE
        round(sp.uv_t / sp.uv_0, 9) BETWEEN round(1.0 / 3, 9) AND 3
        AND round(sp.fw_t / sp.fw_0, 9) BETWEEN round(1.0 / 3, 9) AND 3
    GROUP BY ALL
    HAVING count(*) >= 10
) TO '${WP2_INT}/price_idx_fw/year=${YEAR}.parquet' (FORMAT parquet);
