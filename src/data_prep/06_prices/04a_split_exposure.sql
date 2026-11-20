-- Exposure inflation priced from a random half of households, for target
-- calendar year ${YEAR} (t = ${YEAR}Q1..Q4, base t-4) and random split
-- ${SPLIT}. Robustness for the instrument: T2 prices each household's base
-- basket at state unit values built from other panel households' purchases, and
-- the instrument averages other households' T2, built from the same purchases.

CREATE TEMP TABLE xw AS
SELECT
    upc_2020,
    upc_2021_masked
FROM read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet');

-- Purchases with the buyer's half and state (the state of the panel year, as in
-- state_upc_q)
CREATE TEMP TABLE hu0 AS
SELECT
    h.household_code,
    (
        ('0x' || substr(md5(h.household_code || '-' || ${SPLIT}), 1, 8))
        ::BIGINT % 2
    )::INTEGER AS half,
    hy.fips_state_code AS st,
    h.yq,
    coalesce(
        CASE WHEN h.panel_year <= 2020 THEN x.upc_2021_masked END,
        h.upc
    ) AS item,
    sum(h.qty) AS qty,
    sum(h.spend) AS spend
FROM read_parquet([${HU_FILES}]) AS h
JOIN read_parquet('${WP2_FINAL}/household_year.parquet') AS hy
    ON hy.household_code = h.household_code AND hy.panel_year = h.panel_year
LEFT JOIN xw AS x ON h.upc = x.upc_2020
WHERE h.yq BETWEEN (${YEAR} - 1) * 10 + 1 AND ${YEAR} * 10 + 4
GROUP BY ALL;

CREATE TEMP TABLE hu AS
SELECT
    household_code,
    half,
    yq,
    item,
    sum(qty) AS qty,
    sum(spend) AS spend
FROM hu0
GROUP BY ALL;

CREATE TEMP TABLE su AS
SELECT
    st,
    half,
    yq,
    item,
    sum(qty) AS qty,
    sum(spend) AS spend
FROM hu0
GROUP BY ALL;

-- Household state: the base panel year, as for T2
CREATE TEMP TABLE hs AS
SELECT
    household_code,
    fips_state_code AS st
FROM read_parquet('${WP2_FINAL}/household_year.parquet')
WHERE panel_year = ${YEAR} - 1;

-- Every t-4 basket item, with the household's own purchase of it in t
CREATE TEMP TABLE bc AS
SELECT
    b.household_code,
    b.half,
    b.yq + 10 AS yq,
    b.item,
    b.qty AS q0,
    b.spend AS s0,
    c.qty AS q1,
    c.spend AS s1
FROM hu AS b
LEFT JOIN hu AS c
    ON
        c.household_code = b.household_code
        AND c.item = b.item AND c.yq = b.yq + 10
WHERE b.yq <= (${YEAR} - 1) * 10 + 4;

-- State x half x item price pairs (t and t-4)
CREATE TEMP TABLE sp AS
SELECT
    a.st,
    a.half,
    a.item,
    a.yq,
    a.spend AS st_spend,
    a.qty AS st_qty,
    b.spend AS s0_spend,
    b.qty AS s0_qty
FROM su AS a
JOIN su AS b
    ON
        b.st = a.st AND b.half = a.half
        AND b.item = a.item AND b.yq = a.yq - 10
WHERE a.yq >= ${YEAR} * 10 + 1;

COPY (
    WITH pr AS (
        SELECT
            bc.household_code,
            bc.half,
            bc.yq,
            bc.q0,
            (sp.st_spend - coalesce(bc.s1, 0))
            / nullif(sp.st_qty - coalesce(bc.q1, 0), 0) AS p_t_loo,
            (sp.s0_spend - bc.s0) / nullif(sp.s0_qty - bc.q0, 0) AS p_0_loo
        FROM bc
        JOIN hs ON hs.household_code = bc.household_code
        JOIN sp
            ON
                sp.st = hs.st AND sp.half = bc.half
                AND sp.item = bc.item AND sp.yq = bc.yq
    )

    SELECT
        household_code,
        yq,
        CAST(${SPLIT} AS SMALLINT) AS split,
        half,
        count(*) AS n_priced,
        sum(q0 * p_t_loo) / sum(q0 * p_0_loo) AS idx_half
    FROM pr
    -- Rounded and trimmed as T2 is in 01a_price_indices.sql
    WHERE
        round(p_t_loo, 9) > 0 AND round(p_0_loo, 9) > 0
        AND round(p_t_loo / p_0_loo, 9) BETWEEN round(1.0 / 3, 9) AND 3
    GROUP BY ALL
) TO '${WP2_INT}/price_split/split=${SPLIT}/year=${YEAR}.parquet'
(FORMAT parquet);
