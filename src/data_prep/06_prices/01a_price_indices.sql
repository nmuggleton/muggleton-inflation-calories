-- Household food price indices for target calendar year ${YEAR} (quarters t =
-- ${YEAR}Q1..Q4, base t-4 = same quarter of ${YEAR}-1). Run per year by
-- src/data_prep/06_prices/01_price_indices.sh.

CREATE TEMP TABLE xw AS
SELECT
    upc_2020,
    upc_2021_masked
FROM read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet');

CREATE TEMP TABLE hu AS
SELECT
    h.household_code,
    h.yq,
    coalesce(
        CASE WHEN h.panel_year <= 2020 THEN x.upc_2021_masked END,
        h.upc
    ) AS item,
    sum(h.qty) AS qty,
    sum(h.spend) AS spend
FROM read_parquet([${HU_FILES}]) AS h
LEFT JOIN xw AS x ON h.upc = x.upc_2020   -- (no non-equi term: hash join)
WHERE h.yq BETWEEN (${YEAR} - 1) * 10 + 1 AND ${YEAR} * 10 + 4
GROUP BY ALL;

CREATE TEMP TABLE su AS
SELECT
    s.fips_state_code,
    s.yq,
    coalesce(
        CASE WHEN s.panel_year <= 2020 THEN x.upc_2021_masked END,
        s.upc
    ) AS item,
    sum(s.qty) AS qty,
    sum(s.spend) AS spend,
    sum(s.n_hh) AS n_hh
FROM read_parquet([${SU_FILES}]) AS s
LEFT JOIN xw AS x ON s.upc = x.upc_2020
WHERE s.yq BETWEEN (${YEAR} - 1) * 10 + 1 AND ${YEAR} * 10 + 4
GROUP BY ALL;

-- Household state: the panel year that contains most of quarter t
CREATE TEMP TABLE hs AS
SELECT
    household_code,
    panel_year,
    fips_state_code
FROM read_parquet('${WP2_FINAL}/household_year.parquet')
WHERE panel_year BETWEEN ${YEAR} - 1 AND ${YEAR};

-- One self-join: every t-4 basket item, with the household's own purchase of
-- the same item in t, if any
CREATE TEMP TABLE bc AS
SELECT
    b.household_code,
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

CREATE TEMP TABLE t1 AS
SELECT
    household_code,
    yq,
    count(*) AS n_matched,
    sum(q0 * s1 / q1) AS l_num,
    sum(s0) AS l_den,
    sum(s1) AS p_num,
    sum(q1 * s0 / q0) AS p_den,
    sum(s1) AS spend_matched_t
FROM bc
-- Relatives are rounded before the trim: floating-point noise in the spend sums
-- would otherwise put relatives of exactly 1/3 or 3 on either side
WHERE
    q1 IS NOT NULL
    AND round((s1 / q1) / (s0 / q0), 9) BETWEEN round(1.0 / 3, 9) AND 3
GROUP BY ALL;

-- State x item price pairs (t and t-4), for the leave-one-out unit values
CREATE TEMP TABLE sp AS
SELECT
    a.fips_state_code,
    a.item,
    a.yq,
    a.spend AS st_spend,
    a.qty AS st_qty,
    b.spend AS s0_spend,
    b.qty AS s0_qty
FROM su AS a JOIN su AS b
    ON
        b.fips_state_code = a.fips_state_code
        AND b.item = a.item AND b.yq = a.yq - 10
WHERE a.yq >= ${YEAR} * 10 + 1;

CREATE TEMP TABLE t2 AS
WITH pr AS (
    SELECT
        bc.household_code,
        bc.yq,
        bc.q0,
        (sp.st_spend - coalesce(bc.s1, 0))
        / nullif(sp.st_qty - coalesce(bc.q1, 0), 0) AS p_t_loo,
        (sp.s0_spend - bc.s0) / nullif(sp.s0_qty - bc.q0, 0) AS p_0_loo
    FROM bc
    JOIN hs
        ON
            hs.household_code = bc.household_code
            AND hs.panel_year = ${YEAR} - 1
    JOIN sp ON
        sp.fips_state_code = hs.fips_state_code
        AND sp.item = bc.item AND sp.yq = bc.yq
)

SELECT
    household_code,
    yq,
    count(*) AS n_base_priced,
    sum(q0 * p_t_loo) AS e_num,
    sum(q0 * p_0_loo) AS e_den
FROM pr
-- Rounded as in t1: a state total less the household's own spend can come out
-- as +/-1e-15 instead of 0
WHERE
    round(p_t_loo, 9) > 0 AND round(p_0_loo, 9) > 0
    AND round(p_t_loo / p_0_loo, 9) BETWEEN round(1.0 / 3, 9) AND 3
GROUP BY ALL;

CREATE TEMP TABLE nb AS               -- size of the t-4 basket (coverage)
SELECT
    household_code,
    yq,
    count(*) AS n_base_items,
    sum(s0) AS spend_base
FROM bc
GROUP BY ALL;

COPY (
    SELECT
        coalesce(t1.household_code, t2.household_code) AS household_code,
        coalesce(t1.yq, t2.yq) AS yq,
        t1.n_matched,
        t1.spend_matched_t,
        t1.l_num / t1.l_den AS idx_laspeyres,
        t1.p_num / t1.p_den AS idx_paasche,
        sqrt((t1.l_num / t1.l_den) * (t1.p_num / t1.p_den)) AS idx_fisher,
        t2.n_base_priced,
        nb.n_base_items,
        nb.spend_base,
        t2.e_num / t2.e_den AS idx_exposure,
        ln(t1.l_num / t1.l_den) - ln(t2.e_num / t2.e_den) AS shop_component
    FROM t1 FULL JOIN t2
        ON
            t1.household_code = t2.household_code
            AND t1.yq = t2.yq
    LEFT JOIN nb ON nb.household_code = coalesce(
        t1.household_code,
        t2.household_code
    ) AND nb.yq = coalesce(t1.yq, t2.yq
    )
) TO '${WP2_INT}/price_idx/year=${YEAR}.parquet' (FORMAT parquet);
