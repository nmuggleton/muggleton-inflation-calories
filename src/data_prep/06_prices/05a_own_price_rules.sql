-- The household's own price index (T1 of 01a_price_indices.sql) under
-- alternative rules for extreme price changes, for target calendar year ${YEAR}
-- (t = ${YEAR}Q1..Q4, base t-4). Run per year by 05_own_price_rules.sh.

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
    sum(h.spend) AS spend,
    sum(h.spend_gross) AS gross
FROM read_parquet([${HU_FILES}]) AS h
LEFT JOIN xw AS x ON h.upc = x.upc_2020
WHERE h.yq BETWEEN (${YEAR} - 1) * 10 + 1 AND ${YEAR} * 10 + 4
GROUP BY ALL;

-- Pairs: relatives rounded before any bound is applied, as in 01a
CREATE TEMP TABLE pairs AS
SELECT
    b.household_code,
    c.yq,
    b.qty AS q0,
    b.spend AS s0,
    b.gross AS g0,
    c.qty AS q1,
    c.spend AS s1,
    c.gross AS g1,
    round((c.spend / c.qty) / nullif(b.spend / b.qty, 0), 9) AS r_net,
    round((c.gross / c.qty) / nullif(b.gross / b.qty, 0), 9) AS r_gross
FROM hu AS b
JOIN hu AS c
    ON
        c.household_code = b.household_code
        AND c.item = b.item AND c.yq = b.yq + 10
WHERE b.yq <= (${YEAR} - 1) * 10 + 4;

CREATE TEMP TABLE rules AS
SELECT *
FROM (
    VALUES
    ('net3', FALSE, 3.0),
    ('gross10', TRUE, 10.0),
    ('net10', FALSE, 10.0),
    ('gross3', TRUE, 3.0),
    ('none', FALSE, 'infinity'::DOUBLE)
) AS t (rule, on_gross, bound);

COPY (
    SELECT
        p.household_code,
        p.yq,
        r.rule,
        count(*) AS n_pairs,
        sum(p.q0 * p.s1 / p.q1) AS num,
        sum(p.s0) AS den,
        sum(p.q0 * p.g1 / p.q1) AS gnum,
        sum(p.g0) AS gden
    FROM pairs AS p
    CROSS JOIN rules AS r
    WHERE
        p.r_net > 0 AND isfinite(p.r_net)
        AND CASE WHEN r.on_gross THEN p.r_gross ELSE p.r_net END
        BETWEEN round(1.0 / r.bound, 9) AND r.bound
    GROUP BY ALL
) TO '${WP2_INT}/price_own_rules/index/year=${YEAR}.parquet' (FORMAT parquet);

-- Pairs beyond each bound, among pairs with a positive net price
COPY (
    SELECT
        household_code,
        yq,
        count(*) AS n_pairs,
        count(*) FILTER (r_net > 3) AS net_up3,
        count(*) FILTER (r_net < round(1.0 / 3, 9)) AS net_dn3,
        count(*) FILTER (r_net > 10) AS net_up10,
        count(*) FILTER (r_net < 0.1) AS net_dn10,
        count(*) FILTER (r_gross > 3) AS gross_up3,
        count(*) FILTER (r_gross < round(1.0 / 3, 9)) AS gross_dn3,
        count(*) FILTER (r_gross > 10) AS gross_up10,
        count(*) FILTER (r_gross < 0.1) AS gross_dn10
    FROM pairs
    WHERE r_net > 0 AND isfinite(r_net)
    GROUP BY ALL
) TO '${WP2_INT}/price_own_rules/tails/year=${YEAR}.parquet' (FORMAT parquet);

-- Coupon use over everything bought in the quarter, in t and in t-4: the coupon
-- value as a share of pre-coupon spending, and the share of items bought with a
-- coupon
COPY (
    WITH q AS (
        SELECT
            household_code,
            yq,
            count(*) AS n_items,
            count(*) FILTER (gross > spend + 0.005) AS n_coupon,
            sum(spend) AS spend,
            sum(gross) AS gross
        FROM hu
        GROUP BY ALL
    )

    SELECT
        c.household_code,
        c.yq,
        c.n_items,
        c.n_coupon,
        c.spend,
        c.gross,
        b.n_items AS n_items_base,
        b.n_coupon AS n_coupon_base,
        b.spend AS spend_base,
        b.gross AS gross_base
    FROM q AS c
    JOIN q AS b
        ON b.household_code = c.household_code AND b.yq = c.yq - 10
    WHERE c.yq >= ${YEAR} * 10 + 1
) TO '${WP2_INT}/price_own_rules/coupons/year=${YEAR}.parquet' (
    FORMAT parquet
);
