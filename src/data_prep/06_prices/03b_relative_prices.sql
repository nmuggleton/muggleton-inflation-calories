-- Where does the gap between a household's own prices and state prices come
-- from? For target calendar year ${YEAR} (t = ${YEAR}Q1..Q4, base t-4), each
-- purchase is priced at what OTHER households in the state paid for the same
-- item, at four grains, each nested in the one before: p0  state x item x
-- quarter               (as the state price in T2) p1  + retailer
-- where the household shops p2  + month                               when,
-- within the quarter p3  + deal flag                           on promotion or
-- not Leave-one-out: the household's own purchases are taken out of every cell.

CREATE TEMP TABLE c AS
SELECT
    p.household_code,
    p.st,
    hs.fips_state_code AS st_base,
    p.ret,
    p.item,
    p.ym,
    p.deal,
    (p.ym // 100) * 10 + ((p.ym % 100) - 1) // 3 + 1 AS yq,
    sum(p.q) AS q,
    sum(p.s) AS s,
    sum(p.g) AS g
FROM read_parquet([${PC_FILES}]) AS p
JOIN read_parquet('${WP2_FINAL}/household_year.parquet') AS hs
    ON hs.household_code = p.household_code AND hs.panel_year = ${YEAR} - 1
WHERE p.ym BETWEEN (${YEAR} - 1) * 100 + 1 AND ${YEAR} * 100 + 12
GROUP BY ALL;

-- Every line with the totals of its cell (all buyers) and the household's own
-- totals in the same cell, at each grain
CREATE TEMP TABLE ln AS
SELECT
    household_code,
    item,
    yq,
    q,
    s,
    g,
    (st = st_base) AS in_base,
    sum(q) OVER w3 AS q3,
    sum(s) OVER w3 AS s3,
    sum(g) OVER w3 AS g3,
    sum(q) OVER w2 AS q2,
    sum(s) OVER w2 AS s2,
    sum(g) OVER w2 AS g2,
    sum(q) OVER w1 AS q1,
    sum(s) OVER w1 AS s1,
    sum(g) OVER w1 AS g1,
    sum(q) OVER w0 AS q0,
    sum(s) OVER w0 AS s0,
    sum(g) OVER w0 AS g0,
    sum(q) OVER h2 AS hq2,
    sum(s) OVER h2 AS hs2,
    sum(g) OVER h2 AS hg2,
    sum(q) OVER h1 AS hq1,
    sum(s) OVER h1 AS hs1,
    sum(g) OVER h1 AS hg1,
    sum(q) OVER h0 AS hq0,
    sum(s) OVER h0 AS hs0,
    sum(g) OVER h0 AS hg0
FROM c
WINDOW
    w3 AS (PARTITION BY st, ret, item, ym, deal),
    w2 AS (PARTITION BY st, ret, item, ym),
    w1 AS (PARTITION BY st, ret, item, yq),
    w0 AS (PARTITION BY st, item, yq),
    h2 AS (PARTITION BY household_code, st, ret, item, ym),
    h1 AS (PARTITION BY household_code, st, ret, item, yq),
    h0 AS (PARTITION BY household_code, st, item, yq);

-- Leave-one-out prices (rounded before the sign test, as in 01a: a cell total
-- less the household's own can come out as +/-1e-15 instead of 0), then
-- household x item x quarter spending at each price
CREATE TEMP TABLE lp AS
WITH pr AS (
    SELECT
        household_code,
        item,
        yq,
        q,
        s,
        g,
        in_base,
        CASE WHEN round(q0 - hq0, 9) > 0 AND round(s0 - hs0, 9) > 0
            THEN (s0 - hs0) / (q0 - hq0) END AS p0,
        CASE WHEN round(q1 - hq1, 9) > 0 AND round(s1 - hs1, 9) > 0
            THEN (s1 - hs1) / (q1 - hq1) END AS p1,
        CASE WHEN round(q2 - hq2, 9) > 0 AND round(s2 - hs2, 9) > 0
            THEN (s2 - hs2) / (q2 - hq2) END AS p2,
        CASE WHEN round(q3 - q, 9) > 0 AND round(s3 - s, 9) > 0
            THEN (s3 - s) / (q3 - q) END AS p3,
        CASE WHEN round(q0 - hq0, 9) > 0 AND round(g0 - hg0, 9) > 0
            THEN (g0 - hg0) / (q0 - hq0) END AS pg0,
        CASE WHEN round(q1 - hq1, 9) > 0 AND round(g1 - hg1, 9) > 0
            THEN (g1 - hg1) / (q1 - hq1) END AS pg1,
        CASE WHEN round(q2 - hq2, 9) > 0 AND round(g2 - hg2, 9) > 0
            THEN (g2 - hg2) / (q2 - hq2) END AS pg2,
        CASE WHEN round(q3 - q, 9) > 0 AND round(g3 - g, 9) > 0
            THEN (g3 - g) / (q3 - q) END AS pg3
    FROM ln
)

SELECT
    household_code,
    item,
    yq,
    sum(q) AS q,
    sum(s) AS s,
    sum(g) AS g,
    bool_and(in_base AND p0 IS NOT NULL AND pg0 IS NOT NULL) AS ok,
    sum(q * p0) AS x0,
    sum(q * coalesce(p1, p0)) AS x1,
    sum(q * coalesce(p2, p1, p0)) AS x2,
    sum(q * coalesce(p3, p2, p1, p0)) AS x3,
    sum(q * coalesce(pg3, pg2, pg1, pg0)) AS xg3,
    coalesce(sum(q) FILTER (p1 IS NOT NULL), 0) / sum(q) AS f1,
    coalesce(sum(q) FILTER (p2 IS NOT NULL), 0) / sum(q) AS f2,
    coalesce(sum(q) FILTER (p3 IS NOT NULL), 0) / sum(q) AS f3
FROM pr
GROUP BY ALL;

COPY (
    WITH m AS (
        SELECT
            b.household_code,
            b.yq + 10 AS yq,
            b.q AS q0,
            b.s AS s0,
            b.g AS g0,
            c.q AS q1,
            c.s AS s1,
            c.g AS g1,
            b.x0 AS b0,
            b.x1 AS b1,
            b.x2 AS b2,
            b.x3 AS b3,
            b.xg3 AS bg3,
            c.x0 AS c0,
            c.x1 AS c1,
            c.x2 AS c2,
            c.x3 AS c3,
            c.xg3 AS cg3,
            b.f1,
            b.f2,
            b.f3
        FROM lp AS b
        JOIN lp AS c
            ON
                c.household_code = b.household_code
                AND c.item = b.item AND c.yq = b.yq + 10
        WHERE
            b.yq <= (${YEAR} - 1) * 10 + 4
            AND b.ok AND c.ok
            AND b.s > 0 AND c.s > 0
            AND round(
                CASE
                    WHEN ${OWN_ON_GROSS}
                        THEN (c.g / c.q) / (b.g / b.q)
                    ELSE (c.s / c.q) / (b.s / b.q)
                END,
                9
            )
            BETWEEN round(1.0 / ${OWN_BOUND}, 9) AND ${OWN_BOUND}
            AND round((c.x0 / c.q) / (b.x0 / b.q), 9)
            BETWEEN round(1.0 / 3, 9) AND 3
    )

    SELECT
        household_code,
        yq,
        count(*) AS n_valid,
        sum(q0 * s1 / q1) / sum(s0) AS own,
        sum(q0 * g1 / q1) / sum(g0) AS own_g,
        sum(q0 * c0 / q1) / sum(b0) AS i0,
        sum(q0 * c1 / q1) / sum(b1) AS i1,
        sum(q0 * c2 / q1) / sum(b2) AS i2,
        sum(q0 * c3 / q1) / sum(b3) AS i3,
        sum(q0 * cg3 / q1) / sum(bg3) AS i3_g,
        sum(s0 * f1) / sum(s0) AS cov1,
        sum(s0 * f2) / sum(s0) AS cov2,
        sum(s0 * f3) / sum(s0) AS cov3
    FROM m
    GROUP BY ALL
) TO '${WP2_INT}/${REL_DIR}/year=${YEAR}.parquet' (FORMAT parquet);
