-- Diagnostics for the rule that screens extreme price changes out of the
-- household's own price index, for target calendar year ${YEAR} (t =
-- ${YEAR}Q1..Q4, base t-4). Run per year by 07_outlier_diagnostics.sh.

CREATE TEMP TABLE xw AS
SELECT
    upc_2020,
    upc_2021_masked
FROM read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet');

CREATE TEMP TABLE hs AS
SELECT
    household_code,
    panel_year,
    fips_state_code
FROM read_parquet('${WP2_FINAL}/household_year.parquet')
WHERE panel_year BETWEEN ${YEAR} - 1 AND ${YEAR} + 1;

CREATE TEMP TABLE raw AS
SELECT
    h.household_code,
    h.panel_year,
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

CREATE TEMP TABLE hu AS
SELECT
    household_code,
    yq,
    item,
    sum(qty) AS qty,
    sum(spend) AS spend,
    sum(gross) AS gross
FROM raw
GROUP BY ALL;

-- state totals as in 01a (net of coupons)
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

-- the same state totals before coupons (state of the panel year, as
-- state_upc_q); spend is kept to check them against su
CREATE TEMP TABLE gsu AS
SELECT
    hs.fips_state_code,
    r.yq,
    r.item,
    sum(r.qty) AS qty,
    sum(r.spend) AS spend,
    sum(r.gross) AS gross
FROM raw AS r
JOIN hs USING (household_code, panel_year)
GROUP BY ALL;
DROP TABLE raw;

-- every t-4 basket item with the household's own purchase in t (if any)
CREATE TEMP TABLE bc AS
SELECT
    b.household_code,
    b.yq + 10 AS yq,
    b.item,
    b.qty AS q0,
    b.spend AS s0,
    b.gross AS g0,
    c.qty AS q1,
    c.spend AS s1,
    c.gross AS g1,
    round((c.spend / c.qty) / nullif(b.spend / b.qty, 0), 9) AS ro,
    round((c.gross / c.qty) / nullif(b.gross / b.qty, 0), 9) AS rg
FROM hu AS b
LEFT JOIN hu AS c
    ON
        c.household_code = b.household_code
        AND c.item = b.item AND c.yq = b.yq + 10
WHERE b.yq <= (${YEAR} - 1) * 10 + 4;
DROP TABLE hu;

-- pairs with a positive net price in both quarters, and their band
CREATE TEMP TABLE pr AS
SELECT
    *,
    CASE
        WHEN ro > 20 THEN 'up_b5'
        WHEN ro > 10 THEN 'up_b4'
        WHEN ro > 5 THEN 'up_b3'
        WHEN ro > 3.5 THEN 'up_b2'
        WHEN ro > 3 THEN 'up_b1'
        WHEN ro > 2 THEN 'up_r2'
        WHEN ro < 0.05 THEN 'dn_b5'
        WHEN ro < 0.1 THEN 'dn_b4'
        WHEN ro < 0.2 THEN 'dn_b3'
        WHEN ro < round(1 / 3.5, 9) THEN 'dn_b2'
        WHEN ro < round(1.0 / 3, 9) THEN 'dn_b1'
        WHEN ro < 0.5 THEN 'dn_r2'
        ELSE 'core'
    END AS band
FROM bc
WHERE q1 IS NOT NULL AND ro > 0 AND isfinite(ro);

COPY (
    PIVOT (
        SELECT
            household_code,
            yq,
            band,
            count(*) OVER w AS n_pairs,
            sum(s0) OVER w AS s0_all,
            s0
        FROM pr
        WINDOW w AS (PARTITION BY household_code, yq)
    )
    ON band IN (
        'up_r2', 'up_b1', 'up_b2', 'up_b3', 'up_b4', 'up_b5',
        'dn_r2', 'dn_b1', 'dn_b2', 'dn_b3', 'dn_b4', 'dn_b5'
    )
    USING count(*) AS nb, coalesce(sum(s0), 0) AS sb
    GROUP BY household_code, yq, n_pairs, s0_all
) TO '${WP2_INT}/outlier_diagnostics/bands/year=${YEAR}.parquet' (
    FORMAT parquet
);

-- exposure with other screens on the state price relative.
CREATE TEMP TABLE en AS
WITH p AS (
    SELECT
        bc.household_code,
        bc.yq,
        bc.q0,
        (s1.spend - coalesce(bc.s1, 0))
        / nullif(s1.qty - coalesce(bc.q1, 0), 0) AS pt,
        (s0.spend - bc.s0) / nullif(s0.qty - bc.q0, 0) AS p0
    FROM bc
    JOIN hs
        ON
            hs.household_code = bc.household_code
            AND hs.panel_year = ${YEAR} - 1
    JOIN su AS s1
        ON
            s1.fips_state_code = hs.fips_state_code
            AND s1.item = bc.item AND s1.yq = bc.yq
    JOIN su AS s0
        ON
            s0.fips_state_code = hs.fips_state_code
            AND s0.item = bc.item AND s0.yq = bc.yq - 10
)

SELECT
    household_code,
    yq,
    count(*) FILTER (
        round(pt / p0, 9) BETWEEN round(1.0 / 3, 9) AND 3
    ) AS en_t3,
    sum(q0 * pt) FILTER (
        round(pt / p0, 9) BETWEEN round(1.0 / 3, 9) AND 3
    ) AS enum_t3,
    sum(q0 * p0) FILTER (
        round(pt / p0, 9) BETWEEN round(1.0 / 3, 9) AND 3
    ) AS eden_t3,
    count(*) FILTER (round(pt / p0, 9) BETWEEN 0.1 AND 10) AS en_t10,
    sum(q0 * pt) FILTER (round(pt / p0, 9) BETWEEN 0.1 AND 10) AS enum_t10,
    sum(q0 * p0) FILTER (round(pt / p0, 9) BETWEEN 0.1 AND 10) AS eden_t10
FROM p
WHERE round(pt, 9) > 0 AND round(p0, 9) > 0
GROUP BY ALL;

CREATE TEMP TABLE eg AS
WITH p AS (
    SELECT
        bc.household_code,
        bc.yq,
        bc.q0,
        (s1.gross - coalesce(bc.g1, 0))
        / nullif(s1.qty - coalesce(bc.q1, 0), 0) AS gt,
        (s0.gross - bc.g0) / nullif(s0.qty - bc.q0, 0) AS g0,
        (s1.spend - coalesce(bc.s1, 0))
        / nullif(s1.qty - coalesce(bc.q1, 0), 0) AS nt,
        (s0.spend - bc.s0) / nullif(s0.qty - bc.q0, 0) AS n0
    FROM bc
    JOIN hs
        ON
            hs.household_code = bc.household_code
            AND hs.panel_year = ${YEAR} - 1
    JOIN gsu AS s1
        ON
            s1.fips_state_code = hs.fips_state_code
            AND s1.item = bc.item AND s1.yq = bc.yq
    JOIN gsu AS s0
        ON
            s0.fips_state_code = hs.fips_state_code
            AND s0.item = bc.item AND s0.yq = bc.yq - 10
)

SELECT
    household_code,
    yq,
    count(*) FILTER (
        round(gt, 9) > 0 AND round(g0, 9) > 0
        AND round(gt / g0, 9) BETWEEN 0.1 AND 10
    ) AS en_g10,
    sum(q0 * gt) FILTER (
        round(gt, 9) > 0 AND round(g0, 9) > 0
        AND round(gt / g0, 9) BETWEEN 0.1 AND 10
    ) AS enum_g10,
    sum(q0 * g0) FILTER (
        round(gt, 9) > 0 AND round(g0, 9) > 0
        AND round(gt / g0, 9) BETWEEN 0.1 AND 10
    ) AS eden_g10,
    sum(q0 * nt) FILTER (
        round(nt, 9) > 0 AND round(n0, 9) > 0
        AND round(nt / n0, 9) BETWEEN round(1.0 / 3, 9) AND 3
    ) AS enum_c3,
    sum(q0 * n0) FILTER (
        round(nt, 9) > 0 AND round(n0, 9) > 0
        AND round(nt / n0, 9) BETWEEN round(1.0 / 3, 9) AND 3
    ) AS eden_c3
FROM p
GROUP BY ALL;

COPY (
    SELECT
        coalesce(en.household_code, eg.household_code) AS household_code,
        coalesce(en.yq, eg.yq) AS yq,
        en.* EXCLUDE (household_code, yq),
        eg.* EXCLUDE (household_code, yq)
    FROM en
    FULL JOIN eg
        ON en.household_code = eg.household_code AND en.yq = eg.yq
) TO '${WP2_INT}/outlier_diagnostics/exposure/year=${YEAR}.parquet' (
    FORMAT parquet
);

-- pairs outside [1/2, 2], counted by band: a coupon on the cheap side (the base
-- quarter for a rise, the current quarter for a fall), a cheap price under 25
-- cents, a pre-coupon relative within threefold, and whether other buyers in
-- the state saw the change.
COPY (
    WITH o AS (
        SELECT
            p.*,
            coalesce(h0.fips_state_code, h1.fips_state_code) AS st0,
            coalesce(h1.fips_state_code, h0.fips_state_code) AS st1
        FROM pr AS p
        LEFT JOIN hs AS h0
            ON
                h0.household_code = p.household_code
                AND h0.panel_year = ${YEAR} - 1
        LEFT JOIN hs AS h1
            ON
                h1.household_code = p.household_code
                AND h1.panel_year = ${YEAR}
        WHERE p.band != 'core'
    ),

    c AS (
        SELECT
            o.band,
            o.ro,
            o.rg,
            o.s0 / o.q0 AS p0,
            o.s1 / o.q1 AS p1,
            CASE
                WHEN o.ro > 1 THEN o.g0 > o.s0 + 0.005
                ELSE o.g1 > o.s1 + 0.005
            END AS coupon_cheap,
            CASE
                WHEN
                    b.n_hh > 1
                    AND round(
                        (b.spend - o.s0) / nullif(b.qty - o.q0, 0), 9
                    ) > 0
                    THEN (b.spend - o.s0) / (b.qty - o.q0)
            END AS p0s,
            CASE
                WHEN
                    a.n_hh > 1
                    AND round(
                        (a.spend - o.s1) / nullif(a.qty - o.q1, 0), 9
                    ) > 0
                    THEN (a.spend - o.s1) / (a.qty - o.q1)
            END AS p1s
        FROM o
        LEFT JOIN su AS b
            ON
                b.fips_state_code = o.st0
                AND b.item = o.item AND b.yq = o.yq - 10
        LEFT JOIN su AS a
            ON
                a.fips_state_code = o.st1
                AND a.item = o.item AND a.yq = o.yq
    ),

    k AS (
        SELECT
            *,
            ln(p1s / p0s) / ln(ro) AS frac,
            greatest(abs(ln(p0 / p0s)), abs(ln(p1 / p1s))) AS odd
        FROM c
    )

    SELECT
        band,
        count(*) AS n,
        count(*) FILTER (coupon_cheap) AS n_coupon_cheap,
        count(*) FILTER (least(p0, p1) < 0.25) AS n_under_25c,
        count(*) FILTER (
            rg BETWEEN round(1.0 / 3, 9) AND 3
        ) AS n_gross_in3,
        count(*) FILTER (frac IS NOT NULL) AS n_classified,
        count(*) FILTER (frac >= 0.5) AS n_state_moved,
        count(*) FILTER (frac < 0.5 AND odd > ln(2)) AS n_odd2,
        count(*) FILTER (frac < 0.5 AND odd > ln(3)) AS n_odd3
    FROM k
    GROUP BY ALL
) TO '${WP2_INT}/outlier_diagnostics/pairs/year=${YEAR}.parquet' (
    FORMAT parquet
);
