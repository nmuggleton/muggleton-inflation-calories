-- Where does the gap between a household's own prices and state prices of the
-- same products sit? For target calendar year ${YEAR} (t = ${YEAR}Q1..Q4, base
-- t-4), the pairs of 03b_relative_prices.sql under gross10 (an item the
-- household bought in t and t-4; state price p0 from other households in both
-- quarters; pre-coupon own relative within [1/10, 10]; state relative within
-- [1/3, 3]) are described and grouped.
--
-- Groups of pairs (grp):
--   all
--   ch:<channel>        channel of ret0 (06a): Grocery, Discount Store,
--                       Warehouse Club, Other (dollar, drug, all the rest)
--   cov:<0|1>           1 if p00 and p11 exist (another household in the
--                       state bought the item at the same retailer, in
--                       both quarters)
--   covch:<channel>|<0|1>
--   sw:<status>         same_store (same recorded store in both quarters),
--                       sw_ret (different main retailer), sw_within
--                       (different recorded store, same retailer),
--                       unk_store (same retailer, a store not recorded)
--   rms:<0|1>           1 if both sides have a Retail Scanner share > 0
--   cov4:1              sw_ret pairs with all four retailer prices
-- Household-quarter sums by group, base quantities throughout:
--   a0, a1    own spending at t-4, and base quantities at own prices of t
--   b0, b1    the same at state prices
--   r00, r11, r01, r10   base quantities at the four retailer prices
-- Run per year by src/data_prep/06_prices/06_price_gap_sources.sh.

-- Purchase lines by store, under the line rules of 03a_purchase_cells.sql; wy
-- is the year of the week ending Saturday, the Retail Scanner convention
CREATE TEMP TABLE ln AS
SELECT
    p.household_code,
    hy.fips_state_code AS st,
    hs.fips_state_code AS st_base,
    coalesce(CASE WHEN p.panel_year <= 2020 THEN x.upc_2021_masked END, p.upc)
        AS item,
    coalesce(p.store_code_uc, 0) AS store,
    p.retailer_code AS ret,
    year(
        p.purchase_date
        + CAST((6 - dayofweek(p.purchase_date)) % 7 AS INTEGER)
    ) AS wy,
    year(p.purchase_date) * 10 + quarter(p.purchase_date) AS yq,
    (coalesce(p.deal_flag_uc, 0) = 1) AS deal,
    sum(p.quantity)::DOUBLE AS q,
    sum(p.net_price_paid) AS s,
    sum(p.total_price_paid) AS g
FROM read_parquet([${PU_FILES}]) AS p
JOIN read_parquet('${WP2_FINAL}/xwalk_module_foodgroup.parquet') AS fg
    USING (product_module_code)
JOIN read_parquet('${WP2_FINAL}/household_year.parquet') AS hy
    ON hy.household_code = p.household_code AND hy.panel_year = p.panel_year
JOIN read_parquet('${WP2_FINAL}/household_year.parquet') AS hs
    ON hs.household_code = p.household_code AND hs.panel_year = ${YEAR} - 1
LEFT JOIN read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet') AS x
    ON p.upc = x.upc_2020
WHERE
    fg.is_human_food AND NOT p.is_magnet_product AND p.quantity > 0
    AND p.net_price_paid > 0
    AND p.purchase_date BETWEEN make_date(${YEAR} - 1, 1, 1)
    AND make_date(${YEAR}, 12, 31)
GROUP BY ALL;

-- The pairs of 03b under gross10: leave-one-out state prices (rounded before
-- the sign test, as in 01a), then the items bought in both t and t-4
CREATE TEMP TABLE c AS
SELECT
    household_code,
    st,
    st_base,
    item,
    yq,
    sum(q) AS q,
    sum(s) AS s,
    sum(g) AS g
FROM ln
GROUP BY ALL;

CREATE TEMP TABLE tot AS
SELECT
    st,
    item,
    yq,
    sum(q) AS q0,
    sum(s) AS s0,
    sum(g) AS g0
FROM c
GROUP BY ALL;

CREATE TEMP TABLE lp AS
WITH pr AS (
    SELECT
        c.household_code,
        c.item,
        c.yq,
        c.q,
        c.s,
        c.g,
        c.st = c.st_base AS in_base,
        CASE WHEN round(t.q0 - c.q, 9) > 0 AND round(t.s0 - c.s, 9) > 0
            THEN (t.s0 - c.s) / (t.q0 - c.q) END AS p0,
        CASE WHEN round(t.q0 - c.q, 9) > 0 AND round(t.g0 - c.g, 9) > 0
            THEN (t.g0 - c.g) / (t.q0 - c.q) END AS pg0
    FROM c
    JOIN tot AS t ON t.st = c.st AND t.item = c.item AND t.yq = c.yq
)

SELECT
    household_code,
    item,
    yq,
    sum(q) AS q,
    sum(s) AS s,
    sum(g) AS g,
    bool_and(in_base AND p0 IS NOT NULL AND pg0 IS NOT NULL) AS ok,
    sum(q * p0) AS x0
FROM pr
GROUP BY ALL;
DROP TABLE c;
DROP TABLE tot;

CREATE TEMP TABLE m AS
SELECT
    b.household_code,
    b.item,
    b.yq + 10 AS yq,
    b.q AS q0,
    b.s AS s0,
    b.x0 AS b0,
    c.q AS q1,
    c.s AS s1,
    c.x0 AS c0
FROM lp AS b
JOIN lp AS c
    ON
        c.household_code = b.household_code
        AND c.item = b.item AND c.yq = b.yq + 10
WHERE
    b.yq BETWEEN (${YEAR} - 1) * 10 + 1 AND (${YEAR} - 1) * 10 + 4
    AND b.ok AND c.ok AND b.s > 0 AND c.s > 0
    AND round((c.g / c.q) / (b.g / b.q), 9) BETWEEN round(1.0 / 10, 9) AND 10
    AND round((c.x0 / c.q) / (b.x0 / b.q), 9)
    BETWEEN round(1.0 / 3, 9) AND 3;
DROP TABLE lp;

-- Each household x item x quarter: main store, main retailer, and shares
CREATE TEMP TABLE sto AS
SELECT DISTINCT
    store_code_uc AS store,
    year AS wy
FROM read_parquet('${WP2_INT}/rms/stores/panel_year=*/data.parquet');

CREATE TEMP TABLE side AS
WITH l AS (
    SELECT
        ln.household_code,
        ln.item,
        ln.yq,
        ln.store,
        ln.ret,
        sum(ln.s) AS s,
        coalesce(sum(ln.s) FILTER (ln.deal), 0) AS s_deal,
        coalesce(sum(ln.s) FILTER (sto.store IS NOT NULL), 0) AS s_rms
    FROM ln
    LEFT JOIN sto ON sto.store = ln.store AND sto.wy = ln.wy
    WHERE
        EXISTS (
            SELECT 1
            FROM m
            WHERE
                m.household_code = ln.household_code AND m.item = ln.item
                AND (ln.yq = m.yq OR ln.yq = m.yq - 10)
        )
    GROUP BY ALL
),

by_ret AS (
    SELECT
        household_code,
        item,
        yq,
        ret,
        sum(s) AS s
    FROM l
    GROUP BY ALL
)

SELECT
    l.household_code,
    l.item,
    l.yq,
    arg_max(l.store, (l.s, -l.store)) AS store,
    any_value(r.ret) AS ret,
    sum(l.s_deal) / sum(l.s) AS f_deal,
    sum(l.s_rms) / sum(l.s) AS f_rms
FROM l
JOIN (
    SELECT
        household_code,
        item,
        yq,
        arg_max(ret, (s, -coalesce(ret, 0))) AS ret
    FROM by_ret
    GROUP BY ALL
) AS r USING (household_code, item, yq)
GROUP BY ALL;
DROP TABLE ln;

CREATE TEMP TABLE a AS
SELECT
    m.*,
    hs.fips_state_code AS st,
    s0.store AS store0,
    s1.store AS store1,
    s0.ret AS ret0,
    s1.ret AS ret1,
    s0.f_rms AS f0,
    s1.f_rms AS f1,
    s0.f_deal AS d0,
    s1.f_deal AS d1
FROM m
JOIN read_parquet('${WP2_FINAL}/household_year.parquet') AS hs
    ON hs.household_code = m.household_code AND hs.panel_year = ${YEAR} - 1
JOIN side AS s0
    ON
        s0.household_code = m.household_code AND s0.item = m.item
        AND s0.yq = m.yq - 10
JOIN side AS s1
    ON
        s1.household_code = m.household_code AND s1.item = m.item
        AND s1.yq = m.yq;
DROP TABLE m;
DROP TABLE side;

-- Retailer prices from the other households in the state
CREATE TEMP TABLE rc AS
SELECT
    p.household_code,
    p.st,
    p.ret,
    p.item,
    (p.ym // 100) * 10 + ((p.ym % 100) - 1) // 3 + 1 AS yq,
    sum(p.q) AS q,
    sum(p.s) AS s
FROM read_parquet([${PC_FILES}]) AS p
WHERE p.ym BETWEEN (${YEAR} - 1) * 100 + 1 AND ${YEAR} * 100 + 12
GROUP BY ALL;

CREATE TEMP TABLE rt AS
SELECT
    st,
    ret,
    item,
    yq,
    sum(q) AS q,
    sum(s) AS s
FROM rc
GROUP BY ALL;

-- The four retailer x quarter cells each pair needs
CREATE TEMP TABLE k AS
SELECT DISTINCT
    a.household_code,
    a.st,
    a.item,
    r.ret,
    r.yq
FROM a,
    LATERAL (
        VALUES
        (a.ret0, a.yq - 10),
        (a.ret0, a.yq),
        (a.ret1, a.yq - 10),
        (a.ret1, a.yq)
    ) AS r (ret, yq)
WHERE r.ret IS NOT NULL;

CREATE TEMP TABLE kp AS
SELECT
    k.household_code,
    k.item,
    k.ret,
    k.yq,
    CASE
        WHEN
            round(t.q - coalesce(h.q, 0), 9) > 0
            AND round(t.s - coalesce(h.s, 0), 9) > 0
            THEN (t.s - coalesce(h.s, 0)) / (t.q - coalesce(h.q, 0))
    END AS p
FROM k
JOIN rt AS t
    ON t.st = k.st AND t.ret = k.ret AND t.item = k.item AND t.yq = k.yq
LEFT JOIN rc AS h
    ON
        h.household_code = k.household_code AND h.st = k.st
        AND h.ret = k.ret AND h.item = k.item AND h.yq = k.yq;
DROP TABLE rc;
DROP TABLE rt;
DROP TABLE k;
DELETE FROM kp WHERE p IS NULL;

CREATE TEMP TABLE x AS
SELECT
    a.household_code,
    a.yq,
    a.q0,
    a.s0,
    a.b0,
    a.q1,
    a.s1,
    a.c0,
    a.d0,
    a.d1,
    (a.f0 > 0 AND a.f1 > 0) AS rms,
    k00.p AS p00,
    k01.p AS p01,
    k10.p AS p10,
    k11.p AS p11,
    (k00.p IS NOT NULL AND k11.p IS NOT NULL) AS cov,
    CASE
        WHEN a.store0 = a.store1 AND a.store0 <> 0 THEN 'same_store'
        WHEN a.ret0 IS DISTINCT FROM a.ret1 THEN 'sw_ret'
        WHEN a.store0 <> 0 AND a.store1 <> 0 THEN 'sw_within'
        ELSE 'unk_store'
    END AS sw,
    CASE
        WHEN ch.channel IN ('Grocery', 'Discount Store', 'Warehouse Club')
            THEN ch.channel
        ELSE 'Other'
    END AS chan
FROM a
LEFT JOIN
    read_parquet('${WP2_INT}/price_gap_sources/retailer_channel.parquet')
        AS ch
    ON ch.ret = a.ret0
LEFT JOIN kp AS k00
    ON
        k00.household_code = a.household_code AND k00.item = a.item
        AND k00.ret = a.ret0 AND k00.yq = a.yq - 10
LEFT JOIN kp AS k01
    ON
        k01.household_code = a.household_code AND k01.item = a.item
        AND k01.ret = a.ret0 AND k01.yq = a.yq
LEFT JOIN kp AS k10
    ON
        k10.household_code = a.household_code AND k10.item = a.item
        AND k10.ret = a.ret1 AND k10.yq = a.yq - 10
LEFT JOIN kp AS k11
    ON
        k11.household_code = a.household_code AND k11.item = a.item
        AND k11.ret = a.ret1 AND k11.yq = a.yq;
DROP TABLE a;
DROP TABLE kp;

COPY (
    WITH u AS (
        SELECT
            *,
            unnest([
                'all',
                'ch:' || chan,
                'cov:' || cov::INT,
                'covch:' || chan || '|' || cov::INT,
                'sw:' || sw,
                'rms:' || rms::INT,
                CASE
                    WHEN
                        sw = 'sw_ret' AND cov
                        AND p01 IS NOT NULL AND p10 IS NOT NULL
                        THEN 'cov4:1'
                END
            ]) AS grp
        FROM x
    )

    SELECT
        household_code,
        yq,
        grp,
        count(*) AS n,
        sum(s0) AS a0,
        sum(b0) AS b0,
        sum(q0 * s1 / q1) AS a1,
        sum(q0 * c0 / q1) AS b1,
        sum(q0 * p00) AS r00,
        sum(q0 * p11) AS r11,
        sum(q0 * p01) AS r01,
        sum(q0 * p10) AS r10
    FROM u
    WHERE grp IS NOT NULL
    GROUP BY ALL
) TO '${WP2_INT}/price_gap_sources/groups/year=${YEAR}.parquet' (
    FORMAT parquet, COMPRESSION zstd
);

COPY (
    SELECT
        household_code,
        yq,
        count(*) AS n,
        sum(s0) AS s0,
        coalesce(sum(s0) FILTER (sw = 'same_store'), 0) AS s_same,
        coalesce(sum(s0) FILTER (sw = 'sw_within'), 0) AS s_within,
        coalesce(sum(s0) FILTER (sw = 'sw_ret'), 0) AS s_ret,
        coalesce(sum(s0) FILTER (sw = 'unk_store'), 0) AS s_unk,
        sum(s0 * d0) AS s_deal0,
        sum(s0 * d1) AS s_deal1
    FROM x
    GROUP BY ALL
) TO '${WP2_INT}/price_gap_sources/switch/year=${YEAR}.parquet' (
    FORMAT parquet, COMPRESSION zstd
);
