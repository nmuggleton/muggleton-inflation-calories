-- Why is a shelf seen through panel purchases cheaper than the true RMS shelf
-- of the same stores? For one RMS year x module file, state ladders built step
-- by step from the RMS shelf towards the panel shelf, so each step changes one
-- thing: r0  RMS: every item x store cell sold (= 05a state ladder) r1  RMS
-- cells of items the panel bought somewhere in the state that quarter (which
-- items panellists see) r2  RMS cells of item x store pairs the panel bought
-- (which stores) r3  RMS prices, panel calories as weights, panel cells matched
-- to an RMS cell (how much of each panellists buy) pm  panel prices and
-- calories, the same matched cells (what panellists pay for the same item in
-- the same store and quarter) p   panel prices and calories, all panel cells
-- (adds panel cells with no RMS sales of that item in that store and quarter)
-- All steps use r0's national trim bounds, so a step moves the ladder only
-- through what it changes.
CREATE TEMP TABLE uk AS
SELECT
    k.upc,
    k.upc_ver_uc,
    k.item,
    k.kcal_unit
FROM read_parquet('${UK}') AS k
JOIN (
    SELECT item FROM read_parquet('${UK}')
    GROUP BY item
    HAVING min(module) = ${MODULE}
) USING (item);

CREATE TEMP TABLE ver AS
SELECT
    upc,
    any_value(upc_ver_uc) AS upc_ver_uc,
    count(*) AS n
FROM read_parquet('${RMS_VER}')
GROUP BY upc;
SELECT
    CASE WHEN max(n) > 1 THEN error('rms_versions ${YEAR}: upc not unique')
    END AS chk_ver
FROM ver;

CREATE TEMP TABLE sto AS
SELECT
    store_code_uc::INTEGER AS store_code_uc,
    any_value(fips_state_code)::INTEGER AS state
FROM read_parquet('${RMS_STO}')
GROUP BY 1;

-- RMS cells, aggregated before any join, as in 05a
CREATE TEMP TABLE mu AS
WITH m AS (
    SELECT
        TRY_CAST(store_code_uc AS INTEGER) AS store_code_uc,
        store_code_uc IS NULL AS no_store,
        upc,
        TRY_STRPTIME(week_end, '%Y%m%d')::DATE AS week_end,
        TRY_CAST(units AS DOUBLE) AS units,
        TRY_CAST(prmult AS DOUBLE) AS prmult,
        TRY_CAST(price AS DOUBLE) AS price
    FROM read_csv(
        '${MV_FILE}', delim = '\t', header = true,
        all_varchar = true
    )
)

SELECT
    store_code_uc,
    no_store,
    upc,
    year(week_end) * 10 + quarter(week_end) AS yq,
    sum(units * price / prmult) AS spend,
    sum(units) AS units,
    min(week_end) AS wk_min,
    max(week_end) AS wk_max
FROM m
WHERE units > 0 AND price > 0 AND prmult > 0
GROUP BY ALL;

SELECT CASE
    WHEN
        count(
            *) FILTER ((store_code_uc IS NULL AND NOT no_store)
        OR yq IS NULL
        ) > 0
        OR year(min(wk_min)) <> ${YEAR} OR year(max(wk_max)) <> ${YEAR}
        THEN
            error(
                'decomp ${YEAR} module ${MODULE}: '
                || 'bad store/week or week outside year'
            )
END AS chk_wk FROM mu;

CREATE TEMP TABLE rc AS
SELECT
    m.store_code_uc,
    s.state,
    m.yq,
    u.item,
    sum(m.spend) AS spend,
    sum(m.units * u.kcal_unit) AS kcal
FROM mu AS m
JOIN ver AS v ON v.upc = m.upc
JOIN uk AS u ON u.upc = m.upc AND u.upc_ver_uc = v.upc_ver_uc
-- LEFT JOIN and trim over all cells, as in 05a; stores with no state (or not in
-- the stores file) are left out of the state ladders below
LEFT JOIN sto AS s ON s.store_code_uc = m.store_code_uc
WHERE NOT m.no_store
GROUP BY ALL
HAVING sum(m.units * u.kcal_unit) > 0 AND sum(m.spend) > 0;
DROP TABLE mu;

-- Panel cells at RMS stores, at the shelf price before coupons
CREATE TEMP TABLE pc AS
SELECT
    p.store_code_uc,
    s.state,
    year(p.purchase_date) * 10 + quarter(p.purchase_date) AS yq,
    u.item,
    sum(p.total_price_paid) AS spend,
    sum(p.quantity * u.kcal_unit) AS kcal,
    bool_or(p.is_private_label) AS private_label
FROM read_parquet('${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet') AS p
JOIN sto AS s ON s.store_code_uc = p.store_code_uc
JOIN uk AS u
    ON
        u.upc = p.upc
        AND u.upc_ver_uc = coalesce(p.upc_ver_uc, 0)::SMALLINT
WHERE
    NOT p.is_magnet_product AND p.quantity > 0 AND p.total_price_paid > 0
    AND year(p.purchase_date) = ${YEAR} AND s.state IS NOT NULL
GROUP BY ALL
HAVING sum(p.quantity * u.kcal_unit) > 0;

COPY (
    SELECT
        ${YEAR}::SMALLINT AS year,
        ${MODULE}::INTEGER AS module,
        count(*) AS n_panel_cells,
        count(r.item) AS n_matched,
        sum(pc.spend) AS panel_spend,
        sum(pc.spend) FILTER (r.item IS NOT NULL) AS panel_spend_matched,
        sum(pc.spend) FILTER (pc.private_label) AS panel_spend_pl,
        sum(pc.spend) FILTER (r.item IS NULL AND pc.private_label)
            AS panel_spend_unmatched_pl,
        (SELECT count(*) FROM rc) AS n_rms_cells
    FROM pc LEFT JOIN rc AS r USING (store_code_uc, yq, item)
) TO '${OUT_MAT}' (FORMAT parquet);

CREATE TEMP TABLE trm AS
SELECT
    yq,
    quantile_cont(spend / kcal, 0.01) AS lo,
    quantile_cont(spend / kcal, 0.99) AS hi
FROM rc
GROUP BY yq;

CREATE TEMP TABLE steps AS
SELECT
    'r0' AS step,
    store_code_uc,
    state,
    yq,
    item,
    kcal,
    spend / kcal AS dpk
FROM rc
UNION ALL
SELECT
    'r1',
    r.store_code_uc,
    r.state,
    r.yq,
    r.item,
    r.kcal,
    r.spend / r.kcal
FROM rc AS r
SEMI JOIN (SELECT DISTINCT
    state,
    yq,
    item
FROM pc) USING (state, yq, item)
UNION ALL
SELECT
    'r2',
    r.store_code_uc,
    r.state,
    r.yq,
    r.item,
    r.kcal,
    r.spend / r.kcal
FROM rc AS r SEMI JOIN pc USING (store_code_uc, yq, item)
UNION ALL
SELECT
    'r3',
    p.store_code_uc,
    p.state,
    p.yq,
    p.item,
    p.kcal,
    r.spend / r.kcal
FROM pc AS p JOIN rc AS r USING (store_code_uc, yq, item)
UNION ALL
SELECT
    'pm',
    p.store_code_uc,
    p.state,
    p.yq,
    p.item,
    p.kcal,
    p.spend / p.kcal
FROM pc AS p SEMI JOIN rc AS r USING (store_code_uc, yq, item)
UNION ALL
SELECT
    'p',
    store_code_uc,
    state,
    yq,
    item,
    kcal,
    spend / kcal
FROM pc;

COPY (
    WITH f AS (
        SELECT s.* FROM steps AS s JOIN trm USING (yq)
        WHERE s.dpk BETWEEN trm.lo AND trm.hi AND s.state IS NOT NULL
    ),

w AS (
        SELECT
            *,
            sum(kcal)
                OVER (
                    PARTITION BY step, state, yq
                    ORDER BY dpk, item, store_code_uc
                    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
                )
            / sum(kcal) OVER (PARTITION BY step, state, yq) AS cum_hi
        FROM f
    )

    SELECT
        step,
        state,
        yq,
        ${MODULE}::INTEGER AS module,
        ${YEAR}::SMALLINT AS year,
        count(DISTINCT item) AS n_items,
        count(DISTINCT item) >= 5 AS valid,
        sum(kcal) AS kcal_tot,
        min(dpk) FILTER (cum_hi >= 0.25) AS p25,
        min(dpk) FILTER (cum_hi >= 0.50) AS p50
    FROM w
    GROUP BY ALL
) TO '${OUT_DEC}' (FORMAT parquet);
