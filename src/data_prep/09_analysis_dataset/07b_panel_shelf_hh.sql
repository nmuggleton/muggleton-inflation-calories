-- The ladder of reach on panel-observed shelves: what each f1 household-quarter
-- could have saved on its base basket (quarter t-4) by buying each module's
-- calories lower down the shelf, as the shelf widens. Run per target quarter t
-- = ${YEAR}Q${QTR} by src/data_prep/09_analysis_dataset/07_panel_shelf.sh.
--
-- Rungs, each re-pricing the same base calories (max(0, spend - kcal x
-- rung price), summed over modules, as 05c):
--   retailer  the shelf of the retailer the calories were bought at, as
--             panellists in the household's state see it
--   setx      the cheapest retailer shelf, module by module, among the
--             retailers the household used in t-4 and the three quarters
--             before, CROSS-FITTED: the cheapest retailer is picked on one
--             half of the panel and priced on the other, both ways, and
--             the two averaged. The plain minimum over full-panel shelves
--             (set, kept as a diagnostic) is a minimum of noisy quantiles
--             and rises with the number of retailers
--   zip3      everything panellists living in the household's zip3 bought
--   county    ... in its county
--   state     ... in its state
-- Two ladders. POOLED: retailer, zip3, county, state (p25 of all items
-- pooled). BEST SINGLE RETAILER: setx, then bz3 / bcty / bst, the
-- cheapest retailer shelf among retailers used by >= 2 panellists living
-- in the household's zip3 / county (in the base quarter), or any with a
-- shelf in its state; cross-fitted like setx. The two are reported side
-- by side: the minimum over single shelves can sit below a pooled
-- quantile, so they are not one nested ladder.
-- Common base: base rows whose module is valid at all eight rungs.
-- own_sh_zip3 / own_sh_cty: the household's own calories in the module as
-- a share of the zip3 / county pooled ladder (spend-weighted over the
-- common base), to size circularity where few panellists live.
CREATE TEMP TABLE gk AS
SELECT
    household_code,
    yq,
    state::INTEGER AS state
FROM read_parquet('${DP}', hive_partitioning = true)
WHERE est_gate AND yq = ${YEAR} * 10 + ${QTR};

CREATE TEMP TABLE hy AS
SELECT
    household_code,
    fips_state_code::INTEGER * 1000 + fips_county_code::INTEGER AS county,
    substr(panelist_zip_code, 1, 3)::INTEGER AS zip3
FROM read_parquet('${WP2_FINAL}/household_year.parquet')
WHERE panel_year = ${YEAR} - 1;

SELECT CASE
    WHEN count(*) <> count(DISTINCT household_code)
        THEN error('panel shelf ${YEAR}q${QTR}: household_year not unique')
END AS chk_hy FROM hy;

CREATE TEMP MACRO qi(yq) AS (yq // 10) * 4 + yq % 10;

CREATE TEMP TABLE hs AS
SELECT
    h.household_code,
    h.yq,
    h.retailer_code,
    h.module,
    sum(h.spend) AS spend,
    sum(h.kcal) AS kcal
FROM read_parquet([${BASE_FILES}]) AS h
SEMI JOIN gk USING (household_code)
WHERE
    qi(h.yq) BETWEEN qi(${YEAR} * 10 + ${QTR}) - 7
    AND qi(${YEAR} * 10 + ${QTR}) - 4
GROUP BY ALL;

CREATE TEMP TABLE lad AS
SELECT * FROM read_parquet('${LAD}')
WHERE yq = (${YEAR} - 1) * 10 + ${QTR} AND valid;

SELECT CASE
    WHEN count(*) <> count(DISTINCT(reach, state, key, module))
        THEN
            error('panel ladder ${YEAR}q${QTR}: duplicate reach x key x module')
END AS chk_lad FROM lad;

CREATE TEMP TABLE base AS
SELECT
    hs.*,
    g.state,
    y.county,
    y.zip3
FROM hs JOIN gk AS g USING (household_code)
LEFT JOIN hy AS y USING (household_code)
WHERE hs.yq = (${YEAR} - 1) * 10 + ${QTR} AND hs.kcal > 0;

-- the household's own calories per module in the base quarter
CREATE TEMP TABLE hk AS
SELECT
    household_code,
    module,
    sum(kcal) AS kcal_hh
FROM base
GROUP BY ALL;

-- the household's retailers over the year to t-4, and its best shelf per
-- module among them
CREATE TEMP TABLE cs AS
SELECT DISTINCT
    hs.household_code,
    hs.retailer_code,
    g.state
FROM hs JOIN gk AS g USING (household_code);

CREATE TEMP TABLE best AS
SELECT
    c.household_code,
    l.module,
    min(l.p25) AS p25,
    min(l.p50) AS p50,
    count(*) AS n_shelves
FROM cs AS c JOIN lad AS l
    ON
        l.reach = 'retailer' AND l.state = c.state
        AND l.key = c.retailer_code
GROUP BY ALL;

-- own retailers whose shelf is valid in both halves of the panel
CREATE TEMP TABLE rh AS
SELECT
    c.household_code,
    a.module,
    c.retailer_code,
    a.p25 AS p25_0,
    b.p25 AS p25_1,
    a.p50 AS p50_0,
    b.p50 AS p50_1
FROM cs AS c
JOIN lad AS a
    ON
        a.reach = 'retailer_h0' AND a.state = c.state
        AND a.key = c.retailer_code
JOIN lad
    AS b ON b.reach = 'retailer_h1' AND b.state = c.state
AND b.key = c.retailer_code AND b.module = a.module;

-- pick on one half, price on the other; ties broken by retailer code
CREATE TEMP TABLE bestx AS
WITH s AS (
    SELECT
        household_code,
        module,
        'p25_01' AS k,
        p25_1 AS v
    FROM rh
    QUALIFY
        row_number() OVER (
            PARTITION BY household_code, module
            ORDER BY p25_0, retailer_code
        ) = 1
    UNION ALL
    SELECT
        household_code,
        module,
        'p25_10',
        p25_0
    FROM rh
    QUALIFY
        row_number() OVER (
            PARTITION BY household_code, module
            ORDER BY p25_1, retailer_code
        ) = 1
    UNION ALL
    SELECT
        household_code,
        module,
        'p50_01',
        p50_1
    FROM rh
    QUALIFY
        row_number() OVER (
            PARTITION BY household_code, module
            ORDER BY p50_0, retailer_code
        ) = 1
    UNION ALL
    SELECT
        household_code,
        module,
        'p50_10',
        p50_0
    FROM rh
    QUALIFY row_number() OVER (
        PARTITION BY household_code, module
        ORDER BY p50_1, retailer_code
    ) = 1
)

SELECT
    household_code,
    module,
    avg(v) FILTER (k LIKE 'p25%') AS p25,
    avg(v) FILTER (k LIKE 'p50%') AS p50,
    count(*) AS n_picks
FROM s
GROUP BY ALL;

SELECT CASE
    WHEN count(*) FILTER (n_picks <> 4) > 0
        THEN error('panel shelf ${YEAR}q${QTR}: cross-fit picks missing')
END AS chk_bestx FROM bestx;

-- best single retailer per area x module, cross-fitted; areas are (state,
-- zip3), (state, county) and the state.
CREATE TEMP TABLE pres AS
SELECT
    level,
    state,
    key,
    retailer_code
FROM read_parquet('${PRES}')
WHERE yq = (${YEAR} - 1) * 10 + ${QTR} AND n_hh >= 2
UNION ALL
SELECT DISTINCT
    'state',
    state,
    state,
    key
FROM lad
WHERE reach = 'retailer';

CREATE TEMP TABLE ah AS
SELECT
    p.level,
    p.state,
    p.key,
    a.module,
    p.retailer_code,
    a.p25 AS p25_0,
    b.p25 AS p25_1,
    a.p50 AS p50_0,
    b.p50 AS p50_1
FROM pres AS p
JOIN lad AS a
    ON
        a.reach = 'retailer_h0' AND a.state = p.state
        AND a.key = p.retailer_code
JOIN lad
    AS b ON b.reach = 'retailer_h1' AND b.state = p.state
AND b.key = p.retailer_code AND b.module = a.module;

CREATE TEMP TABLE bestarea AS
WITH s AS (
    SELECT
        level,
        state,
        key,
        module,
        'p25' AS k,
        p25_1 AS v
    FROM ah
    QUALIFY
        row_number() OVER (
            PARTITION BY level, state, key, module
            ORDER BY p25_0, retailer_code
        ) = 1
    UNION ALL
    SELECT
        level,
        state,
        key,
        module,
        'p25',
        p25_0
    FROM ah
    QUALIFY
        row_number() OVER (
            PARTITION BY level, state, key, module
            ORDER BY p25_1, retailer_code
        ) = 1
    UNION ALL
    SELECT
        level,
        state,
        key,
        module,
        'p50',
        p50_1
    FROM ah
    QUALIFY
        row_number() OVER (
            PARTITION BY level, state, key, module
            ORDER BY p50_0, retailer_code
        ) = 1
    UNION ALL
    SELECT
        level,
        state,
        key,
        module,
        'p50',
        p50_0
    FROM ah
    QUALIFY row_number() OVER (
        PARTITION BY level, state, key, module
        ORDER BY p50_1, retailer_code
    ) = 1
)

SELECT
    level,
    state,
    key,
    module,
    avg(v) FILTER (k = 'p25') AS p25,
    avg(v) FILTER (k = 'p50') AS p50,
    count(*) AS n_picks
FROM s
GROUP BY ALL;

SELECT CASE
    WHEN
        count(*) FILTER (n_picks <> 4) > 0
        OR count(*) <> count(DISTINCT(level, state, key, module))
        THEN
            error(
                'panel shelf ${YEAR}q${QTR}: best-retailer '
                || 'picks missing or duplicated'
            )
END AS chk_bestarea FROM bestarea;

CREATE TEMP TABLE rg AS
SELECT
    b.household_code,
    b.retailer_code,
    b.module,
    b.spend,
    b.kcal,
    o.p25 AS p25_ret,
    o.p50 AS p50_ret,
    x.p25 AS p25_set,
    x.p50 AS p50_set,
    e.p25 AS p25_setx,
    e.p50 AS p50_setx,
    z.p25 AS p25_zip3,
    z.p50 AS p50_zip3,
    c.p25 AS p25_cty,
    c.p50 AS p50_cty,
    s.p25 AS p25_st,
    s.p50 AS p50_st,
    bz.p25 AS p25_bz3,
    bz.p50 AS p50_bz3,
    bc.p25 AS p25_bcty,
    bc.p50 AS p50_bcty,
    bs.p25 AS p25_bst,
    bs.p50 AS p50_bst,
    k.kcal_hh / z.kcal_tot AS own_zip3,
    k.kcal_hh / c.kcal_tot AS own_cty,
    o.p25 IS NOT NULL AND e.p25 IS NOT NULL AND z.p25 IS NOT NULL
    AND c.p25 IS NOT NULL AND s.p25 IS NOT NULL
    AND bz.p25 IS NOT NULL AND bc.p25 IS NOT NULL
    AND bs.p25 IS NOT NULL AS common
FROM base AS b
JOIN hk AS k USING (household_code, module)
LEFT JOIN lad AS o
    ON
        o.reach = 'retailer' AND o.state = b.state
        AND o.key = b.retailer_code AND o.module = b.module
LEFT JOIN best AS x
    ON
        x.household_code = b.household_code
        AND x.module = b.module
LEFT JOIN bestx AS e
    ON
        e.household_code = b.household_code
        AND e.module = b.module
LEFT JOIN
    lad AS z
    ON z.reach = 'zip3' AND z.key = b.zip3 AND z.module = b.module
LEFT JOIN
    lad AS c
    ON c.reach = 'county' AND c.key = b.county AND c.module = b.module
LEFT JOIN
    lad AS s
    ON s.reach = 'state' AND s.key = b.state AND s.module = b.module
LEFT JOIN bestarea AS bz
    ON
        bz.level = 'zip3' AND bz.state = b.state
        AND bz.key = b.zip3 AND bz.module = b.module
LEFT JOIN bestarea AS bc
    ON
        bc.level = 'county' AND bc.state = b.state
        AND bc.key = b.county AND bc.module = b.module
LEFT JOIN bestarea
    AS bs ON bs.level = 'state' AND bs.state = b.state
AND bs.key = b.state AND bs.module = b.module;

CREATE TEMP TABLE hho AS
SELECT
    household_code,
    sum(spend) AS pb_spend_lab,
    sum(spend) FILTER (common) AS pb_spend,
    sum(kcal) FILTER (common) AS pb_kcal,
    sum(greatest(spend - kcal * p25_ret, 0)) FILTER (common) AS sav25_ret,
    sum(greatest(spend - kcal * p50_ret, 0)) FILTER (common) AS sav50_ret,
    sum(greatest(spend - kcal * p25_set, 0)) FILTER (common) AS sav25_set,
    sum(greatest(spend - kcal * p50_set, 0)) FILTER (common) AS sav50_set,
    sum(greatest(spend - kcal * p25_setx, 0)) FILTER (common) AS sav25_setx,
    sum(greatest(spend - kcal * p50_setx, 0)) FILTER (common) AS sav50_setx,
    sum(greatest(spend - kcal * p25_zip3, 0)) FILTER (common) AS sav25_zip3,
    sum(greatest(spend - kcal * p50_zip3, 0)) FILTER (common) AS sav50_zip3,
    sum(greatest(spend - kcal * p25_cty, 0)) FILTER (common) AS sav25_cty,
    sum(greatest(spend - kcal * p50_cty, 0)) FILTER (common) AS sav50_cty,
    sum(greatest(spend - kcal * p25_st, 0)) FILTER (common) AS sav25_st,
    sum(greatest(spend - kcal * p50_st, 0)) FILTER (common) AS sav50_st,
    sum(greatest(spend - kcal * p25_bz3, 0)) FILTER (common) AS sav25_bz3,
    sum(greatest(spend - kcal * p50_bz3, 0)) FILTER (common) AS sav50_bz3,
    sum(greatest(spend - kcal * p25_bcty, 0)) FILTER (common) AS sav25_bcty,
    sum(greatest(spend - kcal * p50_bcty, 0)) FILTER (common) AS sav50_bcty,
    sum(greatest(spend - kcal * p25_bst, 0)) FILTER (common) AS sav25_bst,
    sum(greatest(spend - kcal * p50_bst, 0)) FILTER (common) AS sav50_bst,
    sum(spend * own_zip3) FILTER (common) / sum(spend) FILTER (common)
        AS own_sh_zip3,
    sum(spend * own_cty) FILTER (common) / sum(spend) FILTER (common)
        AS own_sh_cty
FROM rg
GROUP BY household_code;

COPY (
    SELECT
        g.household_code,
        g.yq,
        g.state,
        y.county,
        y.zip3,
        coalesce(n.n_retailers, 0) AS n_retailers,
        o.* EXCLUDE (household_code)
    FROM gk AS g
    LEFT JOIN hy AS y USING (household_code)
    LEFT JOIN (
        SELECT
            household_code,
            count(DISTINCT retailer_code) AS n_retailers
        FROM cs
        GROUP BY ALL
    ) AS n
        USING (household_code)
    LEFT JOIN hho AS o USING (household_code)
) TO '${OUT_HH}' (FORMAT parquet, COMPRESSION zstd);
