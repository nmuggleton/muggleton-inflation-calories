-- The within-module calorie-price ladder ("the shelf") and each f1
-- household-quarter's position on it, for target quarter t = ${YEAR}Q${QTR}
-- (base t-4 = same quarter of ${YEAR}-1, tier check quarter t-8 = same quarter
-- of ${YEAR}-2). Run per quarter by
-- src/data_prep/09_analysis_dataset/01_ladder.sh (one quarter at a time keeps
-- DuckDB spill small).
--
-- Definitions (all prices nominal $ per kcal, quarter FE absorb the CPI)
--   item         label UPC (the item key of 06_prices/01a), labelled kcal > 0 only
--   ladder       within state x quarter x product module, items sorted by
--                unit $/kcal and weighted by kcal sold to ALL panellists in
--                the state (state_upc_q): p25, p50 = calorie-weighted
--                quantiles. Items outside the national module x quarter
--                1st-99th percentile of item $/kcal are dropped from the
--                ladder (guards against package-size errors, which would
--                otherwise dominate a calorie-weighted cheap tier).
--   tier         cheap = item's calories start in the cheapest 25% of the
--                module's calories, premium = end in the dearest 25%.
--   valid ladder >= 5 items in the state x quarter x module.
--   savings      base-basket spend that buying each module's base calories
--                at the state p25 (p50) $/kcal would have saved, counting
--                only modules where that is cheaper (max(0, .)).
--   cf nutrients base calories of each module re-bought at the cheap-tier
--                nutrient-per-kcal (and kcal per gram) profile of that
--                module in the state and base quarter.
--   F indices    household base module spend weights x state module-tier
--                fixed-set Laspeyres of $/kcal (base kcal weights,
--                relatives in [1/3, 3]). Tier membership for the price
--                indices must not use the base-quarter state unit value
--                itself: an item that looks cheap at t-4 because of a
--                transitory low price mechanically "inflates" by t
--                (regression to the mean). Three definitions are kept:
--                  _own  same state, base quarter (biased, reference only)
--                  _lso  base quarter, ranked by the leave-state-out
--                        national $/kcal (primary)
--                  _b2   same state, ranked at t-8 (items sold then)
--   E_own        household base items x state unit $/kcal in t and t-4
--                (a labelled-only, non leave-out analogue of T2).
--   decomposition (Tornqvist weights cbar = mean kcal share over modules
--                bought, with valid ladders, in both quarters):
--                W = sum cbar dln($/kcal)_m = M + L, M = sum cbar dln p50_m
--                (market ladder), L = sum cbar d r_m with r_m = ln(own
--                $/kcal / p50) (own rung). T = dln total $/kcal (all
--                labelled kcal>0 items). Mix = T - W.
-- Sums of doubles are taken as fixed-point decimals (exact, so the same in
-- any order): DuckDB adds doubles in parallel, and the last-digit noise
-- moved items across the 1-99% trim, the [1/3, 3] relative filter and the
-- 25/50/75% calorie cut-offs from build to build.
CREATE TEMP TABLE uk AS
SELECT
    upc,
    upc_ver_uc,
    item,
    module,
    kcal_unit,
    g_unit,
    sugar_unit,
    satfat_unit,
    sodium_unit,
    fiber_unit,
    protein_unit
FROM read_parquet('${UK}');

CREATE TEMP TABLE gk AS
SELECT
    household_code,
    yq,
    state
FROM read_parquet('${DP}', hive_partitioning = true)
WHERE est_gate AND yq = ${YEAR} * 10 + ${QTR};

CREATE TEMP TABLE hk AS
SELECT
    household_code,
    yq,
    yq AS t,
    1 AS is_t,
    state
FROM gk
UNION ALL
SELECT
    household_code,
    yq - 10 AS yq,
    yq AS t,
    0 AS is_t,
    state
FROM gk;

CREATE TEMP TABLE hi AS
SELECT
    hk.household_code,
    hk.t,
    hk.is_t,
    hk.state,
    h.yq,
    u.item,
    min(u.module) AS module,   -- items pooled over modules: lowest
    sum(CAST(h.spend AS DECIMAL(38, 9)))::DOUBLE AS spend,
    sum(CAST(h.qty * u.kcal_unit AS DECIMAL(38, 9)))::DOUBLE AS kcal,
    sum(CAST(h.qty * u.g_unit AS DECIMAL(38, 9)))::DOUBLE AS grams,
    sum(
        CAST(
            CASE
                WHEN u.g_unit > 0 THEN h.qty * u.kcal_unit ELSE 0
            END AS DECIMAL(38, 9)
        )
    )::DOUBLE AS kcal_g,
    sum(CAST(h.qty * u.sugar_unit AS DECIMAL(38, 9)))::DOUBLE AS sugar,
    sum(CAST(h.qty * u.satfat_unit AS DECIMAL(38, 9)))::DOUBLE AS satfat,
    sum(CAST(h.qty * u.sodium_unit AS DECIMAL(38, 9)))::DOUBLE AS sodium,
    sum(CAST(h.qty * u.fiber_unit AS DECIMAL(38, 9)))::DOUBLE AS fiber,
    sum(CAST(h.qty * u.protein_unit AS DECIMAL(38, 9)))::DOUBLE AS protein
FROM read_parquet([${HU_FILES}]) AS h
JOIN hk ON hk.household_code = h.household_code AND hk.yq = h.yq
JOIN uk AS u ON u.upc = h.upc AND u.upc_ver_uc = h.upc_ver_uc
WHERE h.yq IN ((${YEAR} - 1) * 10 + ${QTR}, ${YEAR} * 10 + ${QTR})
GROUP BY ALL;

CREATE TEMP TABLE si AS
SELECT
    s.fips_state_code AS state,
    s.yq,
    u.item,
    min(u.module) AS module,   -- items pooled over modules: lowest
    sum(CAST(s.spend AS DECIMAL(38, 9)))::DOUBLE AS spend,
    sum(CAST(s.qty * u.kcal_unit AS DECIMAL(38, 9)))::DOUBLE AS kcal,
    sum(CAST(s.qty * u.g_unit AS DECIMAL(38, 9)))::DOUBLE AS grams,
    sum(
        CAST(
            CASE
                WHEN u.g_unit > 0 THEN s.qty * u.kcal_unit ELSE 0
            END AS DECIMAL(38, 9)
        )
    )::DOUBLE AS kcal_g,
    sum(CAST(s.qty * u.sugar_unit AS DECIMAL(38, 9)))::DOUBLE AS sugar,
    sum(CAST(s.qty * u.satfat_unit AS DECIMAL(38, 9)))::DOUBLE AS satfat,
    sum(CAST(s.qty * u.sodium_unit AS DECIMAL(38, 9)))::DOUBLE AS sodium,
    sum(CAST(s.qty * u.fiber_unit AS DECIMAL(38, 9)))::DOUBLE AS fiber,
    sum(CAST(s.qty * u.protein_unit AS DECIMAL(38, 9)))::DOUBLE AS protein,
    sum(CAST(s.n_hh AS DECIMAL(38, 9)))::DOUBLE AS n_hh
FROM read_parquet([${SU_FILES}]) AS s
JOIN uk AS u ON u.upc = s.upc AND u.upc_ver_uc = s.upc_ver_uc
WHERE
    s.yq IN (
        (${YEAR} - 2) * 10 + ${QTR}, (${YEAR} - 1) * 10 + ${QTR},
        ${YEAR} * 10 + ${QTR}
    )
GROUP BY ALL;

CREATE TEMP TABLE trm AS
SELECT
    yq,
    module,
    quantile_cont(spend / kcal, 0.01) AS lo,
    quantile_cont(spend / kcal, 0.99) AS hi
FROM si
WHERE kcal > 0 AND spend > 0
GROUP BY ALL;

CREATE TEMP TABLE st AS
WITH f AS (
    SELECT
        s.*,
        s.spend / s.kcal AS dpk
    FROM si AS s JOIN trm AS t ON t.yq = s.yq AND t.module = s.module
    WHERE
        s.kcal > 0 AND s.spend > 0
        AND s.spend / s.kcal BETWEEN t.lo AND t.hi
),

c AS (
    SELECT
        *,
        sum(kcal::DECIMAL(38, 9))
            OVER (
                PARTITION BY state, yq, module
                ORDER BY dpk, item
                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            )
        / sum(kcal::DECIMAL(38, 9))
            OVER (PARTITION BY state, yq, module)
            AS cum_hi,
        count(*) OVER (PARTITION BY state, yq, module) AS n_items
    FROM f
)

SELECT
    *,
    cum_hi
    - kcal / sum(kcal::DECIMAL(38, 9)) OVER (PARTITION BY state, yq, module)
        AS cum_lo,
    CASE WHEN
        cum_hi
        - kcal / sum(kcal::DECIMAL(38, 9)) OVER (PARTITION BY state, yq, module)
        < 0.25 THEN 'cheap'
    WHEN cum_hi > 0.75 THEN 'premium' ELSE 'mid' END AS tier
FROM c;

CREATE TEMP TABLE lad AS
SELECT
    state,
    yq,
    module,
    any_value(n_items) AS n_items,
    any_value(n_items) >= 5 AS valid,
    sum(kcal) AS kcal_tot,
    min(dpk) FILTER (cum_hi >= 0.25) AS p25,
    min(dpk) FILTER (cum_hi >= 0.50) AS p50,
    sum(spend) FILTER (tier = 'cheap')
    / sum(kcal) FILTER (tier = 'cheap') AS cheap_dpk,
    sum(sugar) FILTER (tier = 'cheap')
    / sum(kcal) FILTER (tier = 'cheap') AS ch_sugar_pk,
    sum(satfat) FILTER (tier = 'cheap')
    / sum(kcal) FILTER (tier = 'cheap') AS ch_satfat_pk,
    sum(sodium) FILTER (tier = 'cheap')
    / sum(kcal) FILTER (tier = 'cheap') AS ch_sodium_pk,
    sum(fiber) FILTER (tier = 'cheap')
    / sum(kcal) FILTER (tier = 'cheap') AS ch_fiber_pk,
    sum(protein) FILTER (tier = 'cheap')
    / sum(kcal) FILTER (tier = 'cheap') AS ch_protein_pk,
    sum(kcal_g) FILTER (tier = 'cheap' AND grams > 0)
    / nullif(sum(grams) FILTER (tier = 'cheap' AND grams > 0), 0)
        AS ch_kcal_per_g,
    sum(sugar) / sum(kcal) AS all_sugar_pk,
    sum(kcal_g) FILTER (grams > 0)
    / nullif(sum(grams) FILTER (grams > 0), 0) AS all_kcal_per_g
FROM st
GROUP BY ALL;

-- national leave-state-out $/kcal and t-8 same-state tiers for ranking
CREATE TEMP TABLE nat AS
SELECT
    yq,
    item,
    sum(spend::DECIMAL(38, 9))::DOUBLE AS n_spend,
    sum(kcal::DECIMAL(38, 9))::DOUBLE AS n_kcal
FROM st
GROUP BY ALL;

CREATE TEMP TABLE rk AS
WITH b AS (
    SELECT
        s.state,
        s.yq,
        s.module,
        s.item,
        s.kcal,
        (n.n_spend - s.spend) / nullif(n.n_kcal - s.kcal, 0) AS dpk_lso
    FROM st AS s JOIN nat AS n ON n.yq = s.yq AND n.item = s.item
    WHERE s.yq = (${YEAR} - 1) * 10 + ${QTR}
),

c AS (
    SELECT
        *,
        sum(kcal::DECIMAL(38, 9))
            OVER (
                PARTITION BY state, module ORDER BY dpk_lso, item
                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            )
        / sum(kcal::DECIMAL(38, 9)) OVER (PARTITION BY state, module) AS cum_hi
    FROM b
    WHERE dpk_lso > 0
)

SELECT
    state,
    item,
    CASE
        WHEN
            cum_hi
            - kcal / sum(kcal::DECIMAL(38, 9)) OVER (PARTITION BY state, module)
            < 0.25
            THEN 'cheap'
        WHEN cum_hi > 0.75 THEN 'premium' ELSE 'mid'
    END AS tier_lso
FROM c;

-- fixed-set price relatives: base-quarter items re-priced in t (same state)
CREATE TEMP TABLE pr AS
SELECT
    b.state,
    a.yq AS t,
    b.module,
    b.item,
    b.tier AS tier_own,
    r.tier_lso,
    e.tier AS tier_b2,
    b.kcal AS kcal0,
    b.spend AS spend0,
    b.dpk AS dpk0,
    a.dpk AS dpk1
FROM st AS b JOIN st AS a
    ON
        a.state = b.state AND a.item = b.item
        AND a.yq = b.yq + 10
LEFT JOIN rk AS r ON r.state = b.state AND r.item = b.item
LEFT JOIN st AS e
    ON
        e.state = b.state AND e.item = b.item
        AND e.yq = b.yq - 10
WHERE
    b.yq = (${YEAR} - 1) * 10 + ${QTR}
    AND a.dpk / b.dpk BETWEEN 1.0 / 3 AND 3;

CREATE TEMP TABLE tierx AS
SELECT
    state,
    t,
    module,
    sum(kcal0 * dpk1) / sum(spend0) AS l_all,
    sum(kcal0 * dpk1) FILTER (tier_own = 'cheap')
    / sum(spend0) FILTER (tier_own = 'cheap') AS l_cheap_own,
    sum(kcal0 * dpk1) FILTER (tier_own = 'premium')
    / sum(spend0) FILTER (tier_own = 'premium') AS l_prem_own,
    sum(kcal0 * dpk1) FILTER (tier_lso = 'cheap')
    / sum(spend0) FILTER (tier_lso = 'cheap') AS l_cheap,
    sum(kcal0 * dpk1) FILTER (tier_lso = 'mid')
    / sum(spend0) FILTER (tier_lso = 'mid') AS l_mid,
    sum(kcal0 * dpk1) FILTER (tier_lso = 'premium')
    / sum(spend0) FILTER (tier_lso = 'premium') AS l_prem,
    sum(kcal0 * dpk1) FILTER (tier_b2 = 'cheap')
    / sum(spend0) FILTER (tier_b2 = 'cheap') AS l_cheap_b2,
    sum(kcal0 * dpk1) FILTER (tier_b2 = 'mid')
    / sum(spend0) FILTER (tier_b2 = 'mid') AS l_mid_b2,
    sum(kcal0 * dpk1) FILTER (tier_b2 = 'premium')
    / sum(spend0) FILTER (tier_b2 = 'premium') AS l_prem_b2,
    sum(spend0) AS spend0,
    sum(spend0) FILTER (tier_b2 IS NOT NULL) AS spend0_b2,
    count(*) AS n_items
FROM pr
GROUP BY ALL;

COPY (SELECT * FROM tierx) TO '${OUT_TIER}' (FORMAT parquet);

CREATE TEMP TABLE hm AS
SELECT
    household_code,
    t,
    is_t,
    state,
    yq,
    module,
    sum(spend) AS spend,
    sum(kcal) AS kcal,
    sum(grams) AS grams,
    sum(kcal_g) AS kcal_g,
    sum(sugar) AS sugar,
    sum(satfat) AS satfat,
    sum(sodium) AS sodium,
    sum(fiber) AS fiber,
    sum(protein) AS protein
FROM hi
GROUP BY ALL;

-- (a) base-basket savings and cheap-tier counterfactual nutrition
CREATE TEMP TABLE base AS
SELECT
    m.household_code,
    m.t,
    sum(m.spend) AS b_spend_lab,
    sum(m.spend) FILTER (l.valid) AS b_spend,
    sum(m.kcal) FILTER (l.valid) AS b_kcal,
    sum(greatest(m.spend - m.kcal * l.p25, 0)) FILTER (l.valid) AS sav25,
    sum(greatest(m.spend - m.kcal * l.p50, 0)) FILTER (l.valid) AS sav50,
    sum(m.kcal * l.cheap_dpk) FILTER (l.valid) AS cost_cheap,
    sum(m.sugar) FILTER (l.valid) AS own_sugar,
    sum(m.kcal * l.ch_sugar_pk) FILTER (l.valid) AS cf_sugar,
    sum(m.satfat) FILTER (l.valid) AS own_satfat,
    sum(m.kcal * l.ch_satfat_pk) FILTER (l.valid) AS cf_satfat,
    sum(m.sodium) FILTER (l.valid) AS own_sodium,
    sum(m.kcal * l.ch_sodium_pk) FILTER (l.valid) AS cf_sodium,
    sum(m.fiber) FILTER (l.valid) AS own_fiber,
    sum(m.kcal * l.ch_fiber_pk) FILTER (l.valid) AS cf_fiber,
    sum(m.protein) FILTER (l.valid) AS own_protein,
    sum(m.kcal * l.ch_protein_pk) FILTER (l.valid) AS cf_protein,
    sum(
        m.kcal_g) FILTER (
        l.valid AND l.ch_kcal_per_g > 0
        AND m.grams > 0
    ) AS own_kcal_g,
    sum(
        m.grams) FILTER (
        l.valid AND l.ch_kcal_per_g > 0
        AND m.grams > 0
    ) AS own_grams,
    sum(
        m.kcal_g / l.ch_kcal_per_g) FILTER (
        l.valid
        AND l.ch_kcal_per_g > 0 AND m.grams > 0
    ) AS cf_grams
FROM hm AS m
LEFT JOIN lad AS l ON l.state = m.state AND l.yq = m.yq AND l.module = m.module
WHERE m.is_t = 0
GROUP BY ALL;

-- (b) tier price indices on household base module weights
CREATE TEMP TABLE fx AS
SELECT
    m.household_code,
    m.t,
    100 * (sum(m.spend * x.l_cheap) / sum(m.spend) - 1) AS f_cheap,
    100 * (sum(m.spend * x.l_all) / sum(m.spend) - 1) AS f_all,
    100 * (sum(m.spend * x.l_prem) / sum(m.spend) - 1) AS f_prem,
    100 * (sum(m.spend * x.l_cheap_own) / sum(m.spend) - 1) AS f_cheap_own,
    100 * (sum(m.spend * x.l_prem_own) / sum(m.spend) - 1) AS f_prem_own,
    sum(m.spend) AS fx_spend
FROM hm AS m
JOIN tierx AS x ON x.state = m.state AND x.t = m.t AND x.module = m.module
WHERE
    m.is_t = 0 AND x.l_cheap IS NOT NULL AND x.l_prem IS NOT NULL
    AND x.l_cheap_own IS NOT NULL AND x.l_prem_own IS NOT NULL
GROUP BY ALL;

CREATE TEMP TABLE fx2 AS
SELECT
    m.household_code,
    m.t,
    100 * (sum(m.spend * x.l_cheap_b2) / sum(m.spend) - 1) AS f_cheap_b2,
    100 * (sum(m.spend * x.l_prem_b2) / sum(m.spend) - 1) AS f_prem_b2
FROM hm AS m
JOIN tierx AS x ON x.state = m.state AND x.t = m.t AND x.module = m.module
WHERE m.is_t = 0 AND x.l_cheap_b2 IS NOT NULL AND x.l_prem_b2 IS NOT NULL
GROUP BY ALL;

-- (c) own base items at state $/kcal (labelled, non leave-out T2 analogue)
CREATE TEMP TABLE eo AS
SELECT
    i.household_code,
    i.t,
    100 * (sum(i.kcal * p.dpk1) / sum(i.kcal * p.dpk0) - 1) AS e_own,
    count(*) AS n_e_items
FROM hi AS i
JOIN pr AS p ON p.state = i.state AND p.t = i.t AND p.item = i.item
WHERE i.is_t = 0
GROUP BY ALL;

-- (d) cheap-tier calorie share in t and t-4 (tiers of the same quarter)
CREATE TEMP TABLE tq AS
SELECT
    i.household_code,
    i.t,
    sum(i.kcal) FILTER (i.is_t = 1 AND s.tier = 'cheap') AS kcal_cheap1,
    sum(i.kcal) FILTER (i.is_t = 1 AND s.tier IS NOT NULL) AS kcal_tier1,
    sum(i.kcal) FILTER (i.is_t = 0 AND s.tier = 'cheap') AS kcal_cheap0,
    sum(i.kcal) FILTER (i.is_t = 0 AND s.tier IS NOT NULL) AS kcal_tier0,
    sum(i.kcal) FILTER (i.is_t = 1 AND s.tier = 'premium') AS kcal_prem1,
    sum(i.kcal) FILTER (i.is_t = 0 AND s.tier = 'premium') AS kcal_prem0,
    sum(i.spend) FILTER (i.is_t = 1) AS spend1,
    sum(i.kcal) FILTER (i.is_t = 1) AS kcal1,
    sum(i.spend) FILTER (i.is_t = 0) AS spend0,
    sum(i.kcal) FILTER (i.is_t = 0) AS kcal0
FROM hi AS i
LEFT JOIN st AS s ON s.state = i.state AND s.yq = i.yq AND s.item = i.item
GROUP BY ALL;

-- (e) Tornqvist decomposition of dln($/kcal) into market ladder, own rung
--     and between-module mix
CREATE TEMP TABLE dc AS
WITH r AS (
    SELECT
        m.household_code,
        m.t,
        m.is_t,
        m.module,
        m.kcal,
        ln(m.spend / m.kcal) AS ln_dpk,
        ln(l.p50) AS ln_p50
    FROM hm AS m JOIN lad AS l
        ON
            l.state = m.state AND l.yq = m.yq
            AND l.module = m.module
    WHERE l.valid AND m.kcal > 0 AND m.spend > 0 AND l.p50 > 0
),

w AS (
    SELECT
        a.household_code,
        a.t,
        a.module,
        a.kcal / sum(a.kcal) OVER (PARTITION BY a.household_code, a.t) AS c1,
        b.kcal / sum(b.kcal) OVER (PARTITION BY b.household_code, b.t) AS c0,
        a.ln_dpk - b.ln_dpk AS d_dpk,
        a.ln_p50 - b.ln_p50 AS d_p50
    FROM r AS a JOIN r AS b
        ON
            b.household_code = a.household_code AND b.t = a.t
            AND b.module = a.module AND b.is_t = 0
    WHERE a.is_t = 1
)

SELECT
    household_code,
    t,
    100 * sum(0.5 * (c1 + c0) * d_dpk) AS dec_w,
    100 * sum(0.5 * (c1 + c0) * d_p50) AS dec_m,
    100 * sum(0.5 * (c1 + c0) * (d_dpk - d_p50)) AS dec_l,
    count(*) AS n_mod_both
FROM w
GROUP BY ALL;

COPY (
    SELECT
        g.household_code,
        g.yq,
        g.state,
        b.* EXCLUDE (household_code, t),
        f.* EXCLUDE (household_code, t),
        x2.* EXCLUDE (household_code, t),
        e.* EXCLUDE (household_code, t),
        q.* EXCLUDE (household_code, t),
        d.* EXCLUDE (household_code, t),
        100 * (ln(q.spend1 / q.kcal1) - ln(q.spend0 / q.kcal0)) AS dec_t
    FROM gk AS g
    LEFT JOIN base AS b ON b.household_code = g.household_code AND b.t = g.yq
    LEFT JOIN fx AS f ON f.household_code = g.household_code AND f.t = g.yq
    LEFT JOIN fx2 AS x2 ON x2.household_code = g.household_code AND x2.t = g.yq
    LEFT JOIN eo AS e ON e.household_code = g.household_code AND e.t = g.yq
    LEFT JOIN tq AS q ON q.household_code = g.household_code AND q.t = g.yq
    LEFT JOIN dc AS d ON d.household_code = g.household_code AND d.t = g.yq
) TO '${OUT_HH}' (FORMAT parquet, COMPRESSION zstd);
