-- National product-module price shocks for target calendar year ${YEAR} (t =
-- ${YEAR}Q1..Q4 vs t-4), the "shifts" of the ext1 shift-share instrument. Run
-- per year by src/data_prep/09_analysis_dataset/03_shiftshare.sh, which first
-- creates the temp table stdiv (state -> census division).

CREATE OR REPLACE TEMP TABLE xw AS
SELECT
    upc_2020,
    upc_2021_masked
FROM read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet');

CREATE OR REPLACE TEMP TABLE pm AS
SELECT
    upc,
    upc_ver_uc,
    product_module_code AS module
FROM read_parquet('${WP2_FINAL}/dim_products_b1.parquet');

-- 2021+ products are keyed on (upc, panel_year): each year's own file
CREATE OR REPLACE TEMP TABLE pm2 AS
-- deterministic, as the ladder
SELECT
    upc,
    panel_year,
    min(product_module_code) AS module
FROM read_parquet('${WP2_FINAL}/dim_products_b2.parquet')
WHERE panel_year >= 2021
GROUP BY upc, panel_year;

CREATE OR REPLACE TEMP TABLE su AS
SELECT
    s.fips_state_code AS st,
    s.yq,
    coalesce(
        CASE WHEN s.panel_year <= 2020 THEN x.upc_2021_masked END,
        s.upc
    ) AS item,
    -- items pooled over modules take the lowest module (deterministic)
    min(coalesce(
        CASE WHEN s.panel_year <= 2020 THEN m.module END,
        CASE WHEN s.panel_year >= 2021 THEN m2.module END
    ))
        AS module,
    -- exact fixed-point sums: the same in any order
    sum(s.qty::DECIMAL(38, 9))::DOUBLE AS q,
    sum(s.spend::DECIMAL(38, 9))::DOUBLE AS sp
FROM read_parquet([${SU_FILES}]) AS s
LEFT JOIN xw AS x ON s.upc = x.upc_2020
LEFT JOIN pm AS m ON m.upc = s.upc AND m.upc_ver_uc = s.upc_ver_uc
-- the panel_year >= 2021 condition sits in the SELECT, not the ON clause:
-- a non-key predicate there makes DuckDB plan a nested-loop join
LEFT JOIN pm2 AS m2 ON m2.upc = s.upc AND m2.panel_year = s.panel_year
WHERE s.yq BETWEEN (${YEAR} - 1) * 10 + 1 AND ${YEAR} * 10 + 4
GROUP BY s.fips_state_code, s.yq, item;

CREATE OR REPLACE TEMP TABLE nat AS
SELECT
    yq,
    item,
    min(module) AS module,
    sum(q::DECIMAL(38, 9))::DOUBLE AS q,
    sum(sp::DECIMAL(38, 9))::DOUBLE AS sp
FROM su
GROUP BY yq, item;

-- national item price pairs t vs t-4
CREATE OR REPLACE TEMP TABLE np AS
SELECT
    a.yq,
    a.item,
    b.module,
    a.sp AS sp1,
    a.q AS q1,
    b.sp AS sp0,
    b.q AS q0,
    ((a.sp / a.q) / (b.sp / b.q)) BETWEEN 1.0 / 3 AND 3 AS ok
FROM nat AS a JOIN nat AS b ON b.item = a.item AND b.yq = a.yq - 10
WHERE a.yq >= ${YEAR} * 10 + 1 AND b.module IS NOT NULL;

COPY (
    SELECT
        module,
        yq,
        sum(CASE WHEN ok THEN sp0 * ln((sp1 / q1) / (sp0 / q0)) END) AS A,
        sum(CASE WHEN ok THEN sp0 END) AS W,
        count(*) FILTER (ok) AS n_items
    FROM np
    GROUP BY ALL
) TO '${SHIFTSHARE_INT}/shock_nat/year=${YEAR}.parquet' (FORMAT parquet);

-- leave-own-area-out corrections, for area = state and area = division
CREATE OR REPLACE TEMP TABLE au AS
SELECT
    'st' AS lvl,
    st AS area,
    yq,
    item,
    q,
    sp
FROM su
UNION ALL
SELECT
    'div' AS lvl,
    d.division AS area,
    s.yq,
    s.item,
    sum(s.q::DECIMAL(38, 9))::DOUBLE,
    sum(s.sp::DECIMAL(38, 9))::DOUBLE
FROM su AS s JOIN stdiv AS d ON d.state = s.st
GROUP BY d.division, s.yq, s.item;

CREATE OR REPLACE TEMP TABLE corr AS
WITH ap AS (
    SELECT
        coalesce(a.lvl, b.lvl) AS lvl,
        coalesce(a.area, b.area) AS area,
        coalesce(a.yq, b.yq + 10) AS yq,
        coalesce(a.item, b.item) AS item,
        coalesce(a.sp, 0) AS s1,
        coalesce(a.q, 0) AS sq1,
        coalesce(b.sp, 0) AS s0,
        coalesce(b.q, 0) AS sq0
    FROM (
        SELECT * FROM au
        WHERE yq >= ${YEAR} * 10 + 1
    ) AS a
    FULL JOIN (
        SELECT * FROM au
        WHERE yq <= (${YEAR} - 1) * 10 + 4
    ) AS b
        ON
            b.lvl = a.lvl AND b.area = a.area AND b.item = a.item
            AND b.yq = a.yq - 10
),

lo AS (
    SELECT
        ap.lvl,
        ap.area,
        np.module,
        np.yq,
        np.ok,
        np.sp0,
        np.sp1,
        np.q1,
        np.q0,
        np.sp1 - ap.s1 AS l_sp1,
        np.q1 - ap.sq1 AS l_q1,
        np.sp0 - ap.s0 AS l_sp0,
        np.q0 - ap.sq0 AS l_q0
    FROM ap JOIN np ON np.item = ap.item AND np.yq = ap.yq
),

lv AS (
    -- 1e-9 guards against float residue when the area is the only buyer
    SELECT
        *,
        (
            l_sp1 > 1e-9 AND l_q1 > 1e-9 AND l_sp0 > 1e-9
            AND l_q0 > 1e-9
        ) AS pos
    FROM lo
)

SELECT
    lvl,
    area,
    module,
    yq,
    sum(CASE
        WHEN
            pos AND ((l_sp1 / l_q1) / (l_sp0 / l_q0))
            BETWEEN 1.0 / 3 AND 3
            THEN l_sp0 * ln((l_sp1 / l_q1) / (l_sp0 / l_q0))
        ELSE 0
    END
    - CASE WHEN ok THEN sp0 * ln((sp1 / q1) / (sp0 / q0)) ELSE 0 END)
        AS C,
    sum(CASE
        WHEN
            pos AND ((l_sp1 / l_q1) / (l_sp0 / l_q0))
            BETWEEN 1.0 / 3 AND 3 THEN l_sp0
        ELSE 0
    END
    - CASE WHEN ok THEN sp0 ELSE 0 END) AS D
FROM lv
GROUP BY ALL;

COPY (SELECT
    area AS state,
    module,
    yq,
    C,
    D
FROM corr
WHERE lvl = 'st')
TO '${SHIFTSHARE_INT}/shock_st/year=${YEAR}.parquet' (FORMAT parquet);
COPY (SELECT
    area AS division,
    module,
    yq,
    C,
    D
FROM corr
WHERE lvl = 'div')
TO '${SHIFTSHARE_INT}/shock_div/year=${YEAR}.parquet' (FORMAT parquet);
