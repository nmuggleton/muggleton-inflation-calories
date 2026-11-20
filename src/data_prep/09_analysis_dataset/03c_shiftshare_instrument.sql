-- Household shift-share (Bartik) exposure instrument for target calendar year
-- ${YEAR}. Run per year by src/data_prep/09_analysis_dataset/03_shiftshare.sh
-- after src/data_prep/09_analysis_dataset/03b_shiftshare_hhmod.sql and
-- src/data_prep/09_analysis_dataset/03a_shiftshare_shocks.sql.

-- shocks of years Y-1, Y, Y+1 (${NAT_FILES}, ${ST_FILES}), so the same t-4
-- shares can be combined with the lagged (t-8 -> t-4) and the future (t -> t+4)
-- shock for the dynamic placebo
CREATE OR REPLACE TEMP TABLE g AS
SELECT
    n.module,
    n.yq,
    s.state,
    n.A,
    n.W,
    s.C,
    s.D
FROM read_parquet([${NAT_FILES}]) AS n
LEFT JOIN read_parquet([${ST_FILES}]) AS s USING (module, yq);

CREATE OR REPLACE TEMP TABLE gn AS
SELECT
    module,
    yq,
    CASE WHEN W > 0 THEN A / W END AS g_nat
FROM read_parquet([${NAT_FILES}]);

CREATE OR REPLACE TEMP TABLE gs AS
SELECT
    module,
    yq,
    state,
    CASE WHEN W + D > 1e-9 THEN (A + C) / (W + D) END AS g_st
FROM g
WHERE state IS NOT NULL;

CREATE OR REPLACE TEMP TABLE gd AS
SELECT
    n.module,
    n.yq,
    d.division,
    CASE WHEN n.W + d.D > 1e-9 THEN (n.A + d.C) / (n.W + d.D) END AS g_div
FROM read_parquet('${SHIFTSHARE_INT}/shock_nat/year=${YEAR}.parquet') AS n
JOIN read_parquet('${SHIFTSHARE_INT}/shock_div/year=${YEAR}.parquet') AS d
    USING (module, yq);

CREATE OR REPLACE TEMP TABLE b4 AS
WITH r AS (
    SELECT
        hq.household_code,
        hq.yq,
        b.module,
        b.spend,
        gn.g_nat,
        -- modules the state did not buy at all: leave-out = national
        CASE WHEN gs.module IS NULL THEN gn.g_nat ELSE gs.g_st END AS g_st,
        CASE WHEN gd.module IS NULL THEN gn.g_nat ELSE gd.g_div END AS g_div,
        CASE WHEN sl.module IS NULL THEN nl.g_nat ELSE sl.g_st END AS g_lag,
        CASE WHEN sf.module IS NULL THEN nf.g_nat ELSE sf.g_st END AS g_lead
    FROM hq
    JOIN hm4 AS b
        ON b.household_code = hq.household_code AND b.yq = hq.yq
    LEFT JOIN gn ON gn.module = b.module AND gn.yq = hq.yq
    LEFT JOIN gs
        ON
            gs.module = b.module AND gs.yq = hq.yq
            AND gs.state = hq.state
    LEFT JOIN gd
        ON
            gd.module = b.module AND gd.yq = hq.yq
            AND gd.division = hq.division
    LEFT JOIN gn AS nl ON nl.module = b.module AND nl.yq = hq.yq - 10
    LEFT JOIN gs AS sl
        ON
            sl.module = b.module AND sl.yq = hq.yq - 10
            AND sl.state = hq.state
    LEFT JOIN gn AS nf ON nf.module = b.module AND nf.yq = hq.yq + 10
    LEFT JOIN gs
        AS sf ON sf.module = b.module AND sf.yq = hq.yq + 10
    AND sf.state = hq.state
)

SELECT
    household_code,
    yq,
    count(*) AS n_mod4,
    sum(spend) AS spend_base4,
    100 * sum(spend * g_st) / sum(spend) AS B4_st,
    100 * sum(spend * g_nat) / sum(spend) AS B4_nat,
    100 * sum(spend * g_div) / sum(spend) AS B4_div,
    sum(spend) FILTER (g_st IS NOT NULL) / sum(spend) AS S4_st,
    sum(spend) FILTER (g_nat IS NOT NULL) / sum(spend) AS S4_nat,
    sum(spend) FILTER (g_div IS NOT NULL) / sum(spend) AS S4_div,
    100 * sum(spend * g_lag) / sum(spend) AS B4_lag,
    100 * sum(spend * g_lead) / sum(spend) AS B4_lead,
    sum(spend) FILTER (g_lag IS NOT NULL) / sum(spend) AS S4_lag,
    sum(spend) FILTER (g_lead IS NOT NULL) / sum(spend) AS S4_lead
FROM r
GROUP BY ALL;

CREATE OR REPLACE TEMP TABLE b8 AS
WITH r AS (
    SELECT
        hq.household_code,
        hq.yq,
        b.spend,
        CASE WHEN gs.module IS NULL THEN gn.g_nat ELSE gs.g_st END AS g_st
    FROM hq
    JOIN hm8 AS b
        ON b.household_code = hq.household_code AND b.yq = hq.yq
    LEFT JOIN gn ON gn.module = b.module AND gn.yq = hq.yq
    LEFT JOIN gs ON
        gs.module = b.module AND gs.yq = hq.yq
        AND gs.state = hq.state
)

SELECT
    household_code,
    yq,
    100 * sum(spend * g_st) / sum(spend) AS B8_st,
    sum(spend) FILTER (g_st IS NOT NULL) / sum(spend) AS S8_st
FROM r
GROUP BY ALL;

COPY (
    SELECT
        b4.*,
        b8.B8_st,
        b8.S8_st
    FROM b4 LEFT JOIN b8 USING (household_code, yq)
) TO '${SHIFTSHARE_INT}/bartik/year=${YEAR}.parquet'
(FORMAT parquet, COMPRESSION zstd);
