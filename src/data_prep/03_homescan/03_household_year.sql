-- Household x panel-year panel (all years). One row per (household_code,
-- panel_year) in the panelists files, with demographics as delivered, per-year
-- REPORTING-COVERAGE aggregates from
-- src/data_prep/03_homescan/02c_merge_hh_agg.sql (used for sample definitions,
-- not analysis variables), panel tenure, and health-survey linkage.

CREATE OR REPLACE TEMP VIEW h AS
SELECT * FROM read_parquet(
    '${WP2_INT}/panel/panelists/*/data.parquet',
    union_by_name = true, hive_partitioning = false
);
CREATE OR REPLACE TEMP VIEW a AS
SELECT * FROM read_parquet(
    '${WP2_FINAL}/_agg/hh_year/*.parquet',
    union_by_name = true, hive_partitioning = false
);

CREATE OR REPLACE TEMP TABLE survey AS
SELECT
    household_code,
    CAST(survey_year AS SMALLINT) AS panel_year,
    count(*) AS n_health_survey_rows
FROM
    read_parquet(
        '${WP2_INT}/panelviews/health/*/data.parquet',
        union_by_name = true, hive_partitioning = false
    )
GROUP BY ALL;

COPY (
    SELECT
        h.*,
        a.* EXCLUDE (household_code, panel_year),
        coalesce(s.n_health_survey_rows, 0) AS n_health_survey_rows,
        (s.household_code IS NOT NULL) AS in_health_survey,
        count(*) OVER w_hh AS n_years_in_panel,
        min(h.panel_year) OVER w_hh AS first_panel_year,
        max(h.panel_year) OVER w_hh AS last_panel_year,
        coalesce(lag(h.panel_year) OVER w_ord = h.panel_year - 1, false)
            AS in_prev_year,
        coalesce(lead(h.panel_year) OVER w_ord = h.panel_year + 1, false)
            AS in_next_year
    FROM h
    LEFT JOIN a USING (household_code, panel_year)
    LEFT JOIN survey AS s USING (household_code, panel_year)
    WINDOW
        w_hh AS (PARTITION BY h.household_code),
        w_ord AS (PARTITION BY h.household_code ORDER BY h.panel_year)
) TO '${WP2_FINAL}/household_year.parquet' (FORMAT parquet, COMPRESSION zstd);
