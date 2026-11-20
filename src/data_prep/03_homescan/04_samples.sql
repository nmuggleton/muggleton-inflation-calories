-- Sample-restriction flags. Nothing is dropped: every household-year keeps a
-- row and a set of boolean flags so analysts can combine restrictions
-- explicitly.
--
-- Flag definitions
--   f_static          projection_factor present (NielsenIQ 12-month static;
--                     all Kilts households should pass -> sanity check)
--   f_food_reporter   positive food-department spend AND trips in >= 10
--                     distinct calendar months of the data year
--   f_spend_outlier   annual food spend per household member below the
--                     0.5th or above the 99.5th percentile of its year
--   f_magnet_hh       projection_factor_magnet present AND panel_year >=
--                     2007 (random-weight reporters; use
--                     projection_factor_magnet as weight). The factor is
--                     populated for ~21% of 2004-06 households, but the
--                     manual says Magnet data do not exist before 2007 and
--                     no Magnet lines appear in 2004-06, so those are FALSE.
--   f_health_survey   responded to the PanelViews health survey that year
--   f_store_linkable  >= 50% of spend at stores with store_code_uc > 0
--                     (needed for Retail Scanner price/choice-set links)
--   f_nutri_cov50     2021+: >= 50% of food spend on UPCs with parsed kcal
--   f_core            f_static AND f_food_reporter AND NOT f_spend_outlier
--   bal_<window>      household has f_core in EVERY year of the window
--                     (NULL when the build does not contain all window years)

CREATE OR REPLACE TEMP TABLE hy AS
SELECT * FROM read_parquet('${WP2_FINAL}/household_year.parquet');

CREATE OR REPLACE TEMP TABLE pct AS
SELECT
    panel_year,
    quantile_cont(spend_food / household_size, 0.005) AS lo,
    quantile_cont(spend_food / household_size, 0.995) AS hi
FROM hy
WHERE spend_food > 0
GROUP BY panel_year;

CREATE OR REPLACE TEMP TABLE f AS
SELECT
    hy.household_code,
    hy.panel_year,
    (hy.projection_factor IS NOT NULL) AS f_static,
    (
        coalesce(hy.spend_food, 0) > 0
        AND coalesce(hy.n_months_with_trips, 0) >= 10
    ) AS f_food_reporter,
    (
        hy.spend_food IS NULL OR hy.spend_food <= 0
        OR hy.spend_food / hy.household_size < pct.lo
        OR hy.spend_food / hy.household_size > pct.hi
    ) AS f_spend_outlier,
    (
        hy.projection_factor_magnet IS NOT NULL
        AND hy.panel_year >= 2007
    ) AS f_magnet_hh,
    coalesce(hy.in_health_survey, false) AS f_health_survey,
    -- Shares are rounded before the cut-offs, so that floating-point noise in
    -- the sums cannot put values just either side
    (round(
        coalesce(hy.spend_store_identified, 0)
        / nullif(hy.spend_all, 0), 9
    ) >= 0.5) AS f_store_linkable,
    CASE
        WHEN hy.panel_year >= 2021
            THEN round(coalesce(hy.sh_food_spend_with_kcal, 0), 9) >= 0.5
    END AS f_nutri_cov50
FROM hy LEFT JOIN pct USING (panel_year);

ALTER TABLE f ADD COLUMN f_core BOOLEAN;
UPDATE f SET f_core = f_static AND f_food_reporter AND NOT f_spend_outlier;

-- Balanced windows. A window is evaluated only when all of its years exist.
CREATE OR REPLACE TEMP TABLE windows (name VARCHAR, y0 INT, y1 INT);
INSERT INTO windows VALUES
('bal_2019_2022', 2019, 2022),   -- tight pre/post around the 2021-22 surge
('bal_2018_2023', 2018, 2023),   -- 3 pre + 3 surge/post years
('bal_2016_2023', 2016, 2023),   -- long pre-period for trends
('bal_2007_2012', 2007, 2012);   -- Great Recession comparison episode

CREATE OR REPLACE TEMP TABLE years AS SELECT DISTINCT panel_year FROM f;

CREATE OR REPLACE TEMP TABLE bal AS
SELECT
    w.name,
    f.household_code,
    CASE
        WHEN (
            SELECT count(*) FROM years
            WHERE panel_year BETWEEN w.y0 AND w.y1
        ) = w.y1 - w.y0 + 1
            THEN count(
                *) FILTER (
                WHERE f.f_core
                AND f.panel_year BETWEEN w.y0 AND w.y1
            )
            = w.y1 - w.y0 + 1
    END AS is_bal
FROM windows AS w CROSS JOIN f
GROUP BY w.name, w.y0, w.y1, f.household_code;

CREATE OR REPLACE TEMP TABLE bal_wide AS
PIVOT bal ON name USING any_value(is_bal)
GROUP BY household_code;

COPY (
    SELECT
        f.*,
        b.* EXCLUDE (household_code)
    FROM f LEFT JOIN bal_wide AS b USING (household_code)
    ORDER BY household_code, panel_year
) TO '${WP2_FINAL}/sample_flags.parquet' (FORMAT parquet, COMPRESSION zstd);
