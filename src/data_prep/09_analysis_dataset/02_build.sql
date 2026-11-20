-- The single analysis dataset for the paper. Every estimation script
-- (src/analysis/estimate/) loads it through src/lib/helpers.R; none rebuilds
-- the sample.
CREATE VIEW analysis_hh_quarter AS
SELECT * FROM read_parquet(
    '${WP2_FINAL}/analysis_hh_quarter/*/*.parquet',
    hive_partitioning = true
);

-- 1. Panel ------------------------------------------------------------------
CREATE TEMP TABLE d AS
WITH a AS (
    SELECT
        household_code,
        yq,
        year,
        qtr,
        fips_state_code AS state,
        fips_county_code AS county,
        region_code,
        income_cat,
        projection_factor,
        projection_factor_magnet,
        f_magnet_hh,
        household_size,
        ae_oecd,
        head_age,
        head_college,
        race_f,
        hispanic,
        children_present,
        first_panel_year,
        n_years_in_panel,
        in_next_year,
        bal_2007_2012,
        f_core,
        f_partial_q,
        f_exp_valid,
        f_idx_valid,
        f_spend_outlier,
        days_obs,
        cov_lab,
        infl_exposure,
        infl_laspeyres,
        idx_exposure,
        n_base_items,
        n_base_priced,
        spend_base,
        kcal_per_usd,
        kcal_per_usd_real,
        kcal_per_usd_w,
        spend_food_nomag_ae_day,
        spend_food_nomag_ae_day_real,
        spend_food_nomag,
        spend_food,
        spend_food_mag,
        spend_lab,
        spend_fg_fv,
        spend_fg_fv_nomag,
        kcal_lab,
        kcal_lab_ae_day,
        kcal_fb_ae_day,
        kcal_scaled_ae_day,
        kcal_fb,
        spend_fb,
        grams_lab,
        ml_lab,
        kcal_100g_food,
        health_score,
        panel_year,
        ae_oecd,
        income_real2019,
        fpl_ratio,
        snap_elig_proxy,
        any_head_employed,
        n_heads,
        scantrack_market_code,
        sh_ch_dollar,
        sh_pl,
        sh_deal,
        f_income_topcoded,
        shop_component,
        cpi_fah_yoy,
        episode
    FROM analysis_hh_quarter
),

j AS (
    SELECT
        a.*,
        b.f_core AS f_core_b,
        b.f_partial_q AS f_partial_q_b,
        b.cov_lab AS cov_lab_b,
        b.kcal_per_usd_real AS kpu_b,
        b.household_size AS household_size_b,
        b.days_obs AS days_obs_b,
        b.f_spend_outlier AS f_spend_outlier_b,
        -- per AE-day denominators on each side
        a.ae_oecd * a.days_obs AS aed_a,
        b.ae_oecd * b.days_obs AS aed_b,
        b.kcal_per_usd_real AS lvl_kpu_b,
        b.spend_food_nomag_ae_day_real AS lvl_spend_b,
        b.kcal_lab_ae_day AS lvl_kcal_b,
        b.spend_food_nomag AS spend_food_nomag_b,
        b.spend_food AS spend_food_b,
        b.spend_food_mag AS spend_food_mag_b,
        b.spend_lab AS spend_lab_b,
        b.spend_fg_fv AS spend_fg_fv_b,
        b.kcal_lab AS kcal_lab_b,
        b.kcal_per_usd AS kcal_per_usd_b,
        b.kcal_per_usd_w AS kcal_per_usd_w_b,
        b.spend_food_nomag_ae_day AS spend_nom_aed_b,
        b.spend_food_nomag_ae_day_real AS spend_real_aed_b,
        b.kcal_lab_ae_day AS kcal_lab_ae_day_b,
        b.kcal_fb_ae_day AS kcal_fb_ae_day_b,
        b.kcal_scaled_ae_day AS kcal_scaled_ae_day_b,
        b.kcal_fb AS kcal_fb_b,
        b.spend_fb AS spend_fb_b,
        b.grams_lab AS grams_lab_b,
        b.ml_lab AS ml_lab_b,
        b.kcal_100g_food AS kcal_100g_food_b,
        b.health_score AS health_score_b,
        -- base-quarter moderators (pre-determined)
        b.panel_year AS panel_year_b,
        b.income_real2019 AS inc_real_b,
        b.fpl_ratio AS fpl_b,
        b.snap_elig_proxy AS snap_b,
        b.head_age AS age_b,
        b.head_college AS college_b,
        b.race_f AS race_b,
        b.hispanic AS hisp_b,
        b.children_present AS kids_b,
        b.ae_oecd AS ae_b,
        b.any_head_employed AS emp_b,
        b.n_heads AS nheads_b,
        b.scantrack_market_code AS mkt_b,
        b.sh_ch_dollar AS dollar_b,
        b.sh_pl AS pl_b,
        b.sh_deal AS deal_b,
        b.f_income_topcoded AS topcode_b,
        -- changes in household covariates t-4 -> t
        CAST(a.any_head_employed AS INTEGER)
        - CAST(b.any_head_employed AS INTEGER) AS d_emp,
        100 * (
            (CASE WHEN a.income_real2019 > 0 THEN ln(a.income_real2019) END)
            - (CASE WHEN b.income_real2019 > 0 THEN ln(b.income_real2019) END))
            AS d_linc,
        a.household_size - b.household_size AS d_hhsize,
        -- last year's changes, t-8 -> t-4 (c = the same household at t-8)
        100 * ((
            CASE WHEN b.kcal_per_usd_real > 0 THEN ln(b.kcal_per_usd_real)
            END
        )
        - (
            CASE WHEN c.kcal_per_usd_real > 0 THEN ln(c.kcal_per_usd_real)
            END
        )) AS l_d_lkpu,
        100 * ((CASE
            WHEN b.spend_food_nomag_ae_day_real > 0
                THEN ln(b.spend_food_nomag_ae_day_real)
        END)
        - (CASE
            WHEN c.spend_food_nomag_ae_day_real > 0
                THEN ln(c.spend_food_nomag_ae_day_real)
        END)) AS l_d_lspend,
        100 * (
            (CASE WHEN b.kcal_lab_ae_day > 0 THEN ln(b.kcal_lab_ae_day) END)
            - (CASE WHEN c.kcal_lab_ae_day > 0 THEN ln(c.kcal_lab_ae_day) END))
            AS l_d_lkcal,
        b.kcal_100g_food - c.kcal_100g_food AS l_d_ed,
        b.health_score - c.health_score AS l_d_hs
    FROM a JOIN a AS b
        ON b.household_code = a.household_code AND b.yq = a.yq - 10
    LEFT JOIN a AS c
        ON c.household_code = a.household_code AND c.yq = a.yq - 20
    WHERE
        a.year BETWEEN 2005 AND ${WP2_END_YEAR}
        AND coalesce(a.f_exp_valid, false)
        AND a.state IS NOT NULL
)

SELECT
    household_code,
    yq,
    year,
    qtr,
    panel_year,
    panel_year_b,
    state,
    county,
    region_code,
    income_cat,
    projection_factor,
    projection_factor_magnet,
    f_magnet_hh,
    household_size,
    household_size_b,
    head_age,
    head_college,
    race_f,
    hispanic,
    children_present,
    first_panel_year,
    n_years_in_panel,
    in_next_year,
    bal_2007_2012,
    -- f1 estimation sample
    -- (src/data_prep/07_controls/03_sample_panel.sql est_gate)
    (
        year <> 2020 AND coalesce(f_core, false) AND coalesce(f_core_b, false)
        AND NOT coalesce(f_partial_q, false)
        AND NOT coalesce(f_partial_q_b, false)
        -- rounded before the cut-off, as in 03_sample_panel.sql
        AND coalesce(
            round(cov_lab, 9) >= 0.5 AND round(cov_lab_b, 9) >= 0.5
            AND kcal_per_usd_real > 0 AND kpu_b > 0, false
        )
    ) AS est_gate,
    coalesce(f_core, false) AS core_a,
    coalesce(f_core_b, false) AS core_b,
    coalesce(f_partial_q, false) AS partial_a,
    coalesce(f_partial_q_b, false) AS partial_b,
    coalesce(f_spend_outlier, false) OR coalesce(f_spend_outlier_b, false)
        AS spend_outlier_ab,
    f_idx_valid,
    cov_lab,
    cov_lab_b,
    kcal_per_usd_real,
    kpu_b,
    days_obs,
    days_obs_b,
    infl_exposure,
    infl_laspeyres,
    idx_exposure,
    n_base_items,
    n_base_priced,
    spend_base,
    -- base-quarter levels (pre-determined relative to the t-4 -> t change)
    CASE WHEN lvl_kpu_b > 0 THEN ln(lvl_kpu_b) END AS base_lkpu,
    CASE WHEN lvl_spend_b > 0 THEN ln(lvl_spend_b) END AS base_lspend,
    CASE WHEN lvl_kcal_b > 0 THEN ln(lvl_kcal_b) END AS base_lkcal,
    -- f1 outcomes, same construction as
    -- src/data_prep/07_controls/03_sample_panel.sql
    100 * (
        CASE WHEN kcal_per_usd_real > 0 THEN ln(kcal_per_usd_real) END
        - CASE WHEN kpu_b > 0 THEN ln(kpu_b) END
    ) AS d_lkpu,
    100 * (CASE
        WHEN spend_food_nomag_ae_day_real > 0
            THEN ln(spend_food_nomag_ae_day_real)
    END
    - CASE WHEN spend_real_aed_b > 0 THEN ln(spend_real_aed_b) END)
        AS d_lspend,
    100 * (
        CASE WHEN kcal_lab_ae_day > 0 THEN ln(kcal_lab_ae_day) END
        - CASE WHEN kcal_lab_ae_day_b > 0 THEN ln(kcal_lab_ae_day_b) END)
        AS d_lkcal,
    kcal_100g_food - kcal_100g_food_b AS d_ed,
    health_score - health_score_b AS d_hs,
    -- measurement variants. Nominal outcomes: the national deflator varies
    -- by quarter only and is absorbed by quarter FE.
    100 * (CASE
        WHEN spend_food_nomag_ae_day > 0
            THEN ln(spend_food_nomag_ae_day)
    END
    - CASE WHEN spend_nom_aed_b > 0 THEN ln(spend_nom_aed_b) END)
        AS d_lspend_nom,
    100 * (
        CASE WHEN kcal_per_usd > 0 THEN ln(kcal_per_usd) END
        - CASE WHEN kcal_per_usd_b > 0 THEN ln(kcal_per_usd_b) END)
        AS d_lkpu_nom,
    100 * (
        CASE WHEN kcal_per_usd_w > 0 THEN ln(kcal_per_usd_w) END
        - CASE WHEN kcal_per_usd_w_b > 0 THEN ln(kcal_per_usd_w_b) END)
        AS d_lkpu_w,
    -- labelled spend per AE-day: closes kcal = spend_lab x kcal/$ exactly
    100 * (
        CASE WHEN spend_lab > 0 THEN ln(spend_lab / aed_a) END
        - CASE WHEN spend_lab_b > 0 THEN ln(spend_lab_b / aed_b) END)
        AS d_lspend_lab,
    100 * (CASE
        WHEN spend_food_nomag - spend_lab > 0
            THEN ln((spend_food_nomag - spend_lab) / aed_a)
    END
    - CASE
        WHEN spend_food_nomag_b - spend_lab_b > 0
            THEN ln((spend_food_nomag_b - spend_lab_b) / aed_b)
    END)
        AS d_lspend_unlab,
    100 * (
        CASE WHEN kcal_fb_ae_day > 0 THEN ln(kcal_fb_ae_day) END
        - CASE WHEN kcal_fb_ae_day_b > 0 THEN ln(kcal_fb_ae_day_b) END)
        AS d_lkcal_fb,
    100 * (
        CASE WHEN kcal_scaled_ae_day > 0 THEN ln(kcal_scaled_ae_day) END
        - CASE
            WHEN kcal_scaled_ae_day_b > 0
                THEN ln(kcal_scaled_ae_day_b)
        END
    ) AS d_lkcal_sc,
    100 * (CASE
        WHEN kcal_fb > 0 AND spend_fb > 0
            THEN ln(kcal_fb / spend_fb)
    END
    - CASE
        WHEN kcal_fb_b > 0 AND spend_fb_b > 0
            THEN ln(kcal_fb_b / spend_fb_b)
    END) AS d_lkpu_fb,
    -- physical quantity: labelled grams (foods) plus ml (drinks) per AE-day
    100 * (CASE
        WHEN coalesce(grams_lab, 0) + coalesce(ml_lab, 0) > 0
            THEN ln(
                (coalesce(grams_lab, 0) + coalesce(ml_lab, 0))
                / aed_a
            )
    END
    - CASE
        WHEN coalesce(grams_lab_b, 0) + coalesce(ml_lab_b, 0) > 0
            THEN ln(
                (coalesce(grams_lab_b, 0) + coalesce(ml_lab_b, 0))
                / aed_b
            )
    END) AS d_lvol,
    100 * (
        CASE WHEN grams_lab > 0 THEN ln(grams_lab / aed_a) END
        - CASE WHEN grams_lab_b > 0 THEN ln(grams_lab_b / aed_b) END)
        AS d_lgrams,
    -- household-quarter totals (no AE or day normalisation)
    100 * (
        CASE WHEN spend_food_nomag > 0 THEN ln(spend_food_nomag) END
        - CASE WHEN spend_food_nomag_b > 0 THEN ln(spend_food_nomag_b) END)
        AS d_lspend_tot,
    100 * (
        CASE WHEN kcal_lab > 0 THEN ln(kcal_lab) END
        - CASE WHEN kcal_lab_b > 0 THEN ln(kcal_lab_b) END
    ) AS d_lkcal_tot,
    -- arc (symmetric) percent change of kcal/$: bounded, no log of small
    -- bases
    CASE
        WHEN kcal_per_usd_real + kpu_b > 0
            THEN 200 * (kcal_per_usd_real - kpu_b) / (kcal_per_usd_real + kpu_b)
    END AS d_arc_kpu,
    -- coverage and fresh / random-weight food
    cov_lab - cov_lab_b AS d_cov,
    100 * (
        CASE WHEN spend_food > 0 THEN ln(spend_food / aed_a) END
        - CASE WHEN spend_food_b > 0 THEN ln(spend_food_b / aed_b) END)
        AS d_lspend_all,
    CASE
        WHEN spend_food > 0 AND spend_food_b > 0
            THEN
                coalesce(spend_food_mag, 0) / spend_food
                - coalesce(spend_food_mag_b, 0) / spend_food_b
    END AS d_sh_mag,
    CASE
        WHEN spend_food > 0 AND spend_food_b > 0
            THEN
                coalesce(spend_fg_fv, 0) / spend_food
                - coalesce(spend_fg_fv_b, 0) / spend_food_b
    END AS d_sh_fv_all,
    -- levels used by the descriptive and cell-level exhibits
    kcal_100g_food,
    health_score,
    kcal_100g_food_b,
    health_score_b,
    spend_real_aed_b AS spend_b,
    health_score_b AS hs_b,
    -- base-quarter moderators and covariate changes
    inc_real_b,
    fpl_b,
    snap_b,
    age_b,
    college_b,
    race_b,
    hisp_b,
    kids_b,
    ae_b,
    emp_b,
    nheads_b,
    mkt_b,
    dollar_b,
    pl_b,
    deal_b,
    topcode_b,
    d_emp,
    d_linc,
    d_hhsize,
    l_d_lkpu,
    l_d_lspend,
    l_d_lkcal,
    l_d_ed,
    l_d_hs,
    shop_component,
    cpi_fah_yoy,
    episode,
    -- components for identity checks
    kcal_per_usd,
    spend_lab,
    kcal_lab,
    aed_a,
    spend_food_nomag,
    spend_food_nomag_ae_day,
    spend_food_nomag_ae_day_real
FROM j;

-- 2. Instruments --------------------------------------------------------------
-- Sums reproduce the original build's arithmetic bit for bit (a parallel SUM
-- adds in another order and can differ in the last digits).
CREATE MACRO seqsum(l) AS list_reduce(l, (a, b) -> a + b);
CREATE MACRO nsum_fold(l) AS list_reduce(
    list_transform(l, x -> { 's': x, 'c': 0::DOUBLE }),
    (
        a, b) -> {
        's': a.s + b.s,
        'c': a.c + CASE
            WHEN abs(a.s) >= abs(b.s)
                THEN (a.s - (a.s + b.s)) + b.s
            ELSE (b.s - (a.s + b.s)) + a.s
        END
    }
);
CREATE MACRO nsum(l) AS nsum_fold(l).s + nsum_fold(l).c;

CREATE TEMP TABLE sq AS            -- broad pool: core and complete in t
SELECT
    state,
    yq,
    seqsum(list(infl_exposure ORDER BY household_code, yq)) / count(*) AS zb
FROM d
WHERE core_a AND NOT partial_a
GROUP BY ALL;

CREATE TEMP TABLE inst AS
WITH g AS (
    SELECT
        household_code,
        yq,
        state,
        region_code,
        infl_exposure
    FROM d
    WHERE est_gate
),

s AS (
    SELECT
        state,
        yq,
        count(*) AS n_sq,
        nsum(list(infl_exposure ORDER BY household_code, yq)) AS s_z,
        seqsum(list(infl_exposure ORDER BY household_code, yq)) AS s_sq
    FROM g
    GROUP BY ALL
),

r AS (
    SELECT
        region_code,
        yq,
        count(*) AS n_dq,
        seqsum(list(infl_exposure ORDER BY household_code, yq)) AS s_dq
    FROM g
    GROUP BY ALL
)

SELECT
    g.household_code,
    g.yq,
    (s_z - infl_exposure) / (n_sq - 1) AS z,
    n_sq::INTEGER AS n_sq,
    CASE WHEN n_dq > n_sq THEN (s_dq - s_sq) / (n_dq - n_sq) END AS z_div
FROM g JOIN s USING (state, yq)
JOIN r ON r.region_code IS NOT DISTINCT FROM g.region_code AND r.yq = g.yq;

-- 3. Base-quarter moderators ------------------------------------------------
-- CPI regime cut points: type-7 quantiles (1/3, 2/3) of cpi_fah_yoy over
-- the estimation-sample quarters, computed as R's quantile() does
CREATE TEMP TABLE cuts AS
WITH x AS (
    SELECT
        list_sort(list(cpi_fah_yoy)) AS v,
        count(*) AS n
    FROM (SELECT DISTINCT
        yq,
        cpi_fah_yoy
    FROM d
    WHERE est_gate)
),

p AS (
    SELECT
        v,
        1 + (n - 1) * (1::DOUBLE / 3) AS i1,
        1 + (n - 1) * (2::DOUBLE / 3) AS i2
    FROM x
),

q AS (
    SELECT
        v,
        i1,
        i2,
        floor(i1)::INT AS l1,
        ceil(i1)::INT AS h1,
        floor(i2)::INT AS l2,
        ceil(i2)::INT AS h2
    FROM p
)

SELECT
    CASE
        WHEN i1 > l1 AND v[h1] <> v[l1]
            THEN (1 - (i1 - l1)) * v[l1] + (i1 - l1) * v[h1]
        ELSE v[l1]
    END AS c1,
    CASE
        WHEN i2 > l2 AND v[h2] <> v[l2]
            THEN (1 - (i2 - l2)) * v[l2] + (i2 - l2) * v[h2]
        ELSE v[l2]
    END AS c2
FROM q;
SELECT
    'CPI FAH yoy tercile cut points (quarters)' AS what,
    c1,
    c2
FROM cuts;

CREATE TEMP TABLE mods AS
WITH g AS (
    SELECT
        household_code,
        yq,
        fpl_b,
        age_b,
        emp_b,
        cpi_fah_yoy,
        CASE WHEN inc_real_b > 0 THEN spend_b * ae_b * 365 / inc_real_b END
            AS burden
    FROM d
    WHERE est_gate
),

r AS (
    SELECT
        *,
        -- average rank among non-missing, as data.table::frank(ties =
        -- "average")
        CASE
            WHEN fpl_b IS NOT NULL
                THEN
                    (
                        rank() OVER (ORDER BY fpl_b)
                        + (count(*) OVER (PARTITION BY fpl_b) - 1) / 2
                    )
                    / count(fpl_b) OVER ()
        END AS fpl_rank,
        -- ties broken by key order, as frank(ties = "first") on the keyed table
        CASE
            WHEN burden IS NOT NULL
                THEN
                    row_number()
                        OVER (ORDER BY burden NULLS LAST, household_code, yq)
                    / count(burden) OVER ()
        END AS burden_r
    FROM g
)

SELECT
    household_code,
    yq,
    CASE
        WHEN fpl_b <= 1 THEN '<=100%'
        WHEN fpl_b <= 1.3 THEN '100-130%'
        WHEN fpl_b <= 1.85 THEN '130-185%'
        WHEN fpl_b <= 3 THEN '185-300%'
        WHEN fpl_b <= 5 THEN '300-500%'
        WHEN fpl_b IS NOT NULL THEN '>500%'
    END AS fpl_bin,
    fpl_rank,
    burden,
    -- quintile: right-closed bins on seq(0, 1, 0.2)
    CASE
        WHEN burden_r <= 0.2::DOUBLE THEN 1
        WHEN burden_r <= 2 * 0.2::DOUBLE THEN 2
        WHEN burden_r <= 3 * 0.2::DOUBLE THEN 3
        WHEN burden_r <= 4 * 0.2::DOUBLE THEN 4
        WHEN burden_r <= 1 THEN 5
    END AS burden_q,
    NOT emp_b AS noemp,
    age_b >= 65 AS age65,
    CASE
        WHEN age_b < 35 THEN '<35' WHEN age_b < 45 THEN '35-44'
        WHEN age_b < 55 THEN '45-54' WHEN age_b < 65 THEN '55-64'
        WHEN age_b IS NOT NULL THEN '65+'
    END AS age_bin,
    CASE
        WHEN cpi_fah_yoy <= c1 THEN 'Low' WHEN cpi_fah_yoy <= c2 THEN 'Middle'
        WHEN cpi_fah_yoy IS NOT NULL THEN 'High'
    END AS regime
FROM r, cuts;

SELECT
    fpl_bin,
    count(*) AS n,
    count(DISTINCT household_code) AS hh
FROM mods
GROUP BY 1
ORDER BY 1;
SELECT
    regime,
    count(DISTINCT yq) AS quarters,
    count(*) AS n
FROM mods
GROUP BY 1
ORDER BY 1;

-- 4-5. Ladder position, write ------------------------------------------------
COPY (
    SELECT
        d.* REPLACE (
            d.projection_factor::DOUBLE AS projection_factor,
            d.projection_factor_magnet::DOUBLE AS projection_factor_magnet
        ),
        i.z,
        i.n_sq,
        i.z_div,
        s0.zb,
        s1.zb AS zb_lead,
        s2.zb AS zb_lag,
        d.household_size_b AS hhsize_b,
        m.fpl_bin,
        m.fpl_rank,
        m.burden,
        m.burden_q,
        m.noemp,
        m.age65,
        m.age_bin,
        m.regime,
        h.* EXCLUDE (household_code, yq, state)
        REPLACE (
            h.n_e_items::DOUBLE AS n_e_items,
            h.n_mod_both::DOUBLE AS n_mod_both
        )
    FROM d
    LEFT JOIN inst AS i USING (household_code, yq)
    LEFT JOIN mods AS m USING (household_code, yq)
    LEFT JOIN sq AS s0 ON s0.state = d.state AND s0.yq = d.yq
    LEFT JOIN sq AS s1 ON s1.state = d.state AND s1.yq = d.yq + 10
    LEFT JOIN sq AS s2 ON s2.state = d.state AND s2.yq = d.yq - 10
    LEFT JOIN read_parquet('${WP2_INT}/ladder_hh/*.parquet') AS h
        ON h.household_code = d.household_code AND h.yq = d.yq
) TO '${WP2_FINAL}/f1_analysis' (
    FORMAT parquet, COMPRESSION zstd,
    PARTITION_BY (year), OVERWRITE
);

-- gate rows and households: config/expected_n.tsv; 49 states
SELECT
    count(*) AS n,
    sum(est_gate::INT) AS gate,
    count(DISTINCT household_code) FILTER (est_gate) AS hh,
    count(DISTINCT state) FILTER (est_gate) AS st,
    count(*) - count(DISTINCT(household_code, yq)) AS dup_keys,
    count(*) FILTER (est_gate AND NOT isfinite(z)) AS bad_z,
    count(*) FILTER (infl_exposure IS NULL OR state IS NULL) AS na_key,
    count(*) FILTER (est_gate AND b_spend IS NOT NULL) AS ladder_gate,
    CASE
        WHEN
            count(*) <> (SELECT count(*) FROM d)
            OR sum(est_gate::INT)
            <> (
                SELECT coalesce(max(n), -1)
                FROM
                    read_csv(
                        '${PROJECT_ROOT}/config/expected_n.tsv',
                        delim = '\t',
                        header = true
                    )
                WHERE end_year = ${WP2_END_YEAR} AND key = 'est_gate'
            )
            OR count(DISTINCT household_code) FILTER (est_gate)
            <> (
                SELECT coalesce(max(n), -1)
                FROM
                    read_csv(
                        '${PROJECT_ROOT}/config/expected_n.tsv',
                        delim = '\t',
                        header = true
                    )
                WHERE end_year = ${WP2_END_YEAR} AND key = 'est_gate_hh'
            )
            OR count(DISTINCT state) FILTER (est_gate) <> 49
            OR count(*) <> count(DISTINCT(household_code, yq))
            OR count(*) FILTER (est_gate AND NOT isfinite(z)) > 0
            OR count(*) FILTER (infl_exposure IS NULL OR state IS NULL) > 0
            THEN error('f1_analysis: validation failed')
    END AS chk
FROM read_parquet(
    '${WP2_FINAL}/f1_analysis/*/*.parquet',
    hive_partitioning = true
);
