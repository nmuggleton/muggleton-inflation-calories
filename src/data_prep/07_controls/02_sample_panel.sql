-- Household x quarter sample panel with the estimation-sample flag (est_gate),
-- written to ${WP2_INT}/desc_panel/ (one Parquet file per year). The
-- calorie-price ladder (09_analysis_dataset/01_ladder.sh) and the analysis
-- dataset (09_analysis_dataset/02_build.sh) take their sample from it.
--
-- Sample indicators
--   core_q    f_core AND complete quarter (days observed >= 75%)
--   est_gate  estimation sample: 2005-${WP2_END_YEAR} excl. 2020, base quarter
--             observed, core and complete in t and t-4, valid T2 with
--             known state, labelled share >= .5 and positive kcal per $ in
--             t and t-4
CREATE VIEW analysis_hh_quarter AS
SELECT * FROM read_parquet(
    '${WP2_FINAL}/analysis_hh_quarter/*/*.parquet',
    hive_partitioning = true
);

COPY (
    WITH a AS (
        SELECT
            household_code,
            yq,
            year,
            qtr,
            panel_year,
            fips_state_code AS state,
            region_code,
            scantrack_market_code,
            projection_factor,
            projection_factor_magnet,
            f_core,
            f_partial_q,
            f_exp_valid,
            f_idx_valid,
            f_magnet_hh,
            f_store_linkable,
            f_ae_imputed,
            f_income_topcoded,
            in_next_year,
            in_prev_year,
            bal_2007_2012,
            n_years_in_panel,
            first_panel_year,
            cov_lab,
            cov_lab_pl,
            cov_fb,
            income_cat,
            household_income,
            income_real2019,
            fpl_ratio,
            snap_elig_proxy,
            head_age,
            head_educ,
            head_college,
            race_f,
            hispanic,
            children_present,
            n_children_u14,
            household_size,
            ae_oecd,
            any_head_employed,
            n_heads,
            marital_status,
            kcal_per_usd_real,
            kcal_100g_food,
            kcal_100ml_drink_w,
            kcal_lab_ae_day,
            kcal_fb_ae_day,
            kcal_scaled_ae_day,
            spend_food_nomag_ae_day_real,
            spend_food_ae_day,
            health_score,
            z_fiber,
            z_protein,
            z_fv,
            z_sugar,
            z_satfat,
            z_sodium,
            sugar_per_1000_w,
            satfat_per_1000,
            sodium_per_1000_w,
            fiber_per_1000,
            protein_per_1000,
            sh_fv_nomag,
            sh_fg_ssb,
            sh_fg_sweets,
            sh_fg_salty_snacks,
            sh_fg_prepared,
            sh_fg_protein,
            sh_fg_dairy,
            sh_fg_grains,
            sh_fg_juice100,
            sh_fg_diet_water,
            sh_kcal_snacks_sweets,
            sh_kcal_sweet_drinks,
            sh_kcal_fv,
            sh_pl,
            sh_deal,
            sh_coupon,
            sh_ch_grocery,
            sh_ch_discount,
            sh_ch_club,
            sh_ch_dollar,
            sh_ch_convenience,
            sh_ch_online,
            sh_takeout,
            n_trips_food,
            n_lines_food,
            days_obs,
            infl_laspeyres,
            infl_fisher,
            infl_exposure,
            shop_component,
            gas_exposure,
            gas_share_base,
            cpi_fah_yoy,
            cpi_gas_yoy,
            episode,
            n_base_items,
            n_base_priced,
            n_matched
        FROM analysis_hh_quarter
    )

    SELECT
        a.*,
        b.household_code IS NOT NULL AS has_base,
        coalesce(a.f_core, false)
        AND NOT coalesce(a.f_partial_q, false) AS core_q,
        (
            a.year BETWEEN 2005 AND ${WP2_END_YEAR} AND a.year <> 2020
            AND b.household_code IS NOT NULL
            AND coalesce(a.f_core, false) AND coalesce(b.f_core, false)
            AND NOT coalesce(a.f_partial_q, false)
            AND NOT coalesce(b.f_partial_q, false)
            AND coalesce(a.f_exp_valid, false) AND a.state IS NOT NULL
            -- Shares are rounded before the cut-off: floating-point noise in
            -- the parallel sums would otherwise put exact 0.5 shares on either
            -- side
            AND coalesce(
                round(a.cov_lab, 9) >= 0.5 AND round(b.cov_lab, 9) >= 0.5
                AND a.kcal_per_usd_real > 0 AND b.kcal_per_usd_real > 0,
                false
            )
        ) AS est_gate,
        -- Year-on-year changes: 100 x the log change for ratios (NULL unless
        -- both quarters are positive), the level change otherwise
        100
        * (
            (CASE WHEN a.kcal_per_usd_real > 0 THEN ln(a.kcal_per_usd_real) END)
            - (
                CASE
                    WHEN b.kcal_per_usd_real > 0 THEN ln(b.kcal_per_usd_real)
                END
            )
        ) AS d_lkpu,
        100 * (
            (
                CASE
                    WHEN
                        a.spend_food_nomag_ae_day_real > 0
                        THEN ln(a.spend_food_nomag_ae_day_real)
                END
            )
            - (
                CASE
                    WHEN
                        b.spend_food_nomag_ae_day_real > 0
                        THEN ln(b.spend_food_nomag_ae_day_real)
                END
            )
        ) AS d_lspend,
        100
        * (
            (CASE WHEN a.kcal_lab_ae_day > 0 THEN ln(a.kcal_lab_ae_day) END)
            - (CASE WHEN b.kcal_lab_ae_day > 0 THEN ln(b.kcal_lab_ae_day) END)
        ) AS d_lkcal,
        a.kcal_100g_food - b.kcal_100g_food AS d_ed,
        a.health_score - b.health_score AS d_hs,
        a.sh_fv_nomag - b.sh_fv_nomag AS d_fv,
        a.sh_fg_ssb - b.sh_fg_ssb AS d_ssb,
        a.sh_kcal_snacks_sweets - b.sh_kcal_snacks_sweets AS d_snk,
        a.sh_pl - b.sh_pl AS d_pl,
        a.sh_deal - b.sh_deal AS d_deal,
        a.sh_ch_discount - b.sh_ch_discount AS d_disc,
        a.sh_ch_dollar - b.sh_ch_dollar AS d_dollar,
        a.cov_lab - b.cov_lab AS d_cov
    FROM a LEFT JOIN a AS b
        ON b.household_code = a.household_code AND b.yq = a.yq - 10
) TO '${WP2_INT}/desc_panel' (
    FORMAT parquet, COMPRESSION zstd,
    PARTITION_BY (year), OVERWRITE
);

-- rows = analysis_hh_quarter, est_gate = estimation sample; expected
-- counts for this end year from config/expected_n.tsv
SELECT
    count(*) AS n,
    count(DISTINCT household_code) AS hh,
    sum(core_q::INT) AS core,
    sum(est_gate::INT) AS gate,
    CASE
        WHEN
            count(*)
            <> (
                SELECT coalesce(max(n), -1)
                FROM
                    read_csv(
                        '${PROJECT_ROOT}/config/expected_n.tsv',
                        delim = '\t',
                        header = true
                    )
                WHERE end_year = ${WP2_END_YEAR} AND key = 'hh_quarter'
            )
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
            THEN error('desc_panel: unexpected row or est_gate count')
    END AS chk
FROM
    read_parquet('${WP2_INT}/desc_panel/*/*.parquet', hive_partitioning = true);
