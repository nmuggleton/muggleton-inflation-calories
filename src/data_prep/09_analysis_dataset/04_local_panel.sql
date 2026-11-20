-- The external local context (08_external/03_context.py) merged onto the
-- estimation sample (f1_analysis WHERE est_gate): one compact household-quarter
-- extract for the local-demand, BLS-CPI and other-prices estimates. County
-- comes from household_year (fips_state_code, fips_county_code) on panel_year,
-- as for all demographics; context variables are matched on the purchase
-- quarter yq.
COPY (
    WITH p AS (
        SELECT
            household_code,
            yq,
            year,
            panel_year,
            state,
            region_code,
            income_cat,
            CAST(projection_factor AS DOUBLE) AS projection_factor,
            n_base_items,
            infl_exposure,
            d_lkpu,
            d_lspend,
            d_lkcal,
            d_ed,
            d_hs
        FROM
            read_parquet(
                '${WP2_FINAL}/f1_analysis/*/*.parquet',
                hive_partitioning = true
            )
        WHERE est_gate
    ),

    h AS (
        SELECT
            household_code,
            panel_year,
            CAST(fips_state_code AS INTEGER) * 1000
            + CAST(fips_county_code AS INTEGER) AS fips
        FROM '${WP2_FINAL}/household_year.parquet'
    )

    SELECT
        p.*,
        h.fips,
        cc.area AS cpi_area,
        cc.area_type,
        s.d_ur,
        s.d_pinc,
        s.d_hpi,
        s.dl_ur,
        s.dl_pinc,
        s.dl_hpi,
        c.d_ur_c,
        c.d_emp_c,
        c.dl_ur_c,
        c.dl_emp_c,
        cp.d_cpi,
        cp.dl_cpi
    FROM p
    LEFT JOIN h USING (household_code, panel_year)
    LEFT JOIN '${LOCAL_DATA}/local_context/state_q.parquet' AS s
        ON s.state = p.state AND s.yq = p.yq
    LEFT JOIN '${LOCAL_DATA}/local_context/county_q.parquet' AS c
        ON c.fips = h.fips AND c.yq = p.yq
    LEFT JOIN '${LOCAL_DATA}/local_context/county_cpi.parquet' AS cc
        ON cc.fips = h.fips
    LEFT JOIN '${LOCAL_DATA}/local_context/cpi_q.parquet' AS cp
        ON cp.area = cc.area AND cp.yq = p.yq
) TO '${LOCAL_DATA}/local_context/local_panel.parquet'
(FORMAT parquet, COMPRESSION zstd);

-- the estimation sample (2,783,093 rows); the joins must not fan out; state
-- context covers all 49 states and 2003-2021, so it must be full; county merges
-- (Nielsen FIPS vs LAUS/OMB county lists) must reach 99%
SELECT
    count(*) AS n,
    count(DISTINCT household_code) AS hh,
    avg((fips IS NOT NULL)::INT) AS m_fips,
    avg((fips // 1000 = state)::INT) AS m_state_agree,
    avg((d_ur IS NOT NULL AND dl_ur IS NOT NULL)::INT) AS m_state_ctx,
    avg((d_ur_c IS NOT NULL)::INT) AS m_county_ctx,
    avg((dl_ur_c IS NOT NULL)::INT) AS m_county_lag,
    avg((d_cpi IS NOT NULL)::INT) AS m_cpi,
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
                WHERE end_year = ${WP2_END_YEAR} AND key = 'est_gate'
            )
            OR count(*) <> count(DISTINCT(household_code, yq))
            OR avg((d_ur IS NOT NULL AND dl_ur IS NOT NULL)::INT) < 1
            OR avg((fips IS NOT NULL)::INT) <= 0.99
            OR avg((d_ur_c IS NOT NULL)::INT) <= 0.99
            OR avg((d_cpi IS NOT NULL)::INT) <= 0.99
            THEN error('local_panel: validation failed')
    END AS chk
FROM '${LOCAL_DATA}/local_context/local_panel.parquet';
