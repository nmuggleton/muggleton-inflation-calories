-- The inputs of the 05a ladder core for one module of an RMS year from 2021 on,
-- from the cells 05d1 wrote from every movement file. Run with
-- 05a_rms_shelf_ladder_core.sql appended, by
-- src/data_prep/09_analysis_dataset/05d_rms_shelf_b2.sh.
CREATE TEMP TABLE uk AS
SELECT
    upc,
    upc_ver_uc,
    item,
    kcal_unit
FROM read_parquet('${UK}');

CREATE TEMP TABLE im AS
SELECT
    item,
    min(module) AS module
FROM read_parquet('${UK}')
GROUP BY item;

CREATE TEMP TABLE sto AS
SELECT
    store_code_uc,
    fips_state_code::INTEGER AS state,
    fips_state_code::INTEGER * 1000 + fips_county_code::INTEGER AS county
FROM read_parquet('${RMS_STO}');

-- A UPC sits in one super-category file, so this sum adds nothing within a
-- file; it only joins the files
CREATE TEMP TABLE mu AS
SELECT
    store_code_uc,
    no_store,
    upc,
    yq,
    sum(spend) AS spend,
    sum(units) AS units,
    sum(n_rows) AS n_rows,
    min(wk_min) AS wk_min,
    max(wk_max) AS wk_max
FROM read_parquet('${CELLS}', hive_partitioning = false)
GROUP BY ALL;

-- RMS has no versions file from 2021, so products are version 0, the version
-- that 2021-on panel purchases carry (coalesce(upc_ver_uc, 0) in 05b)
CREATE TEMP TABLE ver AS
SELECT DISTINCT
    upc,
    0::SMALLINT AS upc_ver_uc
FROM mu;
