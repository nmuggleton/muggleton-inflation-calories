-- Retail Scanner calorie-price ladders for one RMS year x module, at three
-- reaches: each store, all stores in a county, all stores in a state. Run per
-- year x module file by src/data_prep/09_analysis_dataset/05_rms_shelf.sh (a
-- file at a time keeps the store x item aggregate small; ladders are per
-- module, so modules are independent).
--
-- Definitions (follow 01b_ladder.sql so the rungs differ only in reach)
--   cell         store x quarter x item: revenue = sum units x price /
--                prmult over the quarter's weeks, kcal = units x kcal per
--                package. Quarter of the week-ending Saturday (each RMS
--                year file holds only weeks ending in that year, checked
--                below, so no quarter is split across files).
--   item, module the label UPC and its module from ladder_upc_kcal; an
--                item spanning modules takes the lowest (as in 01b).
--   trim         cells outside the national module x quarter 1st-99th
--                percentile of $/kcal are dropped (as in 01b).
--   ladder       within reach key x quarter x module, CELLS sorted by
--                $/kcal and weighted by kcal sold. A county or state
--                ladder pools every store's cells, so the same item sold
--                cheaper in another store is a separate, cheaper rung.
--                (01b pools an item's price over households instead.)
--   reach        store (key store_code_uc), county (key state x 1000 +
--                county FIPS of the store), state (key state FIPS).
--   valid        >= 5 distinct items, as in 01b.
--   p25, p50     calorie-weighted quantiles of $/kcal; cheap_dpk the mean
--                $/kcal of the cheapest 25% of calories.
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

CREATE TEMP TABLE ver AS
SELECT
    upc,
    any_value(upc_ver_uc) AS upc_ver_uc,
    count(*) AS n
FROM read_parquet('${RMS_VER}')
GROUP BY upc;

SELECT
    CASE WHEN max(n) > 1 THEN error('rms_versions ${YEAR}: upc not unique')
    END AS chk_ver
FROM ver;

CREATE TEMP TABLE sto AS
SELECT
    store_code_uc,
    fips_state_code::INTEGER AS state,
    fips_state_code::INTEGER * 1000 + fips_county_code::INTEGER AS county
FROM read_parquet('${RMS_STO}');

-- One pass over the TSVs to store x UPC x quarter, BEFORE any join.
CREATE TEMP TABLE mu AS
WITH m AS (
    SELECT
        TRY_CAST(store_code_uc AS INTEGER) AS store_code_uc,
        store_code_uc IS NULL AS no_store,   -- empty in raw
        upc,
        TRY_STRPTIME(week_end, '%Y%m%d')::DATE AS week_end,
        TRY_CAST(units AS DOUBLE) AS units,
        TRY_CAST(prmult AS DOUBLE) AS prmult,
        TRY_CAST(price AS DOUBLE) AS price
    FROM read_csv(
        '${MV_FILE}', delim = '\t', header = true,
        all_varchar = true
    )
)

SELECT
    store_code_uc,
    no_store,
    upc,
    year(week_end) * 10 + quarter(week_end) AS yq,
    sum(units * price / prmult) AS spend,
    sum(units) AS units,
    count(*) AS n_rows,
    min(week_end) AS wk_min,
    max(week_end) AS wk_max
FROM m
WHERE units > 0 AND price > 0 AND prmult > 0
GROUP BY ALL;
