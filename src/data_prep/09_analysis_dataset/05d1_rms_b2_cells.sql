-- One Retail Scanner movement file of the 2021-on layout to store x UPC x
-- quarter cells of human-food modules. Run per file by
-- src/data_prep/09_analysis_dataset/05d_rms_shelf_b2.sh.
CREATE TEMP TABLE food AS
SELECT column0::INTEGER AS module
FROM read_csv('${FOOD}', header = false, columns = { 'column0': 'VARCHAR' });

CREATE TEMP TABLE um AS SELECT
    upc,
    module
FROM read_parquet('${UPC_MOD}');

-- One pass over the TSV, aggregated before any join (as in 05a)
CREATE TEMP TABLE mu AS
WITH m AS (
    SELECT
        TRY_CAST(store_code_uc AS INTEGER) AS store_code_uc,
        store_code_uc IS NULL AS no_store,   -- empty in raw
        upc,
        coalesce(
            TRY_STRPTIME(week_end, '%Y-%m-%d'),
            TRY_STRPTIME(week_end, '%Y%m%d')
        )::DATE AS week_end,
        TRY_CAST(units AS DOUBLE) AS units,
        TRY_CAST(prmult AS DOUBLE) AS prmult,
        TRY_CAST(price AS DOUBLE) AS price
    FROM
        read_csv(
            '${MV_FILE}', delim = '\t', header = true,
            all_varchar = true
        )
    -- This pass's share of the UPCs (05d_rms_shelf_b2.sh: NPASS passes)
    WHERE hash(upc) % ${NPASS} = ${PASS}
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

-- A failed cast lands in a NULL store or week, and every week must end in
-- ${YEAR}, or a quarter would be split across years.
SELECT
    count(*) AS n_cells,
    sum(n_rows) AS n_rows,
    sum(spend) / 1e6 AS rev_m,
    sum(spend) FILTER (um.upc IS NULL) / sum(spend) AS sh_rev_no_module,
    sum(spend) FILTER (um.module IN (SELECT module FROM food)) / sum(spend)
        AS sh_rev_food,
    min(wk_min) AS wk_min,
    max(wk_max) AS wk_max,
    CASE
        WHEN
            count(
                *) FILTER ((store_code_uc IS NULL AND NOT no_store)
            OR yq IS NULL
            ) > 0
            OR year(min(wk_min)) <> ${YEAR} OR year(max(wk_max)) <> ${YEAR}
            THEN
                error(
                    'rms ${YEAR} ${FILE_ID} pass ${PASS}: '
                    || 'bad store/week or week outside year'
                )
    END AS chk
FROM mu LEFT JOIN um USING (upc);

COPY (
    SELECT
        um.module,
        mu.*
    FROM mu JOIN um USING (upc)
    WHERE um.module IN (SELECT module FROM food)
) TO '${OUT_DIR}' (FORMAT parquet, COMPRESSION zstd, PARTITION_BY (module));
