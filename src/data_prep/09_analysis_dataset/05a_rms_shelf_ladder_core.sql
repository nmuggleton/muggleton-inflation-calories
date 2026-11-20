-- Ladders and coverage of one RMS year x module from mu (store x UPC x
-- quarter), ver (upc -> upc_ver_uc), uk, im and sto, which the SQL run before
-- it creates: 05a_rms_shelf_ladder.sql (RMS to 2020) or 05d2_rms_b2_mu.sql
-- (2021 on). Definitions and outputs: see 05a_rms_shelf_ladder.sql.

-- Unlabelled rows are kept with item NULL, to track coverage
CREATE TEMP TABLE c0 AS
SELECT m.store_code_uc, m.no_store, m.yq, u.item,
  sum(m.spend)                                   AS spend,
  sum(m.units * u.kcal_unit)                     AS kcal,
  sum(m.n_rows)                                  AS n_rows,
  min(m.wk_min)                                  AS wk_min,
  max(m.wk_max)                                  AS wk_max
FROM mu m
LEFT JOIN ver v ON v.upc = m.upc
LEFT JOIN uk u ON u.upc = m.upc AND u.upc_ver_uc = v.upc_ver_uc
GROUP BY ALL;
DROP TABLE mu;

-- A failed cast lands in a NULL store or week, and a week outside the year
-- would split a quarter across files.
SELECT count(*) FILTER ((store_code_uc IS NULL AND NOT no_store)
                        OR yq IS NULL) AS bad_key,
  sum(n_rows) FILTER (no_store) AS n_nostore,
  min(wk_min) AS wk_min, max(wk_max) AS wk_max,
  CASE WHEN count(*) FILTER ((store_code_uc IS NULL AND NOT no_store)
                             OR yq IS NULL) > 0
         OR year(min(wk_min)) <> ${YEAR} OR year(max(wk_max)) <> ${YEAR}
       THEN error('rms ${YEAR} module ${MODULE}: bad '
           || 'store/week or week outside year')
       WHEN coalesce(sum(n_rows) FILTER (no_store), 0) > 1e-4 * sum(n_rows)
       THEN error('rms ${YEAR} module ${MODULE}: '
           || 'over 0.01% of rows with no store')
  END AS chk_wk
FROM c0;

CREATE TEMP TABLE ns AS
SELECT yq, sum(n_rows) AS n_nostore FROM c0 WHERE no_store GROUP BY yq;
DELETE FROM c0 WHERE no_store;

COPY (
  SELECT ${YEAR}::SMALLINT AS year, ${GROUP}::INTEGER AS grp,
    ${MODULE}::INTEGER AS module, yq,
    sum(spend) AS rev, sum(spend) FILTER (item IS NOT NULL) AS rev_lab,
    sum(spend) FILTER (im.module = ${MODULE}) AS rev_kept,
    sum(n_rows) AS n_rows, count(DISTINCT store_code_uc) AS n_stores,
    coalesce(any_value(ns.n_nostore), 0) AS n_nostore
  FROM c0 LEFT JOIN im USING (item) LEFT JOIN ns USING (yq) GROUP BY yq
) TO '${OUT_COV}' (FORMAT parquet);

CREATE TEMP TABLE c AS
SELECT c0.store_code_uc, c0.yq, c0.item, im.module, c0.spend, c0.kcal,
       c0.spend / c0.kcal AS dpk, s.state, s.county
FROM c0 JOIN im USING (item)
LEFT JOIN sto s USING (store_code_uc)
WHERE c0.kcal > 0 AND c0.spend > 0
  AND im.module = ${MODULE};    -- this file's module only, so no module is
                                -- Split across files: cells of an item whose
                                -- lowest module is another module are dropped,
                                -- and the share kept is logged as sh_kept

CREATE TEMP TABLE trm AS
SELECT yq, module, quantile_cont(dpk, 0.01) AS lo,
       quantile_cont(dpk, 0.99) AS hi
FROM c GROUP BY ALL;

CREATE TEMP TABLE f AS
SELECT c.* FROM c JOIN trm t USING (yq, module)
WHERE c.dpk BETWEEN t.lo AND t.hi;

CREATE TEMP TABLE r AS
SELECT 'store' AS reach, store_code_uc AS key, * EXCLUDE (state, county) FROM f
UNION ALL
SELECT 'county', county, * EXCLUDE (state, county)
FROM f
WHERE county IS NOT NULL
UNION ALL
SELECT 'state', state, * EXCLUDE (state, county) FROM f WHERE state IS NOT NULL;

COPY (
  WITH w AS (
    SELECT *,
      sum(kcal) OVER (PARTITION BY reach, key, yq, module
                      ORDER BY dpk, item, store_code_uc
                      ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
        / sum(kcal) OVER (PARTITION BY reach, key, yq, module) AS cum_hi,
      kcal / sum(kcal) OVER (PARTITION BY reach, key, yq, module) AS sh
    FROM r
  )
  SELECT reach, key::INTEGER AS key, yq, module,
    count(DISTINCT item)                           AS n_items,
    count(DISTINCT item) >= 5                      AS valid,
    count(DISTINCT store_code_uc)                  AS n_stores,
    sum(kcal)                                      AS kcal_tot,
    min(dpk) FILTER (cum_hi >= 0.25)               AS p25,
    min(dpk) FILTER (cum_hi >= 0.50)               AS p50,
    sum(spend) FILTER (cum_hi - sh < 0.25)
      / sum(kcal) FILTER (cum_hi - sh < 0.25)      AS cheap_dpk
  FROM w GROUP BY ALL
) TO '${OUT_LAD}' (FORMAT parquet, COMPRESSION zstd);
