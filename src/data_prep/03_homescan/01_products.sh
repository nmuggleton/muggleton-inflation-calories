#!/usr/bin/env bash
# Build product dimensions, module crosswalk and 2021+ nutrition table from
# staged Parquet (src/data_prep/03_homescan/01_products.sql), then run
# key-uniqueness checks. Usage: bash src/data_prep/03_homescan/01_products.sh
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
LOG="$WP2_LOGS/desc_02_products.log"
mkdir -p "$WP2_FINAL"
echo "[$(date '+%F %T')] building product dimensions" | tee -a "$LOG"
run_sql "$PROJECT_ROOT/src/data_prep/03_homescan/01_products.sql" "$LOG"

{
  duck_settings
  cat <<EOF
.mode box
SELECT 'b1 key (upc,upc_ver_uc)' AS chk, count(*) n,
       count(DISTINCT (upc, upc_ver_uc)) n_key
         FROM '$WP2_FINAL/dim_products_b1.parquet';
SELECT 'b2 key (upc,panel_year)' AS chk, count(*) n,
       count(DISTINCT (upc, panel_year)) n_key
         FROM '$WP2_FINAL/dim_products_b2.parquet';
SELECT 'nutrition key' AS chk, count(*) n,
       count(DISTINCT (upc, panel_year)) n_key
         FROM '$WP2_FINAL/nutrition_b2.parquet';
SELECT 'modules with >1 group' AS chk, count(*) n
FROM '$WP2_FINAL/xwalk_module.parquet' WHERE n_groups_check > 1;
SELECT panel_year, nutrition_status, count(*) n
  FROM '$WP2_FINAL/nutrition_b2.parquet'
GROUP BY ALL ORDER BY ALL;
SELECT panel_year, avg(m_hierarchy::INT) r_hier, avg(m_attributes::INT) r_attr,
       avg(is_private_label::INT) sh_pl
         FROM '$WP2_FINAL/dim_products_b2.parquet'
GROUP BY ALL ORDER BY ALL;
EOF
} | duckdb -bail 2>&1 | tee -a "$LOG"

# Stop on duplicate keys: a second source for a year (2022 has both panel and
# scanner files) would otherwise duplicate purchase lines in the merge
dup=$(duckdb -noheader -list -c "
  SELECT (SELECT count(*) - count(DISTINCT (upc, panel_year))
          FROM '$WP2_FINAL/dim_products_b2.parquet')
       + (SELECT count(*) - count(DISTINCT (upc, panel_year))
          FROM '$WP2_FINAL/nutrition_b2.parquet')")
if [ "$dup" != 0 ]; then
  echo "ERROR: $dup duplicate (upc, panel_year) rows in" \
    "dim_products_b2/nutrition_b2" | tee -a "$LOG"
  exit 1
fi
echo "[$(date '+%F %T')] done" | tee -a "$LOG"
