#!/usr/bin/env bash
# Build final/purchases/panel_year=YYYY (purchase-line merge of purchases,
# trips, panelists, products, retailers) and the household-year aggregates.
# Requires staging (02) and products (01_products.sh).
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
LOG="$WP2_LOGS/desc_03_merge.log"
mkdir -p "$WP2_FINAL/purchases" "$WP2_FINAL/_agg/hh_year"

if [ "$#" -gt 0 ]; then YEARS="$*"; else
  YEARS=$(for d in "$WP2_INT"/panel/purchases/panel_year=*; do
    if [ -d "$d" ]; then echo "${d##*panel_year=}"; fi
  done | sort -n | tr '\n' ' ')
fi
echo "[$(date '+%F %T')] years: $YEARS" | tee -a "$LOG"

for Y in $YEARS; do
  export YEAR="$Y"
  out="$WP2_FINAL/purchases/panel_year=$Y/data.parquet"
  mkdir -p "$(dirname "$out")"
  if [ "$Y" -le 2020 ]; then
    SQL="$PROJECT_ROOT/src/data_prep/03_homescan/02a_merge_b1.sql"
  else SQL="$PROJECT_ROOT/src/data_prep/03_homescan/02b_merge_b2.sql"; fi
  echo "[$(date '+%F %T')] merge $Y ($(basename "$SQL"))" | tee -a "$LOG"
  run_sql "$SQL" "$LOG"
  mv -f "$out.tmp" "$out"
  run_sql "$PROJECT_ROOT/src/data_prep/03_homescan/02c_merge_hh_agg.sql" "$LOG"
done

echo "[$(date '+%F %T')] done" | tee -a "$LOG"
