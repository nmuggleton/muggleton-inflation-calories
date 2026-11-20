#!/usr/bin/env bash
# Household food price indices (01a_price_indices.sql) for each target calendar
# year, reading the hh_upc_q and state_upc_q files of panel years Y-1, Y, Y+1
# that exist locally (run after
# src/data_prep/05_household_quarter/01_stream.sh). Output:
# intermediate/price_idx/year=YYYY.parquet.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../../lib/paths.sh"
LOG="$WP2_LOGS/desc_08_indices.log"
mkdir -p "$WP2_INT/price_idx"
YEARS="${WP2_YEARS:-$(seq 2005 "$WP2_END_YEAR")}"
lst() { # lst <table> <Y> -> quoted comma list of existing files
  local out=""
  for y in $(($2 - 1)) "$2" $(($2 + 1)); do
    f="$WP2_INT/$1/panel_year=$y.parquet"
    [ -f "$f" ] && out="$out${out:+, }'$f'"
  done
  echo "$out"
}
for Y in $YEARS; do
  HU_FILES="$(lst hh_upc_q "$Y")"
  SU_FILES="$(lst state_upc_q "$Y")"
  export YEAR="$Y" HU_FILES SU_FILES
  echo "[$(date '+%F %T')] indices $Y" | tee -a "$LOG"
  run_sql "$PROJECT_ROOT/src/data_prep/06_prices/01a_price_indices.sql" \
    "$LOG" >/dev/null
done
