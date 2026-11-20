#!/usr/bin/env bash
# Exposure at fixed-retailer- weight state prices (robustness for the
# instrument; see 02b). Step 1 builds state x retailer x item x quarter totals
# per panel year (02a, 2004 to WP2_END_YEAR); step 2 prices each household's
# base basket at state unit values and at fixed-retailer-weight prices per
# target year (02b).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../../lib/paths.sh"
LOG="$WP2_LOGS/fixed_retailer.log"
mkdir -p "$WP2_INT/state_ret_upc_q" "$WP2_INT/price_idx_fw"

for Y in $(seq 2004 "$WP2_END_YEAR"); do
  out="$WP2_INT/state_ret_upc_q/panel_year=$Y.parquet"
  [ -f "$out" ] && continue
  export YEAR="$Y"
  echo "[$(date '+%F %T')] state x retailer $Y" | tee -a "$LOG"
  run_sql "$HERE/02a_state_retailer_upc.sql" "$LOG" >/dev/null
done

lst() { # lst <table> <Y> -> quoted comma list of existing files
  local out=""
  for y in $(($2 - 1)) "$2" $(($2 + 1)); do
    f="$WP2_INT/$1/panel_year=$y.parquet"
    [ -f "$f" ] && out="$out${out:+, }'$f'"
  done
  echo "$out"
}
for Y in ${WP2_YEARS:-$(seq 2005 "$WP2_END_YEAR")}; do
  [ "$Y" -eq 2020 ] && continue # outside the estimation sample
  HU_FILES="$(lst hh_upc_q "$Y")"
  SR_FILES="$(lst state_ret_upc_q "$Y")"
  export YEAR="$Y" HU_FILES SR_FILES
  echo "[$(date '+%F %T')] fixed-retailer exposure $Y" | tee -a "$LOG"
  run_sql "$HERE/02b_fixed_retailer_exposure.sql" "$LOG" >/dev/null
done
echo "[$(date '+%F %T')] done" | tee -a "$LOG"
