#!/usr/bin/env bash
# The household's own price index under alternative rules for extreme price
# changes, the pairs beyond each bound, and coupon use, for each target year
# (see 05a). Output:
# intermediate/price_own_rules/{index,tails,coupons}/year=YYYY.parquet Run after
# 05_household_quarter/01_stream.sh.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../../lib/paths.sh"
LOG="$WP2_LOGS/own_price_rules.log"
OUT="$WP2_INT/price_own_rules"
mkdir -p "$OUT/index" "$OUT/tails" "$OUT/coupons"

for Y in ${WP2_YEARS:-$(seq 2005 "$WP2_END_YEAR")}; do
  [ "$Y" -eq 2020 ] && continue # outside the estimation sample
  [ -f "$OUT/coupons/year=$Y.parquet" ] && continue
  HU_FILES=""
  for y in $((Y - 1)) "$Y" $((Y + 1)); do
    f="$WP2_INT/hh_upc_q/panel_year=$y.parquet"
    [ -f "$f" ] && HU_FILES="$HU_FILES${HU_FILES:+, }'$f'"
  done
  export YEAR="$Y" HU_FILES
  echo "[$(date '+%F %T')] own-price rules $Y" | tee -a "$LOG"
  run_sql "$HERE/05a_own_price_rules.sql" "$LOG" >/dev/null
done
echo "[$(date '+%F %T')] done" | tee -a "$LOG"
