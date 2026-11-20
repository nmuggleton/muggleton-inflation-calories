#!/usr/bin/env bash
# Where the gap between a household's own prices and state prices of the same
# products sits: by the channel of the retailer, by whether another household in
# the state bought the product at the same retailer, and by whether the
# household changed store or retailer (see 06b). Step 1 gives each retailer its
# channel (06a); step 2 builds household-quarter sums by group per target year
# (06b).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../../lib/paths.sh"
LOG="$WP2_LOGS/price_gap_sources.log"
OUT="$WP2_INT/price_gap_sources"
mkdir -p "$OUT/groups" "$OUT/switch"

if [ ! -f "$OUT/retailer_channel.parquet" ]; then
  echo "[$(date '+%F %T')] retailer channels" | tee -a "$LOG"
  run_sql "$HERE/06a_retailer_channel.sql" "$LOG" >/dev/null
fi

for Y in ${WP2_YEARS:-$(seq 2005 "$WP2_END_YEAR")}; do
  [ "$Y" -eq 2020 ] && continue # outside the estimation sample
  [ -f "$OUT/switch/year=$Y.parquet" ] && continue
  PU_FILES="" PC_FILES=""
  for y in $((Y - 1)) "$Y" $((Y + 1)); do
    f="$WP2_FINAL/purchases/panel_year=$y/data.parquet"
    [ -f "$f" ] && PU_FILES="$PU_FILES${PU_FILES:+, }'$f'"
    f="$WP2_INT/purchase_cells/panel_year=$y.parquet"
    [ -f "$f" ] && PC_FILES="$PC_FILES${PC_FILES:+, }'$f'"
  done
  export YEAR="$Y" PU_FILES PC_FILES
  echo "[$(date '+%F %T')] price gap sources $Y" | tee -a "$LOG"
  run_sql "$HERE/06b_price_gap_sources.sql" "$LOG" >/dev/null
done
echo "[$(date '+%F %T')] done" | tee -a "$LOG"
