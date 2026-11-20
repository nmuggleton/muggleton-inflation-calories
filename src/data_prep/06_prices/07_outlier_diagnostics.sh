#!/usr/bin/env bash
# Diagnostics for the rule that screens extreme price changes out of the
# household's own price index: pairs and base spending by band of the price
# relative, exposure with other screens on the state price relative, and counts
# over the pairs outside [1/2, 2] (see 07a), for each target year. Output:
# intermediate/outlier_diagnostics/{bands,exposure,pairs}/ year=YYYY.parquet Run
# after 05_household_quarter/01_stream.sh.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../../lib/paths.sh"
LOG="$WP2_LOGS/outlier_diagnostics.log"
OUT="$WP2_INT/outlier_diagnostics"
mkdir -p "$OUT/bands" "$OUT/exposure" "$OUT/pairs"

for Y in ${WP2_YEARS:-$(seq 2005 "$WP2_END_YEAR")}; do
  [ "$Y" -eq 2020 ] && continue # outside the estimation sample
  [ -f "$OUT/pairs/year=$Y.parquet" ] && continue
  HU_FILES="" SU_FILES=""
  for y in $((Y - 1)) "$Y" $((Y + 1)); do
    f="$WP2_INT/hh_upc_q/panel_year=$y.parquet"
    [ -f "$f" ] && HU_FILES="$HU_FILES${HU_FILES:+, }'$f'"
    f="$WP2_INT/state_upc_q/panel_year=$y.parquet"
    [ -f "$f" ] && SU_FILES="$SU_FILES${SU_FILES:+, }'$f'"
  done
  export YEAR="$Y" HU_FILES SU_FILES
  echo "[$(date '+%F %T')] outlier diagnostics $Y" | tee -a "$LOG"
  run_sql "$HERE/07a_outlier_diagnostics.sql" "$LOG" >/dev/null
done
echo "[$(date '+%F %T')] done" | tee -a "$LOG"
