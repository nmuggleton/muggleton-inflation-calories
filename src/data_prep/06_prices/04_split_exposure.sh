#!/usr/bin/env bash
# Exposure inflation priced from a random half of households (robustness for the
# instrument: is the result driven by price noise shared by exposure and the
# instrument? see 04a), for each target year and each of WP2_SPLITS random
# splits (default 10). Output:
# intermediate/price_split/split=K/year=YYYY.parquet Run after
# 05_household_quarter/01_stream.sh.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../../lib/paths.sh"
LOG="$WP2_LOGS/split_exposure.log"

lst() { # lst <table> <Y> -> quoted comma list of existing files
  local out=""
  for y in $(($2 - 1)) "$2" $(($2 + 1)); do
    f="$WP2_INT/$1/panel_year=$y.parquet"
    [ -f "$f" ] && out="$out${out:+, }'$f'"
  done
  echo "$out"
}
for K in $(seq 1 "${WP2_SPLITS:-10}"); do
  mkdir -p "$WP2_INT/price_split/split=$K"
  for Y in ${WP2_YEARS:-$(seq 2005 "$WP2_END_YEAR")}; do
    [ "$Y" -eq 2020 ] && continue # outside the estimation sample
    [ -f "$WP2_INT/price_split/split=$K/year=$Y.parquet" ] && continue
    HU_FILES="$(lst hh_upc_q "$Y")"
    export YEAR="$Y" SPLIT="$K" HU_FILES
    echo "[$(date '+%F %T')] split $K, $Y" | tee -a "$LOG"
    run_sql "$HERE/04a_split_exposure.sql" "$LOG" >/dev/null
  done
done
echo "[$(date '+%F %T')] done" | tee -a "$LOG"
