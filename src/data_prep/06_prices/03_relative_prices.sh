#!/usr/bin/env bash
# A household's own prices against what other households in its state paid for
# the same products, by retailer, month and deal flag (where the avoided share
# goes; see 03b). Step 1 builds household x state x retailer x item x month x
# deal totals per panel year (03a, 2004 to WP2_END_YEAR); step 2 builds the
# indices per target year (03b).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../../lib/paths.sh"
LOG="$WP2_LOGS/relative_prices.log"
OWN_RULE="${OWN_RULE:-net3}"
case "$OWN_RULE" in
  net3) OWN_ON_GROSS=FALSE OWN_BOUND=3 REL_DIR=price_rel ;;
  gross10) OWN_ON_GROSS=TRUE OWN_BOUND=10 REL_DIR=price_rel_gross10 ;;
  *) echo "OWN_RULE must be net3 or gross10" >&2 && exit 1 ;;
esac
export OWN_RULE OWN_ON_GROSS OWN_BOUND REL_DIR
mkdir -p "$WP2_INT/purchase_cells" "$WP2_INT/$REL_DIR"

for Y in $(seq 2004 "$WP2_END_YEAR"); do
  out="$WP2_INT/purchase_cells/panel_year=$Y.parquet"
  [ -f "$out" ] && continue
  export YEAR="$Y"
  echo "[$(date '+%F %T')] purchase cells $Y" | tee -a "$LOG"
  run_sql "$HERE/03a_purchase_cells.sql" "$LOG" >/dev/null
done

for Y in ${WP2_YEARS:-$(seq 2005 "$WP2_END_YEAR")}; do
  [ "$Y" -eq 2020 ] && continue # outside the estimation sample
  PC_FILES=""
  for y in $((Y - 1)) "$Y" $((Y + 1)); do
    f="$WP2_INT/purchase_cells/panel_year=$y.parquet"
    [ -f "$f" ] && PC_FILES="$PC_FILES${PC_FILES:+, }'$f'"
  done
  export YEAR="$Y" PC_FILES
  echo "[$(date '+%F %T')] relative prices $Y ($OWN_RULE)" | tee -a "$LOG"
  run_sql "$HERE/03b_relative_prices.sql" "$LOG" >/dev/null
done
echo "[$(date '+%F %T')] done" | tee -a "$LOG"
