#!/usr/bin/env bash
# Run src/data_prep/05_household_quarter/01a_hh_quarter.sql for every panel
# year. Outputs intermediate/{hhq,hh_upc_q,state_upc_q}/panel_year=YYYY.parquet.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../../lib/paths.sh"
# The household x quarter aggregation carries about 70 sums per group; each
# thread needs about 8 GB of DUCKDB_MEMORY to hold its groups, or it spills tens
# of GB (on a 32 GB Mac, 2011: 4 threads spilled more than 20 GiB, 2 threads
# none)
_mem=${DUCKDB_MEMORY%GB}
_thr=$((_mem / 8))
[ "$_thr" -lt 1 ] && _thr=1
[ "$_thr" -lt "$DUCKDB_THREADS" ] && export DUCKDB_THREADS=$_thr
unset _mem _thr
LOG="$WP2_LOGS/05_stream.log"
say() { echo "[$(date '+%F %T')] $*" | tee -a "$LOG"; }
YEARS="${WP2_YEARS:-$(seq 2004 "$WP2_END_YEAR")}"
mkdir -p "$WP2_INT"/{hhq,hh_upc_q,state_upc_q} "$WP2_INT/_done"
for Y in $YEARS; do
  if [ -f "$WP2_INT/_done/05_$Y" ] && [ "${FORCE:-0}" != 1 ]; then
    say "year $Y done"
    continue
  fi
  say "=== 05 $Y ==="
  export YEAR="$Y"
  run_sql \
    "$PROJECT_ROOT/src/data_prep/05_household_quarter/01a_hh_quarter.sql" \
    "$LOG" >/dev/null
  touch "$WP2_INT/_done/05_$Y"
  say "year $Y ok"
done
say "05 done"
