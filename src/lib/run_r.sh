#!/usr/bin/env bash
# run_r.sh -- run one R script under a shared two-slot queue, so that parallel
# jobs never have more than two DuckDB-heavy scripts running at once. Logs to
# logs/<stem>.log and appends the exit status.
#
# usage:  src/lib/run_r.sh src/analysis/estimate/01_main.R
#           (log: logs/01_main.log)
#         src/lib/run_r.sh src/analysis/estimate/01_main.R main_b
#           (custom log stem: logs/main_b.log)
# env:    WP2_DATA, DUCKDB_TMP (defaults from src/lib/paths.sh);
#         RUN_SLOTS (default 2); DUCKDB_MEMORY per process (default 12GB);
#         DUCKDB_THREADS per process (default 4)
set -uo pipefail
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export DUCKDB_MEMORY="${DUCKDB_MEMORY:-12GB}"
export DUCKDB_THREADS="${DUCKDB_THREADS:-4}"
# shellcheck disable=SC1091
source "$PROJECT_ROOT/src/lib/paths.sh"
script="${1:?usage: src/lib/run_r.sh <script>.R [log_stem]}"
stem="${2:-$(basename "$script" .R)}"
log="$PROJECT_ROOT/logs/${stem}.log"
slots="${RUN_SLOTS:-2}"
qdir="$DUCKDB_TMP/run_slots"
mkdir -p "$qdir"
stale_s=21600 # a slot older than 6 h is treated as abandoned

slot=""
while [[ -z "$slot" ]]; do
  for i in $(seq 1 "$slots"); do
    d="$qdir/slot_$i"
    if mkdir "$d" 2>/dev/null; then
      slot="$d"
      break
    fi
    # Modification time: GNU stat first, then BSD (macOS) stat
    mtime=$(stat -c %Y "$d" 2>/dev/null || stat -f %m "$d" 2>/dev/null \
      || echo 0)
    age=$(($(date +%s) - mtime))
    if ((age > stale_s)); then rm -rf "$d"; fi
  done
  [[ -z "$slot" ]] && sleep 10
done
echo "$stem $(date '+%F %T')" >"$slot/owner"
trap 'rm -rf "$slot"' EXIT

cd "$PROJECT_ROOT" || exit 1
start=$(date +%s)
Rscript "$script" >"$log" 2>&1
ec=$?
elapsed=$(($(date +%s) - start))
echo "run_r.sh: exit $ec | ${elapsed}s | slot $(basename "$slot")" >>"$log"
echo "$stem exit $ec"
exit $ec
