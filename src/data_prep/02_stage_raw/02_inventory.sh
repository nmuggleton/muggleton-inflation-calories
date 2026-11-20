#!/usr/bin/env bash
# List every file under NIELSEN_RAW (and RMS_RAW, if it is another tree) and
# classify it. Outputs (in the data tree: they describe this machine's raw copy)
# $WP2_INT/raw_inventory.tsv   every file: relpath, bytes, extension
# $WP2_INT/raw_inventory_rms.tsv  the same for RMS_RAW (only if separate)
# $WP2_INT/raw_manifest.tsv    files mapped to (source, table, year); rms rows
# come from RMS_RAW Usage: bash src/data_prep/02_stage_raw/02_inventory.sh
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
LOG="$WP2_LOGS/desc_00_inventory.log"
: >"$LOG"
echo "[$(date '+%F %T')] NIELSEN_RAW=$NIELSEN_RAW RMS_RAW=$RMS_RAW" \
  | tee -a "$LOG"

for d in "$NIELSEN_RAW" "$RMS_RAW"; do
  if [ ! -d "$d" ]; then
    echo "ERROR: raw tree not reachable: $d" | tee -a "$LOG"
    exit 2
  fi
done

mkdir -p "$WP2_INT"
# stat -f is BSD/macOS; fall back to GNU stat on Linux or the HPC.
if stat -f '%z' "$0" >/dev/null 2>&1; then STATFMT=(-f '%z'); else
  STATFMT=(-c '%s')
fi
# usage: inventory <root> <out.tsv>
inventory() {
  printf 'relpath\tbytes\text\n' >"$2"
  find "$1" -type f ! -name '.DS_Store' ! -name '~$*' -print0 \
    | while IFS= read -r -d '' f; do
      rel="${f#"$1"/}"
      sz=$(stat "${STATFMT[@]}" "$f")
      ext="${f##*.}"
      printf '%s\t%s\t%s\n' "$rel" "$sz" "$ext"
    done >>"$2"
  echo "files under $1: $(($(wc -l <"$2") - 1))" | tee -a "$LOG"
}
INV="$WP2_INT/raw_inventory.tsv"
inventory "$NIELSEN_RAW" "$INV"
INV_RMS=""
if [ "$RMS_RAW" != "$NIELSEN_RAW" ]; then
  INV_RMS="$WP2_INT/raw_inventory_rms.tsv"
  inventory "$RMS_RAW" "$INV_RMS"
fi

python "$PROJECT_ROOT/src/data_prep/02_stage_raw/02a_classify.py" "$INV" \
  "$WP2_INT/raw_manifest.tsv" $INV_RMS 2>&1 | tee -a "$LOG"
echo "[$(date '+%F %T')] done" | tee -a "$LOG"
