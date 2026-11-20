#!/usr/bin/env bash
# Stage the NielsenIQ Retail Scanner (RMS) annual files from RMS_RAW (default
# NIELSEN_RAW), if that tree contains them (the OneDrive pilot copy does not;
# the M-Drive copy does). Uses the manifest from
# src/data_prep/02_stage_raw/02_inventory.sh. stores / rms_versions /
# products_extra / products_master go through
# src/data_prep/02_stage_raw/03_stage_panel.py --source rms, under the same
# harmonisation rules.
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
LOG="$WP2_LOGS/desc_01_stage_rms.log"
MAN="$WP2_INT/raw_manifest.tsv"
# Let awk exit on its own: piping it to `grep -q` kills awk with SIGPIPE, which
# pipefail reports as "no RMS files"
if ! awk -F'\t' '$4=="rms" {f = 1; exit} END {exit !f}' "$MAN"; then
  echo "no RMS files in manifest; nothing to do" | tee -a "$LOG"
  exit 0
fi
echo "[$(date '+%F %T')] RMS_RAW=$RMS_RAW" | tee -a "$LOG"

python "$PROJECT_ROOT/src/data_prep/02_stage_raw/03_stage_panel.py" \
  --source rms 2>&1 | tee -a "$LOG"

# What the shelf step will need: one stores file and one rms_versions file per
# movement year
awk -F'\t' '$4=="rms" &&
  ($5=="movement" || $5=="stores" || $5=="rms_versions") {
  n[$6 FS $5]++; y[$6]=1 }
  END { for (k in y) printf \
    "%s movement files %d | stores %d | rms_versions %d\n",
          k, n[k FS "movement"], n[k FS "stores"], n[k FS "rms_versions"] }' \
  "$MAN" | sort | tee -a "$LOG"
