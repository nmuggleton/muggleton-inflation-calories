#!/usr/bin/env bash
# One-off check before adding panel years 2022-2024: label coverage and UPC
# continuity (03_label_coverage_new_years.sql). Needs the 2022-2024 panel files
# staged (02_stage_raw/03_stage_panel.py).
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
LOG="$WP2_LOGS/check_label_coverage_2224.log"
: >"$LOG"
echo "[$(date '+%F %T')] label coverage, 2022-2024 panel files" | tee -a "$LOG"
run_sql \
  "$PROJECT_ROOT/src/data_prep/04_nutrition/03_label_coverage_new_years.sql" \
  "$LOG"
