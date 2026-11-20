#!/usr/bin/env bash
# run_data_prep.sh -- the full data build, from the raw Kilts files to the
# analysis dataset, in step order (src/data_prep/02_* to 09_*). Safe to re-run:
# staging skips outputs that are already up to date (FORCE=1 rebuilds them).
#
# Usage:  bash src/data_prep/run_data_prep.sh              # steps 02-09
#         FROM=07 bash src/data_prep/run_data_prep.sh      # steps 07-09
#         FROM=04 TO=05 bash src/data_prep/run_data_prep.sh
#         FORCE=1 FROM=05 ...      # rebuild, no skipping
#         CHECK=0 ...              # skip the final check
# Long runs: nohup bash src/data_prep/run_data_prep.sh > logs/run_data_prep.out
# 2>&1 &
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
source "$HERE/../lib/paths.sh"
if [ -d "$WP2_VENV" ]; then source "$WP2_VENV/bin/activate"; fi
F=""
if [ "${FORCE:-0}" = 1 ]; then
  F="--force"
  export FORCE LADDER_REBUILD=1 LADDER_REBUILD_UK=1 SHIFTSHARE_FORCE=1 \
    SHELF_REBUILD=1
fi
FROM="${FROM:-02}"
TO="${TO:-09}"
LOG="$WP2_LOGS/run_data_prep.log"
DP="$PROJECT_ROOT/src/data_prep"
step() { echo "[$(date '+%F %T')] === $* ===" | tee -a "$LOG"; }
# True if step $1 lies between $FROM and $TO (inclusive)
want() {
  [[ "$1" > "$FROM" || "$1" == "$FROM" ]] && [[ "$1" < "$TO" || "$1" == "$TO" ]]
}

if want 02; then
  step "02 check raw files"
  bash "$DP/02_stage_raw/01_check_raw.sh"
  step "02 inventory + classify"
  bash "$DP/02_stage_raw/02_inventory.sh"
  step "02 stage panel"
  python "$DP/02_stage_raw/03_stage_panel.py" $F
  step "02 stage panelviews"
  python "$DP/02_stage_raw/04_stage_panelviews.py" $F
  step "02 stage reference docs"
  python "$DP/02_stage_raw/05_stage_refdocs.py"
  # Retail Scanner 2022 product files: the only source of 2022 labels
  step "02 stage scanner 2022 products"
  python "$DP/02_stage_raw/03_stage_panel.py" --source rms_b2 --year 2022 $F
  step "02 stage rms"
  bash "$DP/02_stage_raw/06_stage_rms.sh"
  # PanelViews economic surveys 2015-2019 (read in place from RMS_RAW)
  step "02 stage economic surveys"
  Rscript "$DP/02_stage_raw/07_stage_econ_surveys.R" "$WP2_DATA" \
    2>&1 | tee -a "$LOG"
fi
if want 03; then
  step "03 products"
  bash "$DP/03_homescan/01_products.sh"
  step "03 merge purchases"
  bash "$DP/03_homescan/02_merge.sh"
  step "03 household-year"
  run_sql "$DP/03_homescan/03_household_year.sql" "$LOG"
  step "03 samples"
  run_sql "$DP/03_homescan/04_samples.sql" "$LOG"
fi
if want 04; then
  step "04 UPC nutrients"
  bash "$DP/04_nutrition/01_upc_nutrients.sh"
  step "04 food groups"
  FG_OUT="$WP2_FINAL/xwalk_module_foodgroup.parquet" \
    run_sql "$DP/04_nutrition/02_food_groups.sql" "$LOG"
fi
if want 05; then
  step "05 household-quarter components"
  bash "$DP/05_household_quarter/01_stream.sh"
  step "05 household-quarter panel"
  Rscript "$DP/05_household_quarter/02_combine.R" "$WP2_DATA" "$PROJECT_ROOT" \
    2>&1 | tee -a "$LOG"
fi
if want 06; then
  step "06 price indices"
  bash "$DP/06_prices/01_price_indices.sh"
  step "06 fixed-retailer prices"
  bash "$DP/06_prices/02_fixed_retailer.sh"
  # Own prices against state prices: first the threefold rule on net prices
  # (net3), then the rule of the paper (gross10)
  step "06 relative prices (net3)"
  bash "$DP/06_prices/03_relative_prices.sh"
  step "06 relative prices (gross10)"
  OWN_RULE=gross10 bash "$DP/06_prices/03_relative_prices.sh"
  step "06 split-sample exposure"
  bash "$DP/06_prices/04_split_exposure.sh"
  step "06 own-price rules"
  bash "$DP/06_prices/05_own_price_rules.sh"
  step "06 price gap sources"
  bash "$DP/06_prices/06_price_gap_sources.sh"
  step "06 outlier diagnostics"
  bash "$DP/06_prices/07_outlier_diagnostics.sh"
fi
if want 07; then
  step "07 controls"
  Rscript "$DP/07_controls/01_controls.R" "$WP2_DATA" "$PROJECT_ROOT" \
    2>&1 | tee -a "$LOG"
  step "07 estimation-sample panel"
  run_sql "$DP/07_controls/02_sample_panel.sql" "$LOG"
fi
if want 08; then
  step "08 download public data"
  bash "$DP/08_external/01_download.sh" 2>&1 | tee -a "$LOG"
  step "08 FRED state controls"
  python "$DP/08_external/02_fred.py" | tee -a "$LOG"
  step "08 local context tables"
  python "$DP/08_external/03_context.py" | tee -a "$LOG"
fi
if want 09; then
  step "09 calorie-price ladder"
  bash "$DP/09_analysis_dataset/01_ladder.sh"
  step "09 analysis dataset"
  run_sql "$DP/09_analysis_dataset/02_build.sql" "$LOG"
  step "09 shift-share instrument"
  bash "$DP/09_analysis_dataset/03_shiftshare.sh"
  step "09 local-context panel"
  run_sql "$DP/09_analysis_dataset/04_local_panel.sql" "$LOG"
  step "09 RMS shelf (validation benchmark)"
  bash "$DP/09_analysis_dataset/05_rms_shelf.sh"
  step "09 RMS shelf, 2021-on files"
  bash "$DP/09_analysis_dataset/05d_rms_shelf_b2.sh"
  step "09 panel vs RMS shelf check"
  bash "$DP/09_analysis_dataset/06_panel_shelf_check.sh"
  step "09 panel shelf (ladder of reach)"
  bash "$DP/09_analysis_dataset/07_panel_shelf.sh"
fi
if [ "${CHECK:-1}" = 1 ]; then
  step "check against config/checksums.json"
  # Differences are reported, not fatal: a rebuild with newer public downloads
  # differs from the paper's inputs, but should still run through
  python "$PROJECT_ROOT/src/tools/check_data.py" 2>&1 | tee -a "$LOG" || true
fi
step "done"
