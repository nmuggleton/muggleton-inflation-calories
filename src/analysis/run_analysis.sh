#!/usr/bin/env bash
# run_analysis.sh -- the paper's analysis: estimation first, then every table,
# figure and in-text script, in the order they appear in the paper.
#
# Usage:  bash src/analysis/run_analysis.sh            # estimate + outputs
#         OUTPUTS_ONLY=1 bash src/analysis/run_analysis.sh
#                                   # outputs only (no data needed: uses
#                                   # the committed results/*/)
# Run from anywhere; paths resolve from the repository root.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"
mkdir -p logs

if [ "${OUTPUTS_ONLY:-0}" != 1 ]; then
  source src/lib/paths.sh
  RUN_R=src/lib/run_r.sh
  for s in 01_main 02_cells 03_descriptive 04_summary 05_identification \
    06_inference 07_dynamics 08_mechanism 09_heterogeneity \
    10_regime 11_leverage 12_measurement 13_sample; do
    "$RUN_R" "src/analysis/estimate/$s.R"
  done
  src/lib/run_r_shiftshare.sh src/analysis/estimate/14_shiftshare.R
  for s in 15_local_demand 16_bls_cpi 17_other_prices; do
    DUCKDB_MEMORY=4GB DUCKDB_THREADS=2 "$RUN_R" "src/analysis/estimate/$s.R"
  done
  # 21-22 need the panel shelves (data prep 09). There is no script 20.
  for s in 18_methods_audit 19_revision 21_panel_shelf_check 22_panel_reach \
    23_national 24_burden 25_eggs 26_fixed_retailer; do
    "$RUN_R" "src/analysis/estimate/$s.R"
  done
  # 27-38 need the price files from data prep 06_prices/03 to 07; 36 also needs
  # the staged economic surveys (02_stage_raw/07_stage_econ_surveys.R); 39 reads
  # results/01_main/.
  for s in 27_relative_prices 28_split_sample 29_persistence_income \
    30_density 31_state_size 32_nonfood 33_own_price 34_egg_episode \
    35_own_price_rules 36_beliefs 37_price_gap_sources \
    38_outlier_diagnostics 39_income_benchmark; do
    "$RUN_R" "src/analysis/estimate/$s.R"
  done
fi

for sec in 0_introduction 1_pay_same_calories 2_not_local_demand \
  3_avoided_share 4_cheaper_calories 5_poorest_pay 6_respond_to_prices \
  7_discussion 8_methods; do
  for f in src/analysis/$sec/*.R; do
    log="logs/${sec}__$(basename "$f" .R).log"
    Rscript "$f" >"$log" 2>&1 || {
      echo "FAILED: $f (see $log)"
      exit 1
    }
    echo "ok: $f"
  done
done
