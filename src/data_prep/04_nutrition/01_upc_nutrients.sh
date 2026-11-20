#!/usr/bin/env bash
# UPC nutrient profiles + PL crosswalk + module fallback
# (src/data_prep/04_nutrition/01a_upc_nutrients.sql). Needs final/nutrition_b2,
# dim_products_b2, xwalk_module (03_homescan) and the Kilts PL crosswalk text
# file.
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
# The Kilts private-label crosswalk (note the spelling of the folder name,
# Kilts')
KILTS_DOCS="$NIELSEN_RAW/kilts_panel/Reference_Documentation"
PL_FILE="Private_Label_2020_Unmasked_2021_Masked_UPCs.txt"
PL_DEFAULT="$KILTS_DOCS/2021-Onward_Documetation/$PL_FILE"
export PL_XWALK="${PL_XWALK:-$PL_DEFAULT}"
mkdir -p "$WP2_INT/ref/pl_crosswalk"
LOG="$WP2_LOGS/desc_05_upc_nutrients.log"
: >"$LOG"
run_sql "$PROJECT_ROOT/src/data_prep/04_nutrition/01a_upc_nutrients.sql" "$LOG"
