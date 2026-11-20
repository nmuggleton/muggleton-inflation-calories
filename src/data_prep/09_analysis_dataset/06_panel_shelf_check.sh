#!/usr/bin/env bash
# Validation of panel-observed shelves against RMS shelves, before panel shelves
# are used for stores RMS does not cover (the largest mass merchandiser,
# warehouse clubs). 1. 06a_panel_shelf_check.sql  per year: panel vs RMS state
# ladders, savings, coverage and density 2. 06b_panel_shelf_decomp.sql per
# module of one year: the gap split into items seen, stores seen, quantities,
# prices (re-reads RMS) Inputs : 05_rms_shelf.sh outputs (lad, hh_store),
# rms/stores, final/purchases, desc_panel Outputs:
# $WP2_INT/panel_shelf_check/{lad,sav,cov}_YYYY.parquet
# $WP2_INT/panel_shelf_check/decomp/{dec,mat}_YYYY_M.parquet summarised by
# src/analysis/estimate/21_panel_shelf_check.R Env    : SHELF_CHECK_YEARS (RMS =
# panel years; each needs target year +1 in est_gate, so not 2019). Default
# spreads the sample.
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
HERE="$PROJECT_ROOT/src/data_prep/09_analysis_dataset"
LOG="$WP2_LOGS/09_panel_shelf_check.log"
: >"$LOG"
say() { echo "[$(date '+%F %T')] $*" | tee -a "$LOG"; }
OUT="$WP2_INT/panel_shelf_check"
mkdir -p "$OUT"
export UK="${LADDER_DATA:-$WP2_INT}/ladder_upc_kcal/data.parquet"
export DP="$WP2_INT/desc_panel/*/*.parquet"
# 2023: RMS from 2021 on (05d_rms_shelf_b2.sh), target year 2024
YEARS="${SHELF_CHECK_YEARS:-2008 2013 2018 2023}"
say "years: $YEARS"
for y in $YEARS; do
  [ "$y" -ne 2019 ] || {
    echo "06_panel_shelf_check: 2019 has no target year (2020 excluded)" >&2
    exit 1
  }
  export YEAR=$y
  export RMS_STO="$WP2_INT/rms/stores/panel_year=$y/data.parquet"
  export LAD="$WP2_INT/rms_shelf/lad/year=$y/*.parquet"
  export HS="$WP2_INT/rms_shelf/hh_store/panel_year=$y.parquet"
  purchases="$WP2_FINAL/purchases/panel_year=$y/data.parquet"
  for f in "$RMS_STO" "$HS" "$purchases"; do
    [ -f "$f" ] || {
      echo "06_panel_shelf_check: missing $f" >&2
      exit 1
    }
  done
  export OUT_LAD="$OUT/lad_$y.parquet" OUT_SAV="$OUT/sav_$y.parquet" \
    OUT_COV="$OUT/cov_$y.parquet"
  t0=$(date +%s)
  run_sql "$HERE/06a_panel_shelf_check.sql" "$LOG" >/dev/null
  say "check $y | $(($(date +%s) - t0)) s"
done

# 2. decomposition ------------------------------------------------------------
y="${DECOMP_YEAR:-2013}"
N="${DECOMP_N:-25}"
export YEAR=$y
export RMS_STO="$WP2_INT/rms/stores/panel_year=$y/data.parquet"
export RMS_VER="$WP2_INT/rms/rms_versions/panel_year=$y/data.parquet"
MAN="$WP2_INT/raw_manifest.tsv"
mkdir -p "$OUT/decomp"
TOP=$(duckdb -noheader -csv -c "
  WITH im AS (
    SELECT item, min(module) AS module FROM read_parquet('$UK') GROUP BY item
  )
  SELECT im.module
    FROM read_parquet('$WP2_FINAL/purchases/panel_year=$y/data.parquet') p
  JOIN read_parquet('$WP2_FINAL/xwalk_module_foodgroup.parquet') fg
    USING (product_module_code)
  JOIN read_parquet('$UK') u ON u.upc = p.upc
    AND u.upc_ver_uc = coalesce(p.upc_ver_uc, 0)::SMALLINT
  JOIN im USING (item)
  SEMI JOIN read_parquet('$RMS_STO') s
    ON s.store_code_uc::INTEGER = p.store_code_uc
  WHERE fg.is_human_food AND NOT p.is_magnet_product AND p.total_price_paid > 0
  GROUP BY 1 ORDER BY sum(p.total_price_paid) DESC LIMIT $N")
say "decomp $y modules: $(echo $TOP)"
for m in $TOP; do
  rel=$(
    awk -F'\t' -v y="$y" -v m="$m" \
      '$4=="rms" && $5=="movement" && $6==y && $8==m {print $1}' "$MAN"
  )
  [ -n "$rel" ] || {
    echo "06_panel_shelf_check: no RMS $y file for module $m" >&2
    exit 1
  }
  export MODULE=$m MV_FILE="$RMS_RAW/$rel"
  export OUT_DEC="$OUT/decomp/dec_${y}_$m.parquet"
  export OUT_MAT="$OUT/decomp/mat_${y}_$m.parquet"
  [ ! -f "$OUT_DEC" ] || [ -n "${SHELF_REBUILD:-}" ] || continue
  # A network share (RMS_RAW on SMB) can drop out for a moment
  for i in $(seq 30); do
    [ -f "$MV_FILE" ] && break
    sleep 10
  done
  t0=$(date +%s)
  run_sql "$HERE/06b_panel_shelf_decomp.sql" "$LOG" >/dev/null
  say "decomp $y module $m | $(($(date +%s) - t0)) s"
done
say "06_panel_shelf_check done"
