#!/usr/bin/env bash
# The "ladder of reach" on panel-observed shelves (every store type, store
# brands included): the calorie-price shelf of the retailer the calories were
# bought at, the household's own retailers (cross-fitted), its zip3, county and
# state, and each f1 household-quarter's savings at each. Replaces RMS shelves,
# which miss ~.65 of panel food spend and all store brands); RMS remains the validation benchmark
# (06_panel_shelf_check.sh). 1. 07a_panel_shelf_ladder.sql per calendar year:
# ladders and base baskets 2. 07b_panel_shelf_hh.sql     per target quarter: the
# rungs Inputs : final/purchases, final/household_year, ladder_upc_kcal,
# final/xwalk_module_foodgroup, intermediate/desc_panel Outputs:
# $WP2_INT/panel_shelf/lad/year=YYYY.parquet
# $WP2_INT/panel_shelf/base/year=YYYY.parquet
# $WP2_INT/panel_shelf/pres/year=YYYY.parquet (retailer presence)
# $WP2_INT/panel_shelf/hh/year=YYYY_qQ.parquet (household-quarters) Env    :
# SHELF_YEARS="2016" subsets TARGET years; SHELF_REBUILD=1 rebuilds outputs that
# exist.
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
HERE="$PROJECT_ROOT/src/data_prep/09_analysis_dataset"
LOG="$WP2_LOGS/09_panel_shelf.log"
: >"$LOG"
say() { echo "[$(date '+%F %T')] $*" | tee -a "$LOG"; }
OUT="$WP2_INT/panel_shelf"
export UK="${LADDER_DATA:-$WP2_INT}/ladder_upc_kcal/data.parquet"
export DP="$WP2_INT/desc_panel/*/*.parquet"
for f in "$UK" "$WP2_FINAL/household_year.parquet" \
  "$WP2_FINAL/xwalk_module_foodgroup.parquet"; do
  [ -f "$f" ] || {
    echo "07_panel_shelf: missing $f" >&2
    exit 1
  }
done
mkdir -p "$OUT"/{lad,base,pres,hh}
rebuild() { [ ! -f "$1" ] || [ -n "${SHELF_REBUILD:-}" ]; }
pfile() { echo "$WP2_FINAL/purchases/panel_year=$1/data.parquet"; }

# Target years: panel years with a base year before them; 2020 is not a target
# year (as in the paper)
ALL=""
for y in $(seq 2005 "$WP2_END_YEAR"); do
  [ "$y" -ne 2020 ] && [ -f "$(pfile $((y - 1)))" ] && ALL="$ALL $y"
done
YEARS="${SHELF_YEARS:-$ALL}"
say "target years: $(echo $YEARS)"

# 1. ladders and base baskets per calendar year -----------------------------
CY=$(for y in $YEARS; do echo $((y - 2)) $((y - 1)); done | tr ' ' '\n' \
  | sort -un)
for c in $CY; do
  [ -f "$(pfile "$c")" ] || continue
  export OUT_LAD="$OUT/lad/year=$c.parquet" \
    OUT_BASE="$OUT/base/year=$c.parquet" \
    OUT_PRES="$OUT/pres/year=$c.parquet"
  rebuild "$OUT_LAD" || continue
  P_FILES="'$(pfile "$c")'"
  [ -f "$(pfile $((c + 1)))" ] && P_FILES="$P_FILES, '$(pfile $((c + 1)))'"
  export YEAR=$c P_FILES
  t0=$(date +%s)
  run_sql "$HERE/07a_panel_shelf_ladder.sql" "$LOG" >/dev/null
  say "ladders $c | $(($(date +%s) - t0)) s"
  duckdb -bail -c "
  SELECT reach, count(*) n, avg(valid::INT) sh_valid,
    count(DISTINCT (state, key)) n_keys, median(n_items) med_items,
    median(n_hh) med_hh,
    CASE WHEN count(*) <> count(DISTINCT (reach, state, key, yq, module))
         THEN error('panel ladders $c: duplicate reach x key x quarter '
                    || 'x module')
    END chk
  FROM read_parquet('$OUT_LAD') GROUP BY reach ORDER BY reach" | tee -a "$LOG"
done

# 2. rungs per target quarter ----------------------------------------------
for y in $YEARS; do
  BASE_FILES=""
  for c in $((y - 2)) $((y - 1)); do
    f="$OUT/base/year=$c.parquet"
    [ -f "$f" ] && BASE_FILES="${BASE_FILES:+$BASE_FILES, }'$f'"
  done
  export BASE_FILES LAD="$OUT/lad/year=$((y - 1)).parquet" \
    PRES="$OUT/pres/year=$((y - 1)).parquet"
  [ -f "$LAD" ] || {
    echo "07_panel_shelf: missing $LAD" >&2
    exit 1
  }
  for q in 1 2 3 4; do
    export OUT_HH="$OUT/hh/year=${y}_q${q}.parquet" YEAR=$y QTR=$q
    rebuild "$OUT_HH" || continue
    run_sql "$HERE/07b_panel_shelf_hh.sql" "$LOG" >/dev/null
    # One row per estimation-sample household-quarter, no drops; the common base
    # lies within labelled base spend
    duckdb -bail -c "
    WITH h AS (SELECT * FROM read_parquet('$OUT_HH')),
    g AS (SELECT count(*) n FROM read_parquet('$DP', hive_partitioning = true)
          WHERE est_gate AND yq = $((y * 10 + q)))
    SELECT count(*) n, count(DISTINCT (household_code, yq)) n_key,
      sum(pb_spend) / sum(pb_spend_lab) sh_common,
      sum(sav25_ret) / sum(pb_spend) ret,
      sum(sav25_setx) / sum(pb_spend) own_setx,
      sum(sav25_set) / sum(pb_spend) own_set_naive,
      sum(sav25_zip3) / sum(pb_spend) zip3,
      sum(sav25_cty) / sum(pb_spend) county,
      sum(sav25_st) / sum(pb_spend) state,
      sum(sav25_bz3) / sum(pb_spend) best_zip3,
      sum(sav25_bcty) / sum(pb_spend) best_cty,
      sum(sav25_bst) / sum(pb_spend) best_st,
      median(own_sh_cty) med_own_cty,
      CASE WHEN count(*) <> count(DISTINCT (household_code, yq))
             OR count(*) <> (SELECT n FROM g)
             OR count(*) FILTER (pb_spend > pb_spend_lab + 1e-6) > 0
           THEN error('panel shelf ${y}q${q}: key, row-count or common-base '
                      || 'check failed')
      END chk
    FROM h" | tee -a "$LOG"
    say "rungs $y q$q"
  done
done
say "07_panel_shelf done"
