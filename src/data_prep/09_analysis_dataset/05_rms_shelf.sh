#!/usr/bin/env bash
# The calorie-price shelf from Retail Scanner store prices, and what each
# household bought in each store and module (the benchmark the panel shelves are
# checked against; 06_panel_shelf_check.sh). 1. 05a_rms_shelf_ladder.sql  per
# RMS year x module file: ladders 2. 05b_rms_shelf_hhstore.sql per panel year:
# household x quarter x store x module labelled spend and kcal Inputs : raw RMS
# movement TSVs (manifest), staged rms/stores and rms/rms_versions
# (src/data_prep/02_stage_raw/06_stage_rms.sh), ladder_upc_kcal (01_ladder.sh),
# final/purchases, final/xwalk_module_foodgroup, final/household_year,
# intermediate/desc_panel Outputs:
# $WP2_INT/rms_shelf/lad/year=YYYY/module=M.parquet
# $WP2_INT/rms_shelf/cov/year=YYYY/module=M.parquet
# $WP2_INT/rms_shelf/hh_store/panel_year=YYYY.parquet Env    :
# SHELF_YEARS="2019" subsets TARGET years (each needs RMS year and panel years
# before it); SHELF_REBUILD=1 rebuilds outputs that exist. Test on one target
# year before the full run.
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
HERE="$PROJECT_ROOT/src/data_prep/09_analysis_dataset"
LOG="$WP2_LOGS/09_rms_shelf.log"
: >"$LOG"
say() { echo "[$(date '+%F %T')] $*" | tee -a "$LOG"; }
OUT="$WP2_INT/rms_shelf"
LSQL="$DUCKDB_TMP/rms_shelf_lad_$$.sql" # read + ladder core, per module
MAN="$WP2_INT/raw_manifest.tsv"
export UK="${LADDER_DATA:-$WP2_INT}/ladder_upc_kcal/data.parquet"
export DP="$WP2_INT/desc_panel/*/*.parquet"
XW="$WP2_FINAL/xwalk_module_foodgroup.parquet"
for f in "$MAN" "$UK" "$XW" "$WP2_FINAL/household_year.parquet"; do
  [ -f "$f" ] || {
    echo "05_rms_shelf: missing $f" >&2
    exit 1
  }
done
mkdir -p "$OUT"/{lad,cov,hh_store,hh}
rebuild() { [ ! -f "$1" ] || [ -n "${SHELF_REBUILD:-}" ]; }

# RMS movement years present; the target years are those whose base year (t-4
# lies in year - 1) has RMS. 2020 is not a target year (as in the paper).
RMS_YEARS=$(
  awk -F'\t' '$4=="rms" && $5=="movement" {print $6}' "$MAN" | sort -un
)
[ -n "$RMS_YEARS" ] || {
  echo "05_rms_shelf: no RMS movement in manifest" >&2
  exit 1
}
ALL=""
for y in $(seq 2005 2019) 2021; do
  echo "$RMS_YEARS" | grep -qx "$((y - 1))" && ALL="$ALL $y"
done
YEARS="${SHELF_YEARS:-$ALL}"
say "RMS years: $(echo $RMS_YEARS) | target years: $(echo $YEARS)"

# Human-food modules and their RMS product group
FOOD="$OUT/food_modules.txt"
duckdb -noheader -csv -c "SELECT product_module_code FROM read_parquet('$XW')
  WHERE is_human_food ORDER BY 1" >"$FOOD"

# 1. ladders per RMS year x module -----------------------------------------
for y in $YEARS; do
  r=$((y - 1))
  export RMS_VER="$WP2_INT/rms/rms_versions/panel_year=$r/data.parquet"
  export RMS_STO="$WP2_INT/rms/stores/panel_year=$r/data.parquet"
  for f in "$RMS_VER" "$RMS_STO"; do
    [ -f "$f" ] || {
      echo "05_rms_shelf: missing $f (run 06_stage_rms.sh)" >&2
      exit 1
    }
  done
  mkdir -p "$OUT/lad/year=$r" "$OUT/cov/year=$r"
  GRPS=$(awk -F'\t' -v y="$r" 'NR==FNR {f[$1]; next}
    $4=="rms" && $5=="movement" && $6==y && ($8 in f) {print $7}' \
    "$FOOD" "$MAN" | sort -un)
  if [ -n "${SHELF_GROUPS:-}" ]; then
    for g in $SHELF_GROUPS; do
      echo "$GRPS" | grep -qx "$g" \
        || {
          echo "05_rms_shelf: no human-food group $g in RMS $r" >&2
          exit 1
        }
    done
    GRPS="$SHELF_GROUPS"
  fi
  # One movement file (= one module) at a time: a whole group can be too large
  # to aggregate (RMS 2018 group 503, 32 GB of TSV, filled the 20 GiB spill cap
  # at 9 threads and at 2); the largest single file, 13 GB, needs about 10 GB
  for g in $GRPS; do
    awk -F'\t' -v y="$r" -v g="$g" 'NR==FNR {f[$1]; next}
      $4=="rms" && $5=="movement" && $6==y && $7==g && ($8 in f) {
        print $8, $1 }' "$FOOD" "$MAN" | sort -n \
      | while read -r m rel; do
        export OUT_LAD="$OUT/lad/year=$r/module=$m.parquet"
        export OUT_COV="$OUT/cov/year=$r/module=$m.parquet"
        rebuild "$OUT_LAD" || continue
        t0=$(date +%s)
        export YEAR=$r GROUP=$g MODULE=$m MV_FILE="$RMS_RAW/$rel"
        # A network share (RMS_RAW on SMB) can drop out for a moment: wait up to
        # 5 min for the file to come back; if it is still missing, read_csv
        # fails as it would have anyway
        for i in $(seq 30); do
          [ -f "$MV_FILE" ] && break
          sleep 10
        done
        # Read (05a) and then build the ladders (core) in one DuckDB session, as
        # one SQL file: a <(...) does not reach the reader inside run_sql (bash
        # 3.2)
        cat "$HERE/05a_rms_shelf_ladder.sql" \
          "$HERE/05a_rms_shelf_ladder_core.sql" >"$LSQL"
        run_sql "$LSQL" "$LOG" >/dev/null
        [ -s "$OUT_LAD" ] && [ -s "$OUT_COV" ] \
          || {
            echo "05_rms_shelf: no ladder or coverage for $r module $m" >&2
            exit 1
          }
        say "ladders $r group $g module $m | $(($(date +%s) - t0)) s"
      done
  done
  # sh_kept: the labelled revenue whose item's ladder module is the file's own
  # module (the rest belongs to items filed under another module)
  duckdb -bail -c "
  SELECT yq, sum(rev) / 1e6 rev_m, sum(rev_lab) / sum(rev) sh_lab,
    sum(rev_kept) / sum(rev_lab) sh_kept, max(n_stores) n_stores_max,
    sum(coalesce(n_nostore, 0)) n_nostore
  FROM (SELECT * FROM read_parquet('$OUT/cov/year=$r/*.parquet',
                                   union_by_name = true)
        -- files built before n_nostore existed lack it (they passed the
        -- strict store check, so 0 is exact); this adds it if no file has it
        UNION ALL BY NAME SELECT NULL::DOUBLE AS n_nostore WHERE false)
  GROUP BY yq ORDER BY yq" \
    | tee -a "$LOG"
  duckdb -bail -c "
  SELECT reach, count(*) n, avg(valid::INT) sh_valid,
    count(DISTINCT key) n_keys, median(n_items) med_items,
    CASE WHEN count(*) <> count(DISTINCT (reach, key, yq, module))
         THEN error('rms ladders $r: duplicate reach x key x quarter x module')
    END chk
  FROM read_parquet('$OUT/lad/year=$r/*.parquet') GROUP BY reach
    ORDER BY reach" \
    | tee -a "$LOG"
done

# 2. household x store x module, panel years y-2 .. y ----------------------
PY=$(
  for y in $YEARS; do echo $((y - 2)) $((y - 1)) "$y"; done \
    | tr ' ' '\n' | sort -un
)
for p in $PY; do
  [ -f "$WP2_FINAL/purchases/panel_year=$p/data.parquet" ] || continue
  export OUT_HS="$OUT/hh_store/panel_year=$p.parquet"
  rebuild "$OUT_HS" || continue
  export YEAR=$p
  run_sql "$HERE/05b_rms_shelf_hhstore.sql" "$LOG" >/dev/null
  say "hh_store $p"
done

say "05_rms_shelf done"
