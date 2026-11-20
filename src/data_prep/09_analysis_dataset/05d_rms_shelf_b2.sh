#!/usr/bin/env bash
# Retail Scanner shelves (05a ladders) and panel household x store spend (05b)
# for RMS years from 2021, whose files differ from 2006-2020 (05_rms_shelf.sh):
# - one movement file per super-category, not per product module, plain TSV
# (2021-2022) or a .tgz per file (2023 on), read through a named pipe so nothing
# is unpacked to disk; - week_end as YYYY-MM-DD; no rms_versions file (products
# are version 0, as 2021-on panel purchases); annual files in 01_Annual_Files.
# Steps per RMS year: 1. stage stores (as 06_stage_rms.sh does for 2006-2020)
# and the year's upc -> module and upc -> super-category 2.
# 05d1_rms_b2_cells.sql per food super-category file: cells by module 3.
# 05d2_rms_b2_mu.sql + 05a_rms_shelf_ladder_core.sql per module: the same
# ladders and coverage as 05a, into rms_shelf/lad, cov 4.
# 05b_rms_shelf_hhstore.sql for the same panel year The panel shelf check
# (06_panel_shelf_check.sh, SHELF_CHECK_YEARS) then runs on these as on
# 2008-2018.
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
HERE="$PROJECT_ROOT/src/data_prep/09_analysis_dataset"
LOG="$WP2_LOGS/09_rms_shelf_b2.log"
say() { echo "[$(date '+%F %T')] $*" | tee -a "$LOG"; }
OUT="$WP2_INT/rms_shelf"
B2="$OUT/b2"
RAW="$RMS_RAW/kilts_scanner/2021-Onward_Scanner_Data"
export UK="${LADDER_DATA:-$WP2_INT}/ladder_upc_kcal/data.parquet"
XW="$WP2_FINAL/xwalk_module_foodgroup.parquet"
for f in "$UK" "$XW"; do
  [ -f "$f" ] || {
    echo "05d_rms_shelf_b2: missing $f" >&2
    exit 1
  }
done
[ -d "$RAW" ] || {
  echo "05d_rms_shelf_b2: no $RAW (M-Drive mounted?)" >&2
  exit 1
}
mkdir -p "$B2" "$OUT"/{lad,cov,hh_store}
rebuild() { [ ! -e "$1" ] || [ -n "${SHELF_REBUILD:-}" ]; }

export FOOD="$OUT/food_modules.txt"
duckdb -noheader -csv -c "SELECT product_module_code FROM read_parquet('$XW')
  WHERE is_human_food ORDER BY 1" >"$FOOD"

# Outputs are written to <name>.tmp and renamed on success, so a failed run
# leaves nothing behind that a rerun would mistake for a finished output.
FIFO="$DUCKDB_TMP/rms_b2_$$.fifo"
TAR_PID=""
open_raw() {
  local f="$1"
  for i in $(seq 30); do
    [ -f "$f" ] && break
    sleep 10
  done # SMB blip
  case "$f" in
    *.tsv) RD="$f" ;;
    *.tgz)
      # A .tgz holds one TSV; a second member's header line would be read as a
      # row with no store and no week, which 05d1 stops on
      rm -f "$FIFO"
      mkfifo "$FIFO"
      tar -xzOf "$f" >"$FIFO" &
      TAR_PID=$!
      RD="$FIFO"
      ;;
    *)
      echo "05d_rms_shelf_b2: unknown file type $f" >&2
      exit 1
      ;;
  esac
}
close_raw() {
  if [ -n "$TAR_PID" ]; then
    wait "$TAR_PID" || {
      echo "05d_rms_shelf_b2: tar failed on $1" >&2
      exit 1
    }
    TAR_PID=""
    rm -f "$FIFO"
  fi
}
trap '[ -z "$TAR_PID" ] || kill "$TAR_PID" 2>/dev/null; rm -f "$FIFO"' EXIT
# File name -> id: upper case, & -> AND, letters and digits only, so that
# "CANDY, GUM, MINTS", "CANDY_GUM_MINTS" and the super-category name all match
norm() {
  echo "$1" | tr '[:lower:]' '[:upper:]' | sed 's/&/AND/g; s/[^A-Z0-9]//g'
}
annual() { # the year's annual file $2 (.tsv or .tgz)
  local d="$RAW/${1}_File_Select_Supercategory_MovementData/01_Annual_Files"
  if [ -f "$d/$2.tsv" ]; then
    echo "$d/$2.tsv"
  elif [ -f "$d/$2.tgz" ]; then
    echo "$d/$2.tgz"
  else
    echo "05d_rms_shelf_b2: no $2 in $d" >&2
    exit 1
  fi
}

for r in ${RMS_B2_YEARS:-2023}; do
  [ "$r" -ge 2021 ] || {
    echo "05d_rms_shelf_b2: $r is before 2021 (05_rms_shelf.sh)" >&2
    exit 1
  }
  YD="$RAW/${r}_File_Select_Supercategory_MovementData"
  [ -d "$YD" ] || {
    echo "05d_rms_shelf_b2: no $YD" >&2
    exit 1
  }
  say "RMS $r: $YD"

  # 1. stores, upc -> module, upc -> super-category -------------------------
  export RMS_STO="$WP2_INT/rms/stores/panel_year=$r/data.parquet"
  if rebuild "$RMS_STO"; then
    mkdir -p "$(dirname "$RMS_STO")"
    open_raw "$(annual "$r" stores)"
    # The columns and types of the 2006-2020 staged stores (06_stage_rms.sh);
    # the 2021-on file has no county, state or DMA names
    duckdb -bail -c "
    COPY (
      SELECT store_code_uc::INTEGER AS store_code_uc, year::SMALLINT AS year,
        parent_code::INTEGER AS parent_code,
        retailer_code::INTEGER AS retailer_code,
        channel_code, store_zip3,
        fips_state_code::SMALLINT AS fips_state_code,
        fips_county_code::SMALLINT AS fips_county_code,
        TRY_CAST(acv AS DOUBLE) AS acv, $r::BIGINT AS panel_year
      FROM read_csv('$RD', delim = '\t', header = true, all_varchar = true)
    ) TO '$RMS_STO.tmp' (FORMAT parquet)" >/dev/null \
      && mv "$RMS_STO.tmp" "$RMS_STO"
    close_raw stores
  fi
  duckdb -bail -c "
  SELECT count(*) n_stores, count(DISTINCT store_code_uc) n_key,
    count(*) FILTER (fips_state_code IS NULL) n_no_state, min(year) y0,
    max(year) y1,
    CASE WHEN count(*) <> count(DISTINCT store_code_uc) OR min(year) <> $r
      OR max(year) <> $r
         THEN error('rms stores $r: store not unique or wrong year') END chk
  FROM read_parquet('$RMS_STO')" | tee -a "$LOG"

  UM="$B2/upc_module_$r.parquet"
  USC="$B2/upc_sc_$r.parquet"
  if rebuild "$UM"; then
    open_raw "$(annual "$r" productdesc)"
    duckdb -bail -c "
    -- module code DEFA (deferred, not yet coded) -> NULL: in no food module
    COPY (SELECT upc, TRY_CAST(product_module_code AS INTEGER) AS module,
            product_module_code AS module_raw
          FROM read_csv('$RD', delim = '\t', header = true, all_varchar = true,
                        quote = '')
    ) TO '$UM.tmp' (FORMAT parquet)" >/dev/null && mv "$UM.tmp" "$UM"
    close_raw productdesc
  fi
  if rebuild "$USC"; then
    open_raw "$(annual "$r" producthierarchy)"
    duckdb -bail -c "
    COPY (SELECT upc, super_category
          FROM read_csv('$RD', delim = '\t', header = true, all_varchar = true,
                        quote = '')
    ) TO '$USC.tmp' (FORMAT parquet)" >/dev/null && mv "$USC.tmp" "$USC"
    close_raw producthierarchy
  fi
  duckdb -bail -c "
  SELECT *, CASE WHEN n_upc <> n_upc_key OR n_sc <> n_sc_key
                 THEN error('rms $r: upc not unique in productdesc or '
                            || 'producthierarchy')
            END chk
  FROM (SELECT (SELECT count(*) FROM '$UM') n_upc,
          (SELECT count(DISTINCT upc) FROM '$UM') n_upc_key,
          (SELECT count(*) FROM '$USC') n_sc,
          (SELECT count(DISTINCT upc) FROM '$USC') n_sc_key,
          (SELECT count(*) FILTER (module IS NULL) FROM '$UM') n_no_module,
          (SELECT string_agg(DISTINCT module_raw, ' ') FROM '$UM'
            WHERE module IS NULL) codes_no_module)" \
    | tee -a "$LOG"

  # Food super-categories: at least 1,000 human-food UPCs and 5% of the
  # super-category's UPCs (the rest hold a handful of food UPCs among thousands
  # of others, vitamins for instance); the food UPCs left out are logged
  SEL="$B2/food_sc_$r.tsv"
  duckdb -bail -noheader -csv -separator $'\t' -c "
  WITH f AS (
    SELECT column0::INTEGER AS module
    FROM read_csv(
      '$FOOD', header = false, columns = {'column0': 'VARCHAR'}
    )
  ),
  s AS (
    SELECT s.super_category sc, count(*) n_all,
      count(*) FILTER (u.module IN (SELECT module FROM f)) n_food
    FROM '$USC' s LEFT JOIN '$UM' u USING (upc) GROUP BY 1)
  SELECT sc, n_food, n_all, n_food >= 1000 AND n_food >= 0.05 * n_all AS keep
  FROM s WHERE n_food > 0 ORDER BY n_food DESC" >"$SEL"
  awk -F'\t' '{ n += $2; if ($4 == "true") k += $2 } END {
    printf "food UPCs in selected super-categories: %d of %d (%.4f)\n",
      k, n, k / n }' \
    "$SEL" | tee -a "$LOG"

  # Movement files -> ids; a .tsv and a .tgz of the same super-category (2022
  # has a few of each) count as one file, and the .tsv is read
  FL="$B2/files_$r.tsv"
  : >"$FL"
  while IFS= read -r f; do
    b=$(basename "$f")
    printf '%s\t%s\n' "$(norm "${b%.*}")" "$f" >>"$FL"
  done < <(find "$YD" -type f \( -name '*.tsv' -o -name '*.tgz' \) \
    -not -path '*/01_Annual_Files/*' | sort)
  awk -F'\t' '{ print $1 "\t" ($2 ~ /\.tsv$/ ? 0 : 1) "\t" $2 }' "$FL" \
    | sort -t $'\t' -k1,1 -k2,2n \
    | awk -F'\t' '!seen[$1]++ { print $1 "\t" $3 }' >"$FL.tmp" \
    && mv "$FL.tmp" "$FL"
  JOBS=$(awk -F'\t' 'NR == FNR { f[$1] = $2; next }
    $4 == "true" {
      id = toupper($1); gsub(/&/, "AND", id); gsub(/[^A-Z0-9]/, "", id)
                   if (!(id in f)) { print "MISSING " $1; next }
                   print id "\t" f[id] }' "$FL" "$SEL")
  if echo "$JOBS" | grep -q '^MISSING'; then
    missing="$(echo "$JOBS" | grep '^MISSING')"
    echo "05d_rms_shelf_b2: no movement file for: $missing" >&2
    exit 1
  fi
  say "RMS $r: $(echo "$JOBS" | wc -l | tr -d ' ') food files"

  # 2. cells per file ----------------------------------------------------------
  # A year whose ladders are complete (marker below) skips steps 2-3
  DONE="$OUT/lad/year=$r/_complete"
  CD="$B2/cells/year=$r"
  mkdir -p "$CD/_done"
  if ! rebuild "$DONE" && [ -z "${RMS_B2_FILES:-}" ]; then
    say "RMS $r: ladders complete; steps 2-3 skipped"
    JOBS=""
  fi
  export UPC_MOD="$UM" YEAR=$r
  while IFS=$'\t' read -r id f; do
    [ -n "$id" ] || continue
    if [ -n "${RMS_B2_FILES:-}" ] \
      && ! echo " $RMS_B2_FILES " | grep -q " $id "; then
      continue
    fi
    [ ! -f "$CD/_done/$id" ] || [ -n "${SHELF_REBUILD:-}" ] || continue
    t0=$(date +%s)
    # Passes: a large file is read NPASS times, each pass aggregating the UPCs
    # with hash(upc) % NPASS = PASS, so that the store x UPC x quarter aggregate
    # stays within memory and the spill cap (PREPARED FOODS, 45 GB of TSV,
    # overran 20 GiB in a single pass).
    sz=$(stat -f %z "$f")
    case "$f" in *.tgz) sz=$((sz * 5)) ;; esac
    export NPASS=$(((sz + 7999999999) / 8000000000))
    # Cells of an earlier, failed run of this file go first
    rm -f "$CD"/module=*/"${id}"__*.parquet
    for ((pass = 0; pass < NPASS; pass++)); do
      # Write to a fresh directory, then move each module's file into place, so
      # a failed pass leaves no partial file
      export PASS=$pass OUT_DIR="$B2/tmp_$id" FILE_ID="$id"
      rm -rf "$OUT_DIR"
      open_raw "$f"
      export MV_FILE="$RD"
      run_sql "$HERE/05d1_rms_b2_cells.sql" "$LOG" >/dev/null
      close_raw "$id"
      for d in "$OUT_DIR"/module=*; do
        [ -d "$d" ] || continue
        m="${d##*/}"
        mkdir -p "$CD/$m"
        n=0
        for p in "$d"/*.parquet; do
          mv "$p" "$CD/$m/${id}__${pass}_$n.parquet"
          n=$((n + 1))
        done
      done
      rm -rf "$OUT_DIR"
    done
    touch "$CD/_done/$id"
    size_so_far="$(du -sh "$CD" | cut -f1)"
    say "cells $r $id | $NPASS pass(es) | $(($(date +%s) - t0)) s |" \
      "$size_so_far so far"
  done <<<"$JOBS"
  if [ -n "${RMS_B2_FILES:-}" ]; then
    say "RMS_B2_FILES set: ladders skipped"
    continue
  fi
  for id in $(cut -f1 <<<"$JOBS"); do
    [ -f "$CD/_done/$id" ] || {
      echo "05d_rms_shelf_b2: cells of $r $id missing" >&2
      exit 1
    }
  done
  if [ -n "$JOBS" ]; then

    # 3. ladders per module --------------------------------------------------
    mkdir -p "$OUT/lad/year=$r" "$OUT/cov/year=$r"
    export GROUP=0 # no product groups from 2021
    # One SQL file: a <(...) does not reach the reader inside run_sql (bash 3.2)
    LSQL="$DUCKDB_TMP/rms_b2_lad_$$.sql"
    cat "$HERE/05d2_rms_b2_mu.sql" "$HERE/05a_rms_shelf_ladder_core.sql" \
      >"$LSQL"
    for d in "$CD"/module=*; do
      m="${d##*module=}"
      export MODULE=$m CELLS="$d/*.parquet"
      export OUT_LAD="$OUT/lad/year=$r/module=$m.parquet"
      export OUT_COV="$OUT/cov/year=$r/module=$m.parquet"
      rebuild "$OUT_LAD" || continue
      t0=$(date +%s)
      run_sql "$LSQL" "$LOG" >/dev/null
      [ -s "$OUT_LAD" ] && [ -s "$OUT_COV" ] \
        || {
          echo "05d_rms_shelf_b2: no ladder or coverage for $r module $m" >&2
          exit 1
        }
      say "ladders $r module $m | $(($(date +%s) - t0)) s"
    done
    rm -f "$LSQL"
    # Every module with cells has its ladders before the marker is written, and
    # before the cells are deleted
    for d in "$CD"/module=*; do
      m="${d##*module=}"
      [ -s "$OUT/lad/year=$r/module=$m.parquet" ] \
        || {
          echo "05d_rms_shelf_b2: ladders of $r module $m missing" >&2
          exit 1
        }
    done
    touch "$DONE"
    if [ -z "${RMS_B2_KEEP_CELLS:-}" ]; then
      rm -rf "$CD"
      say "cells $r deleted"
    fi
  fi
  # The summaries 05_rms_shelf.sh prints for 2006-2020
  duckdb -bail -c "
  SELECT yq, sum(rev) / 1e6 rev_m, sum(rev_lab) / sum(rev) sh_lab,
    sum(rev_kept) / sum(rev_lab) sh_kept, max(n_stores) n_stores_max,
    sum(n_nostore) n_nostore
  FROM read_parquet('$OUT/cov/year=$r/*.parquet') GROUP BY yq
    ORDER BY yq" | tee -a "$LOG"
  duckdb -bail -c "
  SELECT reach, count(*) n, avg(valid::INT) sh_valid,
    count(DISTINCT key) n_keys, median(n_items) med_items,
    CASE WHEN count(*) <> count(DISTINCT (reach, key, yq, module))
         THEN error('rms ladders $r: duplicate reach x key x quarter x module')
    END chk
  FROM read_parquet('$OUT/lad/year=$r/*.parquet') GROUP BY reach
    ORDER BY reach" \
    | tee -a "$LOG"

  # 4. household x store x module spend, panel year r ------------------------
  export OUT_HS="$OUT/hh_store/panel_year=$r.parquet"
  if rebuild "$OUT_HS"; then
    run_sql "$HERE/05b_rms_shelf_hhstore.sql" "$LOG" >/dev/null
    say "hh_store $r"
  fi
done
say "05d_rms_shelf_b2 done"
