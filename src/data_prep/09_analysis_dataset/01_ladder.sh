#!/usr/bin/env bash
# The calorie-price ladder ("the shelf") and each estimation-sample
# household-quarter's position on it: 01a_ladder_upc_kcal.sql once, then
# 01b_ladder.sql once per target quarter (one quarter at a time keeps DuckDB
# spill small). Inputs : final/dim_products_b1, final/upc_nutrients,
# intermediate/hh_upc_q, intermediate/state_upc_q, intermediate/desc_panel
# (est_gate) Outputs: $LADDER_DATA (default
# $WP2_INT)/ladder_upc_kcal/data.parquet .../ladder_hh/year=YYYY_qQ.parquet
# (household-quarters) .../ladder_tier/year=YYYY_qQ.parquet (state x module
# tiers) Env    : LADDER_YEARS="2008 2009" subsets target years;
# LADDER_REBUILD=1 rebuilds existing quarters; LADDER_REBUILD_UK=1 rebuilds
# upc_kcal Usage  : bash src/data_prep/09_analysis_dataset/01_ladder.sh
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
HERE="$PROJECT_ROOT/src/data_prep/09_analysis_dataset"
LOG="$WP2_LOGS/09_ladder.log"
: >"$LOG"
EXT="${LADDER_DATA:-$WP2_INT}"
mkdir -p "$EXT/ladder_upc_kcal" "$EXT/ladder_hh" "$EXT/ladder_tier"
export OUT_UK="$EXT/ladder_upc_kcal/data.parquet"
export UK="$OUT_UK"
export DP="$WP2_INT/desc_panel/*/*.parquet"

# quoted, comma-separated list of the existing panel_year files of a table
files() {
  local tab="$1"
  shift
  local out="" f
  for y in "$@"; do
    f="$WP2_INT/$tab/panel_year=$y.parquet"
    [ -f "$f" ] && out="${out:+$out, }'$f'"
  done
  echo "$out"
}

# 1. UPC version -> kcal and nutrients per package
if [ ! -f "$OUT_UK" ] || [ -n "${LADDER_REBUILD_UK:-}" ]; then
  run_sql "$HERE/01a_ladder_upc_kcal.sql" "$LOG" >/dev/null
fi
duckdb -bail -c "
SELECT count(*) n, count(DISTINCT (upc, upc_ver_uc)) n_key,
       count(DISTINCT item) n_item, avg(is_pl::INT) sh_pl,
       median(kcal_unit) med_kcal, quantile_cont(kcal_unit, .999) p999,
       CASE WHEN count(*) <> count(DISTINCT (upc, upc_ver_uc))
         OR count(*) <= 100000
            THEN error('ladder_upc_kcal: duplicate keys or too few rows')
       END chk
FROM read_parquet('$OUT_UK')" | tee -a "$LOG"

# 2. per target quarter: ladder, tiers and household-quarter metrics
# 2020 is not a target year
ALL_YEARS="$(seq 2005 "$WP2_END_YEAR" | grep -vx 2020 | tr '\n' ' ')"
YEARS="${LADDER_YEARS:-$ALL_YEARS}"
for y in $YEARS; do
  for q in 1 2 3 4; do
    stem="year=${y}_q${q}.parquet"
    export OUT_HH="$EXT/ladder_hh/$stem" OUT_TIER="$EXT/ladder_tier/$stem"
    if [ -f "$OUT_HH" ] && [ -z "${LADDER_REBUILD:-}" ]; then continue; fi
    t0=$(date +%s)
    export YEAR=$y QTR=$q
    HU_FILES="$(files hh_upc_q "$((y - 1))" "$y" "$((y + 1))")"
    SU_FILES="$(files state_upc_q \
      "$((y - 3))" "$((y - 2))" "$((y - 1))" "$y" "$((y + 1))")"
    if [ -z "$HU_FILES" ] || [ -z "$SU_FILES" ]; then
      echo "01_ladder: no hh_upc_q / state_upc_q files for $y" >&2
      exit 1
    fi
    export HU_FILES SU_FILES
    run_sql "$HERE/01b_ladder.sql" "$LOG" >/dev/null
    # one row per estimation-sample household-quarter, no drops
    duckdb -bail -c "
    WITH h AS (SELECT * FROM read_parquet('$OUT_HH')),
    g AS (SELECT count(*) n FROM read_parquet('$DP', hive_partitioning = true)
          WHERE est_gate AND yq = $((y * 10 + q)))
    SELECT count(*) n, count(DISTINCT (household_code, yq)) n_key,
      avg((b_spend IS NOT NULL)::INT) sh_base,
      sum(sav25) / sum(b_spend) sav25, sum(sav50) / sum(b_spend) sav50,
      avg(f_cheap) f_cheap, avg(f_prem) f_prem, avg(e_own) e_own,
      CASE WHEN count(*) <> count(DISTINCT (household_code, yq))
             OR count(*) <> (SELECT n FROM g)
             OR avg((b_spend IS NOT NULL)::INT) <= .9
             -- tier inflation needs t-quarter prices: all-missing meant no
             -- product of that year met the ladder
             OR avg((f_all IS NOT NULL)::INT) <= .9
           THEN error('ladder_hh ${y}q${q}: key, coverage, base or tier '
                      || 'check failed')
      END chk
    FROM h" | tee -a "$LOG"
    echo "year $y q$q | $((($(date +%s) - t0) / 60)) min" | tee -a "$LOG"
  done
done
