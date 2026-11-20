#!/usr/bin/env bash
# The shift-share (Bartik) exposure instrument: household base-basket module
# shares x national leave-own-state-out module price changes.
# 03a_shiftshare_shocks.sql      module price shocks, every target year first
# (each instrument year also needs the shocks of Y-1 and Y+1)
# 03b_shiftshare_hhmod.sql       base-quarter module spend (t-4 and t-8)
# 03c_shiftshare_instrument.sql  household instrument, per target year Inputs :
# intermediate/hh_upc_q, intermediate/state_upc_q, ref/pl_crosswalk,
# final/dim_products_b1/b2, final/f1_analysis (02_build.sql) Outputs:
# $SHIFTSHARE_INT (default $WP2_INT/shiftshare)/{shock_nat,
# shock_st,shock_div,bartik}/year=YYYY.parquet Env    : SHIFTSHARE_YEARS="2008
# 2009" restricts target years; SHIFTSHARE_FORCE=1 rebuilds existing files Usage
# : bash src/data_prep/09_analysis_dataset/03_shiftshare.sh
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
HERE="$PROJECT_ROOT/src/data_prep/09_analysis_dataset"
export SHIFTSHARE_INT="${SHIFTSHARE_INT:-$WP2_INT/shiftshare}"
LOG="$WP2_LOGS/09_shiftshare.log"
: >"$LOG"
for o in shock_nat shock_st shock_div bartik; do
  mkdir -p "$SHIFTSHARE_INT/$o"
done
F1="read_parquet(
  '$WP2_FINAL/f1_analysis/*/*.parquet', hive_partitioning = true
)"
YEARS="${SHIFTSHARE_YEARS:-$(seq -s ' ' 2005 "$WP2_END_YEAR")}"

# quoted, comma-separated list of the existing files; fails if none exist
files() {
  local out="" f
  for f in "$@"; do [ -f "$f" ] && out="${out:+$out, }'$f'"; done
  [ -n "$out" ] || {
    echo "no input files among: $*" >&2
    return 1
  }
  echo "$out"
}
pq() { echo "$SHIFTSHARE_INT/$1/year=$2.parquet"; }
duck() {
  {
    duck_settings
    cat
  } | duckdb -bail 2>&1 | tee -a "$LOG"
  return "${PIPESTATUS[1]}"
}

# state -> census division (modal division of the state's panel rows)
STDIV="CREATE TEMP TABLE stdiv AS
  SELECT state, mode(region_code) AS division FROM $F1
  WHERE state IS NOT NULL AND region_code IS NOT NULL GROUP BY state;"

# 1. module price shocks
for y in $YEARS; do
  if [ ! -f "$(pq shock_nat "$y")" ] || [ "${SHIFTSHARE_FORCE:-}" = 1 ]; then
    export YEAR=$y
    SU_DIR="$WP2_INT/state_upc_q"
    export SU_FILES="$(files \
      "$SU_DIR"/panel_year={$((y - 1)),$y,$((y + 1))}.parquet)"
    {
      echo "$STDIV"
      subst_env "$HERE/03a_shiftshare_shocks.sql"
    } | duck >/dev/null
  fi
  duck <<EOF
SELECT $y AS year, count(*) AS n, count(A) AS n_def, sum(n_items) AS items,
  median(A / W) AS med_g, quantile_cont(A / W, 0.05) AS p05,
  quantile_cont(A / W, 0.95) AS p95,
  CASE WHEN count(*) <> count(DISTINCT (module, yq))
    OR count(A) <= 0.9 * count(*)
       THEN error('shock_nat $y: duplicate keys or too few defined shocks')
  END AS chk
FROM read_parquet('$(pq shock_nat "$y")');
EOF
done

# 2. household instrument
for y in $YEARS; do
  if [ ! -f "$(pq bartik "$y")" ] || [ "${SHIFTSHARE_FORCE:-}" = 1 ]; then
    {
      echo "CREATE TEMP TABLE hq AS SELECT household_code, yq, state,
              region_code AS division FROM $F1 WHERE year = $y;"
      for k in 4 8; do
        by=$((y - k / 4))
        if [ "$by" -ge 2004 ]; then
          HU_DIR="$WP2_INT/hh_upc_q"
          TAG=$k LAG=$((10 * k / 4)) BASEYEAR=$by \
            HU_FILES="$(files \
              "$HU_DIR"/panel_year={$((by - 1)),$by,$((by + 1))}.parquet)" \
            subst_env "$HERE/03b_shiftshare_hhmod.sql"
          # spend without a module must be negligible
          echo "SELECT $by AS base_year, $k AS lag_q, count(*) AS n,
                  coalesce(sum(spend) FILTER (module IS NULL), 0)
                    / sum(spend) AS sh_na,
                  CASE WHEN coalesce(sum(spend) FILTER (module IS NULL), 0)
                            / sum(spend) >= 0.01
                       THEN error('hm$k: spend without a module') END AS chk
                FROM hm$k;"
        else
          echo "CREATE TEMP TABLE hm$k (household_code INTEGER, yq BIGINT,
                  module INTEGER, spend DOUBLE);"
        fi
      done
      YEAR=$y \
        NAT_FILES="$(files "$(pq shock_nat $((y - 1)))" "$(pq shock_nat "$y")" \
          "$(pq shock_nat $((y + 1)))")" \
        ST_FILES="$(files "$(pq shock_st $((y - 1)))" "$(pq shock_st "$y")" \
          "$(pq shock_st $((y + 1)))")" \
        subst_env "$HERE/03c_shiftshare_instrument.sql"
    } | duck >/dev/null
  fi
  # every has_base household-quarter has a base basket unless its base
  # quarter had no qualifying food line; allow 6% (T2 is valid for 96%)
  duck <<EOF
SELECT $y AS year, count(*) AS n,
  (SELECT count(*) FROM $F1 WHERE year = $y) AS n_hq,
  avg(S4_st) AS s4, avg(B4_st) AS b4, stddev(B4_st) AS sd_b4,
  corr(B4_st, B4_nat) AS r_nat, corr(B4_st, B4_div) AS r_div,
  count(B8_st) AS n_b8, count(B4_lag) AS n_lag, count(B4_lead) AS n_lead,
  CASE WHEN count(*) <> count(DISTINCT (household_code, yq))
         OR count(*) < 0.94 * (SELECT count(*) FROM $F1 WHERE year = $y)
         OR min(S4_st) < 0 OR avg(S4_st) > 1
       THEN error('bartik $y: validation failed') END AS chk
FROM read_parquet('$(pq bartik "$y")');
EOF
done
