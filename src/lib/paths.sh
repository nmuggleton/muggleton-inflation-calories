#!/usr/bin/env bash
# Central path configuration for the WP2 NielsenIQ build. Every data-prep and
# estimation script sources this file, directly or via run_r.sh.
#
# Layout
#   code, logs, docs     -> this repository
#   raw NielsenIQ files  -> NIELSEN_RAW (read-only, never copied)
#   raw Retail Scanner   -> RMS_RAW (default NIELSEN_RAW; set it when RMS
#                           lives in another tree, e.g. a network share)
#   built data           -> WP2_DATA (intermediate/, final/)
#   python venv, spill   -> WP2_VENV, DUCKDB_TMP (local disk)

# The repository root: two folders up from this file
_this_dir="$(dirname "${BASH_SOURCE[0]}")"
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$_this_dir/../.." && pwd)}"
unset _this_dir
export PROJECT_ROOT

WP2_USER="${WP2_USER:-${USER:-$(whoami)}}"
_cfg="$PROJECT_ROOT/config/users/$WP2_USER.sh"
if [ -f "$_cfg" ]; then
  # shellcheck disable=SC1090
  source "$_cfg"
elif [ -z "${WP2_DATA:-}" ]; then
  echo "paths.sh: no config/users/$WP2_USER.sh. Copy config/users/TEMPLATE.sh" \
    "to that name, or set WP2_USER to an existing file's name." >&2
  return 1 2>/dev/null || exit 1
fi
unset _cfg
export WP2_USER NIELSEN_RAW="${NIELSEN_RAW:-}" WP2_DATA
export RMS_RAW="${RMS_RAW:-$NIELSEN_RAW}"
export WP2_VENV="${WP2_VENV:-$HOME/venvs/inflation_wp2}"
export DUCKDB_TMP="${DUCKDB_TMP:-$HOME/tmp/duckdb_wp2}"
export DUCKDB_THREADS="${DUCKDB_THREADS:-8}"
export DUCKDB_MEMORY="${DUCKDB_MEMORY:-20GB}"
# Cap on DuckDB spill files: fail with a clear error rather than fill the disk
# (a full disk makes macOS start evicting OneDrive files, the repo included)
export DUCKDB_MAX_TEMP="${DUCKDB_MAX_TEMP:-20GiB}"
# Last panel year of the estimation sample (2024, with the 2022-2024 panel).
export WP2_END_YEAR="${WP2_END_YEAR:-2024}"
# Optional: restrict the annual files to these panel years, e.g. "2019 2021"
export WP2_YEARS="${WP2_YEARS:-}"

# Sub-directories of the data tree.
export WP2_INT="$WP2_DATA/intermediate"
export WP2_FINAL="$WP2_DATA/final"
export WP2_LOGS="${WP2_LOGS:-$PROJECT_ROOT/logs}"
# Build products of the external-data and shift-share steps
export LOCAL_DATA="${LOCAL_DATA:-$WP2_INT}"
export SHIFTSHARE_INT="${SHIFTSHARE_INT:-$WP2_INT/shiftshare}"

mkdir -p "$WP2_LOGS" "$DUCKDB_TMP"

# DuckDB settings prepended to every SQL run (spill to LOCAL disk)
duck_settings() {
  cat <<EOF
SET threads = ${DUCKDB_THREADS};
SET memory_limit = '${DUCKDB_MEMORY}';
SET temp_directory = '${DUCKDB_TMP}';
SET max_temp_directory_size = '${DUCKDB_MAX_TEMP}';
SET preserve_insertion_order = false;
EOF
}

# Substitute ${VAR} placeholders from the environment (macOS has no envsubst;
# unknown placeholders are left as they are, so that errors stay visible).
subst_env() {
  perl -pe 's/\$\{(\w+)\}/exists $ENV{$1} ? $ENV{$1} : $&/ge' <"$1"
}

# Run a SQL file with the settings and ${VAR} substitution, appending to a log.
# usage: run_sql file.sql logfile [database]
run_sql() {
  local f="$1" log="${2:-/dev/null}" db="${3:-}"
  {
    duck_settings
    subst_env "$f"
  } | duckdb -bail $db 2>&1 | tee -a "$log"
  # An unreadable SQL file leaves DuckDB an empty script, which exits 0
  local st=("${PIPESTATUS[@]}")
  [ "${st[0]}" -eq 0 ] || {
    echo "run_sql: cannot read $f" >&2
    return 1
  }
  return "${st[1]}"
}
