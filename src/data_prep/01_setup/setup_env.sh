#!/usr/bin/env bash
# One-off, per machine (my laptop AND desktop). Creates the Python venv
# ($WP2_VENV, default ~/venvs/inflation_wp2), then checks the DuckDB CLI version
# and installs the pinned R packages (src/lib/packages.R).
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"
PY="${PYTHON:-python3}"
if [ ! -d "$WP2_VENV" ]; then "$PY" -m venv "$WP2_VENV"; fi
source "$WP2_VENV/bin/activate"
pip install --upgrade pip
pip install -r "$PROJECT_ROOT/config/requirements.txt"
# DuckDB CLI: the version must match the duckdb Python package in
# config/requirements.txt
DUCK_V=1.5.6
if ! duckdb --version 2>/dev/null | grep -q "v$DUCK_V"; then
  echo "Install DuckDB CLI v$DUCK_V:" \
    "https://github.com/duckdb/duckdb/releases/tag/v$DUCK_V"
fi
Rscript "$PROJECT_ROOT/src/lib/packages.R"
echo "venv: $WP2_VENV"
duckdb --version || true
