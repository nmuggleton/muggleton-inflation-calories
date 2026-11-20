#!/usr/bin/env bash
# Run a shift-share (appendix) R script through src/lib/run_r.sh with its data
# location set. Shift-share datasets are rebuildable build products, so they
# live under $WP2_DATA/intermediate and never in the project folder. usage:
# src/lib/run_r_shiftshare.sh src/analysis/estimate/14_shiftshare.R [log_stem]
set -uo pipefail
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck disable=SC1091
source "$PROJECT_ROOT/src/lib/paths.sh"
export SHIFTSHARE_INT="${SHIFTSHARE_INT:-$WP2_INT/shiftshare}"
mkdir -p "$SHIFTSHARE_INT"
exec "$(dirname "$0")/run_r.sh" "$@"
