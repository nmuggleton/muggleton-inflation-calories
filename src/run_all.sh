#!/usr/bin/env bash
# run_all.sh -- the master file: data preparation, then the analysis, then the
# manuscript and SI. Each stage can also be run on its own.
#
# Usage:  bash src/run_all.sh
#         SKIP_DATA_PREP=1 bash src/run_all.sh      # dataset already built
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
if [ "${SKIP_DATA_PREP:-0}" != 1 ]; then
  bash "$HERE/data_prep/run_data_prep.sh"
fi
bash "$HERE/analysis/run_analysis.sh"
bash "$HERE/tools/compile.sh"
