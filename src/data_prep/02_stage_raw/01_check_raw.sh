#!/usr/bin/env bash
# Stop early unless every raw Kilts file the build reads is present, at the size
# of the download the paper used.
#
# Two trees:
#   NIELSEN_RAW  local copy: config/raw_files.tsv (Consumer Panel 2004-2021,
#                PanelViews 2011-2023, reference documents, Retail Scanner
#                2022 product files)
#   RMS_RAW      read in place, too large to copy: config/raw_files_rms.tsv
#                (Consumer Panel 2022-2024, the PanelViews economic surveys
#                2015-2019, and the Retail Scanner, about 9 TB)
#
# Usage: bash src/data_prep/02_stage_raw/01_check_raw.sh
#        CHECK_RMS_MOVEMENT=1 bash src/data_prep/02_stage_raw/01_check_raw.sh
set -euo pipefail
source "$(dirname "$0")/../../lib/paths.sh"

# check_list <root> <manifest> <skip-movement: 0 or 1> prints MISSING / WRONG
# SIZE lines and returns 1 if any file is bad
check_list() {
  local root="$1" list="$2" skip_movement="$3"
  local bad=0 n=0 rel bytes source table f size
  while IFS=$'\t' read -r rel bytes source table; do
    [ "$rel" = relpath ] && continue
    if [ "$skip_movement" = 1 ] && [ "${table:-}" = movement ]; then
      continue
    fi
    n=$((n + 1))
    f="$root/$rel"
    if [ ! -f "$f" ]; then
      echo "MISSING    $rel"
      bad=$((bad + 1))
      continue
    fi
    size="$(wc -c <"$f" | tr -d ' ')"
    if [ "$size" != "$bytes" ]; then
      echo "WRONG SIZE $rel ($size bytes, expected $bytes)"
      bad=$((bad + 1))
    fi
  done <"$list"
  echo "checked $n files under $root: $bad missing or incomplete"
  # Exit codes wrap at 256, so return 0 or 1 rather than the count of problems
  [ "$bad" -eq 0 ]
}

# Local copy -------------------------------------------------------------------

[ -d "$NIELSEN_RAW" ] || {
  echo "NIELSEN_RAW not found: $NIELSEN_RAW" >&2
  exit 1
}
check_list "$NIELSEN_RAW" "$PROJECT_ROOT/config/raw_files.tsv" 0 || {
  echo "raw files missing or incomplete under $NIELSEN_RAW" >&2
  exit 1
}

# Files read in place ----------------------------------------------------------

if [ -z "${RMS_RAW:-}" ] || [ ! -d "$RMS_RAW" ]; then
  echo "RMS_RAW is not set or not mounted: the Consumer Panel 2022-2024" \
    "and the Retail Scanner are not checked (steps 02, 05 and 09 need them)"
  exit 0
fi
skip=1
[ "${CHECK_RMS_MOVEMENT:-0}" = 1 ] && skip=0
check_list "$RMS_RAW" "$PROJECT_ROOT/config/raw_files_rms.tsv" "$skip" || {
  echo "raw files missing or incomplete under $RMS_RAW" >&2
  exit 1
}
echo "raw files: all present"
