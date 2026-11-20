#!/usr/bin/env bash
# Fetch public context data for the f1 package: BLS LAUS county and state
# unemployment (download.bls.gov/pub/time.series/la) BLS CPI-U all current
# series incl. regional/metro food at home (.../cu) BEA quarterly state personal
# income and FHFA state HPI via FRED OMB 2013 CBSA delineation file
# (www2.census.gov) Output : ${LOCAL_DATA:-$WP2_DATA/intermediate}/local_raw/
# (never OneDrive), plus a manifest with URL, bytes and download time
# (logs/18a_local_download.log). Run    : [BLS_UA="<descriptive agent with
# contact e-mail>"] \ src/data_prep/08_external/01_download.sh \ >
# logs/18a_local_download.log 2>&1 BLS rejects scripted requests without a
# contact address in the User-Agent (its published usage policy).
set -euo pipefail
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
# shellcheck disable=SC1091
source "$PROJECT_ROOT/src/lib/paths.sh"
BLS_UA="${BLS_UA:-ECHO WP2 research naomi.muggleton@gmail.com}"
# LOCAL_DATA overrides the data root when $WP2_INT is read-only (a read-only checkout)
out="${LOCAL_DATA:-$WP2_INT}/local_raw"
mkdir -p "$out/fred"
# The manifest records each file's first download; cached files keep their row
man="$out/manifest.tsv"
[[ -s "$man" ]] || printf "file\turl\tbytes\tdownloaded\n" >"$man"

get() { # get <url> <dest> [bls]
  local url="$1" dest="$2"
  [[ -s "$dest" ]] && return 0 # already fetched in this data root
  if [[ "${3:-}" == "bls" ]]; then
    curl -sSf -m 900 --retry 2 -A "$BLS_UA" -o "$dest" "$url"
  else
    curl -sSf -m 120 --retry 2 -o "$dest" "$url"
  fi
  printf "%s\t%s\t%s\t%s\n" "$(basename "$dest")" "$url" \
    "$(wc -c <"$dest" | tr -d ' ')" "$(date -u '+%F %T UTC')" >>"$man"
}

# Vaghul and Zipperer's historical minimum wages (master branch)
mw="$WP2_INT/minwage_raw"
mkdir -p "$mw"
gh_repo="benzipperer/historicalminwage"
gh="https://raw.githubusercontent.com/$gh_repo/master/rawdata"
for f in StateMinimumWage_Changes.xlsx FederalMinimumWage_Changes.xlsx; do
  get "$gh/$f" "$mw/$f"
done

bls="https://download.bls.gov/pub/time.series"
for f in cu.area cu.item cu.series cu.data.0.Current; do
  get "$bls/cu/$f" "$out/$f" bls
done
for f in la.area la.series la.data.3.AllStatesS la.data.64.County; do
  get "$bls/la/$f" "$out/$f" bls
done

# FRED: {ST}OTOT = BEA total personal income, quarterly SAAR ($ mn);
#       {ST}STHPI = FHFA all-transactions house price index, quarterly.
states="AL AZ AR CA CO CT DE DC FL GA ID IL IN IA KS KY LA ME MD MA MI MN \
MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV \
WI WY"
fred="https://fred.stlouisfed.org/graph/fredgraph.csv?id="
for s in $states; do
  for k in OTOT STHPI; do
    id="${s}${k}"
    # NCOTOT on FRED is a bank charge-off series; North Carolina income is
    # NCOTOTL
    [[ "$id" == "NCOTOT" ]] && id="NCOTOTL"
    get "$fred$id" "$out/fred/${s}${k}.csv"
  done
done
# OMB February 2013 CBSA delineation (county -> CBSA); the BLS 2018 CPI
# geography is built on these CBSA definitions.
get "https://www2.census.gov/programs-surveys/metro-micro/geographies/\
reference-files/2013/delineation-files/list1.xls" "$out/cbsa_list1_2013.xls"
awk -F'\t' '{printf "%-24s %s\n", $1, $3}' "$man" | head -20
echo "files: $(($(wc -l <"$man") - 1))"
