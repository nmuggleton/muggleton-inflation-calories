# Machine-specific settings for the build. Copy it to config/users/<your login
# name>.sh (the output of `whoami`), fill it in, and commit it. src/lib/paths.sh
# loads config/users/${WP2_USER:-$USER}.sh.

# Raw Kilts NielsenIQ files (under licence, so they stay off GitHub): a folder
# holding the files listed in config/raw_files.tsv, at those relative paths.
: "${NIELSEN_RAW:=$HOME/data/inflation/raw}"
# Files read in place (config/raw_files_rms.tsv): the Consumer Panel 2022-2024
# and the Retail Scanner (RMS), in a tree with the same layout, e.g. a network
# share.
: "${WP2_DATA:=$HOME/data/inflation/data}"
# Python venv and DuckDB spill space; both should be on a local disk
: "${WP2_VENV:=$HOME/venvs/inflation_wp2}"
: "${DUCKDB_TMP:=$HOME/data/inflation/tmp}"
# Resources: about 2 GB of DUCKDB_MEMORY per thread. More threads than that make
# the large aggregations spill to disk (bounded by DUCKDB_MAX_TEMP).
: "${DUCKDB_MEMORY:=12GB}"
: "${DUCKDB_THREADS:=6}"
