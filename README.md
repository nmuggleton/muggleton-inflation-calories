# Households absorb food inflation without trading down to cheaper calories

Code and estimates for the paper. No household data are included: the
NielsenIQ licence does not allow it. Every table, figure and number in the
text can be rebuilt from the saved estimates in `results/` without the data.

## Rebuild the outputs (no data needed)

    Rscript src/lib/packages.R
    OUTPUTS_ONLY=1 bash src/analysis/run_analysis.sh

Tables go to `manuscript/tables`, figures to `manuscript/figures`. Each
section folder under `src/analysis` has an `in_text.R` that prints the numbers
quoted in that section of the paper.

## Re-estimate (needs the data)

The scripts in `src/analysis/estimate` read the analysis dataset built by
`src/data_prep`. Set its location in `config/users/<login>.sh` (copy
`TEMPLATE.sh`), then

    bash src/analysis/run_analysis.sh

Start with `estimate/01_main.R`, the headline. Each script checks its sample
against `config/expected_n.tsv` and stops if it differs. Most take under a
minute; `31_state_size.R` takes about eight.

## Rebuild from the raw files

    bash src/run_all.sh

Needs the Kilts NielsenIQ files listed in `config/raw_files.tsv` and
`config/raw_files_rms.tsv`, about 100 GB of local disk for the build, and the
Retail Scanner (about 9 TB) read in place. `src/README.md` lists the steps. The
build ends with `src/tools/check_data.py`, which compares every built table
with `config/checksums.json`.

## Software

R 4.4.2 with data.table, fixest, duckdb, ggplot2, patchwork, scales, arrow,
readxl and DBI (versions pinned in `src/lib/packages.R`); DuckDB 1.5.6; Python
3.12 with `config/requirements.txt`.

## Data

The Kilts NielsenIQ Consumer Panel and Retail Scanner data are available to
academic researchers through the Kilts Center for Marketing at Chicago Booth
(https://www.chicagobooth.edu/research/kilts/datasets/nielsen). The conclusions
drawn from the NielsenIQ data are those of the researcher(s) and do not reflect
the views of NielsenIQ. NielsenIQ is not responsible for, had no role in, and
was not involved in analyzing and preparing the results reported herein.

Public data (BLS, BEA and FHFA series via FRED, Census delineations, Vaghul and
Zipperer minimum wages) are downloaded by `src/data_prep/08_external`.
