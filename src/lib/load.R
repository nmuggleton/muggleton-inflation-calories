# Load WP2 built datasets into data.table. Paths come from the same environment
# variables as src/lib/paths.sh.
suppressPackageStartupMessages({
    library(data.table)
    library(DBI)
    library(duckdb)
})

wp2_paths <- function() {
    # WP2_DATA is defined in config/users/<name>.sh, sourced by
    # src/lib/paths.sh.
    data <- Sys.getenv('WP2_DATA')
    if (!nzchar(data)) {
        stop(
            'WP2_DATA is not set: run via src/lib/run_r.sh, ',
            'or source src/lib/paths.sh first'
        )
    }
    list(data = data, final = file.path(data, 'final'))
}

wp2_con <- function() {
    # DuckDB in memory only, with views into the final Parquet (nothing is
    # copied).
    p <- wp2_paths()
    stopifnot(dir.exists(p$final))
    con <- DBI::dbConnect(duckdb::duckdb())
    # Per-process memory limits (src/lib/paths.sh; src/lib/run_r.sh sets them
    # conservatively, 12GB/4, so two scripts can run in parallel).
    DBI::dbExecute(
        con,
        sprintf(
            "SET memory_limit = '%s'",
            Sys.getenv('DUCKDB_MEMORY', '20GB')
        )
    )
    DBI::dbExecute(
        con,
        sprintf(
            'SET threads = %s',
            Sys.getenv('DUCKDB_THREADS', '8')
        )
    )
    tmp <- Sys.getenv('DUCKDB_TMP', '')
    if (nzchar(tmp)) {
        dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
        DBI::dbExecute(con, sprintf("SET temp_directory = '%s'", tmp))
    }
    fp <- function(x) file.path(p$final, x)
    views <- c(
        purchases = sprintf(
            paste0(
                "read_parquet('%s', union_by_name = true, ",
                'hive_partitioning = false)'
            ),
            fp('purchases/*/data.parquet')
        ),
        household_year = sprintf("'%s'", fp('household_year.parquet')),
        sample_flags = sprintf("'%s'", fp('sample_flags.parquet')),
        dim_products_b1 = sprintf("'%s'", fp('dim_products_b1.parquet')),
        dim_products_b2 = sprintf("'%s'", fp('dim_products_b2.parquet')),
        nutrition_b2 = sprintf("'%s'", fp('nutrition_b2.parquet')),
        xwalk_module = sprintf("'%s'", fp('xwalk_module.parquet')),
        # Built by src/data_prep/07_controls/01_controls.R; one file per year
        analysis_hh_quarter = sprintf(
            paste0(
                "read_parquet('%s', hive_partitioning = true)"
            ),
            fp('analysis_hh_quarter/*/*.parquet')
        ),
        hh_quarter_outcomes = sprintf(
            paste0(
                "read_parquet('%s', hive_partitioning = true)"
            ),
            fp('hh_quarter_outcomes/*/*.parquet')
        ),
        hh_quarter_treatments = sprintf(
            "'%s'",
            fp('hh_quarter_treatments.parquet')
        ),
        upc_nutrients = sprintf("'%s'", fp('upc_nutrients.parquet')),
        module_nutrient_fallback = sprintf(
            "'%s'",
            fp('module_nutrient_fallback.parquet')
        ),
        xwalk_module_foodgroup = sprintf(
            "'%s'",
            fp('xwalk_module_foodgroup.parquet')
        ),
        # Helper file of src/data_prep/07_controls/03_sample_panel.sql; one file
        # per year
        desc_panel = sprintf(
            paste0(
                "read_parquet('%s', hive_partitioning = true)"
            ),
            file.path(p$data, 'intermediate', 'desc_panel', '*', '*.parquet')
        )
    )
    have <- vapply(
        views,
        function(v) {
            f <- sub("^.*'(.*)'.*$", '\\1', v)
            length(Sys.glob(f)) > 0
        },
        logical(1)
    )
    views <- views[have]
    for (v in names(views)) {
        DBI::dbExecute(
            con,
            sprintf(
                'CREATE VIEW %s AS SELECT * FROM %s',
                v,
                views[[v]]
            )
        )
    }
    con
}

wp2_sql <- function(query) {
    con <- wp2_con()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    data.table::setDT(DBI::dbGetQuery(con, query))[]
}
