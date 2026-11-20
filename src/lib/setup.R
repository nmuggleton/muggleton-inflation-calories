# Sets up the R session for the scripts in src/. Every analysis script starts by
# sourcing src/lib/format.R, which sources this file first, so each script can
# be run in RStudio as it stands.


# Check the working directory --------------------------------------------------

if (!file.exists('src/lib/paths.sh')) {
    stop(
        'The working directory is not the repository root. ',
        'Open inflation.Rproj, or setwd() to the repository root.'
    )
}


# Read the data paths ----------------------------------------------------------

if (!nzchar(Sys.getenv('WP2_DATA'))) {
    # Run src/lib/paths.sh in bash and capture every variable it exports.
    exported <- suppressWarnings(system2(
        'bash',
        c('-c', shQuote('source src/lib/paths.sh > /dev/null 2>&1 && env')),
        stdout = TRUE
    ))
    exported <- exported[grepl('^[A-Z0-9_]+=', exported)]

    values <- sub('^[A-Z0-9_]+=', '', exported)
    names(values) <- sub('=.*$', '', exported)

    # Keep only the variables the R scripts actually use
    needed <- c(
        'PROJECT_ROOT', 'WP2_DATA', 'WP2_INT', 'WP2_FINAL', 'WP2_LOGS',
        'WP2_END_YEAR', 'NIELSEN_RAW', 'LOCAL_DATA', 'SHIFTSHARE_INT',
        'DUCKDB_TMP', 'DUCKDB_MAX_TEMP', 'DUCKDB_MEMORY', 'DUCKDB_THREADS'
    )
    values <- values[intersect(needed, names(values))]

    have_paths <- !is.na(values['WP2_DATA']) && nzchar(values['WP2_DATA'])
    if (have_paths) {
        do.call(Sys.setenv, as.list(values))
        message(
            'Data paths read from config/users/ (WP2_DATA = ',
            Sys.getenv('WP2_DATA'),
            ')'
        )
    } else {
        message(
            'No data paths (no config/users/<name>.sh): output scripts ',
            'run from results/; estimation scripts need the data.'
        )
    }

    rm(exported, values, needed, have_paths)
}


# Record the software versions -------------------------------------------------

# One line at the top of every log with the R and package versions (pinned in
# src/lib/packages.R), so that each result can be traced to the environment that
# produced it
versions <- c(R = format(getRversion()))
for (pkg in c('data.table', 'fixest', 'duckdb', 'ggplot2')) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        versions[pkg] <- format(packageVersion(pkg))
    }
}
cat(paste(names(versions), versions, collapse = ' | '), '\n')
rm(versions, pkg)
