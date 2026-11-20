# The R packages the paper uses, at the exact versions its results were produced
# with (R 4.4.2). Run once per machine: Rscript src/lib/packages.R Installs any
# package that is missing or at another version.
pins <- c(
    data.table = '1.18.6.1', DBI = '1.3.0', duckdb = '1.5.5',
    arrow = '25.0.0', fixest = '0.14.2', ggplot2 = '4.0.3',
    scales = '1.4.0', patchwork = '1.3.2', readxl = '1.5.0.1'
)
repos <- 'https://cloud.r-project.org'
if (getRversion() != '4.4.2') {
    warning('results were produced with R 4.4.2; this is R ', getRversion())
}
have <- function(p) {
    if (requireNamespace(p, quietly = TRUE)) {
        as.character(packageVersion(p))
    } else {
        NA_character_
    }
}
inst <- vapply(names(pins), have, '')
todo <- names(pins)[is.na(inst) | inst != pins]
if (length(todo)) {
    if (!requireNamespace('remotes', quietly = TRUE)) {
        install.packages('remotes', repos = repos)
    }
    # install_version(upgrade = "never") leaves dependencies as they are, so a
    # too-old gtable or cpp11 stops ggplot2 / readxl installing.
    db <- available.packages(repos = repos)
    deps <- unique(
        unlist(
            tools::package_dependencies(
                todo,
                db = db,
                which = c('Depends', 'Imports', 'LinkingTo'),
                recursive = TRUE
            )
        )
    )
    deps <- setdiff(
        deps,
        c(
            names(pins), rownames(
                installed.packages(
                    priority = 'base'
                )
            )
        )
    )
    deps <- intersect(deps, rownames(db))
    old <- vapply(
        deps,
        function(d) {
            !requireNamespace(d, quietly = TRUE) ||
                packageVersion(d) < db[d, 'Version']
        },
        logical(1)
    )
    if (any(old)) install.packages(deps[old], repos = repos)
    for (p in todo) {
        remotes::install_version(
            p,
            pins[[p]],
            repos = repos,
            upgrade = 'never'
        )
    }
}
print(
    data.frame(
        package = names(pins),
        pinned = pins,
        installed = vapply(names(pins), have, '')
    ),
    row.names = FALSE
)
