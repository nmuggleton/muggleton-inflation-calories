# Shared by every analysis script: results I/O, console output and outcome
# labels. Sourced, never run alone. estimate/NN_*.R   save_results("NN_name", a
# = ..., b = ...) writes results/NN_name/ section scripts
# load_results("NN_name") reads results/NN_name/ Tables are written by
# write_tex_table() in src/lib/tables.R.
source('src/lib/setup.R')

suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
})
source('src/lib/tables.R') # fmt_num, fmt_p, ...
source('src/lib/theme_print.R') # theme_print, pal, save_fig

RESULTS_DIR <- 'results'
# Last panel year (src/lib/paths.sh)
END_YEAR <- as.integer(Sys.getenv('WP2_END_YEAR', '2024'))

# console output ---------------------------------------------------------------
# One quoted number per line, printf-style, suitable for the in-text scripts'
# logs.
say <- function(fmt, ...) cat(sprintf(fmt, ...), '\n', sep = '')

# results: one folder per estimation script, one CSV per object ----------------
# results/<name>/<object>.csv  data.table / data.frame, matrix, atomic vector
#                              (columns name,value) or list of scalars
# results/<name>/<object>.tex  etable(tex = TRUE) output, one line per line
# results/<name>/_manifest.csv object, kind, file, class, named   (read order)
# results/<name>/_classes.csv  object, column, class, levels      (tables)
# Doubles are written with 15 significant digits (data.table::fwrite).
.LV_SEP <- '\u001f'


save_results <- function(name, ...) {
    x <- list(...)
    stopifnot(
        length(x) > 0,
        !is.null(names(x)),
        all(nzchar(names(x))),
        !anyDuplicated(names(x))
    )
    dir <- file.path(RESULTS_DIR, name)
    unlink(dir, recursive = TRUE)
    dir.create(dir, recursive = TRUE)
    man <- list()
    cls <- list()
    for (k in names(x)) {
        v <- x[[k]]
        if (inherits(v, 'etable_tex') || inherits(v, 'etable')) {
            f <- paste0(k, '.tex')
            writeLines(as.character(v), file.path(dir, f))
            kind <- 'text'
            named <- FALSE
        } else if (is.data.frame(v)) {
            f <- paste0(k, '.csv')
            kind <- 'table'
            named <- FALSE
            v <- as.data.table(v)
            fwrite(v, file.path(dir, f))
            cls[[k]] <- data.table(
                object = k,
                column = names(v),
                class = vapply(v, function(z) class(z)[1], ''),
                levels = vapply(
                    v,
                    function(z) {
                        if (is.factor(z)) {
                            paste(levels(z), collapse = .LV_SEP)
                        } else {
                            NA_character_
                        }
                    },
                    ''
                )
            )
        } else if (is.matrix(v)) {
            f <- paste0(k, '.csv')
            kind <- 'matrix'
            named <- FALSE
            d <- as.data.table(v)
            d[, .row := if (is.null(rownames(v))) {
                NA_character_
            } else {
                rownames(v)
            }]
            setcolorder(d, '.row')
            fwrite(d, file.path(dir, f))
        } else if (is.list(v)) {
            ok <- all(
                vapply(
                    v,
                    function(z) is.atomic(z) && length(z) == 1L,
                    NA
                )
            )
            if (!ok || is.null(names(v))) {
                stop(
                    'save_results(',
                    name,
                    "): '",
                    k,
                    "' is a list that is not a ",
                    'named list of scalars; convert it to a data.table.'
                )
            }
            f <- paste0(k, '.csv')
            kind <- 'list'
            named <- TRUE
            vec <- unlist(v)
        } else if (is.atomic(v) && !is.null(v)) {
            f <- paste0(k, '.csv')
            kind <- 'vector'
            named <- !is.null(names(v))
            vec <- v
        } else {
            stop(
                'save_results(',
                name,
                "): cannot save '",
                k,
                "' (class ",
                class(v)[1],
                ') as CSV; convert it to a data.table.'
            )
        }
        # a list of scalars or a vector: one row per element, its name (NA if
        # unnamed) and its value
        if (kind %in% c('list', 'vector')) {
            fwrite(
                data.table(
                    name = if (is.null(names(vec))) {
                        NA_character_
                    } else {
                        names(vec)
                    },
                    value = as.vector(
                        if (is.factor(vec)) as.character(vec) else vec
                    )
                ),
                file.path(dir, f)
            )
        }
        man[[k]] <- data.table(
            object = k,
            kind = kind,
            file = f,
            class = class(v)[1],
            named = named
        )
    }
    fwrite(rbindlist(man), file.path(dir, '_manifest.csv'))
    fwrite(
        if (length(cls)) {
            rbindlist(cls)
        } else {
            data.table(
                object = character(),
                column = character(),
                class = character(),
                levels = character()
            )
        },
        file.path(dir, '_classes.csv')
    )
    invisible(dir)
}


load_results <- function(name) {
    dir <- file.path(RESULTS_DIR, name)
    if (!dir.exists(dir)) {
        stop(
            'load_results: ',
            dir,
            ' not found; run the estimation script ',
            'src/analysis/estimate/',
            name,
            '.R first.'
        )
    }
    man <- fread(file.path(dir, '_manifest.csv'), na.strings = '')
    cls <- fread(
        file.path(dir, '_classes.csv'),
        na.strings = '',
        colClasses = 'character'
    )
    out <- list()
    for (i in seq_len(nrow(man))) {
        k <- man$object[i]
        f <- file.path(dir, man$file[i])
        out[[k]] <- switch(
            man$kind[i],
            text = structure(readLines(f), class = 'etable_tex'),
            table = {
                cc <- cls[object == k]
                rd <- setNames(
                    ifelse(
                        cc$class %in% c(
                            'integer', 'numeric',
                            'character', 'logical'
                        ),
                        cc$class,
                        'character'
                    ),
                    cc$column
                )
                d <- fread(f, na.strings = '', colClasses = rd)
                # Restore each column's class, and factor levels, using
                # _classes.csv
                for (j in seq_len(nrow(cc))) {
                    col <- cc$column[j]
                    z <- d[[col]]
                    z <- switch(
                        cc$class[j],
                        integer = as.integer(z),
                        numeric = as.numeric(z),
                        character = as.character(z),
                        logical = as.logical(z),
                        factor = factor(
                            z,
                            levels = strsplit(
                                cc$levels[j],
                                .LV_SEP,
                                fixed = TRUE
                            )[[1]]
                        ),
                        Date = as.Date(z),
                        IDate = as.IDate(z),
                        z
                    )
                    set(d, j = col, value = z)
                }
                d
            },
            matrix = {
                d <- fread(f, na.strings = '')
                r <- d$.row
                d[, .row := NULL]
                m <- as.matrix(d)
                if (!anyNA(r)) rownames(m) <- r
                m
            },
            list = ,
            vector = {
                d <- fread(
                    f,
                    na.strings = '',
                    colClasses = c(name = 'character')
                )
                v <- d$value
                v <- switch(
                    man$class[i],
                    integer = as.integer(v),
                    numeric = as.numeric(v),
                    character = as.character(v),
                    logical = as.logical(v),
                    factor = as.character(v),
                    v
                )
                if (isTRUE(man$named[i])) names(v) <- d$name
                if (man$kind[i] == 'list') as.list(v) else v
            }
        )
    }
    out
}

# The headline 2SLS estimate saved in results/01_main/ by 01_main.R (default:
# the calories-per-dollar outcome).
headline_estimate <- function(y = 'd_lkpu') {
    load_results('01_main')$headline[outcome == y, b_iv]
}


# heterogeneity ----------------------------------------------------------------
HET_Y <- c(
    d_lkcal = 'Calories bought (log kcal per AE-day x100)',
    d_lkpu = 'Calories per dollar (log kcal per real $ x100)',
    d_lspend = 'Real food spend (log $ per AE-day x100)',
    d_ed = 'Energy density (kcal/100 g)',
    d_hs = 'Health score'
)
HET_YS <- c(
    d_lkcal = 'Calories bought', d_lkpu = 'Calories per dollar',
    d_lspend = 'Real food spend', d_ed = 'Energy density',
    d_hs = 'Health score'
)
HET_G <- 49L # state clusters in the gate sample (01_main.R)

# robustness -------------------------------------------------------------------
ROB_YS <- c(
    d_lkpu = 'Change log kcal per real $ (x100)',
    d_lspend = 'Change log real spend per AE-day (x100)',
    d_lkcal = 'Change log kcal per AE-day (x100)',
    d_ed = 'Change energy density (kcal/100 g)',
    d_hs = 'Change health score'
)
ROB_SHORT <- c(
    d_lkpu = 'kcal per $', d_lspend = 'Spend',
    d_lkcal = 'Calories', d_ed = 'Energy density',
    d_hs = 'Health score'
)
