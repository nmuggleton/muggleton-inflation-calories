# Number formatting, figure export and table helpers shared by all analysis
# scripts.
suppressPackageStartupMessages(library(data.table))

# number formatting ------------------------------------------------------------
# d may be one number or one per value (health-score rows need more)
fmt_num <- function(x, d = 2, big = FALSE) {
    d <- rep_len(d, length(x))
    out <- vapply(
        seq_along(x),
        function(i) {
            formatC(
                x[i],
                format = 'f',
                digits = d[i],
                big.mark = if (big) ',' else ''
            )
        },
        ''
    )
    out[is.na(x)] <- NA_character_
    out
}

# Bounded quantities only: stops if any |x| > 1.
fmt_bounded <- function(x, d = 3) {
    if (any(abs(x) > 1, na.rm = TRUE)) {
        stop('fmt_bounded(): value outside [-1, 1]; use fmt_num().')
    }
    sub('^(-?)0\\.', '\\1.', fmt_num(x, d))
}

# APA p-value: "< .001" below the floor, otherwise no leading zero.
fmt_p <- function(p, d = 3) {
    out <- fmt_bounded(p, d)
    out[!is.na(p) & p < 10^-d] <- paste0('< .', strrep('0', d - 1), '1')
    out
}


# etable post-processing -------------------------------------------------------
# Strip the leading zero from fit-statistic rows (the R-squared variants are
# bounded). Use in etable() as postprocess.tex = tex_fit_apa.
tex_fit_apa <- function(x) {
    i <- grepl('R\\$\\^2\\$', x)
    x[i] <- gsub('(?<![0-9.])0\\.(?=[0-9])', '.', x[i], perl = TRUE)
    x
}

# figure export ----------------------------------------------------------------
# 127 mm is 0.85 of the 5.9 in text width of the draft style (paper.sty), so
# type prints at 1:1.
FIG_W_MM <- 127
FIG_H_MM <- 76
save_fig <- function(p, stem, w = FIG_W_MM, h = FIG_H_MM) {
    dir.create('manuscript/figures', showWarnings = FALSE)
    ggplot2::ggsave(
        file.path('manuscript', 'figures', paste0(stem, '.pdf')),
        plot = p,
        width = w,
        height = h,
        units = 'mm',
        device = grDevices::cairo_pdf
    )
}

# tables (booktabs + siunitx) --------------------------------------------------
# Every table in the paper is written by write_tex_table().
#
# dt       data.table. Column 1 is the row label. Every other column holds
#          numbers already rounded for display, as text: fmt_num(x, 3),
#          fmt_p(p). Plain numbers only (no commas, brackets or %): siunitx
#          groups the digits and aligns every column on the decimal point.
#          "< .001" is allowed, and so is text either side of the number
#          in braces: parens() gives "{(}0.014{)}". A cell that is one
#          brace group, e.g. "{OLS}" or "{$\checkmark$}", is printed as text
#          and ignored when sizing the column. A cell can also span columns,
#          "\multicolumn{2}{c}{OLS}", with the cells it covers set to ".skip".
#          NA prints as an em dash.
# header   column headings, one per column.
# spanners optional headings over groups of the number columns, as a named
#          vector of widths: c("Calories per $" = 2, "Real spend" = 2).
#          Use "" as the name for columns with no spanner.
# panels   optional panel headings, as a named vector: heading = first row.
# indent   TRUE indents every row label with \quad, so the rows sit under
#          their (non-indented) panel headings.
# rule_before  optional row numbers to draw a \midrule above, e.g. between
#          the estimates and the rows saying what each column includes.
# title    optional heading above the table ("A. Spending and calories").
# append   TRUE adds this table below the one already in `file`: for a
#          table too wide for the page, write its panels as two tables
#          with different columns, one under the other.
#
# Each number column is sized to its widest cell (S[table-format=-1.3]), so
# the table is as wide as its numbers and no wider. Estimates and SEs go in
# separate columns (see wide_bse()). % & # _ $ in labels are escaped, so
# "Calories per $" works; LaTeX commands pass through (\texttimes{},
# \num{-0.866}), math mode does not. Writes the tabular only; caption,
# label and note live in the .tex file.
# indent: rows set in (\\quad) from the italic panel headings; the default
# when there are panels.
# longtable = TRUE writes a longtable instead, for a table longer than a
# page: it breaks across pages and repeats its header ("(continued)"). It
# is not a float, so the caption and label come here (caption, label; the
# first part only when appending) and the .tex file \input's it outside a
# table environment, note below it. Needs \usepackage{longtable}.
write_tex_table <- function(
    dt, file, header = names(dt), spanners = NULL,
    panels = NULL, title = NULL, append = FALSE,
    rule_before = NULL, indent = !is.null(panels),
    longtable = FALSE, caption = NULL, label = NULL
) {
    stopifnot(is.data.table(dt), length(header) == ncol(dt))
    # labels: escape LaTeX specials, then set plain-text maths as maths
    # (x100, >=, <=, +/-, and quarters such as t-4)
    esc <- function(z) {
        z <- gsub('(?<!\\\\)([%&#_$])', '\\\\\\1', z, perl = TRUE)
        z <- gsub('x100', '\\(\\times\\)100', z, fixed = TRUE)
        z <- gsub('>=', '\\(\\geq\\)', z, fixed = TRUE)
        z <- gsub('<=', '\\(\\leq\\)', z, fixed = TRUE)
        z <- gsub('+/-', '\\(\\pm\\)', z, fixed = TRUE)
        z <- gsub(
            '(?<![A-Za-z])t([-+][0-9])(?!\\\\\\))',
            '\\\\(t\\1\\\\)',
            z,
            perl = TRUE
        )
        gsub(' t (and|vs) ', ' \\\\(t\\\\) \\1 ', z)
    }
    num_cols <- names(dt)[-1L]

    # table-format of one column: sign, digits before and after the point,
    # and room for text either side of the number ("{(}0.014{)}")
    spec <- c('l', setNames(rep('', length(num_cols)), num_cols))
    for (i_col in seq_along(num_cols)) {
        z <- dt[[num_cols[i_col]]]
        # text cells ("{OLS}"), cells spanning columns, and the cells they cover
        # do not set the format
        z <- z[!is.na(z) & !grepl(
            '^\\{[^{}]*\\}$|^\\\\multicolumn|^\\.skip$',
            z
        )]
        if (!length(z)) {
            spec[i_col + 1L] <- 'c'
            next
        }
        # significance stars (stars()): the column reserves room for three
        star <- any(grepl('\\\\sym\\{\\*+\\}$', z))
        z <- sub('\\\\sym\\{\\*+\\}$', '', z)
        pre <- regmatches(z, regexpr('^\\{[^}]*\\}', z))
        post <- regmatches(z, regexpr('\\{[^}]*\\}$', z))
        z <- sub('\\{[^}]*\\}$', '', sub('^\\{[^}]*\\}', '', z))
        bad <- !grepl('^(< ?)?-?[0-9]*\\.?[0-9]+$', z)
        if (any(bad)) {
            stop(
                'write_tex_table(): not a number: ',
                paste(utils::head(z[bad], 5), collapse = ', ')
            )
        }
        lt <- any(grepl('^<', z))
        z <- sub('^< ?', '', z)
        neg <- any(startsWith(z, '-'))
        z <- sub('^-', '', z)
        int <- max(nchar(sub('\\..*$', '', z)), 1L)
        if (int >= 4) int <- int + (int - 1L) %/% 3L # room for the commas
        dec <- max(nchar(ifelse(grepl('\\.', z), sub('^.*\\.', '', z), '')))
        no_zero <- all(startsWith(z, '.')) # APA: bounded, no leading zero
        spec[i_col + 1L] <- paste0(
            'S[table-format=',
            if (lt) '<',
            if (neg) '-',
            int,
            if (dec > 0) paste0('.', dec),
            if (length(pre)) {
                paste0(
                    ', table-space-text-pre=',
                    pre[1],
                    ', table-align-text-before=false'
                )
            },
            if (length(post)) {
                paste0(
                    ', table-space-text-post=',
                    post[1],
                    ', table-align-text-after=false'
                )
            },
            if (star) {
                paste0(
                    ', table-space-text-post=\\sym{***},',
                    ' table-align-text-after=false'
                )
            },
            if (no_zero) ', print-zero-integer=false',
            ']'
        )
    }

    # Statistics in maths: N, p, b, r, t, F as whole words, SD and SE as \mathit
    # words, and percentiles P10, P90 as a lowercase p with a subscript.
    stats <- esc(header[-1])
    plain <- !grepl('\\\\\\(|\\\\textit|\\$', stats)
    stats[plain] <- gsub(
        '\\b(SD|SE)\\b',
        '\\\\(\\\\mathit{\\1}\\\\)',
        stats[plain],
        perl = TRUE
    )
    stats[plain] <- gsub(
        '(?<![{\\\\])\\b(N|p|b|r|t|F)\\b',
        '\\\\(\\1\\\\)',
        stats[plain],
        perl = TRUE
    )
    stats[plain] <- gsub(
        '\\bP([0-9]{2})\\b',
        '\\\\(p_{\\1}\\\\)',
        stats[plain],
        perl = TRUE
    )
    header <- c(header[1], stats)
    head_row <- paste0(
        c(
            esc(header[1]),
            sprintf('{%s}', header[-1])
        ),
        collapse = ' & '
    )
    top <- NULL
    if (!is.null(spanners)) {
        stopifnot(sum(spanners) == length(num_cols))
        first <- cumsum(c(2L, spanners))[seq_along(spanners)]
        top <- c(
            paste0(
                ' & ',
                paste(
                    sprintf(
                        '\\multicolumn{%d}{c}{%s}',
                        spanners,
                        esc(names(spanners))
                    ),
                    collapse = ' & '
                ),
                ' \\\\'
            ),
            paste(
                sprintf('\\cmidrule(lr){%d-%d}', first, first + spanners - 1L)
                [nzchar(trimws(names(spanners)))],
                collapse = ''
            )
        )
    }

    cells <- copy(dt)
    set(cells, j = 1L, value = esc(as.character(cells[[1L]])))
    if (indent) set(cells, j = 1L, value = paste0('\\quad ', cells[[1L]]))
    for (j in num_cols) {
        set(
            cells,
            j = j,
            value = fifelse(
                is.na(cells[[j]]),
                '{---}',
                sub('^< ', '<', cells[[j]])
            )
        )
    }
    # APA in mixed columns: a column holding both bounded numbers (".90", a
    # correlation or share) and others ("0.89", a ratio) prints the leading zero
    # for all, as siunitx sets it per column.
    for (k in seq_along(num_cols)) {
        z <- cells[[num_cols[k]]]
        bnd <- grepl('^-?\\.[0-9]', z)
        if (!any(bnd) || all(bnd | !grepl('^-?[0-9]', z))) next
        fmt <- regmatches(
            spec[k + 1L],
            regexpr('table-format=[^],]+', spec[k + 1L])
        )
        z[bnd] <- sprintf(
            '{\\tablenum[%s, print-zero-integer=false]{%s}}',
            fmt,
            sub('^(-?)\\.', '\\10.', z[bnd])
        )
        set(cells, j = num_cols[k], value = z)
    }
    # ".skip" cells sit under a \multicolumn to their left and are dropped
    body <- paste0(
        apply(
            as.matrix(cells),
            1,
            function(r) paste(r[r != '.skip'], collapse = ' & ')
        ),
        ' \\\\'
    )
    # panel headings and rules, inserted from the bottom up so that the row
    # numbers still refer to the rows of dt
    rules <- if (length(rule_before)) {
        setNames(
            rule_before,
            rep('.rule', length(rule_before))
        )
    }
    inserts <- c(panels, rules)
    for (k in if (length(inserts)) order(-inserts) else integer(0)) {
        i <- inserts[[k]]
        lines <- if (names(inserts)[k] == '.rule') {
            '\\midrule'
        } else {
            lab <- sprintf(
                '\\multicolumn{%d}{l}{\\textit{%s}} \\\\',
                ncol(dt),
                esc(names(inserts)[k])
            )
            if (i > 1 && !(i %in% rule_before)) {
                c(
                    '\\addlinespace',
                    lab
                )
            } else {
                lab
            }
        }
        body <- append(body, lines, after = i - 1L)
    }

    # 2,783,093 and 1,619: commas, as in the text; decimals are not grouped
    setup <- paste0(
        '\\sisetup{group-digits=integer, group-separator={,}, ',
        'group-minimum-digits=4}'
    )
    # Significance stars make the estimate columns wider, so the gaps between
    # columns are a little narrower (4 pt instead of the default 6 pt) and the
    # table keeps its earlier width.
    if (any(grepl('table-space-text-post=\\\\sym', spec))) {
        setup <- c(setup, '\\setlength{\\tabcolsep}{4pt}')
    }
    if (append && !longtable) setup <- '\\par\\bigskip'
    title_txt <- title
    if (!is.null(title)) {
        title <- sprintf(
            '\\multicolumn{%d}{c}{\\textit{%s}} \\\\',
            ncol(dt),
            esc(title)
        )
    }
    head <- c('\\toprule', top, paste0(head_row, ' \\\\'), '\\midrule')
    if (!longtable) {
        tex <- c(
            setup, '\\begin{tabular}{', paste0('    ', spec), '}', title, head,
            body, '\\bottomrule', '\\end{tabular}'
        )
    } else {
        cap <- if (!is.null(caption)) {
            sprintf(
                '\\caption{%s}%s \\\\',
                caption,
                if (is.null(label)) '' else sprintf('\\label{%s}', label)
            )
        }
        cont <- sprintf(
            '\\multicolumn{%d}{l}{\\textit{%s(continued)}} \\\\',
            ncol(dt),
            if (is.null(title_txt)) '' else paste0(esc(title_txt), ' ')
        )
        # A group, so the \tabcolsep and \sisetup changes stay local to the
        # table. longtable steps the table counter even without a caption; undo
        # that for a continuation part, so the next table's number does not
        # skip.
        uncount <- if (is.null(caption)) '\\addtocounter{table}{-1}'
        tex <- c(
            '\\begingroup', setup, uncount, '\\begin{longtable}{', paste0(
                '    ',
                spec
            ), '}',
            cap, title, head, '\\endfirsthead', cont, head, '\\endhead',
            '\\bottomrule',
            '\\endlastfoot',
            body,
            '\\end{longtable}',
            '\\endgroup'
        )
    }
    if (append) tex <- c(readLines(file), tex)
    writeLines(tex, file)
    invisible(file)
}

# Significance stars to append to an estimate cell: "\sym{***}" for p < .01,
# "\sym{**}" < .05, "\sym{*}" < .10, "" otherwise. \sym (manuscript and
# supplement preambles) sets them as a superscript; write_tex_table() leaves
# room for them after the number.
stars <- function(p) {
    out <- fcase(
        p < .01,
        '\\sym{***}',
        p < .05,
        '\\sym{**}',
        p < .10,
        '\\sym{*}',
        default = ''
    )
    out[is.na(p)] <- ''
    out
}

# An SE cell for write_tex_table(): "0.014" -> "{(}0.014{)}"
parens <- function(x) ifelse(is.na(x), NA_character_, paste0('{(}', x, '{)}'))

# estimates_to_wtt() turns long estimates (one row per id x outcome, with
# columns b and se) into one row per id with an estimate column and an SE column
# (in parentheses) for each outcome, ready for write_tex_table(). digits sets
# the decimals: one number for all outcomes, or a named vector for the
# exceptions, e.g. c(d_hs = 5).
wide_bse <- function(x, id, outcomes, digits = 3, df = 48) {
    d <- setNames(rep(3L, length(outcomes)), outcomes)
    if (is.null(names(digits))) d[] <- digits else d[names(digits)] <- digits
    x <- as.data.table(x)[outcome %in% outcomes]
    x <- x[, .(
        id = get(id),
        outcome,
        b = paste0(fmt_num(b, d[outcome]), stars(2 * pt(-abs(b / se), df))),
        se = parens(fmt_num(se, d[outcome]))
    )]
    # One estimate per row and outcome; dcast() would count duplicates
    stopifnot(!anyDuplicated(x[, .(id, outcome)]))
    w <- dcast(x, id ~ outcome, value.var = c('b', 'se'))
    w <- w[match(unique(x$id), id)]
    setkey(w, NULL)
    setnames(w, 'id', id)
    setcolorder(w, c(id, paste0(c('b_', 'se_'), rep(outcomes, each = 2))))
    w
}
