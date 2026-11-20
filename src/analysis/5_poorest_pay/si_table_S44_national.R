# SI table: quarter means of household year-on-year changes regressed across 76
# quarters on national mean exposure inflation, with a linear trend and
# Newey-West SEs (4 lags); all households and households at or below the poverty
# line. Stars from normal p-values.

rm(list = ls())

source('src/lib/format.R')

ts <- load_results('23_national')$ts[x == 'expo' & trend == TRUE]
ts # one row per group x outcome

rows <- c(
    d_lspend_nom = 'Nominal spend', d_lspend = 'Real spend',
    d_lkcal = 'Calories bought', d_lkpu = 'Calories per dollar',
    d_ed = 'Energy density', d_hs = 'Health score'
)
k <- ifelse(names(rows) == 'd_hs', 4, 3)
tab <- rbindlist(
    Map(
        function(yy, k) {
            a <- ts[group == 'all' & y == yy]
            p <- ts[group == 'poor' & y == yy]
            stopifnot(nrow(a) == 1, nrow(p) == 1)
            data.table(
                Outcome = rows[[yy]],
                b0 = paste0(
                    fmt_num(a$b, k), stars(2 * pnorm(-abs(a$b / a$se)))
                ),
                se0 = parens(fmt_num(a$se, k)),
                b1 = paste0(
                    fmt_num(p$b, k), stars(2 * pnorm(-abs(p$b / p$se)))
                ),
                se1 = parens(fmt_num(p$se, k))
            )
        },
        names(rows),
        k
    )
)
stopifnot(!anyNA(tab))
tab

write_tex_table(
    tab,
    'manuscript/tables/national.tex',
    header = c('Outcome', 'b', 'SE', 'b', 'SE'),
    spanners = c('All households' = 2, 'At or below poverty line' = 2)
)
