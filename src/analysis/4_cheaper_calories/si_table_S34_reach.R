# Table S34: the ladder of reach on panel-observed shelves (Figure 3B): the mean
# saving from buying each module's base calories at the 25th or 50th percentile
# of a wider shelf, pooled or best single retailer.

rm(list = ls())

source('src/lib/format.R')

pr <- load_results('22_panel_reach')
reach <- pr$reach[ladder != 'reference']
reach # mean, median saving (%), one row per ladder x rung x quantile
ci <- pr$reach_ci
ci # p25 mean and SE, clustered by household
pr$flow # household-quarters behind the table (last row)
pr$kept # retailer-used saving as a share of each wider rung

labels <- c(
    ret = 'Retailer where the calories were bought',
    zip3 = '3-digit zip code', cty = 'County', st = 'State',
    setx = 'Own retailers', bz3 = '3-digit zip code',
    bcty = 'County', bst = 'State'
)
rows <- data.table(
    ladder = c(
        'pooled', 'pooled', 'pooled', 'pooled',
        'best retailer', 'best retailer', 'best retailer', 'best retailer'
    ),
    rung = c('ret', 'zip3', 'cty', 'st', 'setx', 'bz3', 'bcty', 'bst')
)
p25 <- reach[quantile == 25]
p50 <- reach[quantile == 50]
tab <- data.table(
    rung = labels[rows$rung],
    mean25 = fmt_num(
        p25[match(paste(rows$ladder, rows$rung), paste(ladder, rung)), mean],
        2
    ),
    se25 = parens(fmt_num(ci[match(rows$rung, rung), se], 3)),
    med25 = fmt_num(
        p25[match(paste(rows$ladder, rows$rung), paste(ladder, rung)), median],
        2
    ),
    mean50 = fmt_num(
        p50[match(paste(rows$ladder, rows$rung), paste(ladder, rung)), mean],
        2
    )
)
stopifnot(!anyNA(tab$mean25), !anyNA(tab$se25))
tab

write_tex_table(
    tab,
    'manuscript/tables/reach.tex',
    header = c('Shelf', 'Mean', 'SE', 'Median', 'Mean'),
    spanners = c('25th percentile' = 3, '50th percentile' = 1),
    panels = c(
        'A. All stores in area' = 1,
        'B. Best single retailer (cross-fitted)' = 5
    ),
    indent = TRUE
)
