# Table S32: panel-observed shelves against the Retail Scanner (RMS) shelves of
# the same stores: state ladders built from panel purchases at RMS stores
# compared with the RMS state ladders, and the state-rung saving each gives on
# target-year base baskets.

rm(list = ls())

source('src/lib/format.R')

chk <- load_results('21_panel_shelf_check')
lad <- chk$lad
lad # one row per year x variant: coverage and price ratios
sav <- chk$sav
sav # one row per year x variant x shelf source: savings (shares)

years <- sort(unique(lad$year))
v_lab <- c(
    all = 'All purchases', nodeal = 'No deal or coupon purchases',
    nopl = 'National brands only'
)

# A. Ladders: coverage and level, panel against RMS
rows_a <- rbindlist(
    lapply(
        names(v_lab),
        function(vv) {
            x <- lad[v == vv][order(year)]
            data.table(
                row = paste0(
                    v_lab[[vv]],
                    c(
                        ': panel ladder valid', ': p25, panel / RMS',
                        ': p50, panel / RMS'
                    )
                ),
                rbind(
                    fmt_bounded(
                        x$sh_panel_valid,
                        2
                    ),
                    fmt_num(x$gm_ratio_p25, 2),
                    fmt_num(x$gm_ratio_p50, 2)
                )
            )
        }
    )
)
setnames(rows_a, c('row', as.character(years)))

# B. State-rung saving at p25 (%), on the common rows each year
rms <- sav[v == 'all' & source == 'full'][order(year)]
rows_b <- data.table(row = 'RMS shelf', t(fmt_num(100 * rms$sav25_rms, 1)))
src_lab <- c(full = '', other = ', other half of the panel')
for (vv in names(v_lab)) {
    for (ss in names(src_lab)) {
        x <- sav[v == vv & source == ss][order(year)]
        rows_b <- rbind(
            rows_b,
            data.table(
                row = paste0(
                    'Panel shelf, ',
                    tolower(v_lab[[vv]]),
                    src_lab[[ss]]
                ),
                t(fmt_num(100 * x$sav25_panel, 1))
            ),
            use.names = FALSE
        )
    }
}
setnames(rows_b, c('row', as.character(years)))
corr <- sav[v == 'all' & source == 'full'][order(year), corr_row_p25]
rows_b <- rbind(
    rows_b,
    data.table(
        row = 'Correlation, RMS and panel (all purchases)',
        t(fmt_bounded(corr, 2))
    ),
    use.names = FALSE
)

tab <- rbind(rows_a, rows_b, use.names = FALSE)
tab

write_tex_table(
    tab,
    'manuscript/tables/shelf_validation.tex',
    header = c('', as.character(years)),
    panels = c(
        'A. State ladders, where the RMS ladder is valid' = 1,
        'B. State-rung saving at the 25th percentile (percent)' =
            nrow(rows_a) + 1
    ),
    indent = TRUE
)
