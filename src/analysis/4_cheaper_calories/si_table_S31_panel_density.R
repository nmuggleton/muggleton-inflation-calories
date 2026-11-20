# Table S31: how densely panel purchases see a shelf: the share of coded
# labelled food spend whose module x quarter cell holds at least five distinct
# panel-observed items, by how purchases are pooled.

rm(list = ls())

source('src/lib/format.R')

dens <- load_results('21_panel_shelf_check')$dens
dens # one row per year x store kind

pools <- c(
    store = 'Single store',
    retailer_zip3 = 'Retailer \\texttimes{} 3-digit zip code',
    retailer_state = 'Retailer \\texttimes{} state',
    retailer = 'Retailer, national'
)
long <- melt(
    dens,
    id.vars = c('year', 'kind'),
    measure.vars = names(pools),
    variable.name = 'pool',
    value.name = 'sh'
)
tab <- dcast(long, pool ~ year + kind, value.var = 'sh')
tab <- tab[match(names(pools), pool)]
years <- sort(unique(dens$year))
cols <- as.vector(rbind(paste0(years, '_coded_non_rms'), paste0(years, '_rms')))
tab <- tab[, c('pool', cols), with = FALSE]
for (j in cols) set(tab, j = j, value = fmt_bounded(tab[[j]], 2))
tab[, pool := pools[pool]]
tab

write_tex_table(
    tab,
    'manuscript/tables/panel_density.tex',
    header = c(
        'Purchases pooled by',
        rep(c('Not in RMS', 'In RMS'), length(years))
    ),
    spanners = setNames(rep(2, length(years)), years)
)
