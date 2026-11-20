# Table S38: responses by tercile of shelf slack (the room to trade down).

rm(list = ls())

source('src/lib/format.R')

het <- load_results('08_mechanism')$het
het # one row per slack tercile x outcome

outcomes <- c(d_lkpu = 'kcal per $', d_lspend = 'Spend', d_lkcal = 'kcal')
tab <- wide_bse(het, 'slack3', names(outcomes))
tab

write_tex_table(
    tab,
    'manuscript/tables/slack_het.tex',
    header = c('Slack tercile', rep(c('b', 'SE'), 3)),
    spanners = setNames(rep(2, 3), outcomes)
)
