# Table S33: why a panel-observed shelf differs from the RMS shelf of the same
# stores: state ladders built step by step from the RMS shelf towards the panel
# shelf, changing one thing at a time. RMS 2013, the 25 human-food modules with
# the most panel spend at RMS stores.

rm(list = ls())

source('src/lib/format.R')

chk <- load_results('21_panel_shelf_check')
dec <- chk$dec_sav
dec # one row per step: savings at p25 and p50 (shares)
dl <- chk$dec_lad
dl # one row per step: ladder level relative to the RMS shelf
chk$match # panel cells matched to an RMS cell; store-brand shares
chk$repl # the RMS step against the main RMS ladders

labels <- c(
    r0 = 'RMS shelf: every item and store',
    r1 = 'Only items panellists bought in the state',
    r2 = 'Only item \\texttimes{} store pairs panellists bought',
    r3 = "Panellists' calories as weights",
    pm = "Panellists' prices, same matched cells",
    p = 'Add panel purchases with no RMS record'
)
stopifnot(identical(dec$step, names(labels)), identical(dl$step, names(labels)))
tab <- data.table(
    step = labels[dec$step],
    sav25 = fmt_num(100 * dec$sav25, 1),
    change = fmt_num(100 * c(NA, diff(dec$sav25)), 1),
    sav50 = fmt_num(100 * dec$sav50, 1),
    ratio25 = fmt_num(dl$gm_ratio_p25, 3)
)
tab

write_tex_table(
    tab,
    'manuscript/tables/shelf_decomposition.tex',
    header = c('Step', 'Saving, p25', 'Change', 'Saving, p50', 'p25 / RMS p25')
)
