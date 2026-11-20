# SI Table S46: household egg purchases in the national egg price episode of
# 2022Q2-2023Q1, against ordinary quarters.
#
# Columns 1-2 are the means of the year-on-year change over quarters, in
# ordinary quarters and in the episode; columns 3-4 their difference, with a
# Newey-West standard error from the 72 quarter means. Columns 5-6 are the extra
# change in the episode for households at or below 130 percent of the poverty
# line (quarter fixed effects, 49 state clusters).

rm(list = ls())

source('src/lib/format.R')

res <- load_results('34_egg_episode')
est <- res$est
low <- res$low
est
low

row_labels <- c(
    d_lprice = 'Price paid per egg, percent',
    d_leggs = 'Eggs bought, percent',
    buy = 'Still buys eggs, percent of households',
    d_arc = 'Eggs bought, with those who stop, percent',
    d_store_brand = 'Store-brand share of eggs, pp',
    d_ltier = 'Price tier of eggs bought, percent',
    d_lprotein = 'Other protein foods, packs, percent',
    d_substitute = 'Buys egg substitutes, pp'
)
stopifnot(
    setequal(est$outcome, names(row_labels)),
    setequal(low$outcome, names(row_labels))
)
est <- est[match(names(row_labels), outcome)]
low <- low[match(names(row_labels), outcome)]

tab <- data.table(
    Outcome = row_labels[est$outcome],
    ordinary = fmt_num(est$ordinary, 2),
    episode = fmt_num(est$episode, 2),
    diff = paste0(fmt_num(est$diff, 2), stars(est$p)),
    diff_se = parens(fmt_num(est$se, 2)),
    low = paste0(fmt_num(low$b, 2), stars(low$p)),
    low_se = parens(fmt_num(low$se, 2))
)
tab

write_tex_table(
    tab,
    'manuscript/tables/egg_episode.tex',
    header = c('', 'Ordinary', 'Episode', 'b', 'SE', 'b', 'SE'),
    spanners = c(
        'Mean change' = 2,
        'Episode less ordinary' = 2,
        'Low income, extra' = 2
    ),
    rule_before = 5
)
