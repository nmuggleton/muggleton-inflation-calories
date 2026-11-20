# Table S43: responses by the intensity of national food inflation.

rm(list = ls())

source('src/lib/format.R')

res <- copy(load_results('10_regime')$res)[moderator != 'year_f']
res # one row per moderator x group x outcome

panel <- c(
    regime = 'National food-at-home inflation of the quarter',
    epi = 'Food-price episodes (2007Q4-08Q4, 2011, 2021Q2--23Q3)'
)
res[, row := paste(moderator, group)] # group labels may repeat

tab <- wide_bse(res, 'row', names(HET_YS), digits = c(d_hs = 4))
info <- res[outcome == names(HET_YS)[1]][match(tab$row, row)]
tab[, `:=`(row = info$group, N = fmt_num(info$n, 0))]
setcolorder(tab, c('row', 'N'))
tab

first_row <- info[, .(i = .I[1]), by = moderator]
write_tex_table(
    tab,
    'manuscript/tables/intensity.tex',
    header = c('Group', 'N', rep(c('b', 'SE'), 5)),
    spanners = c(' ' = 1, setNames(rep(2, 5), HET_YS)),
    panels = setNames(first_row$i, panel[first_row$moderator]),
    indent = TRUE
) # rows set in from the italic panel headings
