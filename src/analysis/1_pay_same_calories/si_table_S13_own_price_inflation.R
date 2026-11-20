# Table S13: the conventional regression. OLS estimates of the outcomes on the
# household's own paid-price inflation, for comparison with OLS and 2SLS on
# exposure, all on the household-quarters with a valid own-price index.

rm(list = ls())

source('src/lib/format.R')

r <- load_results('33_own_price')
r$moments # SDs of exposure and own-price inflation, and their correlation
est <- r$est[sample == 'own-price sample']
est # one row per specification x outcome

specs <- c(
    '2SLS on exposure' = 'Exposure, instrumented (main design)',
    'OLS on exposure' = 'Exposure, OLS',
    'OLS on own-price inflation' = 'Own-price inflation, OLS',
    'OLS on own-price inflation, trimmed' =
        'Own-price inflation, OLS, trimmed',
    'OLS on own-price inflation, given margin changes' =
        'Own-price inflation, OLS, given shopping changes'
)
shown <- c('d_lspend', 'd_lkcal', 'd_lkpu')
est <- est[spec %in% names(specs) & outcome %in% shown]
est <- est[order(match(spec, names(specs)))]
stopifnot(nrow(est) == length(specs) * length(shown))
est[, row := specs[spec]]

tab <- wide_bse(est, 'row', shown)
by_row <- est[outcome == 'd_lkpu'][match(tab$row, row)]
tab[, `:=`(
    avoided = fmt_num(100 * (1 + by_row$b), 0),
    n = fmt_num(by_row$n, 0)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/own_price_inflation.tex',
    header = c('', rep(c('b', 'SE'), 3), 'Avoided, %', '\\(N\\)'),
    spanners = c(setNames(rep(2, 3), ROB_SHORT[shown]), ' ' = 2)
)

# Numbers for the text and the table note
say(
    'SD of own-price inflation %.1f, of exposure %.1f; correlation %.2f',
    r$moments$sd_own, r$moments$sd_exposure, r$moments$cor
)
r$bargain[spec == 'changes in all margins' & lhs == 'infl_laspeyres']
