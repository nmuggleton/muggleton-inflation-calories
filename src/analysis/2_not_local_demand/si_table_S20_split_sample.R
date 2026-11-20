# Table S20: estimates with an instrument whose price noise is independent of
# the noise in exposure. Households are split at random into two halves and each
# basket is priced from its own half; the instrument is the mean exposure of the
# other households in the same half (which shares the noise) or in the other
# half (independent noise).

rm(list = ls())

source('src/lib/format.R')

r <- load_results('28_split_sample')
est <- copy(r$est)
est # one row per specification x outcome, averaged over splits
r$flow

specs <- c(
    main = 'Main specification',
    `same half` = 'Half-sample prices, same-half instrument',
    `other half` = 'Half-sample prices, other-half instrument'
)
shown <- c('d_lkpu', 'd_lspend', 'd_lkcal', 't1_log')
est <- est[spec %in% names(specs) & outcome %in% shown]
est <- est[order(match(spec, names(specs)))]
stopifnot(nrow(est) == length(specs) * length(shown))
est[, row := specs[spec]]

tab <- wide_bse(est, 'row', shown)
# First-stage coefficient, from the calories-per-dollar models
first_stage <- est[outcome == 'd_lkpu'][match(tab$row, row), fs]
tab[, fs := fmt_num(first_stage, 3)]
tab

write_tex_table(
    tab,
    'manuscript/tables/split_sample.tex',
    header = c('', rep(c('b', 'SE'), 4), 'First stage'),
    spanners = c(
        setNames(rep(2, 3), ROB_SHORT[shown[1:3]]),
        `Own prices` = 2,
        ' ' = 1
    )
)
