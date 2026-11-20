# SI Table S47: stated beliefs about price changes, the food price changes
# households face, and their purchases, in nine waves of the PanelViews economic
# surveys (2015-2019).

rm(list = ls())

source('src/lib/format.R')

res <- load_results('36_beliefs')
tracking <- res$tracking
behave <- res$behave
types <- res$types
tracking
behave
types

# A. tracking ------------------------------------------------------------------
rows_a <- c(
    'All waves' = 'Exposure inflation, all waves',
    'Own-prices wording, 2015-16' = 'Question about own prices, 2015--16',
    'Inflation wording' = 'Question about inflation',
    'Within household' = 'Within household',
    'Own price index' = 'Own price index, all waves'
)
stopifnot(all(names(rows_a) %in% tracking$design))
ta <- tracking[match(names(rows_a), design)]
tab_a <- data.table(
    Measure = rows_a[ta$design],
    b = paste0(fmt_num(ta$b, 3), stars(ta$p)),
    se = parens(fmt_num(ta$se, 3)),
    n = fmt_num(ta$n, 0)
)
tab_a

write_tex_table(
    tab_a,
    'manuscript/tables/beliefs.tex',
    header = c('', 'b', 'SE', 'N'),
    title = 'A. Stated price change on the household\'s inflation'
)

# B. purchases -----------------------------------------------------------------
outcomes <- c(
    d_lspend = 'Spending', d_lkcal = 'Calories', d_lkpu = 'Calories per $'
)
rows_b <- c(
    exposure = 'Exposure inflation',
    belief = 'Stated price change'
)
tb <- behave[design == '2SLS with belief']
stopifnot(nrow(tb) == 6)
tab_b <- wide_bse(tb, 'term', names(outcomes))
tab_b[, term := rows_b[term]]
tab_b

write_tex_table(
    tab_b,
    'manuscript/tables/beliefs.tex',
    header = c('', rep(c('b', 'SE'), 3)),
    spanners = setNames(rep(2, 3), outcomes),
    title = 'B. Purchases in the survey quarters',
    append = TRUE
)

# C. answer types --------------------------------------------------------------
tab_c <- types[, .(
    type,
    share = fmt_num(100 * share, 1),
    share_low = fmt_num(100 * share_low, 1),
    share_high = fmt_num(100 * share_high, 1),
    belief_mean = fmt_num(belief_mean, 2),
    subj_sd = fmt_num(subj_sd, 2)
)]
tab_c

write_tex_table(
    tab_c,
    'manuscript/tables/beliefs.tex',
    header = c(
        'Answer', 'All', 'Low income', 'High income', 'Mean answer',
        'Subjective SD'
    ),
    spanners = c('Share of answers, percent' = 3, ' ' = 2),
    title = 'C. Answer types',
    append = TRUE
)
