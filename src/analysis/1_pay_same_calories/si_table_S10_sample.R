# Table S10: estimates under other samples, thresholds, weights and controls.
# Each row changes one element of the specification.

rm(list = ls())

source('src/lib/format.R')

smp <- load_results('13_sample')
est <- rbind(smp$ra, smp$rb, smp$rc)
est # one row per variant x outcome, with the block (panel) it belongs to

# display labels
est[spec == 'Headline gate', spec := 'Headline']
est[
    spec == 'Inverse probability of gate x projection',
    spec := 'Inverse probability of sample x projection'
]

outcomes <- c(
    d_lkpu = 'kcal per $', d_lkcal = 'kcal', d_lspend = 'Spend',
    d_ed = 'Energy density', d_hs100 = 'Health score x100'
)
tab <- wide_bse(est, 'spec', names(outcomes))
tab[, N := fmt_num(est[outcome == 'd_lkpu'][match(tab$spec, spec), n], 0)]
tab

# One panel per block, starting at its first row
blocks <- unique(est[, .(spec, block)])[match(tab$spec, spec)]
first_row <- blocks[, .(row = .I[1]), by = block]
# All five outcomes are too wide for the page, so two tables sit one under the
# other; together they run longer than a page, so they are longtables (caption
# and label here, not in supplement.tex); rows are set in from the italic panel
# headings
spend_cal <- c(
    'spec', 'b_d_lkpu', 'se_d_lkpu', 'b_d_lkcal', 'se_d_lkcal',
    'b_d_lspend', 'se_d_lspend', 'N'
)
diet <- c('spec', 'b_d_ed', 'se_d_ed', 'b_d_hs100', 'se_d_hs100')
write_tex_table(
    tab[, ..spend_cal],
    'manuscript/tables/sample.tex',
    header = c('Specification', rep(c('b', 'SE'), 3), 'N'),
    spanners = c(setNames(rep(2, 3), outcomes[1:3]), ' ' = 1),
    panels = setNames(first_row$row, first_row$block),
    title = 'A. Spending and calories',
    indent = TRUE,
    longtable = TRUE,
    caption = 'Sample, Threshold, Weight, and Control Variants',
    label = 'tab:a_sample'
)
write_tex_table(
    tab[, ..diet],
    'manuscript/tables/sample.tex',
    header = c('Specification', rep(c('b', 'SE'), 2)),
    spanners = setNames(rep(2, 2), outcomes[4:5]),
    panels = setNames(first_row$row, first_row$block),
    title = 'B. Diet quality',
    append = TRUE,
    indent = TRUE,
    longtable = TRUE
)
