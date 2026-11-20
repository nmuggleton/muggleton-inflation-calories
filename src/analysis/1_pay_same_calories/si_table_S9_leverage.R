# Table S9: leverage. 2SLS estimates on subsamples and, below a rule with its
# own Min and Max headings, the smallest and largest leave-one-out estimates.

rm(list = ls())

source('src/lib/format.R')

lev <- load_results('11_leverage')
lev$all_r # subsample estimates: one row per sample x outcome
lev$loo_sum # leave-one-out: smallest and largest estimate per outcome

outcomes <- names(ROB_SHORT)
digits <- c(d_hs = 5) # the health score is in item-SD units, so SEs are small

# Subsamples: estimate and SE for each outcome
subsamples <- wide_bse(lev$all_r, 'spec', outcomes, digits)

# Leave-one-out: the smallest and largest estimate in the same two columns,
# under a heading row of their own, so that b and SE keep their meaning above
loo <- copy(lev$loo_sum)
loo[, k := fifelse(outcome == 'd_hs', 5L, 3L)]
loo <- loo[, .(
    spec,
    outcome,
    b = fmt_num(min, k),
    se = fmt_num(max, k)
)]
loo <- dcast(loo, spec ~ outcome, value.var = c('b', 'se'))
setcolorder(loo, names(subsamples))
loo_head <- copy(loo[1])
loo_head[, spec := 'Leave-one-out estimates']
for (j in grep('^b_', names(loo_head), value = TRUE)) set(loo_head, j = j, value = '{Min}')
for (j in grep('^se_', names(loo_head), value = TRUE)) set(loo_head, j = j, value = '{Max}')

tab <- rbind(subsamples, loo_head, loo)
n <- lev$all_r[outcome == 'd_lkpu']
tab[, N := fmt_num(n$n[match(spec, n$spec)], 0)]
tab[spec == 'Leave-one-out estimates', N := '{}']
loo_row <- nrow(subsamples) + 1L # a rule above the leave-one-out block
tab

# All five outcomes are too wide for the page, so two tables, one under the
# other
spend_cal <- c(
    'spec', 'b_d_lkpu', 'se_d_lkpu', 'b_d_lspend', 'se_d_lspend',
    'b_d_lkcal', 'se_d_lkcal', 'N'
)
diet <- c('spec', 'b_d_ed', 'se_d_ed', 'b_d_hs', 'se_d_hs')
write_tex_table(
    tab[, ..spend_cal],
    'manuscript/tables/leverage.tex',
    header = c('Sample', rep(c('b', 'SE'), 3), 'N'),
    spanners = c(setNames(rep(2, 3), ROB_SHORT[1:3]), ' ' = 1),
    rule_before = loo_row,
    title = 'A. Spending and calories'
)
write_tex_table(
    tab[, ..diet],
    'manuscript/tables/leverage.tex',
    header = c('Sample', rep(c('b', 'SE'), 2)),
    spanners = setNames(rep(2, 2), ROB_SHORT[4:5]),
    rule_before = loo_row,
    title = 'B. Diet quality',
    append = TRUE
)
