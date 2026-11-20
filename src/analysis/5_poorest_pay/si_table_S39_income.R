# Table S39: responses by household income, food-assistance eligibility and
# food-budget burden.

rm(list = ls())

source('src/lib/format.R')

res <- copy(load_results('09_heterogeneity')$res)
res # one row per moderator x group x outcome

# Panel headings and group labels in plain words; all are defined in the base
# quarter
panel <- c(
    fpl_bin = 'Income relative to the poverty line',
    snap_f =
        'Food assistance (SNAP): income at or below 130% of the poverty line',
    burden_q = 'Food at home as a share of income, fifths'
)
group_label <- c(
    `<=100%` = 'Up to 100%', `100-130%` = '100--130%',
    `130-185%` = '130--185%', `185-300%` = '185--300%',
    `300-500%` = '300--500%', `>500%` = 'Over 500%',
    `SNAP-eligible (proxy)` = 'Likely eligible',
    `Not eligible` = 'Not eligible',
    `1` = '1 (lowest)', `2` = '2', `3` = '3', `4` = '4', `5` = '5 (highest)',
    Q1 = '1 (lowest)', Q2 = '2', Q3 = '3', Q4 = '4', Q5 = '5 (highest)'
)
res[, row := paste(moderator, group)] # group labels repeat across moderators

# Outcome names and order as in the main text (Table 1)
outcomes <- c(
    d_lspend = 'Real spend', d_lkcal = 'Calories bought',
    d_lkpu = 'Calories per dollar'
)
tab <- wide_bse(res, 'row', names(outcomes))

# Group size and first-stage F, taken from the calories row of each group
info <- res[outcome == 'd_lkcal'][match(tab$row, row)]
stopifnot(all(as.character(info$group) %in% names(group_label)))
tab[, `:=`(
    row = group_label[as.character(info$group)],
    N = fmt_num(info$n, 0),
    F = fmt_num(info$fs_F, 0)
)]
setcolorder(tab, c('row', 'N', 'F'))
tab

first_row <- info[, .(i = .I[1]), by = moderator]
write_tex_table(
    tab,
    'manuscript/tables/budget.tex',
    header = c(
        'Group', '\\(N\\)', 'First-stage \\(F\\)',
        rep(c('\\(b\\)', 'SE'), 3)
    ),
    spanners = c(' ' = 2, setNames(rep(2, 3), outcomes)),
    panels = setNames(first_row$i, panel[first_row$moderator]),
    indent = TRUE
) # rows indented under the italic panel headings
