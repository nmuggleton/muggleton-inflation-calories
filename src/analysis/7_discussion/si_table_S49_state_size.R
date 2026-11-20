# Table S49: responses by state size. Panel A: 2SLS in the 4 largest states, the
# next 11, the 34 smallest, the 15 largest together, and all 49, with the wild
# cluster bootstrap p-value and 95% interval for calories.

rm(list = ls())

source('src/lib/format.R')

out <- 'manuscript/tables/state_size.tex'

res <- load_results('31_state_size')
res$boot # one row per group x outcome: 2SLS, SE, bootstrap p and interval
res$alt_cal # one row per group x calorie measure
res$perm # equality of the three groups: permutation p-values (table note)
res$pooled # all 49 states with size-group by quarter effects (table note)
stopifnot(!any(res$boot$edge)) # no interval cut off by the grid

rows <- c(
    'All 49' = 'All 49 states',
    '4 largest' = '4 largest (CA, TX, FL, NY)',
    'Next 11' = 'Next 11',
    '34 smallest' = '34 smallest',
    '15 largest' = '15 largest'
)

# Panel A: estimate and SE side by side. No stars, since the t test over-rejects
# with 4 or 11 clusters; the bootstrap p-value is reported instead.
shown <- c('d_lkpu', 'd_lspend', 'd_lkcal')
boot <- res$boot[outcome %in% shown]
boot[, `:=`(b_cell = fmt_num(b, 3), se_cell = parens(fmt_num(se, 3)))]
wide <- dcast(boot, group ~ outcome, value.var = c('b_cell', 'se_cell'))
kcal <- boot[outcome == 'd_lkcal', .(group, n, p_wild, lo_wild, hi_wild)]
wide <- merge(wide, kcal, by = 'group')[match(names(rows), group)]
stopifnot(!anyNA(wide$n))
tab_a <- wide[, .(
    row = rows[group],
    b_cell_d_lkpu,
    se_cell_d_lkpu,
    b_cell_d_lspend,
    se_cell_d_lspend,
    b_cell_d_lkcal,
    se_cell_d_lkcal,
    p = fmt_p(p_wild),
    lower = fmt_num(lo_wild, 3),
    upper = fmt_num(hi_wild, 3),
    n = fmt_num(n, 0)
)]
tab_a

write_tex_table(
    tab_a,
    out,
    header = c(
        'States', rep(c('\\(b\\)', 'SE'), 3), '\\(p\\)', 'Lower', 'Upper',
        '\\(N\\)'
    ),
    spanners = c(
        setNames(rep(2, 3), ROB_SHORT[shown]),
        'Calories: bootstrap' = 3,
        ' ' = 1
    ),
    title = 'A. Main outcomes'
)

# Panel B: calorie measures that do not depend on label coverage
alt_y <- c(
    d_lkcal_fb = 'Incl. imputed', d_lkcal_tot = 'Per household',
    d_lkcal_sc = 'Scaled by coverage'
)
alt <- res$alt_cal[outcome %in% names(alt_y)]
alt[, `:=`(b_cell = fmt_num(b, 3), se_cell = parens(fmt_num(se, 3)))]
wide_b <- dcast(alt, group ~ outcome, value.var = c('b_cell', 'se_cell'))
wide_b <- wide_b[match(names(rows), group)]
tab_b <- wide_b[, .(
    row = rows[group],
    b_cell_d_lkcal_fb,
    se_cell_d_lkcal_fb,
    b_cell_d_lkcal_tot,
    se_cell_d_lkcal_tot,
    b_cell_d_lkcal_sc,
    se_cell_d_lkcal_sc
)]
tab_b

write_tex_table(
    tab_b,
    out,
    header = c('States', rep(c('\\(b\\)', 'SE'), 3)),
    spanners = setNames(rep(2, 3), alt_y),
    title = 'B. Other calorie measures',
    append = TRUE
)

# numbers in the table note
say(
    'Equality of the three groups, calories: chi-squared p = %.3f, %s',
    res$perm[outcome == 'd_lkcal', p_chi2],
    sprintf('permutation p = %.3f', res$perm[outcome == 'd_lkcal', p_perm])
)
say(
    'Calories per dollar: chi-squared p = %.4f, permutation p = %.3f',
    res$perm[outcome == 'd_lkpu', p_chi2],
    res$perm[outcome == 'd_lkpu', p_perm]
)
say(
    'All 49, size-group by quarter effects, calories: %.3f (SE %.3f)',
    res$pooled[outcome == 'd_lkcal', b],
    res$pooled[outcome == 'd_lkcal', se]
)
