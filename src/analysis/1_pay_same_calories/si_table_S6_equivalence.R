# Table S6: equivalence tests (TOST) for the outcomes that I expect not to move.

rm(list = ls())

source('src/lib/format.R')

hd <- load_results('19_revision')$hd
hd # one row per outcome: 2SLS, 90% CI, equivalence margin, TOST p

labels <- c(
    d_lkcal = 'Calories bought (log x100)',
    d_ed = 'Energy density (kcal/100 g)',
    d_hs = 'Health score (\\(\\mathit{SD}\\) units)'
)
hd[, k := fifelse(outcome == 'd_hs', 5L, 3L)] # health-score SEs are small

tab <- hd[, .(
    Outcome = labels[outcome],
    # Stars: t test with 48 df (49 states)
    b = paste0(fmt_num(b, k), stars(2 * pt(-abs(b / se), 48))),
    se = parens(fmt_num(se, k)),
    # 95% CI (t, 48 df): a CI inside the margin is a TOST at 2.5% each side,
    # stricter than the 90% CI of the 5% TOST
    lo = fmt_num(b - qt(0.975, 48) * se, k),
    hi = fmt_num(b + qt(0.975, 48) * se, k),
    margin = fmt_num(margin, 3),
    p = fmt_p(p_tost)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/rev_equivalence.tex',
    header = c(
        'Outcome', '2SLS', 'SE', 'Lower', 'Upper',
        'Margin (+/-)', 'TOST p'
    ),
    spanners = c(' ' = 2, '95% CI' = 2, ' ' = 2)
)

# Every 95% CI lies inside its margin (the stricter TOST)
stopifnot(hd[, all(abs(b) + qt(0.975, 48) * se < margin)])
