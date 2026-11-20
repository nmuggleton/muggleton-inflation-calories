# Numbers quoted in the Discussion.

rm(list = ls())

source('src/lib/format.R')
main <- load_results('01_main')
oth <- load_results('17_other_prices')
rev <- load_results('19_revision')

cat('== Habit (Table S48)\n')
print(rev$rh[, .(group, outcome, b = round(b, 3), se = round(se, 3))])
print(
    rev$habit_diff[, .(
        outcome,
        b = round(b, 3),
        se = round(se, 3),
        p = round(p, 3)
    )]
)

cat('\n== Naive (OLS) estimates understate the response\n')
headline <- main$headline
for (y in c('d_lkpu', 'd_lspend')) {
    say('%s: 2SLS / OLS = %.1f', y, headline[outcome == y, b_iv / b_ols])
}

cat('\n== Price of food away from home and at-home calories\n')
print(oth$bal[, .(var, b = round(b, 3), se = round(se, 3), p = round(p, 3))])
print(
    oth$est[outcome == 'd_lkcal', .(
        spec,
        b = round(b, 3),
        se = round(se, 3)
    )]
)
