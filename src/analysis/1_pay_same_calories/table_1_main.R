# Table 1: the response of food purchases to local basket inflation. One column
# per specification and one panel per outcome: OLS, the 2SLS, and three checks
# on it (the local economy, local prices of other goods, stricter fixed
# effects).

rm(list = ls())

source('src/lib/format.R')

main <- load_results('01_main')
iden <- load_results('05_identification')
loc <- load_results('15_local_demand')
oth <- load_results('17_other_prices')

# The five specifications, as long data: column, outcome, b, se, n, F ----------
ys <- c(
    d_lspend = 'Real spend', d_lkcal = 'Calories bought',
    d_lkpu = 'Calories per dollar'
)
headline <- main$headline[outcome %in% names(ys)]
est <- rbind(
    headline[, .(col = 1, outcome, b = b_ols, se = se_ols, n, f = NA_real_)],
    headline[, .(
        col = 2,
        outcome,
        b = b_iv,
        se = se_iv,
        n,
        f = main$first_stage$f
    )],
    loc$est[spec == '+ lagged changes of all', .(
        col = 3,
        outcome,
        b,
        se,
        n,
        f
    )],
    oth$est[spec == '+ all four', .(col = 4, outcome, b, se, n, f)],
    iden$res[spec == 'State FE + division x quarter FE', .(
        col = 5,
        outcome,
        b,
        se,
        n,
        f = f1
    )]
)
est <- est[outcome %in% names(ys)]
est[, .N, by = col] # each column should have all three outcomes
est

# Coefficient rows: one per outcome; estimate and SE side by side --------------
coef_rows <- rbindlist(
    lapply(
        names(ys),
        function(y) {
            e <- est[outcome == y][order(col)]
            # Stars: t test with 48 df (49 state clusters, as fixest reports)
            cells <- c(
                rbind(
                    paste0(
                        fmt_num(e$b, 3),
                        stars(2 * pt(-abs(e$b / e$se), 48))
                    ),
                    parens(fmt_num(e$se, 3))
                )
            ) # b1, se1, b2, se2, ...
            as.data.table(as.list(c(row = ys[[y]], cells)))
        }
    )
)

# Specification rows: what each column includes --------------------------------
fn <- est[outcome == 'd_lspend'][order(col)] # first-stage F and N from spending
# Siunitx sets it as a number, with commas
num <- function(x) sprintf('\\num{%.0f}', x)
spec <- data.table(
    row = c(
        'Estimator', 'Quarter FE', 'State FE',
        'Census division \\texttimes{} quarter FE',
        'Local economy controls and lags', 'Local prices of other goods',
        'First-stage \\(F\\)', '\\(N\\) (household-quarters)'
    ),
    c1 = c('OLS', '$\\checkmark$', '', '', '', '', '---', num(fn$n[1])),
    c2 = c('2SLS', '$\\checkmark$', '', '', '', '', num(fn$f[2]), num(fn$n[2])),
    c3 = c(
        '2SLS',
        '$\\checkmark$',
        '',
        '',
        '$\\checkmark$',
        '',
        num(fn$f[3]),
        num(fn$n[3])
    ),
    c4 = c(
        '2SLS',
        '$\\checkmark$',
        '',
        '',
        '',
        '$\\checkmark$',
        num(fn$f[4]),
        num(fn$n[4])
    ),
    c5 = c(
        '2SLS',
        '',
        '$\\checkmark$',
        '$\\checkmark$',
        '',
        '',
        num(fn$f[5]),
        num(fn$n[5])
    )
)
# Each entry spans its column's estimate and SE
spec_rows <- spec[
    ,
    as.data.table(
        t(
            apply(
                .SD,
                1,
                function(r) {
                    unlist(
                        lapply(
                            r,
                            function(x) {
                                c(
                                    sprintf('\\multicolumn{2}{c}{%s}', x),
                                    '.skip'
                                )
                            }
                        )
                    )
                }
            )
        )
    ),
    .SDcols = paste0('c', 1:5)
]
spec_rows <- cbind(row = spec$row, spec_rows)
# Numbers (first-stage F, N) are right-aligned, so they line up as if by decimal
# point; ticks and estimator names stay centred
is_num <- spec_rows$row %in% c(
    'First-stage \\(F\\)',
    '\\(N\\) (household-quarters)'
)
num_cols <- setdiff(names(spec_rows), 'row')
spec_rows[is_num, (num_cols) := lapply(
    .SD,
    sub,
    pattern = '{c}',
    replacement = '{r}',
    fixed = TRUE
),
.SDcols = num_cols
]
spec # N for calories is a few households smaller than for spending

setnames(coef_rows, c('row', paste0('V', 1:10)))
setnames(spec_rows, c('row', paste0('V', 1:10)))
tab <- rbind(coef_rows, spec_rows)
tab

write_tex_table(
    tab,
    'manuscript/tables/table_1.tex',
    # Italic b: the unstandardised coefficient, following APA style
    header = c('', rep(c('\\(b\\)', 'SE'), 5)),
    spanners = setNames(rep(2, 5), sprintf('(%d)', 1:5)),
    rule_before = 4 # a rule between the estimates and the specification rows
)

# numbers in the table note
fs <- main$first_stage
say('First stage %.3f, F = %s', fs$coef, format(round(fs$f), big.mark = ','))
