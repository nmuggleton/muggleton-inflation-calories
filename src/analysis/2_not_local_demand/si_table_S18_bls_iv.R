# Table S18: estimates with an instrument built from BLS prices. One column pair
# (estimate, SE) per instrument: BLS CPI food at home for the household's CPI
# area (all 21 areas; the 14 metropolitan areas), and both instruments together
# with the main one; one row per outcome.

rm(list = ls())

source('src/lib/format.R')

est <- copy(load_results('16_bls_cpi')$est)
est # one row per instrument x outcome

# Columns and rows -------------------------------------------------------------
models <- c('BLS CPI, all areas', 'BLS CPI, 14 metros', 'Both instruments')
clusters <- c(21, 14, 49) # CPI areas, CPI areas, states
ys <- c(
    d_lkpu = 'Calories per dollar', d_lspend = 'Real spend',
    d_lkcal = 'Calories bought', d_ed = 'Energy density',
    d_hs = 'Health score'
)
est <- est[model %in% models & outcome %in% names(ys)]
est[, col := match(model, models)]
stopifnot(nrow(est) == length(models) * length(ys))

# Stars from the clustered t test with G - 1 degrees of freedom (as fixest)
est[, p := 2 * pt(-abs(b / se), df = clusters[col] - 1)]
est[, .(model, outcome, b, se, p)]

# Coefficient rows: estimate (with stars) and SE side by side ------------------
digits <- c(d_hs = 4) # health score (SD) is small; three decimals elsewhere
coef_rows <- rbindlist(
    lapply(
        names(ys),
        function(y) {
            e <- est[outcome == y][order(col)]
            k <- if (y %in% names(digits)) digits[[y]] else 3
            cells <- c(rbind(
                paste0(fmt_num(e$b, k), stars(e$p)),
                parens(fmt_num(e$se, k))
            ))
            as.data.table(as.list(c(row = ys[[y]], cells)))
        }
    )
)

# Specification rows -----------------------------------------------------------
span <- function(x, align = 'c') {
    c(
        sprintf('\\multicolumn{2}{%s}{%s}', align, x),
        '.skip'
    )
}
# Siunitx sets it as a number, with commas
num <- function(x) sprintf('\\num{%.0f}', x)
fn <- est[outcome == 'd_lkpu'][order(col)] # F and N from calories per dollar
spec_rows <- rbind(
    as.data.table(as.list(c(
        row = 'Quarter FE',
        unlist(lapply(rep('$\\checkmark$', 3), span))
    ))),
    as.data.table(
        as.list(
            c(
                row = 'Clustered by',
                unlist(lapply(
                    sprintf(
                        '%s (%d)',
                        c('CPI area', 'CPI area', 'State'),
                        clusters
                    ),
                    span
                ))
            )
        )
    ),
    as.data.table(as.list(c(
        row = 'First-stage \\(F\\)',
        unlist(lapply(num(fn$f), span, align = 'r'))
    ))),
    as.data.table(as.list(c(
        row = '\\(N\\) (household-quarters)',
        unlist(lapply(num(fn$n), span, align = 'r'))
    )))
)
spec_rows

setnames(coef_rows, c('row', paste0('V', 1:6)))
setnames(spec_rows, c('row', paste0('V', 1:6)))
tab <- rbind(coef_rows, spec_rows)
tab

write_tex_table(
    tab,
    'manuscript/tables/local_bls_iv.tex',
    header = c('', rep(c('\\(b\\)', 'SE'), 3)),
    spanners = c(
        `BLS, all 21 areas` = 2,
        `BLS, 14 metro areas` = 2,
        `Both instruments` = 2
    ),
    # A rule between the estimates and the specification rows
    rule_before = length(ys) + 1
)

# numbers for the table note
est[model == 'Both instruments', .(outcome, sargan_p)] # overidentification test
# Anderson-Rubin 95% CIs
est[model != 'Both instruments', .(model, outcome, ar_lo, ar_hi)]
