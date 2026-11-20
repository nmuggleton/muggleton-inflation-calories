# SI table: estimates with an instrument built from state prices at fixed
# retailer weights, so that other households' moves between retailers do not
# move it. Panel A: the headline 2SLS (T2 instrumented) with three instruments
# on the same household-quarters: the main one (leave-one-out unit values),
# state unit values on the products priced at fixed weights, and fixed retailer
# weights.

rm(list = ls())

source('src/lib/format.R')

r <- load_results('26_fixed_retailer')
iv <- copy(r$iv)
rf <- copy(r$rf)
r$decomp # SDs and correlations across state-quarters, for the note
r$flow

ys <- c(
    d_lkpu = 'Calories per dollar', d_lspend = 'Real spend',
    d_lkcal = 'Calories bought', d_ed = 'Energy density',
    d_hs = 'Health score'
)
digits <- c(d_hs = 4) # health score (SD) is small; three decimals elsewhere
k_of <- function(y) if (y %in% names(digits)) digits[[y]] else 3
cell <- function(b, se, k) {
    c(
        paste0(fmt_num(b, k), stars(2 * pt(-abs(b / se), 48))),
        parens(fmt_num(se, k))
    )
}

# Panel A: 2SLS with each instrument -------------------------------------------
insts <- c('z_main', 'z_uv', 'z_fw')
iv <- iv[instrument %in% insts & outcome %in% names(ys)]
stopifnot(nrow(iv) == length(insts) * length(ys))
a_rows <- rbindlist(
    lapply(
        names(ys),
        function(y) {
            e <- iv[outcome == y][match(insts, instrument)]
            as.data.table(
                as.list(
                    c(
                        row = ys[[y]],
                        cell(
                            e$b[1],
                            e$se[1],
                            k_of(y)
                        ), cell(e$b[2], e$se[2], k_of(y)),
                        cell(e$b[3], e$se[3], k_of(y))
                    )
                )
            )
        }
    )
)
span <- function(x, align = 'c') {
    c(
        sprintf('\\multicolumn{2}{%s}{%s}', align, x),
        '.skip'
    )
}
num <- function(x) sprintf('\\num{%.0f}', x)
fs <- iv[outcome == 'd_lkpu'][match(insts, instrument)]
a_spec <- rbind(
    as.data.table(
        as.list(
            c(
                row = 'First-stage coefficient',
                unlist(lapply(fmt_num(fs$fs, 3), span, align = 'r'))
            )
        )
    ),
    as.data.table(
        as.list(
            c(
                row = 'First-stage Wald (clustered)',
                unlist(lapply(num(fs$f), span, align = 'r'))
            )
        )
    ),
    as.data.table(
        as.list(
            c(
                row = '\\(N\\) (household-quarters)',
                unlist(lapply(num(fs$n), span, align = 'r'))
            )
        )
    )
)
setnames(a_rows, c('row', paste0('V', 1:6)))
setnames(a_spec, c('row', paste0('V', 1:6)))
tab_a <- rbind(a_rows, a_spec)
tab_a

write_tex_table(
    tab_a,
    'manuscript/tables/fixed_retailer.tex',
    header = c('', rep(c('\\(b\\)', 'SE'), 3)),
    spanners = c(
        `Main instrument` = 2, `Unit values` = 2,
        `Fixed retailer weights` = 2
    ),
    title = 'A. 2SLS, exposure inflation instrumented',
    rule_before = length(ys) + 1
)

# Panel B: reduced form on both parts ------------------------------------------
rf <- rf[outcome %in% names(ys)]
b_rows <- rbindlist(
    lapply(
        names(ys),
        function(y) {
            f <- rf[outcome == y & term == 'z_fw']
            m <- rf[outcome == y & term == 'z_mix']
            stopifnot(nrow(f) == 1, nrow(m) == 1)
            as.data.table(
                as.list(
                    c(
                        row = ys[[y]], cell(f$b, f$se, k_of(y)),
                        cell(m$b, m$se, k_of(y))
                    )
                )
            )
        }
    )
)
setnames(b_rows, c('row', paste0('V', 1:4)))
b_rows

write_tex_table(
    b_rows,
    'manuscript/tables/fixed_retailer.tex',
    header = c('', rep(c('\\(b\\)', 'SE'), 2)),
    spanners = c(`Fixed retailer weights` = 2, `Retailer mix` = 2),
    title = 'B. Reduced form on both parts of the instrument',
    append = TRUE
)
