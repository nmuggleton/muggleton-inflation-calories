# Table S27: households' own prices relative to what other households in the
# state paid for the same products, and where the gap comes from (the retailer,
# the month, deals, coupons). The second pair of columns keeps
# household-quarters with at least 80 percent of base spending priced at the
# month grain.

rm(list = ls())

source('src/lib/format.R')

r <- load_results('27_relative_prices')
own_rule <- r$own_rule$own_rule
est <- r$est[rule == own_rule]
est # one row per sample x price or step
r$coverage
r$flow

rows <- c(
    t1_log = '(1) Own prices, products bought in both years',
    own_matched = '(2) Own prices, products with a state price',
    state_same = '(3) State prices of the same products',
    gap = '(4) Own less state prices (2 minus 3)',
    where = '(5) Retailer',
    when = '(6) Month of purchase',
    deal = '(7) Deal flag',
    coupon = '(8) Coupons',
    rest = '(9) Within retailer, month and deal flag'
)
est <- est[outcome %in% names(rows)]
stopifnot(nrow(est) == 2 * length(rows))
body <- rbindlist(
    lapply(
        names(rows),
        function(y) {
            a <- est[sample == 'all' & outcome == y]
            d <- est[sample == 'dense' & outcome == y]
            as.data.table(
                as.list(
                    c(
                        row = rows[[y]],
                        paste0(
                            fmt_num(a$b, 3),
                            stars(2 * pt(-abs(a$b / a$se), 48))
                        ),
                        parens(fmt_num(a$se, 3)),
                        paste0(
                            fmt_num(d$b, 3),
                            stars(2 * pt(-abs(d$b / d$se), 48))
                        ),
                        parens(fmt_num(d$se, 3))
                    )
                )
            )
        }
    )
)
n_row <- as.data.table(
    as.list(
        c(
            row = '\\(N\\) (household-quarters)',
            sprintf(
                '\\multicolumn{2}{r}{\\num{%.0f}}',
                est[sample == 'all', n][1]
            ),
            '.skip',
            sprintf(
                '\\multicolumn{2}{r}{\\num{%.0f}}',
                est[sample == 'dense', n][1]
            ),
            '.skip'
        )
    )
)
setnames(body, c('row', paste0('V', 1:4)))
setnames(n_row, c('row', paste0('V', 1:4)))
tab <- rbind(body, n_row)
tab

write_tex_table(
    tab,
    'manuscript/tables/relative_prices.tex',
    header = c('', rep(c('b', 'SE'), 2)),
    spanners = c(`All` = 2, `Densely priced` = 2),
    rule_before = length(rows) + 1
)
