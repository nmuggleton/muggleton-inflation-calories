# Table S28: where the gap between own and state prices of the same products
# sits (row 4 of Table S27). The products are split by the channel of the
# retailer, by whether another household in the state bought the product at the
# same retailer, and by the store.

rm(list = ls())

source('src/lib/format.R')

r <- load_results('37_price_gap_sources')
r$total # the gap in log points (Table S27) and in index points (adds up)
r$contrib
r$own
r$route # channel contributions by a second route

chan <- c(
    'Grocery' = 'Grocery stores',
    'Discount Store' = 'Discount stores',
    'Warehouse Club' = 'Warehouse clubs',
    'Other' = 'Dollar, drug, and other stores'
)
rows <- c(
    setNames(chan, paste0('ch:', names(chan))),
    'cov:1' = 'Yes',
    'cov:0' = 'No',
    'sw:same_store' = 'Same store',
    'sw:sw_within' = 'Another store of the same retailer',
    'sw:sw_ret' = 'Another retailer',
    'sw:unk_store' = 'Store not recorded'
)

# Parts A to C: contributions and own gaps
con <- r$contrib[match(names(rows), grp)]
gap <- r$own[outcome == 'gap'][match(names(rows), grp)]
stopifnot(!anyNA(con$b), !anyNA(gap$b))
cell <- function(b, se) {
    c(
        paste0(fmt_num(b, 3), stars(2 * pt(-abs(b / se), 48))),
        parens(fmt_num(se, 3))
    )
}
tot <- r$total
body <- rbindlist(
    lapply(
        seq_along(rows),
        function(i) {
            as.data.table(
                as.list(
                    c(
                        row = rows[[i]],
                        fmt_num(100 * con$sh_spend[i], 1),
                        cell(con$b[i], con$se[i]),
                        fmt_num(100 * con$share_of_gap[i], 0),
                        cell(gap$b[i], gap$se[i])
                    )
                )
            )
        }
    )
)
top <- as.data.table(
    as.list(
        c(
            row = 'All products',
            '100.0',
            cell(tot[outcome == 'agap', b], tot[outcome == 'agap', se]),
            '100',
            cell(tot[outcome == 'gap', b], tot[outcome == 'gap', se])
        )
    )
)
tab <- rbind(top, body, use.names = FALSE)
setnames(tab, c('row', paste0('V', 1:6)))
tab

write_tex_table(
    tab,
    'manuscript/tables/price_gap_sources.tex',
    header = c('', 'Spending, %', 'b', 'SE', 'Share, %', 'b', 'SE'),
    spanners = c(' ' = 1, 'Part of the gap' = 3, 'Gap on own products' = 2),
    panels = c(
        'A. Channel of the retailer' = 2,
        'B. Another household bought the product at the same retailer' = 6,
        'C. Store of the two purchases' = 8
    )
)

# Part D: products another household bought at the same retailer
parts <- copy(r$parts)
parts[, row := fifelse(
    grp == 'cov:1',
    'All channels',
    chan[sub('^covch:(.*)\\|1$', '\\1', grp)]
)]
parts <- parts[order(match(row, c('All channels', chan)))]
tab_d <- wide_bse(parts, 'row', c('gap', 'within', 'ret_step'))
tab_d

write_tex_table(
    tab_d,
    'manuscript/tables/price_gap_sources.tex',
    header = c('', rep(c('b', 'SE'), 3)),
    spanners = c(
        'Gap' = 2,
        'Household less retailer' = 2,
        'Retailer less state' = 2
    ),
    title = paste(
        'D. Products another household bought at the same retailer:',
        'the gap in two parts'
    ),
    append = TRUE
)

# Part E: do households change store or retailer as exposure rises?
sw <- c(
    sh_within = 'Another store of the same retailer',
    sh_ret = 'Another retailer',
    sh_unk = 'Store not recorded',
    d_deal = 'Bought on deal, current less base quarter'
)
e <- r$switching[years == 'all'][match(names(sw), outcome)]
tab_e <- e[, .(
    row = sw[outcome],
    mean = fmt_num(mean, 1),
    b = paste0(fmt_num(b, 3), stars(2 * pt(-abs(b / se), 48))),
    se = parens(fmt_num(se, 3))
)]
tab_e

write_tex_table(
    tab_e,
    'manuscript/tables/price_gap_sources.tex',
    header = c('', 'Mean, %', 'b', 'SE'),
    title = 'E. Share of base spending on the same products, by store',
    append = TRUE
)

# Numbers for the table note and the SI log
r$store_2011
r$rms
r$moved
r$switching
r$flow
