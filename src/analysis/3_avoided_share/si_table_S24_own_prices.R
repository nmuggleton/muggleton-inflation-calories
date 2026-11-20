# Table S24: calories per dollar split into the household's own prices on the
# products it bought in both years (its Laspeyres index) and a composition
# remainder; row 1 = -(row 2) + row 3. Panel A uses the rule of the paper for
# extreme price changes (a product is left out when its pre-coupon price changes
# by more than a factor of ten); panel B sets it against the alternatives, among
# them the threefold rule on net prices of Kaplan and Schulhofer-Wohl (2017).
# 2SLS coefficients on exposure, 49 state clusters.

rm(list = ls())

source('src/lib/format.R')

r <- load_results('35_own_price_rules')
est <- copy(r$est)
est # one row per rule x outcome: b, se, p, n, f1, pairs kept
main_rule <- r$flow$main_rule

# Panel A: the rule of the paper
rows <- c(
    d_lkpu = 'Calories per dollar (3 minus 2)',
    own = 'Own prices of products bought in both years',
    comp = 'Composition remainder'
)
a <- est[rule == main_rule][match(names(rows), outcome)]
stopifnot(!anyNA(a$b))
# ~0
a[outcome == 'comp', b] - a[outcome == 'own', b] - a[outcome == 'd_lkpu', b]

tab_a <- a[, .(
    Outcome = sprintf('(%d) %s', seq_len(.N), rows[outcome]),
    b = paste0(fmt_num(b, 3), stars(p)),
    se = parens(fmt_num(se, 3)),
    N = fmt_num(n, 0),
    F = fmt_num(f1, 0)
)]
tab_a

write_tex_table(
    tab_a,
    'manuscript/tables/own_prices.tex',
    header = c('', '\\(b\\)', 'SE', '\\(N\\)', 'First-stage \\(F\\)'),
    title = 'A. Price changes beyond tenfold before coupons left out'
)

# Panel B: own prices and composition under each rule
rules <- c(
    gross10 = 'Pre-coupon price, tenfold (panel A)',
    net3 = 'Net price, threefold',
    gross3 = 'Pre-coupon price, threefold',
    net10 = 'Net price, tenfold',
    none = 'None'
)
b <- est[outcome %in% c('own', 'comp')]
b <- b[order(match(rule, names(rules)))]
stopifnot(nrow(b) == 2 * length(rules))
b[, row := rules[rule]]
tab_b <- wide_bse(b, 'row', c('own', 'comp'))
by_rule <- b[outcome == 'own'][match(tab_b$row, row)]
all_pairs <- est[rule == 'none' & outcome == 'own', pairs]
tab_b[, `:=`(
    out = fmt_num(100 * (1 - by_rule$pairs / all_pairs), 2),
    n = fmt_num(by_rule$n, 0)
)]
tab_b

write_tex_table(
    tab_b,
    'manuscript/tables/own_prices.tex',
    header = c(
        'Price change left out beyond', rep(c('\\(b\\)', 'SE'), 2),
        'Left out, %', '\\(N\\)'
    ),
    spanners = c('Own prices' = 2, 'Composition' = 2, ' ' = 2),
    title = 'B. Alternative rules',
    append = TRUE
)

# Numbers for the text and the table note
r$dropped # pairs beyond each bound: pooled %, slope on exposure
r$coupon_use
if (!is.null(r$hand_codes)) r$hand_codes[, .N, keyby = code]
