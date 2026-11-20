# Table S26: other shopping margins (deals, private label, store types, trips)
# and recorded non-food spending.

rm(list = ls())

source('src/lib/format.R')

rm_ <- load_results('19_revision')$rm_
rm_ # one row per margin: 2SLS, SE, p, N
nonfood <- load_results('32_nonfood')$est[sample == 'all']
nonfood # non-food spending, its share, all spending, food spending
rm_ <- rbind(
    rm_[, .(outcome, b, se, p, n)],
    nonfood[
        outcome %in% c('d_lnonfood', 'd_sh_nonfood'),
        .(outcome, b, se, p, n)
    ]
)

labels <- c(
    d_sh_deal = 'Deal (promotion) share of spend, pp',
    d_sh_pl = 'Private-label share of spend, pp',
    d_sh_ch_grocery = 'Grocery-store share, pp',
    d_sh_ch_discount = 'Discount-store share, pp',
    d_sh_ch_club = 'Club-store share, pp',
    d_sh_ch_dollar = 'Dollar-store share, pp',
    d_n_trips_food = 'Food shopping trips per quarter',
    d_lnonfood = 'Non-food spending, percent',
    d_sh_nonfood = 'Non-food share of recorded spending, pp'
)
stopifnot(nrow(rm_) == length(labels))

tab <- rm_[, .(
    Outcome = labels[outcome],
    b = paste0(fmt_num(b, 3), stars(p)),
    se = parens(fmt_num(se, 3)),
    p = fmt_p(p),
    N = fmt_num(n, 0)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/rev_margins.tex',
    header = c('Outcome', '2SLS', 'SE', 'p', 'N')
)
