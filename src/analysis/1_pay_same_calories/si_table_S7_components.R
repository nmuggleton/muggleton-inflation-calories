# Table S7: the six components of the health score.

rm(list = ls())

source('src/lib/format.R')

rc <- load_results('19_revision')$rc
rc # one row per component: 2SLS (and x100), SE, p, N

labels <- c(
    d_z_fiber = 'Fiber per 1,000 kcal',
    d_z_protein = 'Protein per 1,000 kcal',
    d_z_fv = 'Fruit and vegetable spend share',
    d_z_sugar = 'Sugar per 1,000 kcal',
    d_z_satfat = 'Saturated fat per 1,000 kcal',
    d_z_sodium = 'Sodium per 1,000 kcal'
)

tab <- rc[, .(
    Item = labels[outcome],
    b = paste0(fmt_num(b100, 3), stars(p)),
    se = parens(fmt_num(se100, 3)),
    p = fmt_p(p),
    N = fmt_num(n, 0)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/rev_components.tex',
    header = c('Item', '2SLS x100', 'SE', 'p', 'N')
)
