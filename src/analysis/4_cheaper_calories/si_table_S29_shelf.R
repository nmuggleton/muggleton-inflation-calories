# Table S29: the savings available on households' own shelves.

rm(list = ls())

source('src/lib/format.R')

shelf <- load_results('08_mechanism')$shelf
shelf # one row per measure: mean, median, spend-weighted mean (%)

tab <- shelf[, .(
    Measure = measure,
    Mean = fmt_num(mean, 2),
    Median = fmt_num(median, 2),
    wmean = fmt_num(wmean, 2)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/shelf.tex',
    header = c('Measure', 'Mean', 'Median', 'Weighted mean')
)
