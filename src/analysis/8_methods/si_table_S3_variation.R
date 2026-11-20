# Table S3: where the variation in exposure inflation comes from: its spread,
# and how its variance splits into national, state and household parts. (Sample
# sizes and effective clusters are in Table S4.) Statistics from
# estimate/03_descriptive.R.

rm(list = ls())

source('src/lib/format.R')

stats <- load_results('03_descriptive')$stats
stats # one row of sample sizes, moments and variance shares

tab <- data.table(
    Quantity = c(
        'Mean across household-quarters',
        '\\(\\mathit{SD}\\) across household-quarters',
        '\\(\\mathit{SD}\\) across states within a quarter, averaged over quarters',
        '\\(\\mathit{SD}\\) of the instrument after removing quarter means',
        'National: common to all households in a quarter',
        'State: differences between states in the same quarter',
        'Household: differences within a state and quarter'
    ),
    Value = c(
        fmt_num(stats$mean_t2, 2),
        fmt_num(stats$sd_t2, 2),
        fmt_num(stats$sd_w_mean, 2),
        fmt_num(stats$sd_zt, 2),
        fmt_num(100 * stats$sh_national, 1),
        fmt_num(100 * stats$sh_state, 1),
        fmt_num(100 * stats$sh_household, 1)
    )
)
tab

write_tex_table(
    tab,
    'manuscript/tables/variation.tex',
    header = c('', 'Value'),
    panels = c(
        'Exposure inflation \\(T2\\) (percentage points)' = 1,
        'Share of the variance in \\(T2\\) (\\%)' = 5
    ),
    indent = TRUE
)
