# Table S2: the sample path from the panel to the estimation sample, and the
# size of the estimation sample.

rm(list = ls())

source('src/lib/format.R')

summ <- load_results('04_summary')
summ$universe # all household-quarters in the panel
summ$flow # household-quarters left after each step (s0 to s4)
summ$cells # households per state-quarter: median, 10th percentile
households <- load_results('08_mechanism')$flow[1, households]
stats <- load_results('03_descriptive')$stats

# A: from the panel to the estimation sample -----------------------------------
steps <- c(summ$universe$n, unlist(summ$flow[, .(s0, s1, s2, s3, s4)]))
path <- data.table(
    row = c(
        sprintf('All household-quarters, 2004--%d', END_YEAR),
        'Same quarter a year earlier in the panel, valid exposure, known state',
        'Quarter \\(t\\) not in 2020',
        'Complete reporting in \\(t\\) and \\(t-4\\)',
        'Neither quarter partial',
        paste0(
            'Labeled share at least .5 and positive ',
            'calories per dollar in both quarters'
        )
    ),
    number = fmt_num(steps, 0),
    # % of previous step
    kept = c('{---}', fmt_num(100 * steps[-1] / steps[-length(steps)], 1))
)

# B: the estimation sample -----------------------------------------------------
sample <- data.table(
    row = c(
        'Households', 'States (48 contiguous and DC)', 'State-quarters',
        'Households per state-quarter, median',
        'Households per state-quarter, 10th percentile',
        'Effective number of state clusters, by household-quarters',
        'Effective number of state clusters, by identifying variation'
    ),
    number = c(
        fmt_num(households, 0), fmt_num(49, 0), fmt_num(stats$n_cells, 0),
        fmt_num(summ$cells$med, 0), fmt_num(summ$cells$p10, 0),
        fmt_num(stats$eff_n, 1), fmt_num(stats$eff_v, 1)
    ),
    kept = '{}'
)

tab <- rbind(path, sample)
tab

write_tex_table(
    tab,
    'manuscript/tables/sample_path.tex',
    header = c('', 'Number', 'Kept (\\%)'),
    panels = c(
        'From the panel to the estimation sample (household-quarters)' = 1,
        'The estimation sample' = 7
    ),
    indent = TRUE
)
