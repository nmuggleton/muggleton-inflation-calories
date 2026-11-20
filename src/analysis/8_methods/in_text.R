# Numbers quoted in Materials and Methods and the SI data section.

rm(list = ls())

source('src/lib/format.R')
desc <- load_results('03_descriptive')
summ <- load_results('04_summary')

cat('== Data\n')
st <- desc$stats
say(
    'Estimation sample: %s household-quarters, %s state-quarter cells',
    format(st$n_hhq, big.mark = ','),
    format(st$n_cells, big.mark = ',')
)
say(
    'Labeled share of food spending: mean %.2f',
    summ$s[variable == 'cov_lab', mean]
)

cat('\n== Exposure and instrument\n')
q <- desc$q
say(
    'Correlation of national mean exposure with CPI food at home: %s',
    fmt_bounded(cor(q$nat, q$cpi), 3)
)
say(
    'Cross-state SD of exposure within quarter, mean over quarters: %.2f pp',
    st$sd_w_mean
)
say(
    'p90 - p10 of state deviation, mean over quarters: %.2f pp',
    mean(q$p90 - q$p10)
)
say(
    'Variance shares: national %.3f, state within quarter %.3f, household %.3f',
    st$sh_national,
    st$sh_state,
    st$sh_household
)

cat('\n== Sample path (Table S2)\n')
say(
    'Panel universe: %s household-quarters, %s households',
    format(summ$universe$n, big.mark = ','),
    format(summ$universe$hh, big.mark = ',')
)
print(summ$flow)
print(summ$cells)
