# Table S1: summary statistics for the estimation sample.

rm(list = ls())

source('src/lib/format.R')

s <- load_results('04_summary')$s
s # one row per variable: N, mean, SD, percentiles, and the decimals to show

# Group the variables as in Figure 2: spending and calories, then the diet
# measures (energy density, the eight items, and the health score that combines
# six of them), then measurement and inflation, then the changes
panel <- c(
    spend_food_nomag_ae_day_real = 'Spending (2019 dollars)',
    kcal_ae_day = 'Calories',
    kcal_per_usd_real = 'Calories',
    kcal_100g_food = 'Diet',
    sugar = 'Diet',
    satfat = 'Diet',
    sweet = 'Diet',
    snack = 'Diet',
    sodium = 'Diet',
    protein = 'Diet',
    fiber = 'Diet',
    fv = 'Diet',
    health_score = 'Diet',
    cov_lab = 'Measurement',
    infl_exposure = 'Inflation (percentage points)',
    z = 'Inflation (percentage points)',
    d_lspend =
        'Year-on-year changes: spending and calories',
    d_lkcal =
        'Year-on-year changes: spending and calories',
    d_lkpu =
        'Year-on-year changes: spending and calories',
    d_ed = 'Year-on-year changes: diet',
    d_sugar = 'Year-on-year changes: diet',
    d_satfat = 'Year-on-year changes: diet',
    d_sweet = 'Year-on-year changes: diet',
    d_snack = 'Year-on-year changes: diet',
    d_sodium = 'Year-on-year changes: diet',
    d_protein = 'Year-on-year changes: diet',
    d_fiber = 'Year-on-year changes: diet',
    d_fv = 'Year-on-year changes: diet',
    d_hs = 'Year-on-year changes: diet'
)
stopifnot(all(s$variable %in% names(panel)))
s[variable == 'd_sodium', digits := 0L] # whole milligrams, like the level
s[, panel := factor(panel[variable], levels = unique(panel))]
setorder(s, panel)

# Round each variable to its own number of decimals. SD in a label is the
# statistic, so it is set in maths, as in the headings
s[, label := gsub('\\bSD\\b', '\\\\(\\\\mathit{SD}\\\\)', label, perl = TRUE)]
# A unit given in the panel heading is not repeated in its rows; T2 and z are
# symbols; x100 takes a multiplication sign
s[variable == 'spend_food_nomag_ae_day_real',
    label := sub(' (2019 dollars)', '', label, fixed = TRUE)]
s[variable %in% c('infl_exposure', 'z'),
    label := sub(' (pp)', '', label, fixed = TRUE)]
s[variable == 'infl_exposure', label := sub('T2', '\\\\(T2\\\\)', label)]
s[variable == 'z', label := sub(' z$', ' \\\\(z\\\\)', label)]
s[, label := sub('(x100)', '(\\(\\times\\)100)', label, fixed = TRUE)]
tab <- s[, .(
    Variable = label,
    N = fmt_num(n, 0),
    Mean = fmt_num(mean, digits),
    SD = fmt_num(sd, digits),
    P10 = fmt_num(p10, digits),
    Median = fmt_num(p50, digits),
    P90 = fmt_num(p90, digits)
),
by = variable
][, variable := NULL]
tab

# Each panel starts at the first row of its group
first_row <- s[, .(row = .I[1]), by = panel]
write_tex_table(
    tab,
    'manuscript/tables/summary_gate.tex',
    panels = setNames(first_row$row, first_row$panel),
    indent = TRUE
) # rows indented under the italic panel headings
