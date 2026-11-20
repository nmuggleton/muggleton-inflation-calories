# SI Table S45: household egg purchases and state egg inflation, in quarters of
# ordinary and of large national egg inflation.

rm(list = ls())

source('src/lib/format.R')


# Load the estimates -----------------------------------------------------------

eggs <- load_results('25_eggs')
rf <- eggs$rf # reduced forms: one row per outcome and kind of quarter
iv <- eggs$iv # 2SLS elasticities: one row per kind of quarter
rf
iv


# Format each estimate ---------------------------------------------------------

# Estimates to three decimals, with stars from a t test with 48 degrees of
# freedom (49 state clusters) and standard errors in parentheses
rf[, p := 2 * pt(-abs(b / se), df = 48)]
rf[, b_text := paste0(fmt_num(b, 3), stars(p))]
rf[, se_text := parens(fmt_num(se, 3))]

iv[, p := 2 * pt(-abs(b / se), df = 48)]
iv[, b_text := paste0(fmt_num(b, 3), stars(p))]
iv[, se_text := parens(fmt_num(se, 3))]


# Build the table --------------------------------------------------------------

# One row per outcome: estimate and SE in ordinary quarters (x_norm), then in
# egg price shocks (x_shock)
row_labels <- c(
    d_lpegg = '(1) Price paid per egg',
    d_leggs = '(2) Eggs bought',
    buy = '(3) Still buys eggs'
)

rf_wide <- dcast(
    rf,
    outcome ~ term,
    value.var = c('b_text', 'se_text')
)
rf_wide <- rf_wide[match(names(row_labels), outcome)]

tab <- rf_wide[, .(
    Outcome = row_labels[outcome],
    b0 = b_text_x_norm,
    se0 = se_text_x_norm,
    b1 = b_text_x_shock,
    se1 = se_text_x_shock
)]

# Row 4: the elasticities, ordinary quarters first
tab <- rbind(
    tab,
    data.table(
        Outcome = '(4) Elasticity of eggs bought (2 / 1, 2SLS)',
        b0 = iv[shock == FALSE, b_text],
        se0 = iv[shock == FALSE, se_text],
        b1 = iv[shock == TRUE, b_text],
        se1 = iv[shock == TRUE, se_text]
    )
)
tab


# Write ------------------------------------------------------------------------

write_tex_table(
    tab,
    'manuscript/tables/eggs.tex',
    header = c('', 'b', 'SE', 'b', 'SE'),
    spanners = c('Ordinary quarters' = 2, 'Egg price shocks' = 2),
    rule_before = 4
)
