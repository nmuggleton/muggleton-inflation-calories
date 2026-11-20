# Table S4: inference with few clusters. Clustered SEs, wild cluster restricted
# bootstrap (WCR) CIs and p-values, and randomisation inference.

rm(list = ls())

source('src/lib/format.R')

inf <- load_results('06_inference')
inf$an # 2SLS estimate and its SE under each clustering
inf$wcr # WCR 95% CI and p-value, by state and by census division
inf$ri # randomisation inference p-values (b0 = 0 is the null of no effect)

# One panel per outcome; one row per way of doing inference
state <- merge(
    inf$an[, .(outcome, se = se_state)],
    inf$wcr[cluster == 'state', .(outcome, lo, hi, p = p0)]
)
division <- merge(
    inf$an[, .(outcome, se = se_div)],
    inf$wcr[cluster == 'region_code', .(outcome, lo, hi, p = p0)]
)
twoway <- inf$an[, .(outcome, se = se_2way)]
ri <- inf$ri[b0 == 0, .(outcome, p = p_path)]

state[, Inference := 'Clustered by state (49)']
division[, Inference := 'Clustered by census division (9)']
twoway[, Inference := 'Clustered by state and by quarter']
ri[, Inference := "Shuffling states' inflation paths"]

long <- rbind(state, division, twoway, ri, fill = TRUE)
long[, outcome := factor(outcome, levels = inf$an$outcome)]
setorder(long, outcome)
long[, k := fifelse(outcome == 'd_hs', 5L, 3L)] # health-score SEs are small
long

tab <- long[, .(
    Inference,
    SE = fmt_num(se, k),
    Lower = fmt_num(lo, k),
    Upper = fmt_num(hi, k),
    p = fmt_p(p)
)]
tab

# panel headings carry the outcome and its 2SLS estimate
# outcome names as in the main text
outcome_name <- c(
    d_lkpu = 'Calories per dollar', d_lspend = 'Real spend',
    d_lkcal = 'Calories bought', d_ed = 'Energy density',
    d_hs = 'Health score'
)
heading <- inf$an[, sprintf(
    '%s: 2SLS estimate \\num{%s}',
    outcome_name[outcome],
    fmt_num(b, fifelse(outcome == 'd_hs', 5L, 3L))
)]
first_row <- long[, .(row = .I[1]), by = outcome]
write_tex_table(
    tab,
    'manuscript/tables/inference.tex',
    header = c('', 'SE', 'Lower', 'Upper', '\\(p\\)'),
    spanners = c(' ' = 1, 'Bootstrap 95% CI' = 2, ' ' = 1),
    panels = setNames(first_row$row, heading),
    indent = TRUE
) # rows indented under the italic outcome headings

# benchmark tests (kcal/$ b = -1; spend b = +1), quoted in the text
merge(
    inf$wcr[!is.na(p_bench), .(outcome, cluster, p_bench)],
    inf$ri[b0 != 0, .(outcome, b0, p_path, p_withinq)],
    by = 'outcome'
)
