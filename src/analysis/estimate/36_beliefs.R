# Do households' stated beliefs about price changes track the food price changes
# they face, and do their purchases follow the prices or the beliefs? Nine waves
# of the PanelViews economic surveys (2015-2019) ask panel households by how
# much prices changed over the last 12 months.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200)
setFixest_notes(FALSE)

# (1) answers and the estimation sample ----------------------------------------
f <- file.path(
    Sys.getenv('WP2_INT'), 'panelviews', 'economic', 'beliefs_long.parquet'
)
stopifnot(file.exists(f))
svy <- wp2_sql(sprintf("SELECT * FROM read_parquet('%s')", f))
svy[, cell := paste(wave, wording)]

# The stated change, winsorised (as the tracking outcome) or clipped (as a
# regressor)
lim <- quantile(svy$perceived, c(0.01, 0.99))
svy[, belief_w := pmin(pmax(perceived, lim[1]), lim[2])]
svy[, belief := pmin(pmax(perceived, -10), 25)]
cat('answers:', nrow(svy), '| winsorized at', lim, '\n')

# Answer type: zero, a non-zero multiple of 5, another whole number, or a number
# with decimals
svy[, type := fifelse(
    perceived == 0,
    'Zero',
    fifelse(
        perceived %% 5 == 0,
        'Non-zero multiple of 5',
        fifelse(perceived %% 1 == 0, 'Other whole number', 'Decimal')
    )
)]
svy[, `:=`(
    zero = as.numeric(type == 'Zero'),
    mult5 = as.numeric(type == 'Non-zero multiple of 5'),
    precise = as.numeric(type %in% c('Other whole number', 'Decimal'))
)]

d <- f1_gate(
    c(
        'd_lspend', 'd_lkcal', 'd_lkpu', 'infl_laspeyres', 'f_idx_valid',
        'fpl_bin', 'head_college', 'head_age', 'n_base_items', 'cpi_fah_yoy'
    )
)
d <- d[yq %in% unique(svy$yq)]
quarters <- d[, .(
    exposure = mean(infl_exposure),
    cpi_fah = cpi_fah_yoy[1],
    n_sample = .N
), keyby = yq]

a <- merge(svy, d, by = c('household_code', 'yq'))
a[, own := fifelse(f_idx_valid, infl_laspeyres, NA_real_)]
a[, income := fifelse(
    fpl_bin %in% c('<=100%', '100-130%'),
    'Up to 130% of poverty line',
    fifelse(fpl_bin == '>500%', 'Above 500%', 'Between')
)]
cat(
    'answers matched to the estimation sample:', nrow(a),
    '| households', uniqueN(a$household_code),
    '| states', uniqueN(a$state), '\n'
)

# (2) the waves ----------------------------------------------------------------
waves <- a[, .(
    n = .N,
    belief_mean = mean(belief_w),
    belief_median = median(perceived),
    share_positive = mean(perceived > 0),
    exposure_sd = sd(infl_exposure)
), keyby = .(wave, wording, yq)]
waves[quarters, on = 'yq', `:=`(
    exposure = i.exposure,
    cpi_fah = i.cpi_fah
)]
cat('\n== waves\n')
print(waves, digits = 3)
years <- data.table(
    first_year = min(a$yq) %/% 10L,
    last_year = max(a$yq) %/% 10L,
    waves = uniqueN(a$wave),
    cells = uniqueN(a$cell),
    quarters = uniqueN(a$yq),
    n = nrow(a),
    households = uniqueN(a$household_code),
    exposure_mean = mean(a$infl_exposure),
    exposure_sd = sd(a$infl_exposure),
    cpi_fah_min = min(quarters$cpi_fah),
    cpi_fah_max = max(quarters$cpi_fah),
    cpi_fah_mean = mean(quarters$cpi_fah),
    belief_mean = mean(a$belief_w),
    belief_sd = sd(a$belief_w),
    belief_median = median(a$perceived)
)
cat('\n== survey period\n')
print(years, digits = 3)

# (3) tracking -----------------------------------------------------------------
# One row per specification: the stated change regressed on a measure of the
# household's inflation, SEs clustered by state.
track <- function(
    spec, dd, y = 'belief_w', x = 'infl_exposure',
    fe = 'cell'
) {
    m <- feols(
        as.formula(sprintf('%s ~ %s | %s', y, x, fe)),
        dd,
        cluster = ~state
    )
    r <- coef_row(m, spec, y, x)
    r[, `:=`(sd_y = sd(dd[[y]]), sd_x = sd(dd[[x]], na.rm = TRUE))]
    r[]
}
tracking <- rbind(
    track('All waves', a),
    track('Own-prices wording, 2015-16', a[wording == 'own prices']),
    track('Inflation wording', a[wording == 'inflation']),
    track('2015-16', a[yq < 20170]),
    track('2018-19', a[yq > 20170]),
    track('Within household', a, fe = 'cell + household_code'),
    track('Answer clipped to -10 to 25', a, y = 'belief'),
    track('State mean of exposure (instrument)', a, x = 'z'),
    track('Own price index', a[!is.na(own)], x = 'own')
)
tracking[, f_wald := NULL]
# The same slope by 2SLS, with exposure instrumented as in the main design
m_iv <- feols(belief_w ~ 1 | cell | infl_exposure ~ z, a, cluster = ~state)
tracking_iv <- coef_row(m_iv, '2SLS, all waves', 'belief_w')
cat("\n== stated change on the household's inflation (per point)\n")
print(tracking, digits = 3)
print(tracking_iv, digits = 3)
b <- tracking[design == 'All waves']
say(
    'all waves: %.3f (%.3f), 95%% upper bound %.3f, N %d',
    b$b, b$se, b$b + qt(0.975, 48) * b$se, b$n
)

# (4) purchases in the survey quarters: prices or beliefs ----------------------
# The main design (2SLS, exposure instrumented by z, state clusters) on the
# survey sample, with the stated change added as a regressor.
outcomes <- c('d_lspend', 'd_lkcal', 'd_lkpu')
behave <- rbindlist(
    lapply(
        outcomes,
        function(y) {
            m1 <- feols(
                as.formula(
                    sprintf('%s ~ belief | cell | infl_exposure ~ z', y)
                ),
                a,
                cluster = ~state
            )
            m0 <- feols(
                as.formula(sprintf('%s ~ 1 | cell | infl_exposure ~ z', y)),
                a,
                cluster = ~state
            )
            mq <- feols(
                as.formula(
                    sprintf('%s ~ belief | yq | infl_exposure ~ z', y)
                ),
                a,
                cluster = ~state
            )
            mo <- feols(
                as.formula(sprintf('%s ~ infl_exposure + belief | cell', y)),
                a,
                cluster = ~state
            )
            rbind(
                coef_row(m1, '2SLS with belief', y)[, term := 'exposure'],
                coef_row(m1, '2SLS with belief', y, 'belief')[
                    ,
                    term := 'belief'
                ],
                coef_row(m0, '2SLS without belief', y)[, term := 'exposure'],
                coef_row(mq, '2SLS, quarter effects', y)[
                    ,
                    term := 'exposure'
                ],
                coef_row(mq, '2SLS, quarter effects', y, 'belief')[
                    ,
                    term := 'belief'
                ],
                coef_row(mo, 'OLS with belief', y, 'infl_exposure')[
                    ,
                    term := 'exposure'
                ],
                coef_row(mo, 'OLS with belief', y, 'belief')[
                    ,
                    term := 'belief'
                ]
            )
        }
    )
)
cat('\n== purchases on exposure and the stated change (per point)\n')
print(behave, digits = 3)
say('SD of the clipped stated change: %.2f', sd(a$belief))

# (5) rounding -----------------------------------------------------------------
type_levels <- c(
    'Zero', 'Non-zero multiple of 5', 'Other whole number', 'Decimal'
)
types <- a[, .(
    n = .N,
    belief_mean = mean(belief_w),
    subj_sd = mean(subj_sd, na.rm = TRUE),
    n_bins = mean(n_bins, na.rm = TRUE),
    one_bin = mean(n_bins == 1, na.rm = TRUE)
), by = type]
types[, share := n / sum(n)]
by_income <- dcast(
    a[income != 'Between', .N, by = .(type, income)][
        ,
        share := N / sum(N),
        by = income
    ],
    type ~ income,
    value.var = 'share'
)
setnames(
    by_income,
    c('Up to 130% of poverty line', 'Above 500%'),
    c('share_low', 'share_high')
)
types <- types[by_income, on = 'type'][match(type_levels, type)]
cat('\n== answer types\n')
print(types, digits = 3)

# The spread of the respondent's own distribution, against precise answers
m_sd <- feols(subj_sd ~ zero + mult5 | cell, a, cluster = ~household_code)
uncertainty <- rbind(
    coef_row(m_sd, 'cell effects', 'subj_sd', 'zero'),
    coef_row(m_sd, 'cell effects', 'subj_sd', 'mult5')
)
uncertainty[, `:=`(term = c('zero', 'mult5'), f_wald = NULL)]
cat('\n== subjective SD by answer type, against precise answers\n')
print(uncertainty, digits = 3)

# Who rounds: linear probability models with cell effects and household clusters
a[, fpl := relevel(factor(fpl_bin), '>500%')]
a[, age_grp := cut(
    head_age,
    c(0, 39, 54, 64, 120),
    labels = c('<40', '40-54', '55-64', '65+')
)]
m_who <- feols(
    c(zero, mult5, precise) ~
        fpl + head_college + age_grp + log(n_base_items) | cell,
    a,
    cluster = ~household_code
)
who <- rbindlist(
    lapply(
        m_who,
        function(m) {
            ct <- coeftable(m)
            data.table(
                outcome = as.character(m$fml[[2]]),
                term = rownames(ct),
                b = ct[, 1],
                se = ct[, 2],
                p = ct[, 4],
                n = nobs(m)
            )
        }
    )
)
cat('\n== who rounds\n')
print(who, digits = 3)

save_results(
    '36_beliefs',
    years = years,
    waves = waves,
    tracking = tracking,
    tracking_iv = tracking_iv,
    behave = behave,
    types = types,
    uncertainty = uncertainty,
    who = who
)
