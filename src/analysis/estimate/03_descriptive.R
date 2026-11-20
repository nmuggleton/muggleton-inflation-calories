# Where the identifying variation comes from. Households count equally, as in
# the unweighted headline estimator, so these numbers describe the variation
# that estimator actually uses.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)

# FIPS -> postal code (50 states + DC)
FIPS <- c(
    `1` = 'AL', `2` = 'AK', `4` = 'AZ', `5` = 'AR', `6` = 'CA',
    `8` = 'CO', `9` = 'CT', `10` = 'DE', `11` = 'DC', `12` = 'FL',
    `13` = 'GA', `15` = 'HI', `16` = 'ID', `17` = 'IL', `18` = 'IN',
    `19` = 'IA', `20` = 'KS', `21` = 'KY', `22` = 'LA', `23` = 'ME',
    `24` = 'MD', `25` = 'MA', `26` = 'MI', `27` = 'MN', `28` = 'MS',
    `29` = 'MO', `30` = 'MT', `31` = 'NE', `32` = 'NV', `33` = 'NH',
    `34` = 'NJ', `35` = 'NM', `36` = 'NY', `37` = 'NC', `38` = 'ND',
    `39` = 'OH', `40` = 'OK', `41` = 'OR', `42` = 'PA', `44` = 'RI',
    `45` = 'SC', `46` = 'SD', `47` = 'TN', `48` = 'TX', `49` = 'UT',
    `50` = 'VT', `51` = 'VA', `53` = 'WA', `54` = 'WV', `55` = 'WI',
    `56` = 'WY'
)

d <- f1_sql(
    "SELECT household_code, yq, year, qtr, state, n_base_items,
  infl_exposure, cpi_fah_yoy FROM F1 WHERE est_gate"
)
# This should have 2,783,093 observations (the f1 sample, see 01_main.R). No
# observations are lost to the leave-out instrument.
stopifnot(
    nrow(d) == F1_N,
    !anyNA(d$infl_exposure),
    !anyNA(d$state),
    all(d$yq == 10 * d$year + d$qtr)
)
d[, state := as.integer(state)]
stopifnot(all(as.character(d$state) %in% names(FIPS)))
d[, st := FIPS[as.character(state)]]
d[, z := (sum(infl_exposure) - infl_exposure) / (.N - 1), by = .(state, yq)]
stopifnot(all(is.finite(d$z)))
cat(
    'sample flow: est_gate',
    nrow(d),
    'household-quarters |',
    uniqueN(d$household_code),
    'households |',
    uniqueN(d$state),
    'states |',
    uniqueN(d[, .(state, yq)]),
    'state-quarters |',
    uniqueN(d$yq),
    'quarters\n'
)


# 1. Variance decomposition of household T2 (ANOVA, nested) --------------------

# T2 = national quarter mean + (state-quarter mean - national)
# + (household - state-quarter mean)

d[, m_q := mean(infl_exposure), by = yq]
d[, c_sq := mean(infl_exposure), by = .(state, yq)]
d[, `:=`(v_state = c_sq - m_q, v_hh = infl_exposure - c_sq)]
gm <- mean(d$infl_exposure)
ss <- c(
    total = sum((d$infl_exposure - gm)^2),
    national = sum((d$m_q - gm)^2), state = sum(d$v_state^2),
    household = sum(d$v_hh^2)
)
# Nested ANOVA: the components should sum to the total variation (up to
# floating-point error)
stopifnot(abs(sum(ss[-1]) / ss[['total']] - 1) < 1e-8)
sh <- ss[-1] / ss[['total']]
lam <- ss[['state']] / (ss[['state']] + ss[['household']])
cat('SD of household T2 (pp):', sd(d$infl_exposure), '| mean:', gm, '\n')
cat(
    'variance shares: national (between quarters)',
    sh[['national']],
    '| state within quarter',
    sh[['state']],
    '| household within state-quarter',
    sh[['household']],
    '\n'
)
cat('within-quarter share from the state component:', lam, '\n')
cat(
    'SD within quarter: state component',
    sd(d$v_state),
    '| household component',
    sd(d$v_hh),
    '\n'
)
# Variation in the instrument net of quarter fixed effects (the relevant
# variation for the unweighted 2SLS)
d[, zt := z - mean(z), by = yq]
cat('SD of instrument net of quarter FE:', sd(d$zt), '\n')

# Variation by size of base basket (same binning as the reliability figure)
d[, items := cut(
    n_base_items,
    c(0, 50, 100, 200, 400, Inf),
    right = FALSE,
    labels = c(
        '10-49', '50-99', '100-199', '200-399',
        '400+'
    )
)]
stopifnot(!anyNA(d$items))
bb <- d[, .(
    n = .N,
    sd_hh = sd(v_hh),
    sd_state = sd(v_state),
    lam = var(v_state) / (var(v_state) + var(v_hh))
),
keyby = items
]
print(bb)


# 2. Time series: national exposure, CPI food at home, cross-state spread ------

q <- d[, .(
    nat = mean(infl_exposure),
    cpi = mean(cpi_fah_yoy),
    cpi_sd = sd(cpi_fah_yoy),
    n = .N
), keyby = .(year, qtr, yq)]
# CPI is a national series, so it should be constant within quarter
stopifnot(all(is.na(q$cpi_sd) | q$cpi_sd < 1e-9), !anyNA(q$cpi))
cells <- d[
    ,
    .(n = .N, c_sq = c_sq[1], m_q = m_q[1]),
    keyby = .(state, st, year, qtr, yq)
]
cells[, dev := c_sq - m_q]
sp <- cells[, .(
    p10 = quantile(dev, .10),
    p25 = quantile(dev, .25),
    p50 = quantile(dev, .50),
    p75 = quantile(dev, .75),
    p90 = quantile(dev, .90),
    # 90% and 95% ranges across states (for Fig. S1)
    p05 = quantile(dev, .05),
    p95 = quantile(dev, .95),
    p025 = quantile(dev, .025),
    p975 = quantile(dev, .975),
    sd_w = sqrt(sum(n * dev^2) / sum(n)),
    n_states = .N
),
keyby = .(year, qtr, yq)
]
q <- sp[q, on = .(year, qtr, yq)]
cat(
    'cross-state SD of state exposure within quarter (household-weighted):',
    'mean',
    mean(q$sd_w),
    '| min',
    min(q$sd_w),
    '| max',
    max(q$sd_w),
    '\n'
)
cat('p90 - p10 of state deviation: mean', mean(q$p90 - q$p10), '\n')
cat('corr(national T2, CPI FAH) across quarters:', cor(q$nat, q$cpi), '\n')
cat(
    'national T2 range:',
    range(q$nat),
    '| CPI FAH range:',
    range(q$cpi),
    '\n'
)
cat(
    'states per quarter: min',
    min(q$n_states),
    'max',
    max(q$n_states),
    '\n'
)
print(
    q[, .(
        yq,
        nat = round(nat, 2),
        cpi = round(cpi, 2),
        sd_w = round(sd_w, 2),
        p10 = round(p10, 2),
        p90 = round(p90, 2),
        n
    )]
)
# largest episodes of cross-state dispersion
print(q[order(-sd_w)][1:6, .(yq, nat, sd_w)])


# 3. Each state's share of the identifying variation ---------------------------

# share_s = sum over its household-quarters of (z net of quarter FE)^2

ss_st <- d[, .(n = .N, ssz = sum(zt^2)), keyby = .(state, st)]
ss_st[, `:=`(sh_var = ssz / sum(ssz), sh_n = n / sum(n))]
stopifnot(abs(sum(ss_st$sh_var) - 1) < 1e-9)
eff_n <- 1 / sum(ss_st$sh_n^2)
eff_v <- 1 / sum(ss_st$sh_var^2)
cat(
    'effective clusters (1/HHI): household-quarter shares',
    eff_n,
    '| identifying-variation shares',
    eff_v,
    '\n'
)
setorder(ss_st, -sh_var)
ss_st[, cum := cumsum(sh_var)]
cat(
    'states needed for half of the identifying variation:',
    ss_st[, which(cum >= .5)[1]],
    '\n'
)
cat(
    "top 5 states' share of identifying variation:",
    sum(ss_st$sh_var[1:5]),
    '| of household-quarters:',
    sum(ss_st$sh_n[1:5]),
    '\n'
)
print(
    ss_st[, .(
        st,
        n,
        sh_n = round(sh_n, 4),
        sh_var = round(sh_var, 4),
        ratio = round(sh_var / sh_n, 2)
    )]
)


# For the introduction figure (0_introduction/figure_1_schematic.R) ------------

# (a) National exposure vs. the CPI for food at home, with no gap in the series:
# 2020 is left out of the estimation sample only by the year condition, so its
# quarters are added here with every other estimation-sample rule applied
# (02_build.sql, est_gate).

d20 <- f1_sql(
    "SELECT yq, year, qtr, infl_exposure, cpi_fah_yoy FROM F1
  WHERE year = 2020 AND core_a AND core_b AND NOT partial_a AND NOT partial_b
    AND round(cov_lab, 9) >= 0.5 AND round(cov_lab_b, 9) >= 0.5
    AND kcal_per_usd_real > 0 AND kpu_b > 0"
)
cat(
    '2020 household-quarters meeting every other estimation-sample rule:',
    nrow(d20),
    '\n'
)
q_all <- rbind(d[, .(yq, year, qtr, infl_exposure, cpi_fah_yoy)], d20)[
    , .(
        nat = mean(infl_exposure),
        cpi = mean(cpi_fah_yoy),
        n = .N,
        in_sample = year[1] != 2020
    ),
    keyby = .(year, qtr, yq)
]
stopifnot(
    nrow(q_all) == expected_n('quarter_all'),
    !anyNA(q_all$nat),
    !anyNA(q_all$cpi)
)
print(q_all[year == 2020])

# The four quarters of 2021, a representative year: the household mean is within
# 0.3 points of the CPI (Fig. 0A)
q_yr <- 2021 * 10L + 1:4
stopifnot(all(q_yr %in% d$yq))
BIN <- 0.25 # histogram bin width (percentage points)
CLOSE <- 1 # "close to the CPI": within 1 point
h <- d[yq %in% q_yr, .(yq, x = infl_exposure, cpi = cpi_fah_yoy)]
hist_yr <- h[, .(n = .N), keyby = .(yq, bin = floor(x / BIN) * BIN)]
hist_yr[, `:=`(bin_lo = bin, bin_hi = bin + BIN, share = n / sum(n)), by = yq]
hist_yr[, bin := NULL]
spread_yr <- h[, .(
    cpi = cpi[1],
    n = .N,
    mean = mean(x),
    sd = sd(x),
    below = mean(x < cpi - CLOSE),
    close = mean(abs(x - cpi) <= CLOSE),
    above = mean(x > cpi + CLOSE)
),
keyby = yq
]
print(spread_yr)
rm(d20, h)

save_results(
    '03_descriptive',
    q = q,
    q_all = q_all,
    hist_yr = hist_yr,
    spread_yr = spread_yr,
    stats = data.table(
        n_hhq = nrow(d),
        n_cells = nrow(cells),
        med_cell = median(cells$n),
        mean_t2 = gm,
        sd_t2 = sd(d$infl_exposure),
        sh_national = sh[['national']],
        sh_state = sh[['state']],
        sh_household = sh[['household']],
        lam = lam,
        sd_w_mean = mean(q$sd_w),
        sd_zt = sd(d$zt),
        eff_n = eff_n,
        eff_v = eff_v
    ),
    bb = bb
)
print(sessionInfo())
