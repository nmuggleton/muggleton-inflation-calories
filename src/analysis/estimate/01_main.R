# The main estimates. Each outcome (calories per dollar, real food spending,
# calories, energy density and the health score) is a year-on-year change,
# regressed on exposure inflation by OLS and by 2SLS, where the instrument is
# the mean exposure inflation of the other households in the same state and
# quarter.

rm(list = ls())

library(data.table)
library(fixest)

options(width = 200, scipen = 999, datatable.print.nrows = 1000)

source('src/lib/helpers.R')

set.seed(20261001)


# Data -------------------------------------------------------------------------

# Load the estimation sample (2.8m household-quarters).
d <- f1_sql(
    "SELECT
  household_code,
  yq,
  year,
  state,
  region_code,
  income_cat,
  projection_factor,
  n_base_items,
  infl_exposure,
  d_lkpu,
  d_lspend,
  d_lkcal,
  d_ed,
  d_hs,
  d_lspend_lab
  FROM F1
  WHERE est_gate"
)

d[, projection_factor := as.numeric(projection_factor)]
stopifnot(nrow(d) == F1_N, !anyNA(d$infl_exposure), !anyNA(d$state))

# Cell labels, group by quarter.
d[
    ,
    `:=`(
        state_q = paste(state, yq),
        div_q = paste(region_code, yq),
        inc_q = paste(income_cat, yq)
    )
]

# Instrument z: the mean exposure inflation of the other households in the same
# state and quarter, leaving the household's own value out.
d[
    ,
    z := (sum(infl_exposure) - infl_exposure) / (.N - 1),
    by = state_q
]

d <- d[is.finite(z)]
cat(
    'sample:',
    nrow(d),
    'household-quarters,',
    uniqueN(d$household_code),
    'households,',
    uniqueN(d$state),
    'states,',
    uniqueN(d$state_q),
    'state-quarters (',
    F1_N - nrow(d),
    'dropped for lack of an instrument)\n'
)

# Labels and model formulas ----------------------------------------------------
outcomes <- c(
    d_lkpu = 'Change log kcal per real $ (x100)',
    d_lspend = 'Change log real spend per AE-day (x100)',
    d_lkcal = 'Change log kcal per AE-day (x100)',
    d_ed = 'Change energy density (kcal/100 g)',
    d_hs = 'Change health score'
)

setFixest_dict(
    c(
        outcomes,
        infl_exposure = 'Exposure inflation T2 (pp)',
        fit_infl_exposure = 'Exposure inflation T2 (pp)',
        yq = 'Quarter',
        div_q = 'Division x quarter',
        inc_q = 'Income x quarter',
        household_code = 'Household'
    )
)

# 1. Main result: OLS and 2SLS -------------------------------------------------
# I estimate five OLS regressions, one per outcome, in a single call of the form
# c(y1, y2, ...) ~ exposure | quarter FE, where exposure is the household's own
# exposure inflation.
y_list <- paste(names(outcomes), collapse = ', ')
ols_formula <- as.formula(sprintf('c(%s) ~ infl_exposure | yq', y_list))
iv_formula <- as.formula(sprintf('c(%s) ~ 1 | yq | infl_exposure ~ z', y_list))

ols <- feols(ols_formula, d, cluster = ~state)
iv <- feols(iv_formula, d, cluster = ~state)
print(etable(ols))
print(etable(iv, stage = 1:2))

first_stage_stats <- fitstat(iv[[1]], ~ ivf1 + ivwald1)
print(first_stage_stats)
first_stage <- data.table(
    coef = coef(summary(iv[[1]], stage = 1))[['z']],
    f = first_stage_stats$ivf1$stat,
    wald = first_stage_stats$ivwald1$stat
)

cat(
    'first stage (z):',
    first_stage$coef,
    '| F =',
    first_stage$f,
    '| cluster Wald =',
    first_stage$wald,
    '\n'
)

headline <- rbindlist(
    lapply(
        seq_along(outcomes),
        function(k) {
            # one row per outcome: the coefficient on exposure inflation
            data.table(
                outcome = names(outcomes)[k],
                b_ols = coef(ols[[k]])[[1]],
                se_ols = se(ols[[k]])[[1]],
                b_iv = coef(iv[[k]])[[1]],
                se_iv = se(iv[[k]])[[1]],
                n = nobs(iv[[k]])
            )
        }
    )
)
print(headline)

# Budget identity: d log kcal = d log spend + d log kcal per dollar.
iv_est <- function(y) headline[outcome == y, b_iv]
cat(
    'identity check (IV): spend + kcal/$ =',
    iv_est('d_lspend') + iv_est('d_lkpu'),
    'vs kcal =',
    iv_est('d_lkcal'),
    '\n'
)
cat('substitution share s = 1 + beta (IV):', 1 + iv_est('d_lkpu'), '\n')

# The same identity on labelled items only, which is where it closes exactly.
# Add labelled spend as a row of its own, with its own sample and estimates.
ols_lab <- feols(d_lspend_lab ~ infl_exposure | yq, d, cluster = ~state)
iv_lab <- feols(
    d_lspend_lab ~ 1 | yq | infl_exposure ~ z,
    d,
    cluster = ~state
)
table1 <- rbind(
    headline,
    data.table(
        outcome = 'd_lspend_lab',
        b_ols = coef(ols_lab)[[1]],
        se_ols = se(ols_lab)[[1]],
        b_iv = coef(iv_lab)[[1]],
        se_iv = se(iv_lab)[[1]],
        n = nobs(iv_lab)
    )
)
iv_est_lab <- function(y) table1[outcome == y, b_iv]
cat(
    'identity on labeled items (IV, own samples): labeled spend',
    iv_est_lab('d_lspend_lab'),
    '+ kcal/$',
    iv_est_lab('d_lkpu'),
    '=',
    iv_est_lab('d_lspend_lab') + iv_est_lab('d_lkpu'),
    'vs kcal',
    iv_est_lab('d_lkcal'),
    '\n'
)

# Table 1 as it prints in the log: estimate (SE) for each outcome, one row per
# outcome.
table1_labels <- c(
    d_lspend = 'Real food spend per AE-day (log x100)',
    d_lspend_lab = 'Labeled food spend per AE-day (log x100)',
    d_lkpu = 'Calories per real dollar (log x100)',
    d_lkcal = 'Labeled calories per AE-day (log x100)',
    d_ed = 'Energy density, foods (kcal/100 g)',
    d_hs = 'Health score (x100)'
)
table1 <- table1[match(names(table1_labels), outcome)]
unit <- ifelse(table1$outcome == 'd_hs', 100, 1)
print(
    data.table(
        Outcome = table1_labels[table1$outcome],
        OLS = paste0(
            fmt_num(unit * table1$b_ols, 3),
            ' (',
            fmt_num(unit * table1$se_ols, 3),
            ')'
        ),
        IV = paste0(
            fmt_num(unit * table1$b_iv, 3),
            ' (',
            fmt_num(unit * table1$se_iv, 3),
            ')'
        ),
        N = format(table1$n, big.mark = ',')
    )
)

# 2. State pairs-cluster bootstrap of the headline 2SLS ------------------------
# Because the 49 states are so different in size, the estimate rests on fewer
# independent clusters than 49.

n_boot <- 199
states <- sort(unique(d$state))
rows_by_state <- split(seq_len(nrow(d)), d$state)
boot_b <- vapply(
    seq_len(n_boot),
    function(i) {
        drawn <- sample(states, length(states), replace = TRUE)
        rows <- unlist(rows_by_state[as.character(drawn)], use.names = FALSE)
        m <- feols(
            d_lkpu ~ 1 | yq | infl_exposure ~ z,
            d[rows],
            vcov = 'iid',
            lean = TRUE,
            notes = FALSE,
            warn = FALSE
        )
        coef(m)[[1]]
    },
    0
)
pairs_boot <- data.table(
    B = n_boot,
    se = sd(boot_b),
    lo = quantile(boot_b, .025)[[1]],
    hi = quantile(boot_b, .975)[[1]]
)
cat(
    'pairs cluster bootstrap (B =',
    n_boot,
    '): SE',
    pairs_boot$se,
    '| 95% pct CI',
    pairs_boot$lo,
    pairs_boot$hi,
    '\n'
)

# 3. Robustness of the headline 2SLS (calories per dollar; Table S8) ----------
variants <- list(
    `Quarter FE (headline)` = iv[[1]],
    `Division x quarter FE` = feols(
        d_lkpu ~ 1 | div_q | infl_exposure ~ z,
        d,
        cluster = ~state
    ),
    `Income x quarter FE` = feols(
        d_lkpu ~ 1 | inc_q | infl_exposure ~ z,
        d,
        cluster = ~state
    ),
    `Household + quarter FE` = feols(
        d_lkpu ~ 1 | household_code + yq | infl_exposure ~ z,
        d,
        cluster = ~state
    ),
    `Projection-weighted` = feols(
        d_lkpu ~ 1 | yq | infl_exposure ~ z,
        d,
        weights = ~projection_factor,
        cluster = ~state
    ),
    `Excluding 2021` = feols(
        d_lkpu ~ 1 | yq | infl_exposure ~ z,
        d[year != 2021],
        cluster = ~state
    )
)
print(etable(variants))

# fixest reports R2 = .9995 for the weighted 2SLS, which looks unrealistically
# high.
weighted <- variants[['Projection-weighted']]
used <- obs(weighted)
w <- d$projection_factor[used]
y <- d$d_lkpu[used]
quarter <- d$yq[used]
y_within <- y - ave(y * w, quarter, FUN = sum) / ave(w, quarter, FUN = sum)
cat(
    'weighted 2SLS: hand-computed within R2 =',
    1 - sum(w * resid(weighted)^2) / sum(w * y_within^2),
    '\n'
)

robustness <- rbindlist(
    lapply(
        names(variants),
        function(k) {
            m <- variants[[k]]
            data.table(
                spec = k,
                b = coef(m)[[1]],
                se = se(m)[[1]],
                n = nobs(m),
                f = fitstat(m, ~ivf1)[[1]]$stat
            )
        }
    )
)
print(robustness)

# 4. OLS against 2SLS by base-basket size (measurement error; Fig. S3) ---------
# Households with small base baskets have noisier household-level exposure, so
# I expect OLS to be more attenuated than 2SLS for them.

d[, items := cut(
    n_base_items,
    c(0, 50, 100, 200, 400, Inf),
    right = FALSE,
    labels = c('10-49', '50-99', '100-199', '200-399', '400+')
)]
reliability <- rbindlist(
    lapply(
        levels(d$items),
        function(bin) {
            sub <- d[items == bin]
            o <- feols(d_lkpu ~ infl_exposure | yq, sub, cluster = ~state)
            i <- feols(
                d_lkpu ~ 1 | yq | infl_exposure ~ z,
                sub,
                cluster = ~state
            )
            rbind(
                data.table(
                    items = bin,
                    est_type = 'OLS (household T2)',
                    est = coef(o)[[1]],
                    se = se(o)[[1]],
                    n = nobs(o)
                ),
                data.table(
                    items = bin,
                    est_type = '2SLS (state-quarter mean)',
                    est = coef(i)[[1]],
                    se = se(i)[[1]],
                    n = nobs(i)
                )
            )
        }
    )
)
print(reliability)

# Save -------------------------------------------------------------------------
save_results(
    '01_main',
    headline = headline,
    table1 = table1,
    robustness = robustness,
    reliability = reliability,
    first_stage = first_stage,
    pairs_boot = pairs_boot,
    item_levels = levels(d$items)
)
print(sessionInfo())
