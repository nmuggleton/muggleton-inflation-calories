# Does the instrument stand in for local demand?

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
setFixest_nthreads(2)

root <- Sys.getenv('LOCAL_DATA', file.path(wp2_paths()$data, 'intermediate'))
pq <- file.path(root, 'local_context', 'local_panel.parquet')
con <- DBI::dbConnect(duckdb::duckdb())
d <- setDT(DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s'", pq)))
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(nrow(d) == F1_N, !anyNA(d$infl_exposure))
d[
    ,
    z := (sum(infl_exposure) - infl_exposure) / (.N - 1),
    by = .(state, yq)
]
d[, div_q := paste(region_code, yq)]
stopifnot(all(is.finite(d$z)))

# County employment from LAUS has rebenchmarking spikes (the minimum
# year-on-year change is -154 log points in the ext2_02 log), so it is
# winsorised at the 1st and 99th percentiles
for (v in c('d_emp_c', 'dl_emp_c')) {
    q <- quantile(d[[v]], c(.01, .99), na.rm = TRUE)
    d[, (v) := pmin(pmax(get(v), q[[1]]), q[[2]])]
}

ctl_state <- c('d_ur', 'd_pinc', 'd_hpi')
ctl_cty <- c('d_ur_c', 'd_emp_c')
ctl_lag <- c('dl_ur', 'dl_pinc', 'dl_hpi', 'dl_ur_c', 'dl_emp_c')
ctl_all <- c(ctl_state, ctl_cty, ctl_lag)
d[, ctl_ok := complete.cases(d[, ..ctl_all])]
cat(
    'sample flow: f1 gate',
    nrow(d),
    '-> all controls non-missing',
    d[ctl_ok == TRUE, .N],
    'household-quarters;',
    uniqueN(d[ctl_ok == TRUE, household_code]),
    'households;',
    d[ctl_ok == FALSE, uniqueN(state)],
    'state(s) lose rows:',
    paste(
        d[ctl_ok == FALSE, .N, by = state][order(-N)][
            1:3,
            paste0(state, '=', N)
        ],
        collapse = ' '
    ),
    '\n'
)
lab <- c(
    d_ur = 'State unemployment rate (pp)',
    d_pinc = 'State personal income (log x100)',
    d_hpi = 'State house prices, FHFA (log x100)',
    d_ur_c = 'County unemployment rate (pp)',
    d_emp_c = 'County employment (log x100)',
    dl_ur = 'State unemployment, lagged change',
    dl_pinc = 'State income, lagged change',
    dl_hpi = 'State house prices, lagged change',
    dl_ur_c = 'County unemployment, lagged change',
    dl_emp_c = 'County employment, lagged change'
)
print(
    d[ctl_ok == TRUE, lapply(
        .SD,
        function(x) {
            round(
                c(
                    mean = mean(x), sd = sd(x)
                ),
                3
            )
        }
    ), .SDcols = ctl_all]
)


# A. Balance: regress each year-on-year local shock on z (quarter FE) ----------

dc <- d[ctl_ok == TRUE]
zr <- resid(feols(z ~ 1 | yq, dc))
bal <- rbindlist(
    lapply(
        c(ctl_state, ctl_cty),
        function(v) {
            m <- feols(as.formula(paste(v, '~ z | yq')), dc, cluster = ~state)
            vr <- resid(feols(as.formula(paste(v, '~ 1 | yq')), dc))
            k <- sd(zr) / sd(vr) # SD of x and y net of quarter FE
            data.table(
                var = v,
                b = coef(m)[['z']],
                se = se(m)[['z']],
                p = pvalue(m)[['z']],
                b_std = k * coef(m)[['z']],
                se_std = k * se(m)[['z']],
                sd_y = sd(vr)
            )
        }
    )
)
cat('SD of z net of quarter FE:', sd(zr), '\n')
print(bal)
# Joint test: do the local shocks (and their lags) predict z?
mj <- feols(
    as.formula(
        paste(
            'z ~',
            paste(ctl_all, collapse = ' + '),
            '| yq'
        )
    ),
    dc,
    cluster = ~state
)
print(summary(mj))
wj <- wald(mj, print = FALSE)
cat(
    'joint Wald (state clusters): stat',
    wj$stat,
    'p',
    wj$p,
    'df1',
    wj$df1,
    '| within R2',
    fitstat(mj, ~wr2)[[1]],
    '\n'
)
# The same test at the state-quarter level (one row per shock cell)
sq <- dc[, c(
    list(zbar = mean(infl_exposure), n = .N),
    lapply(.SD, mean)
),
by = .(state, yq),
.SDcols = c(ctl_state, 'dl_ur', 'dl_pinc', 'dl_hpi')
]
ms <- feols(
    as.formula(
        paste(
            'zbar ~',
            paste(
                c(
                    ctl_state, 'dl_ur',
                    'dl_pinc', 'dl_hpi'
                ),
                collapse = ' + '
            ),
            '| yq'
        )
    ),
    sq,
    cluster = ~state
)
ws <- wald(ms, print = FALSE)
cat(
    'state-quarter level (',
    nrow(sq),
    'cells): joint Wald stat',
    ws$stat,
    'p',
    ws$p,
    '| within R2',
    fitstat(ms, ~wr2)[[1]],
    '\n'
)


# B. 2SLS with local-demand controls, all five outcomes ------------------------

ys <- c(
    d_lkpu = 'Change log kcal per real $ (x100)',
    d_lspend = 'Change log real spend per AE-day (x100)',
    d_lkcal = 'Change log kcal per AE-day (x100)',
    d_ed = 'Change energy density (kcal/100 g)',
    d_hs = 'Change health score'
)
setFixest_dict(
    c(
        ys, lab,
        infl_exposure = 'Exposure inflation T2 (pp)',
        fit_infl_exposure = 'Exposure inflation T2 (pp)',
        yq = 'Quarter', div_q = 'Division x quarter'
    )
)
specs <- list(
    `f1 headline (full gate sample)` = list(c(), 'yq', d),
    `Baseline, control sample` = list(c(), 'yq', dc),
    `+ state U, income, house prices` = list(ctl_state, 'yq', dc),
    `+ county U and employment` = list(c(ctl_state, ctl_cty), 'yq', dc),
    `+ lagged changes of all` = list(ctl_all, 'yq', dc),
    `+ division x quarter FE` = list(ctl_all, 'div_q', dc)
)
# Each specification lists its controls, fixed effects and sample
fits <- lapply(
    specs,
    function(s) {
        ctl <- s[[1]]
        fe <- s[[2]]
        data <- s[[3]]
        rhs <- if (length(ctl)) paste(ctl, collapse = ' + ') else '1'
        # Lean: only coefficients, SEs, N and the first-stage F are read from
        # these six five-outcome fits, which are all held at once (full fits
        # would exceed 15 GB)
        feols(
            as.formula(
                sprintf(
                    'c(%s) ~ %s | %s | infl_exposure ~ z',
                    paste(names(ys), collapse = ', '),
                    rhs,
                    fe
                )
            ),
            data,
            cluster = ~state,
            lean = TRUE
        )
    }
)
est <- rbindlist(
    lapply(
        names(fits),
        function(k) {
            rbindlist(
                lapply(
                    seq_along(ys),
                    function(j) {
                        m <- fits[[k]][[j]]
                        data.table(
                            spec = k,
                            outcome = names(ys)[j],
                            b = coef(m)[['fit_infl_exposure']],
                            se = se(m)[['fit_infl_exposure']],
                            n = nobs(m),
                            f = fitstat(m, ~ivf1)[[1]]$stat
                        )
                    }
                )
            )
        }
    )
)
est[, spec := factor(spec, levels = names(specs))]
print(dcast(est, spec ~ outcome, value.var = 'b'))
print(dcast(est, spec ~ outcome, value.var = 'se'))
print(est[outcome == 'd_lkpu', .(spec, b, se, n, f)])
# The shift relative to the control-sample baseline, in units of the baseline SE
bl <- est[spec == 'Baseline, control sample', .(outcome, b0 = b, se0 = se)]
est <- merge(est, bl, by = 'outcome')
est[, shift_se := (b - b0) / se0]
print(est[, .(spec, outcome, shift_se = round(shift_se, 2))][order(outcome)])

full <- fits[['+ lagged changes of all']]
print(
    etable(
        full,
        keep = c(
            'Exposure', 'unemployment', 'income',
            'house', 'employment'
        ),
        fitstat = ~ n + ivf1
    )
)


# C. Income-side vs price-side squeeze (from the fully controlled model): ------

# The response per 1 pp rise in county unemployment against the response per 1
# pp rise in basket prices

sq2 <- rbindlist(
    lapply(
        seq_along(ys),
        function(j) {
            m <- full[[j]]
            data.table(
                outcome = names(ys)[j],
                term = c('fit_infl_exposure', 'd_ur_c', 'd_ur'),
                b = coef(m)[c('fit_infl_exposure', 'd_ur_c', 'd_ur')],
                se = se(m)[c('fit_infl_exposure', 'd_ur_c', 'd_ur')]
            )
        }
    )
)
print(sq2)

save_results(
    '15_local_demand',
    bal = bal,
    lab = lab,
    joint_hh = data.table(
        stat = wj$stat,
        p = wj$p,
        df1 = wj$df1,
        wr2 = fitstat(mj, ~wr2)[[1]]
    ),
    joint_sq = data.table(
        stat = ws$stat,
        p = ws$p,
        cells = nrow(sq),
        wr2 = fitstat(ms, ~wr2)[[1]]
    ),
    est = est,
    spec_names = names(specs),
    sq2 = sq2
)
print(sessionInfo())
