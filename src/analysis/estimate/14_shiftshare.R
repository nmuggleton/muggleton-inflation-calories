# An alternative design: a household-level shift-share instrument (base-basket
# module shares interacted with national, leave-own-state-out module price
# changes, with state x quarter fixed effects), alongside the main state-mean
# instrument. It is not used as evidence because it fails a dynamic placebo test
# (future price shocks predict current outcomes); the results are reported in
# Fig.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
set.seed(20261002)
setFixest_nthreads(2)

xint <- Sys.getenv('SHIFTSHARE_INT')
stopifnot(nzchar(xint))
bk <- file.path(xint, 'bartik', '*.parquet')
sc <- file.path(xint, 'state_controls.parquet')
# Designs B and C use FRED controls (src/data_prep/08_external/02_fred.py),
# which need network access to fred.stlouisfed.org.
has_sc <- file.exists(sc)
if (!has_sc) cat('NOTE:', sc, 'missing; designs B and C skipped\n')
sc_join <- if (has_sc) {
    sprintf(
        "LEFT JOIN read_parquet('%s') c ON c.state = d.state AND c.yq = d.yq",
        sc
    )
} else {
    ''
}
sc_cols <- if (has_sc) {
    'c.d_ur, c.d_lci'
} else {
    'NULL::DOUBLE AS d_ur, NULL::DOUBLE AS d_lci'
}
d <- f1_sql(
    sprintf(
        "
  SELECT d.household_code, d.yq, d.year, d.state, d.region_code,
    d.income_cat, d.projection_factor, d.n_base_items, d.infl_exposure,
    d.d_lkpu, d.d_lspend, d.d_lkcal, d.d_ed, d.d_hs,
    b.B4_st, b.B4_nat, b.B4_div, b.S4_st, b.S4_nat, b.S4_div, b.B8_st,
    b.S8_st, b.B4_lead, b.S4_lead, b.B4_lag, b.S4_lag, %2$s
  FROM F1 d
  LEFT JOIN read_parquet('%1$s') b USING (household_code, yq)
  %3$s
  WHERE d.est_gate",
        bk,
        sc_cols,
        sc_join
    )
)
d[, projection_factor := as.numeric(projection_factor)]
# N = 2,783,093 (the est_gate sample, see 01_main.R)
stopifnot(nrow(d) == F1_N, uniqueN(d[, .(household_code, yq)]) == nrow(d))
d[, `:=`(state_q = paste(state, yq), inc_q = paste(income_cat, yq))]
d[, z := (sum(infl_exposure) - infl_exposure) / (.N - 1), by = state_q]
stopifnot(all(is.finite(d$z)))

# Leads and lags of z are defined the same way: leave-household-out state x
# quarter means of T2 in the est_gate sample at t + 4 and t - 4 (the household's
# own T2 removed when present)
zs <- d[, .(sx = sum(infl_exposure), n = .N), by = .(state, yq)]
own <- d[, .(household_code, state, yq, own = infl_exposure)]
for (k in c(lead = 10L, lag = -10L)) {
    nm <- if (k > 0) 'z_lead' else 'z_lag'
    tmp <- d[, .(household_code, state, yq2 = yq + k)]
    tmp <- merge(
        tmp,
        zs,
        by.x = c('state', 'yq2'),
        by.y = c('state', 'yq'),
        all.x = TRUE,
        sort = FALSE
    )
    tmp <- merge(
        tmp,
        own,
        by.x = c('household_code', 'state', 'yq2'),
        by.y = c('household_code', 'state', 'yq'),
        all.x = TRUE,
        sort = FALSE
    )
    tmp[, v := fifelse(is.na(own), sx / n, (sx - own) / (n - 1))]
    tmp[, yq := yq2 - k]
    d[tmp, (nm) := i.v, on = .(household_code, yq)]
}


# sample flow ------------------------------------------------------------------

flow <- data.table(
    step = c(
        'est_gate', 'B4 (t-4 shares) defined', 'B8 (t-8 shares)',
        'state controls (excl. DC: no coincident index)',
        'dynamic sample: z lead and lag, 2006-2018',
        'dynamic sample: B lead and lag, 2006-2018'
    ),
    hh_quarters = c(
        nrow(d), d[!is.na(B4_st), .N], d[!is.na(B8_st), .N],
        d[!is.na(d_ur) & !is.na(d_lci), .N],
        d[
            !is.na(z_lead) & !is.na(z_lag) & year %between%
                c(2006, 2018), .N
        ],
        d[
            !is.na(B4_lead) & !is.na(B4_lag) & year %between%
                c(2006, 2018), .N
        ]
    ),
    households = c(
        uniqueN(d$household_code),
        uniqueN(d[!is.na(B4_st), household_code]),
        uniqueN(d[!is.na(B8_st), household_code]),
        uniqueN(d[!is.na(d_ur) & !is.na(d_lci), household_code]),
        NA, NA
    )
)
print(flow)
# Rows should only be dropped at this step when the base basket has no
# module-mapped spend; more than 1% missing would point to a join bug
stopifnot(d[is.na(B4_st), .N] < 0.01 * nrow(d))
print(
    d[, .(
        mean_T2 = mean(infl_exposure),
        sd_T2 = sd(infl_exposure),
        mean_B4 = mean(B4_st, na.rm = TRUE),
        sd_B4 = sd(B4_st, na.rm = TRUE),
        mean_S4 = mean(
            S4_st,
            na.rm = TRUE
        ),
        min_S4 = min(S4_st, na.rm = TRUE),
        cor_T2_B4 = cor(infl_exposure, B4_st, use = 'complete'),
        cor_z_B4 = cor(z, B4_st, use = 'complete'),
        cor_B4_nat = cor(B4_st, B4_nat, use = 'complete'),
        cor_B4_B8 = cor(B4_st, B8_st, use = 'complete')
    )]
)
# Within state-quarter variation of T2 and B, which is what the B design uses
wv <- d[!is.na(B4_st), .(
    sd_T2_w = sd(
        infl_exposure - ave(
            infl_exposure,
            state_q
        )
    ),
    sd_B4_w = sd(B4_st - ave(B4_st, state_q))
)]
print(wv)

ys <- c(
    d_lkpu = 'Change log kcal per real $ (x100)',
    d_lspend = 'Change log real spend per AE-day (x100)',
    d_lkcal = 'Change log kcal per AE-day (x100)',
    d_ed = 'Change energy density (kcal/100 g)',
    d_hs = 'Change health score'
)
setFixest_dict(
    c(
        ys,
        infl_exposure = 'Exposure inflation T2 (pp)',
        fit_infl_exposure = 'Exposure inflation T2 (pp)',
        z = 'State-quarter mean T2 (z)', B4_st = 'Household shift-share',
        yq = 'Quarter', state_q = 'State x quarter',
        inc_q = 'Income x quarter'
    )
)
lhs <- paste0('c(', paste(names(ys), collapse = ', '), ')')
iv <- function(fe, inst, data, ctrl = '1') {
    feols(
        as.formula(
            sprintf(
                '%s ~ %s | %s | infl_exposure ~ %s',
                lhs,
                ctrl,
                fe,
                inst
            )
        ),
        data,
        cluster = ~state
    )
}


# 1. Designs (2SLS, five outcomes each) ----------------------------------------

dB <- d[!is.na(B4_st)]
dC <- d[!is.na(d_ur) & !is.na(d_lci)]
d8 <- d[!is.na(B8_st)]
# The designs are stored as unevaluated calls and fitted one at a time: each
# five-outcome fit holds n x 5 vectors, and holding all eleven at once exceeds
# 16 GB
des <- alist(
    `A. z, quarter FE (f1 headline)` = iv('yq', 'z', d),
    `B. z, quarter FE, no DC` = iv('yq', 'z', dC),
    `C. z + state unemployment, activity` = iv(
        'yq',
        'z',
        dC,
        'd_ur + d_lci'
    ),
    `D. B, quarter FE` = iv('yq[S4_st]', 'B4_st', dB),
    `E. B, state x quarter FE` = iv('state_q[S4_st]', 'B4_st', dB),
    `F. E + income x quarter FE` = iv('state_q[S4_st] + inc_q', 'B4_st', dB),
    `G. B leave-division-out, state x quarter FE` =
        iv('state_q[S4_div]', 'B4_div', d[!is.na(B4_div)]),
    `H. B national, state x quarter FE` =
        iv('state_q[S4_nat]', 'B4_nat', d[!is.na(B4_nat)]),
    `I. E on t-8 sample` = iv('state_q[S4_st]', 'B4_st', d8[!is.na(B4_st)]),
    `J. B with t-8 shares, state x quarter FE` =
        iv('state_q[S8_st]', 'B8_st', d8),
    `K. E, projection-weighted` = feols(
        as.formula(
            sprintf(
                '%s ~ 1 | state_q[S4_st] | infl_exposure ~ B4_st',
                lhs
            )
        ),
        dB,
        cluster = ~state,
        weights = ~projection_factor
    )
)
if (!has_sc) {
    des[c(
        'B. z, quarter FE, no DC',
        'C. z + state unemployment, activity'
    )] <- NULL
}
fs_show <- c(
    'D. B, quarter FE', 'E. B, state x quarter FE',
    'J. B with t-8 shares, state x quarter FE'
)
res <- rbindlist(
    lapply(
        names(des),
        function(k) {
            m <- eval(des[[k]])
            out <- rbindlist(
                lapply(
                    seq_along(ys),
                    function(j) {
                        coef_row(m[[j]], k, names(ys)[j])
                    }
                )
            )
            if (k %in% fs_show) {
                cat('first stage,', k, '\n')
                print(coeftable(summary(m[[1]], stage = 1)))
            }
            if (k == 'C. z + state unemployment, activity') {
                cat('coefficient on controls in C (kcal/$ equation):\n')
                print(coeftable(m[[1]]))
            }
            rm(m)
            invisible(gc())
            out
        }
    )
)
print(
    res[, .(
        design,
        outcome,
        b = round(b, 4),
        se = round(se, 4),
        p = signif(p, 3),
        n,
        f_wald = round(f_wald, 1)
    )]
)


# 2. Dynamic placebo (reduced form): outcome change t-4 -> t on the ------------

# Instrument for the change from t-8 to t-4 (lag), t-4 to t (contemporaneous),
# and t to t+4 (lead)

dz <- d[!is.na(z_lead) & !is.na(z_lag) & year %between% c(2006, 2018)]
db <- d[
    !is.na(B4_lead) & !is.na(B4_lag) & !is.na(B4_st) &
        year %between% c(2006, 2018)
]
rf_z <- feols(
    as.formula(sprintf('%s ~ z_lag + z + z_lead | yq', lhs)),
    dz,
    cluster = ~state
)
rf_b <- feols(
    as.formula(
        sprintf(
            '%s ~ B4_lag + B4_st + B4_lead | state_q[S4_st, S4_lag, S4_lead]',
            lhs
        )
    ),
    db,
    cluster = ~state
)
fs_z <- feols(infl_exposure ~ z_lag + z + z_lead | yq, dz, cluster = ~state)
fs_b <- feols(
    infl_exposure ~ B4_lag + B4_st + B4_lead |
        state_q[S4_st, S4_lag, S4_lead],
    db,
    cluster = ~state
)
print(etable(fs_z, fs_b))
print(etable(rf_z))
print(etable(rf_b))
dyn <- rbindlist(
    lapply(
        list(z = rf_z, B = rf_b),
        function(m) {
            rbindlist(
                lapply(
                    seq_along(ys),
                    function(j) {
                        ct <- coeftable(m[[j]])
                        data.table(
                            outcome = names(ys)[j],
                            term = rownames(ct),
                            b = ct[, 1],
                            se = ct[, 2],
                            p = ct[, 4],
                            n = nobs(m[[j]])
                        )
                    }
                )
            )
        }
    ),
    idcol = 'instrument'
)
dyn[, horizon := fifelse(
    grepl('lag', term),
    -1L,
    fifelse(grepl('lead', term), 1L, 0L)
)]
# Also scale by the contemporaneous first stage, so the 0 horizon reads as a
# 2SLS-type effect per pp of T2
fsb <- c(z = coef(fs_z)[['z']], B = coef(fs_b)[['B4_st']])
dyn[, b_scaled := b / fsb[instrument]]
dyn[, se_scaled := se / fsb[instrument]]
print(
    dyn[, .(
        instrument,
        outcome,
        horizon,
        b = round(b, 4),
        se = round(se, 4),
        p = signif(p, 3),
        b_scaled =
            round(b_scaled, 4),
        n
    )]
)
cat(
    'first-stage coefficient (contemporaneous): z',
    fsb[['z']],
    '| B',
    fsb[['B']],
    '\n'
)
# The joint test that the lead coefficients are zero across the three budget
# outcomes is not stacked here; each lead is reported with its own p-value

save_results(
    '14_shiftshare',
    res = res,
    dyn = dyn,
    flow = flow,
    first_stage = data.table(instrument = names(fsb), coef = fsb)
)
print(sessionInfo())
