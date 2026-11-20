# Is the instrument a food price shock, or a local demand or income shock in
# disguise? All the tests here would pick up a demand shock, if there is one.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)


# Data and the two helpers used throughout -------------------------------------

d <- rob_load()
ys <- ROB_YS
lhs <- paste(names(ys), collapse = ', ')

# Estimate several outcomes at a time by 2SLS on instrumented exposure
# inflation, with SEs clustered by state.
fit_iv <- function(
    data, controls = '1', fe = 'yq', instruments = 'z',
    outcomes = lhs, lean = TRUE
) {
    feols(
        as.formula(
            sprintf(
                'c(%s) ~ %s | %s | infl_exposure ~ %s',
                outcomes,
                controls,
                fe,
                instruments
            )
        ),
        data,
        cluster = ~state,
        lean = lean
    )
}

# Each row is one coefficient (term) from a fitted model, for one outcome.
ext <- function(m, v, term, spec) {
    rbindlist(
        lapply(
            seq_along(v), function(k) {
                mk <- if (inherits(m, 'fixest_multi')) m[[k]] else m
                data.table(
                    outcome = v[k],
                    spec = spec,
                    term = term,
                    b = coef(mk)[[term]],
                    se = se(mk)[[term]],
                    p = pvalue(mk)[[term]],
                    n = nobs(mk),
                    f1 = tryCatch(
                        fitstat(mk, ~ivf1)[[1]]$stat,
                        error = function(e) NA_real_
                    )
                )
            }
        )
    )
}

# Number of rows with each variable the tests use.
cat('\nsample flow:\n')
print(
    data.table(
        step = c(
            'estimation sample',
            'lead zb(t+4) available',
            'lag zb(t-4) available',
            'lagged change d_lkpu(t-4) available',
            'z_div available',
            'T1 valid (f_idx_valid)'
        ),
        rows = c(
            nrow(d),
            sum(!is.na(d$zb_lead)),
            sum(!is.na(d$zb_lag)),
            sum(!is.na(d$l_d_lkpu)),
            sum(!is.na(d$z_div)),
            sum(d$f_idx_valid %in% TRUE & !is.na(d$infl_laspeyres))
        )
    )
)
for (v in c('zb', 'zb_lead', 'zb_lag', 'z_div')) {
    say('corr(z, %s) = %.4f', v, cor(d$z, d[[v]], use = 'complete.obs'))
}

# The headline, for reference
base <- fit_iv(d)
res <- ext(base, names(ys), 'fit_infl_exposure', 'Headline')


# (a) Leads --------------------------------------------------------------------

# This year's change regressed on instrumented exposure and next year's
# instrument (zb at t+4).
dl <- d[!is.na(zb_lead)]
m_lead <- fit_iv(dl, controls = 'zb_lead')
m_lead2 <- fit_iv(dl[!is.na(zb_lag)], controls = 'zb_lead + zb_lag')
res <- rbind(
    res,
    ext(m_lead, names(ys), 'zb_lead', 'Lead zb(t+4)'),
    ext(m_lead, names(ys), 'fit_infl_exposure', 'T2 | lead control'),
    ext(m_lead2, names(ys), 'zb_lead', 'Lead zb(t+4) | lag control'),
    ext(m_lead2, names(ys), 'fit_infl_exposure', 'T2 | lead + lag controls')
)


# (b) Pre-trend placebo --------------------------------------------------------

# Regress last year's change (t-8 to t-4) on this year's instrumented exposure,
# controlling for last year's instrument, which does drive last year's change.
lys <- paste(paste0('l_', names(ys)), collapse = ', ')
m_pre <- fit_iv(d[!is.na(zb_lag)], controls = 'zb_lag', outcomes = lys)
pre <- ext(
    m_pre,
    names(ys),
    'fit_infl_exposure',
    "Placebo: last year's change on T2"
)
pre_lag <- ext(m_pre, names(ys), 'zb_lag', "Last year's change on zb(t-4)")
res <- rbind(res, pre, pre_lag)


# (c) Balance on household economic changes ------------------------------------

cov <- c(
    d_emp = 'Change any head employed (0/1)',
    d_linc = 'Change log real income (x100)',
    d_hhsize = 'Change household size'
)
m_bal <- fit_iv(d, outcomes = paste(names(cov), collapse = ', '))
bal <- ext(m_bal, names(cov), 'fit_infl_exposure', 'Balance')

cat('\ncovariate means and SDs (for scale):\n')
print(
    d[
        ,
        lapply(.SD, function(x) c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))),
        .SDcols = names(cov)
    ]
)
print(bal)


# (d) Stronger fixed effects and economic controls -----------------------------

# Mean of each covariate among the other households in the state-quarter
# (ignoring missing values): conditions in the household's local economy.
for (v in names(cov)) {
    d[, value := fifelse(is.na(get(v)), 0, get(v))]
    d[, has_value := as.integer(!is.na(get(v)))]
    d[
        ,
        (paste0(
            'lo_',
            v
        )) := (sum(value) - value) / (sum(has_value) - has_value),
        by = state_q
    ]
}
d[, c('value', 'has_value') := NULL]
stopifnot(!anyNA(d[, .(lo_d_emp, lo_d_linc, lo_d_hhsize)]))

specs <- list(
    `State FE` = 'state + yq',
    `State FE + division x quarter FE` = 'state + div_q',
    `State x year FE` = 'state_y + yq',
    `State FE + division x quarter + income x quarter FE` =
        'state + div_q + inc_q'
)
for (s in names(specs)) {
    m <- fit_iv(d, fe = specs[[s]])
    res <- rbind(res, ext(m, names(ys), 'fit_infl_exposure', s))
}

# Controls for the household and the state-quarter economy (a full fit, since
# etable() needs the residuals).
m_ctl <- fit_iv(
    d,
    controls = paste(
        'd_emp + d_linc + d_hhsize',
        '+ lo_d_emp + lo_d_linc + lo_d_hhsize'
    ),
    lean = FALSE
)
res <- rbind(
    res,
    ext(
        m_ctl,
        names(ys),
        'fit_infl_exposure',
        'Household + state-quarter economic controls'
    )
)
print(etable(m_ctl[[1]]))


# (e) Neighbour-state instrument and over-identification -----------------------

# z_div: exposure averaged over the other states in the household's census
# division.
dn <- d[!is.na(z_div)]
m_nb <- fit_iv(dn, instruments = 'z_div')
m_nb2 <- fit_iv(dn, fe = 'div_q')
m_oid <- fit_iv(dn, instruments = 'z + z_div', lean = FALSE)
res <- rbind(
    res,
    ext(
        m_nb,
        names(ys),
        'fit_infl_exposure',
        'Neighbor-state instrument (leave-own-state division)'
    ),
    ext(m_oid, names(ys), 'fit_infl_exposure', 'Both instruments')
)
print(fitstat(m_nb[[1]], ~ ivf1 + ivwald1))

oid <- rbindlist(
    lapply(
        seq_along(ys), function(k) {
            s <- fitstat(m_oid[[k]], ~sargan)[[1]]
            data.table(outcome = names(ys)[k], sargan = s$stat, p = s$p)
        }
    )
)
cat('\nover-identification (Sargan, iid; z and z_div):\n')
print(oid)

# A cluster-robust test that the two instruments give the same coefficient:
# stack the data set on itself, one copy per instrument, with copy-specific
# quarter fixed effects and slopes; state clusters span both copies.
rm(m, base, m_lead, m_lead2, m_pre, m_bal, m_ctl, m_nb, m_nb2, m_oid)
invisible(gc())

dn[, s := 1L]
stk <- rbind(dn, copy(dn)[, s := 2L])
stk[, x1 := infl_exposure * (s == 1L)]
stk[, x2 := infl_exposure * (s == 2L)]
stk[, i1 := z * (s == 1L)]
stk[, i2 := z_div * (s == 2L)]

# Estimate one outcome at a time (lean fits use about a fifth of the memory; the
# estimates are the same).
dif <- rbindlist(
    lapply(
        seq_along(ys), function(k) {
            m <- feols(
                as.formula(sprintf(
                    '%s ~ 1 | yq^s | x1 + x2 ~ i1 + i2',
                    names(ys)[k]
                )),
                stk,
                cluster = ~state,
                lean = TRUE
            )
            b <- coef(m)
            V <- vcov(m)
            data.table(
                outcome = names(ys)[k],
                b_own = b[['fit_x1']],
                b_nb = b[['fit_x2']],
                diff = b[['fit_x2']] - b[['fit_x1']],
                se_diff = sqrt(
                    V['fit_x1', 'fit_x1'] + V['fit_x2', 'fit_x2'] -
                        2 * V['fit_x1', 'fit_x2']
                )
            )
        }
    )
)
dif[, t_diff := diff / se_diff]
dif[, p_diff := 2 * pnorm(-abs(diff / se_diff))]
cat('\nneighbour minus own-state 2SLS (stacked, state clusters):\n')
print(dif)
rm(stk)
invisible(gc())


# (f) Same-product prices against changes in what is bought --------------------

# Calories per dollar = minus the same-product price change (T1, a Laspeyres
# index of the basket the household actually bought) plus the change that comes
# from buying different products (comp).
dt1 <- d[
    f_idx_valid %in% TRUE & is.finite(infl_laspeyres) &
        infl_laspeyres > -100
]
dt1[, t1_log := 100 * log1p(infl_laspeyres / 100)]
dt1[, comp := d_lkpu + t1_log]
m_t1 <- feols(
    c(d_lkpu, t1_log, comp) ~ 1 | yq | infl_exposure ~ z,
    dt1,
    cluster = ~state
)
tu <- ext(
    m_t1,
    c('d_lkpu', 't1_log', 'comp'),
    'fit_infl_exposure',
    'T1-valid sample'
)
cat('\ncalories per dollar = -same-product price + composition:\n')
print(tu)
say(
    'check: b(kcal/$) + b(T1) - b(comp) = %.2e',
    tu[outcome == 'd_lkpu', b] + tu[outcome == 't1_log', b] -
        tu[outcome == 'comp', b]
)

print(res)


# Save -------------------------------------------------------------------------

save_results(
    '05_identification',
    res = res,
    pre = pre,
    bal = bal,
    oid = oid,
    dif = dif,
    tu = tu,
    spec_names = names(specs)
)
print(sessionInfo())
