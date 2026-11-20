# The timing of the response.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)

# The models are large but only ever read through coef, se, nobs, vcov and the
# first-stage F, so they are stored lean: identical estimates, much smaller
# objects (otherwise the run does not fit in 15 GB).
setFixest_estimation(lean = TRUE)
d <- rob_load()
ys <- ROB_YS
lhs <- paste(names(ys), collapse = ', ')
lys <- paste(paste0('l_', names(ys)), collapse = ', ')
grab <- function(m, spec, term = 'fit_infl_exposure') {
    rbindlist(
        lapply(
            seq_along(ys),
            function(k) {
                data.table(
                    spec = spec,
                    outcome = names(ys)[k],
                    b = coef(m[[k]])[[term]],
                    se = se(m[[k]])[[term]],
                    n = nobs(m[[k]]),
                    f1 = fitstat(m[[k]], ~ivf1)[[1]]$stat
                )
            }
        )
    )
}

# (1a) pre-trend placebo under stronger FE -------------------------------------
dp <- d[!is.na(zb_lag)]
pre <- rbindlist(
    lapply(
        c(
            `Quarter FE` = 'yq', `State FE` = 'state + yq',
            `State FE + division x quarter FE` =
                'state + div_q'
        ),
        function(fe) {
            feols(
                as.formula(
                    sprintf(
                        'c(%s) ~ zb_lag | %s | infl_exposure ~ z',
                        lys,
                        fe
                    )
                ),
                dp,
                cluster = ~state
            )
        }
    ) |> (\(l) Map(grab, l, names(l)))()
)
print(pre)

# (1b) headline conditional on last year's own change --------------------------
# l_d_y and d_y share the t-4 level, so the lagged change is mechanically
# negatively correlated with d_y.
res <- list()
for (k in seq_along(ys)) {
    v <- names(ys)[k]
    s <- d[!is.na(get(paste0('l_', v))) & !is.na(zb_lag) & is.finite(get(v))]
    a <- feols(
        as.formula(sprintf('%s ~ 1 | yq | infl_exposure ~ z', v)),
        s,
        cluster = ~state
    )
    b <- feols(
        as.formula(
            sprintf(
                '%s ~ l_%s + zb_lag | yq | infl_exposure ~ z',
                v,
                v
            )
        ),
        s,
        cluster = ~state
    )
    res[[k]] <- rbind(
        data.table(
            spec = 'Lag sample, headline spec',
            outcome = v,
            b = coef(a)[[1]],
            se = se(a)[[1]],
            n = nobs(a)
        ),
        data.table(
            spec = 'Lag sample, + own lagged change + zb(t-4)',
            outcome = v,
            b = coef(b)[['fit_infl_exposure']],
            se = se(b)[['fit_infl_exposure']],
            n = nobs(b),
            rho = coef(b)[[paste0('l_', v)]]
        ),
        fill = TRUE
    )
}
cond <- rbindlist(res, fill = TRUE)
print(cond)

# (2) persistent vs transient components of the instrument ---------------------
# z_ann is the household-weighted mean of z within state x year, and z_dev = z -
# z_ann.
d[, z_ann := mean(z), by = state_y]
d[, z_dev := z - z_ann]
cat(
    'SD z:',
    sd(d$z),
    '| SD z_ann:',
    sd(d$z_ann),
    '| SD z_dev:',
    sd(d$z_dev),
    '| share of var (net of quarter means) in z_dev:',
    var(d$z_dev - ave(d$z_dev, d$yq)) / var(d$z - ave(d$z, d$yq)),
    '\n'
)
m_ann <- feols(
    as.formula(
        sprintf(
            'c(%s) ~ 1 | yq | infl_exposure ~ z_ann',
            lhs
        )
    ),
    d,
    cluster = ~state
)
m_dev <- feols(
    as.formula(
        sprintf(
            'c(%s) ~ 1 | yq | infl_exposure ~ z_dev',
            lhs
        )
    ),
    d,
    cluster = ~state
)
m_sy <- feols(
    as.formula(
        sprintf(
            'c(%s) ~ 1 | state_y + yq | infl_exposure ~ z',
            lhs
        )
    ),
    d,
    cluster = ~state
)
per <- rbind(
    grab(m_ann, 'Annual state component (z_ann)'),
    grab(m_dev, 'Within-state-year deviation (z_dev)'),
    grab(m_sy, 'State x year FE (reference)')
)
print(per)
# A test that the two just-identified estimates are equal: the data stacked,
# clustered by state.
base <- d[, .(
    state,
    yq,
    d_lkpu,
    d_lspend,
    d_lkcal,
    d_ed,
    d_hs,
    infl_exposure,
    z_ann,
    z_dev
)]
stk <- rbind(
    base[, `:=`(s = 1L, inst = z_ann)],
    copy(base)[, `:=`(s = 2L, inst = z_dev)]
)
rm(base)
stk[, `:=`(
    x1 = infl_exposure * (s == 1L),
    x2 = infl_exposure * (s == 2L),
    i1 = inst * (s == 1L),
    i2 = inst * (s == 2L)
)]
m_stk <- feols(
    as.formula(
        sprintf(
            'c(%s) ~ 1 | yq^s | x1 + x2 ~ i1 + i2',
            lhs
        )
    ),
    stk,
    cluster = ~state
)
eq <- rbindlist(
    lapply(
        seq_along(ys),
        function(k) {
            b <- coef(m_stk[[k]])
            V <- vcov(m_stk[[k]])
            dd <- b[['fit_x2']] - b[['fit_x1']]
            sd_ <- sqrt(
                V['fit_x1', 'fit_x1'] + V['fit_x2', 'fit_x2'] -
                    2 * V['fit_x1', 'fit_x2']
            )
            data.table(
                outcome = names(ys)[k],
                diff_dev_minus_ann = dd,
                se = sd_,
                p = 2 * pnorm(-abs(dd / sd_))
            )
        }
    )
)
cat('\ntransient minus persistent (stacked, state clusters):\n')
print(eq)
rm(stk)
invisible(gc())

save_results('07_dynamics', pre = pre, cond = cond, per = per, eq = eq)
print(sessionInfo())
