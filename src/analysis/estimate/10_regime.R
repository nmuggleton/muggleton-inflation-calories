# Is "pay, don't trade down" an artefact of low inflation?

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)

# Every fit here is read only through coef, se, vcov, coeftable and the
# first-stage F, so the models are stored lean: identical estimates, much
# smaller objects (otherwise the run exceeds 15 GB)
setFixest_estimation(lean = TRUE)
d <- het_load(
    c(
        'household_code', 'yq', 'year', 'state', 'infl_exposure',
        'z', names(HET_Y), 'regime', 'cpi_fah_yoy', 'episode'
    )
)
cat('sample flow: gate', nrow(d), '\n')
print(quantile(d$z, c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1)))
print(
    d[
        ,
        .(n = .N, quarters = uniqueN(yq), z_mean = mean(z)),
        keyby = episode
    ]
)
ys <- names(HET_Y)

# The health-score scale, for reading the nutrition effects in SD units
print(
    d[, .(
        sd_d_hs = sd(d_hs, na.rm = TRUE),
        sd_d_ed = sd(d_ed, na.rm = TRUE)
    )]
)


# 1. National food-inflation regime and calendar year --------------------------

d[, year_f := factor(year)]
d[, epi := factor(
    fifelse(
        episode == 'none',
        'Outside episodes',
        'Food-price episode'
    ),
    levels = c('Outside episodes', 'Food-price episode')
)]
# year_f (16 groups), one outcome at a time: same estimates and Wald tests, but
# the five-outcome fit needs more than 32 GB (tried locally on 2 Oct 2026)
res <- rbindlist(
    c(
        list(het_grp(d, 'regime'), het_grp(d, 'epi')),
        lapply(
            names(HET_Y),
            function(y) {
                r <- het_grp(d, 'year_f', ys = y)
                gc()
                r
            }
        )
    )
)
stopifnot(all(res$fs_F > 100)) # yearly cells are smaller; F >> 10
print(res[moderator != 'year_f'])
print(res[moderator == 'year_f' & outcome %in% c('d_lkcal', 'd_lkpu')])
wt <- unique(res[, .(moderator, outcome, wald_F, wald_p)])


# 2. Intensity: quadratic 2SLS in total exposure inflation ---------------------

d[, `:=`(t2sq = infl_exposure^2, zsq = z^2)]
mq <- feols(
    as.formula(
        sprintf(
            'c(%s) ~ 1 | yq | infl_exposure + t2sq ~ z + zsq',
            paste(ys, collapse = ', ')
        )
    ),
    d,
    cluster = ~state,
    notes = FALSE
)
print(etable(mq, digits = 5, fitstat = ~ n + ivf1))
print(fitstat(mq[[1]], ~ ivf + ivwald))
pts <- c(0, 2, 4, 6, 8) # pp of exposure inflation (z p1 to p99 range)
qe <- rbindlist(
    lapply(
        seq_along(ys),
        function(j) {
            b <- coef(mq[[j]])
            V <- vcov(mq[[j]])
            ct <- coeftable(mq[[j]])
            rbindlist(
                lapply(
                    pts,
                    function(t) {
                        a <- c(1, 2 * t)
                        data.table(
                            outcome = ys[j],
                            t2 = t,
                            me = sum(a * b),
                            se = sqrt(drop(t(a) %*% V %*% a)),
                            b_sq = b[['fit_t2sq']],
                            se_sq = ct['fit_t2sq', 2],
                            p_sq = 2 * pt(
                                abs(ct['fit_t2sq', 3]),
                                HET_G - 1,
                                lower.tail = FALSE
                            )
                        )
                    }
                )
            )
        }
    )
)
print(qe)


# 3. Asymmetry: local basket inflation above vs below the quarter's mean -------

# Across states (dev = z - the quarter mean of z)

d[, dev_up := (z - mean(z)) > 0, by = yq]
d[, `:=`(
    x_up = infl_exposure * dev_up,
    x_dn = infl_exposure * !dev_up,
    z_up = z * dev_up,
    z_dn = z * !dev_up
)]
ma <- feols(
    as.formula(
        sprintf(
            'c(%s) ~ 1 | yq^dev_up | x_up + x_dn ~ z_up + z_dn',
            paste(ys, collapse = ', ')
        )
    ),
    d,
    cluster = ~state,
    notes = FALSE
)
asy <- rbindlist(
    lapply(
        seq_along(ys),
        function(j) {
            b <- coef(ma[[j]])
            V <- vcov(ma[[j]])
            a <- c(1, -1)
            est <- sum(a * b)
            s <- sqrt(drop(t(a) %*% V %*% a))
            data.table(
                outcome = ys[j],
                b_up = b[['fit_x_up']],
                se_up = sqrt(V[1, 1]),
                b_dn = b[['fit_x_dn']],
                se_dn = sqrt(V[2, 2]),
                diff = est,
                se_diff = s,
                p = 2 * pt(abs(est / s), HET_G - 1, lower.tail = FALSE)
            )
        }
    )
)
print(asy)

# Family C tests, Holm-corrected across regime, episode, year, convexity and
# asymmetry (5 tests x 5 outcomes = 25)
tests <- rbind(
    wt[, .(test = moderator, outcome, stat = wald_F, p = wald_p)],
    unique(qe[, .(test = 'convexity', outcome, stat = b_sq, p = p_sq)]),
    asy[, .(test = 'asymmetry', outcome, stat = diff, p)]
)
tests[, p_holm := p.adjust(p, 'holm')]
print(tests)

save_results(
    '10_regime',
    res = res,
    wt = wt,
    qe = qe,
    asy = asy,
    tests = tests
)
print(sessionInfo())
