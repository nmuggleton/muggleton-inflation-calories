# Inference with few, spatially correlated clusters. There are 49 states, but
# their sizes differ a lot, so there are only about 25 effective clusters.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
set.seed(20261001)
setFixest_nthreads(4)

d <- rob_load()
cat(
    'sample:',
    nrow(d),
    'household-quarters,',
    uniqueN(d$household_code),
    'households,',
    uniqueN(d$state),
    'states,',
    uniqueN(d$region_code),
    'divisions,',
    uniqueN(d$state_q),
    'state-quarters\n'
)
ys <- ROB_YS


# (a) Analytic standard errors under other clusterings -------------------------

iv <- feols(
    as.formula(
        sprintf(
            'c(%s) ~ 1 | yq | infl_exposure ~ z',
            paste(names(ys), collapse = ', ')
        )
    ),
    d,
    cluster = ~state
)
an <- rbindlist(
    lapply(
        seq_along(ys),
        function(k) {
            m <- iv[[k]]
            data.table(
                outcome = names(ys)[k],
                b = coef(m)[[1]],
                se_state = se(m)[[1]],
                se_div = se(summary(m, cluster = ~region_code))[[1]],
                se_2way = se(summary(m, cluster = ~ state + yq))[[1]],
                se_hh = se(summary(m, cluster = ~household_code))[[1]],
                n = nobs(m)
            )
        }
    )
)
print(an)
# This should reproduce the headline saved by 01_main.R; results should agree to
# 1e-6 (the order of floating-point sums differs).
stopifnot(abs(an[outcome == 'd_lkpu', b] - headline_estimate()) < 1e-6)


# (b) Wild cluster bootstrap of the Anderson-Rubin statistic -------------------

# Computed from cluster-level sums.
B <- 9999
# Absolute t-statistic of the AR coefficient and its bootstrap distribution, for
# one null value b0.
ar_p <- function(st, b0, W) {
    S <- st$A - b0 * st$C
    Qt <- sum(st$Q)
    b <- sum(S) / Qt
    t0 <- b / sqrt(sum((S - b * st$Q)^2)) # common scale factors cancel
    bs <- as.vector(W %*% S) / Qt
    sc <- sweep(W, 2, S, '*') - outer(bs, st$Q)
    ts <- bs / sqrt(rowSums(sc^2))
    mean(abs(ts) >= abs(t0))
}
# One row per clustering and outcome, in this order (the order matters: the
# bootstrap draws random weights sequentially, so it fixes the draws).
combos <- data.table(
    cluster = rep(c('state', 'region_code'), each = length(ys)),
    outcome = rep(names(ys), times = 2)
)
wcr <- rbindlist(
    lapply(
        seq_len(nrow(combos)), function(i) {
            cl <- combos$cluster[i]
            v <- combos$outcome[i]
            # Cluster-level sums of the scores, after partialling out quarter
            # fixed effects (FWL).
            s <- d[is.finite(get(v))]
            m <- demean(
                X = as.matrix(s[, .(y = get(v), x = infl_exposure, z)]),
                f = s[, .(yq)]
            )
            g <- as.integer(factor(s[[cl]]))
            st <- list(
                A = as.vector(rowsum(m[, 3] * m[, 1], g)),
                C = as.vector(rowsum(m[, 3] * m[, 2], g)),
                Q = as.vector(rowsum(m[, 3]^2, g)),
                G = max(g)
            )

            # Rademacher weights with many clusters; Webb (2014) six-point
            # weights with few (for G = 9 there are only 2^9 = 512 Rademacher
            # sign patterns).
            if (st$G > 20) {
                W <- matrix(
                    sample(c(-1, 1), B * st$G, replace = TRUE), B, st$G
                )
            } else {
                w6 <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
                W <- matrix(sample(w6, B * st$G, replace = TRUE), B, st$G)
            }

            # Confidence interval from a grid of values centred on the analytic
            # estimate: +-h SE with 801 points (step h/400 SE).
            a <- an[outcome == v]
            se_cl <- if (cl == 'state') a$se_state else a$se_div
            for (h in c(8, 16, 32)) {
                grid <- seq(a$b - h * se_cl, a$b + h * se_cl, length.out = 801)
                p <- vapply(grid, function(b0) ar_p(st, b0, W), 0)
                acc <- grid[p > .05]
                stopifnot(length(acc) > 0)
                if (min(acc) > min(grid) && max(acc) < max(grid)) break
            }
            stopifnot(min(acc) > min(grid), max(acc) < max(grid))
            ci <- c(lo = min(acc), hi = max(acc))

            # The benchmark null: b = -1 for calories per dollar, b = +1 for
            # spending.
            bench <- c(d_lkpu = -1, d_lspend = 1)[v]
            p_bench <- if (is.na(bench)) NA_real_ else ar_p(st, bench, W)

            data.table(
                outcome = v,
                cluster = cl,
                G = st$G,
                b_check = sum(st$A) / sum(st$C),
                p0 = ar_p(st, 0, W),
                p_bench = p_bench,
                lo = ci[['lo']],
                hi = ci[['hi']]
            )
        }
    )
)
print(wcr)
# The FWL 2SLS computed by hand from cluster sums should match fixest's 2SLS.
stopifnot(
    all(
        abs(wcr$b_check - an$b[match(wcr$outcome, an$outcome)]) <
            1e-8
    )
)


# (c) Randomization inference on the reduced form ------------------------------

# Here z_sq, the state-quarter mean of T2, is permuted across the 49 states (its
# correlation with the leave-out z is printed below).
cat('corr(z, z_sq) =', cor(d$z, d$z_sq), '\n')
stopifnot(uniqueN(d$state) * uniqueN(d$yq) == uniqueN(d$state_q))
R <- 4999
st_lev <- sort(unique(d$state))
q_lev <- sort(unique(d$yq))
Zm <- as.matrix(
    dcast(
        unique(d[, .(state, yq, z_sq)]),
        state ~ yq,
        value.var = 'z_sq'
    )[, -1]
)
ri_one <- function(v, b0) {
    s <- d[is.finite(get(v)), .(state, yq, y = get(v) - b0 * infl_exposure)]
    s[, y := y - mean(y), by = yq]
    a <- s[, .(Y = sum(y), N = .N), keyby = .(state, yq)]
    Ym <- as.matrix(dcast(a, state ~ yq, value.var = 'Y')[, -1])
    Nm <- as.matrix(dcast(a, state ~ yq, value.var = 'N')[, -1])
    stopifnot(!anyNA(Ym), !anyNA(Nm), all(dim(Ym) == dim(Zm)))
    stat <- function(Zp) {
        zb <- colSums(Nm * Zp) / colSums(Nm)
        sum(Zp * Ym) / sum(Nm * sweep(Zp, 2, zb)^2)
    }
    obs <- stat(Zm)
    path <- vapply(seq_len(R), function(r) stat(Zm[sample(nrow(Zm)), ]), 0)
    wq <- vapply(
        seq_len(R),
        function(r) {
            stat(apply(Zm, 2, sample))
        },
        0
    )
    data.table(
        outcome = v,
        b0 = b0,
        rf_obs = obs,
        p_path = (1 + sum(abs(path) >= abs(obs))) / (R + 1),
        p_withinq = (1 + sum(abs(wq) >= abs(obs))) / (R + 1),
        path_q975 = quantile(abs(path), .975)
    )
}
ri <- rbindlist(
    c(
        lapply(names(ys), ri_one, b0 = 0),
        list(ri_one('d_lkpu', -1), ri_one('d_lspend', 1))
    )
)
print(ri)


# Save -------------------------------------------------------------------------

save_results('06_inference', an = an, wcr = wcr, ri = ri)
print(sessionInfo())
