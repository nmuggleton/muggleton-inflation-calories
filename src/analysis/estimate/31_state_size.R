# Does the calorie response depend on the size of the state? Found in
# 30_density.R: calories fall in the states with the most panel households and
# rise in those with the fewest, and the pooled estimate averages the two.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

int <- Sys.getenv('WP2_INT')
root <- Sys.getenv('LOCAL_DATA', file.path(wp2_paths()$data, 'intermediate'))
outcomes <- c('d_lkpu', 'd_lspend', 'd_lkcal')

con <- DBI::dbConnect(duckdb::duckdb())
d <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            "SELECT household_code, yq, state, cpi_area, infl_exposure,
          d_cpi, d_lkpu, d_lspend, d_lkcal
   FROM '%s'",
            file.path(root, 'local_context', 'local_panel.parquet')
        )
    )
)
half <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            paste(
                'SELECT household_code, yq, half, n_priced, idx_half',
                "FROM read_parquet('%s/price_split/split=1/year=*.parquet')"
            ),
            int
        )
    )
)
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(nrow(d) == F1_N)
d[, z := (sum(infl_exposure) - infl_exposure) / (.N - 1), by = .(state, yq)]

# States split into thirds of household-quarters, largest first
size <- d[, .(n_state = .N), by = state][order(-n_state)]
size[, cum := cumsum(n_state) / sum(n_state)]
size[, group := cut(
    cum,
    c(0, 1 / 3, 2 / 3, 1),
    labels = c('largest', 'middle', 'smallest')
)]
print(size[, .(states = .N, hh_quarters = sum(n_state)), keyby = group])
print(size[group == 'largest'])
d[size, on = 'state', `:=`(group = i.group, l_size = log(i.n_state))]

fit <- function(data, y, inst, cl, spec) {
    m <- feols(
        as.formula(sprintf('%s ~ 1 | yq | infl_exposure ~ %s', y, inst)),
        data,
        cluster = as.formula(paste0('~', cl))
    )
    data.table(
        spec = spec,
        outcome = y,
        b = coef(m)[[1]],
        se = se(m)[[1]],
        n = nobs(m),
        clusters = uniqueN(data[[cl]])
    )
}
all_outcomes <- function(data, inst, cl, spec) {
    rbindlist(lapply(outcomes, function(y) fit(data, y, inst, cl, spec)))
}

# (1) and (2) by state size, main and BLS instruments --------------------------
by_size <- rbindlist(
    lapply(
        levels(d$group),
        function(g) {
            dg <- d[group == g]
            cbind(
                group = g,
                rbind(
                    all_outcomes(dg, 'z', 'state', 'main'),
                    all_outcomes(dg, 'd_cpi', 'cpi_area', 'BLS')
                )
            )
        }
    )
)
print(
    dcast(by_size, spec + group + clusters ~ outcome, value.var = 'b'),
    digits = 3
)
print(
    dcast(by_size, spec + group + clusters ~ outcome, value.var = 'se'),
    digits = 3
)

# (3) does the response change with log state size? ----------------------------
d[, l_size_c := l_size - mean(l_size)]
d[, `:=`(x_size = infl_exposure * l_size_c, z_size = z * l_size_c)]
gradient <- rbindlist(
    lapply(
        outcomes,
        function(y) {
            m <- feols(
                as.formula(
                    sprintf(
                        '%s ~ 1 | yq | infl_exposure + x_size ~ z + z_size',
                        y
                    )
                ),
                d,
                cluster = ~state
            )
            ct <- coeftable(m)
            data.table(
                outcome = y,
                term = rownames(ct),
                b = ct[, 1],
                se = ct[, 2],
                p = ct[, 4]
            )
        }
    )
)
cat('\n== response by log state size (centred), 49 clusters\n')
print(gradient, digits = 3)

# The three groups in one model (group-specific quarter effects, all 49 state
# clusters), with a Wald test that the three responses are the same; and the
# log-size interaction again, with group-specific quarter effects
for (k in seq_along(levels(d$group))) {
    in_group <- d$group == levels(d$group)[k]
    set(d, j = paste0('x_g', k), value = d$infl_exposure * in_group)
    set(d, j = paste0('z_g', k), value = d$z * in_group)
}
contrast <- rbind(c(1, -1, 0), c(0, 1, -1))
equality <- rbindlist(
    lapply(
        outcomes,
        function(y) {
            m <- feols(
                as.formula(
                    sprintf(
                        paste(
                            '%s ~ 1 | yq^group |',
                            'x_g1 + x_g2 + x_g3 ~ z_g1 + z_g2 + z_g3'
                        ),
                        y
                    )
                ),
                d,
                cluster = ~state
            )
            b <- coef(m)
            gap <- contrast %*% b
            stat <- drop(
                t(gap) %*% solve(contrast %*% vcov(m) %*% t(contrast)) %*% gap
            )
            m2 <- feols(
                as.formula(
                    sprintf(
                        paste(
                            '%s ~ 1 | yq^group |',
                            'infl_exposure + x_size ~ z + z_size'
                        ),
                        y
                    )
                ),
                d,
                cluster = ~state
            )
            data.table(
                outcome = y,
                b_largest = b[[1]],
                b_middle = b[[2]],
                b_smallest = b[[3]],
                p_equal = pf(stat / 2, 2, 48, lower.tail = FALSE),
                b_log_size = coeftable(m2)['fit_x_size', 1],
                se_log_size = coeftable(m2)['fit_x_size', 2],
                p_log_size = coeftable(m2)['fit_x_size', 4]
            )
        }
    )
)
cat('\n== equality across size groups, and the log-size slope\n')
print(equality, digits = 3)

# (4) without California; leave out each of the largest states -----------------
no_ca <- rbind(
    cbind(group = 'all', all_outcomes(d[state != 6], 'z', 'state', 'no CA')),
    cbind(
        group = 'largest',
        all_outcomes(d[state != 6 & group == 'largest'], 'z', 'state', 'no CA')
    ),
    cbind(group = 'all', all_outcomes(d, 'z', 'state', 'main'))
)
cat('\n== without California\n')
print(dcast(no_ca, spec + group ~ outcome, value.var = 'b'), digits = 3)
print(dcast(no_ca, spec + group ~ outcome, value.var = 'se'), digits = 3)

big <- d[group == 'largest']
leave_out <- rbindlist(
    lapply(
        unique(big$state),
        function(s) {
            cbind(
                dropped = s,
                fit(big[state != s], 'd_lkcal', 'z', 'state', 'largest')
            )
        }
    )
)
cat('\n== calories in the largest states, leaving one out\n')
print(leave_out[, .(dropped, b, se, n)], digits = 3)

# (5) thinner prices: exposure from a random half of households ----------------
h <- merge(d, half, by = c('household_code', 'yq'))
h <- h[n_priced >= 10 & is.finite(idx_half)]
h[, x_half := 100 * (idx_half - 1)]
h[, `:=`(sum_h = sum(x_half), n_h = .N), by = .(state, yq, half)]
h[, z_half := (sum_h - x_half) / (n_h - 1)]
h <- h[is.finite(z_half)]
h[, half_q := paste(half, yq)]
thin <- rbindlist(
    lapply(
        levels(h$group),
        function(g) {
            hg <- h[group == g]
            rbindlist(
                lapply(
                    outcomes,
                    function(y) {
                        m <- feols(
                            as.formula(
                                sprintf(
                                    '%s ~ 1 | half_q | x_half ~ z_half',
                                    y
                                )
                            ),
                            hg,
                            cluster = ~state
                        )
                        data.table(
                            group = g,
                            outcome = y,
                            b = coef(m)[[1]],
                            se = se(m)[[1]],
                            n = nobs(m)
                        )
                    }
                )
            )
        }
    )
)
cat('\n== half-sample prices and instrument, by state size\n')
print(dcast(thin, group ~ outcome, value.var = 'b'), digits = 3)
print(dcast(thin, group ~ outcome, value.var = 'se'), digits = 3)

# (6) few-cluster inference by state size --------------------------------------
# With 4 or 11 clusters the cluster-robust t test over-rejects.
samples <- list(
    'All 49' = d,
    '4 largest' = d[group == 'largest'],
    'Next 11' = d[group == 'middle'],
    '34 smallest' = d[group == 'smallest'],
    '15 largest' = d[group != 'smallest']
)
grid <- round(seq(-2.5, 2.5, by = 0.005), 3)
boot <- rbindlist(
    lapply(
        names(samples),
        function(s) {
            ds <- samples[[s]]
            rbindlist(
                lapply(
                    outcomes,
                    function(y) {
                        w <- ms_wcr_ar(
                            ds[[y]],
                            ds$infl_exposure,
                            ds$z,
                            ds$yq,
                            ds$state,
                            grid,
                            B = 9999,
                            seed = 20261117
                        )
                        cbind(
                            group = s,
                            fit(ds, y, 'z', 'state', 'main'),
                            p_wild = w$p[b0 == 0, p],
                            lo_wild = w$ci[1],
                            hi_wild = w$ci[2],
                            edge = w$edge # TRUE: the interval hits the grid
                        )
                    }
                )
            )
        }
    )
)
cat('\n== wild cluster bootstrap by state size\n')
print(boot[, !'spec'], digits = 3)

# (7) all 49 states, size-group by quarter effects -----------------------------
pooled <- rbindlist(
    lapply(
        outcomes,
        function(y) {
            m <- feols(
                as.formula(
                    sprintf('%s ~ 1 | yq^group | infl_exposure ~ z', y)
                ),
                d,
                cluster = ~state
            )
            data.table(
                outcome = y,
                b = coef(m)[[1]],
                se = se(m)[[1]],
                n = nobs(m)
            )
        }
    )
)
cat('\n== pooled, size-group by quarter effects\n')
print(pooled, digits = 3)

# (8) is the size grouping special? --------------------------------------------
# The Wald test of equal responses in (3) treats a variance estimated from 4
# clusters as known.
cells <- d[, .(
    n = .N,
    x = mean(infl_exposure),
    z = mean(z),
    d_lkcal = mean(d_lkcal, na.rm = TRUE),
    d_lkpu = mean(d_lkpu, na.rm = TRUE)
), by = .(state, yq)]
within_q <- function(v, w, q) {
    v - ave(v * w, q, FUN = sum) / ave(w, q, FUN = sum)
}
states <- size$state # largest first
n_perm <- 5000
set.seed(20261117)
perm <- rbindlist(
    lapply(
        c('d_lkcal', 'd_lkpu'),
        function(y) {
            # The observed order of states first, then n_perm random orders; in
            # each order, states 1-4, 5-15 and 16-49 form the three groups
            orders <- c(
                list(states),
                replicate(n_perm, sample(states), simplify = FALSE)
            )
            # The Wald statistic that the three groups respond equally, for each
            # order
            wald <- numeric(length(orders))
            for (i in seq_along(orders)) {
                s <- orders[[i]]
                groups <- list(s[1:4], s[5:15], s[16:49])
                # In each group of states, the 2SLS slope (b) and its
                # state-clustered variance (v)
                b <- numeric(3)
                v <- numeric(3)
                for (k in 1:3) {
                    cc <- cells[state %in% groups[[k]]]
                    zt <- within_q(cc$z, cc$n, cc$yq)
                    yt <- within_q(cc[[y]], cc$n, cc$yq)
                    xt <- within_q(cc$x, cc$n, cc$yq)
                    den <- sum(cc$n * zt * xt)
                    b[k] <- sum(cc$n * zt * yt) / den
                    score <- tapply(
                        cc$n * zt * (yt - b[k] * xt), cc$state, sum
                    )
                    v[k] <- sum(score^2) / den^2
                }
                gap <- contrast %*% b
                v_gap <- contrast %*% diag(v) %*% t(contrast)
                wald[i] <- drop(t(gap) %*% solve(v_gap) %*% gap)
            }
            observed <- wald[1]
            draws <- wald[-1]
            data.table(
                outcome = y,
                wald = observed,
                p_chi2 = pchisq(observed, 2, lower.tail = FALSE),
                p_perm = mean(draws >= observed),
                perm_95 = quantile(draws, 0.95),
                n_perm = n_perm
            )
        }
    )
)
cat('\n== equality across size groups: permutation of the grouping\n')
print(perm, digits = 3)

# (9) calorie measures that do not depend on label coverage --------------------
# Labelled calories rise where the labelled share of spending rises.
alt_y <- c('d_lkcal_fb', 'd_lkcal_tot', 'd_lkcal_sc', 'd_cov')
alt <- f1_read(c('household_code', 'yq', alt_y), 'est_gate')
alt_cal <- rbindlist(
    lapply(
        names(samples),
        function(s) {
            ds <- merge(
                samples[[s]][, .(household_code, yq, state, infl_exposure, z)],
                alt,
                by = c('household_code', 'yq')
            )
            cbind(
                group = s,
                rbindlist(
                    lapply(alt_y, function(y) fit(ds, y, 'z', 'state', 'main'))
                )
            )
        }
    )
)
cat('\n== coverage-robust calorie measures by state size\n')
print(dcast(alt_cal, group ~ outcome, value.var = 'b'), digits = 3)
print(dcast(alt_cal, group ~ outcome, value.var = 'se'), digits = 3)

save_results(
    '31_state_size',
    size = size,
    by_size = by_size,
    gradient = gradient,
    equality = equality,
    no_ca = no_ca,
    leave_out = leave_out,
    thin = thin,
    boot = boot,
    pooled = pooled,
    perm = perm,
    alt_cal = alt_cal
)
