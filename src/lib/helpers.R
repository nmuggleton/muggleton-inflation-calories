# Common code for the estimation scripts in src/analysis/estimate/. This file is
# sourced, never run on its own.

suppressPackageStartupMessages({
    library(data.table)
    library(fixest)
    library(ggplot2)
})
source('src/lib/load.R')
source('src/lib/format.R')


# Sample counts ----------------------------------------------------------------

# Exact counts expected for the last panel year (WP2_END_YEAR,
# src/lib/paths.sh), from config/expected_n.tsv: estimation-sample rows,
# state-quarter cells (49 states times the number of quarters), quarters in the
# sample, and quarters from 2005 including 2020.
END_YEAR <- as.integer(Sys.getenv('WP2_END_YEAR', '2024'))
expected_n <- function(k) {
    e <- fread('config/expected_n.tsv')[end_year == END_YEAR & key == k, n]
    stopifnot(length(e) == 1)
    as.integer(e)
}
F1_N <- expected_n('est_gate')


# Reading the analysis dataset -------------------------------------------------

f1_dir <- function() file.path(wp2_paths()$final, 'f1_analysis')

# Columns of the analysis dataset for the rows selected by `where`
f1_read <- function(cols, where = 'TRUE') {
    stopifnot(dir.exists(f1_dir()))
    d <- wp2_sql(
        sprintf(
            paste0(
                "SELECT %s FROM read_parquet('%s/*/*.parquet', ",
                'hive_partitioning = true) WHERE %s'
            ),
            paste(unique(cols), collapse = ', '),
            f1_dir(),
            where
        )
    )
    for (v in intersect(
        c('projection_factor', 'projection_factor_magnet'),
        names(d)
    )) {
        set(d, j = v, value = as.numeric(d[[v]]))
    }
    d
}

# SQL over the analysis dataset: write F1 where a table name goes, e.g.
# f1_sql("SELECT state, avg(d_lkpu) FROM F1 WHERE est_gate GROUP BY 1")
f1_sql <- function(sql) {
    src <- sprintf(
        "read_parquet('%s/*/*.parquet', hive_partitioning = true)",
        f1_dir()
    )
    wp2_sql(gsub('\\bF1\\b', src, sql, perl = TRUE))
}

# The f1 estimation sample, with the instrument and the grouping keys
f1_gate <- function(cols = NULL) {
    d <- f1_read(
        unique(
            c(
                'household_code', 'yq', 'year', 'state', 'z',
                'infl_exposure', cols
            )
        ),
        'est_gate'
    )
    stopifnot(
        nrow(d) == F1_N,
        !anyNA(d$infl_exposure),
        !anyNA(d$z),
        !anyNA(d$state)
    )
    d[, state_q := paste(state, yq)]
    d[]
}


# Loaders for heterogeneity and robustness -------------------------------------

# Loader for the heterogeneity analyses: gate rows with the moderators measured
# in the base (t-4) quarter.
HET_LEVELS <- list(
    fpl_bin = c(
        '<=100%', '100-130%', '130-185%', '185-300%', '300-500%',
        '>500%'
    ),
    age_bin = c('<35', '35-44', '45-54', '55-64', '65+'),
    regime = c('Low', 'Middle', 'High')
)
het_load <- function(cols = NULL) {
    d <- f1_gate(cols)
    for (v in intersect(names(HET_LEVELS), names(d))) {
        stopifnot(all(is.na(d[[v]]) | d[[v]] %in% HET_LEVELS[[v]]))
        set(d, j = v, value = factor(d[[v]], levels = HET_LEVELS[[v]]))
    }
    d[]
}

# Loader for the robustness analyses: gate rows with the alternative instruments
# and lags
rob_load <- function() {
    d <- f1_gate(
        c(
            'qtr', 'region_code', 'income_cat', 'projection_factor',
            'n_base_items', 'infl_laspeyres', 'f_idx_valid',
            'd_lkpu', 'd_lspend', 'd_lkcal', 'd_ed', 'd_hs', 'd_cov',
            'panel_year', 'panel_year_b', 'd_emp', 'd_linc',
            'd_hhsize', 'l_d_lkpu', 'l_d_lspend', 'l_d_lkcal',
            'l_d_ed', 'l_d_hs', 'z_div', 'zb', 'zb_lead', 'zb_lag'
        )
    )
    d[, `:=`(
        div_q = paste(region_code, yq),
        inc_q = paste(income_cat, yq),
        state_y = paste(state, year)
    )]
    d[, `:=`(z_sq = mean(infl_exposure), n_sq = .N), by = state_q]
    d[]
}



# Heterogeneity: 2SLS by group and by gradient ---------------------------------

# Group-specific 2SLS. g names a factor or logical column (rows with NA are
# dropped).
het_grp <- function(d, g, ys = names(HET_Y), weights = NULL) {
    dd <- d[!is.na(get(g))]
    dd[, gg := factor(as.character(get(g)))]
    if (is.factor(d[[g]])) {
        dd[, gg := factor(
            as.character(gg),
            levels = intersect(levels(d[[g]]), unique(gg))
        )]
    }
    lv <- levels(dd$gg)
    K <- length(lv)
    xs <- paste0('x_', seq_len(K))
    zs <- paste0('zz_', seq_len(K))
    for (k in seq_len(K)) {
        set(dd, j = xs[k], value = dd$infl_exposure * (dd$gg == lv[k]))
        set(dd, j = zs[k], value = dd$z * (dd$gg == lv[k]))
    }
    fm <- as.formula(
        sprintf(
            'c(%s) ~ 1 | yq^gg | %s ~ %s',
            paste(ys, collapse = ', '),
            paste(xs, collapse = ' + '),
            paste(zs, collapse = ' + ')
        )
    )
    w <- if (is.null(weights)) NULL else as.formula(paste0('~', weights))
    m <- feols(fm, dd, cluster = ~state, weights = w, notes = FALSE)
    # The first stage estimated separately by group (same instrument, same FE),
    # for the group F statistic
    fsF <- vapply(
        lv,
        function(k) {
            f <- feols(
                infl_exposure ~ z | yq,
                dd[gg == k],
                cluster = ~state,
                weights = w,
                notes = FALSE
            )
            unname((coef(f)[['z']] / se(f)[['z']])^2)
        },
        0
    )
    ns <- dd[, .N, keyby = gg]
    nh <- dd[, uniqueN(household_code), keyby = gg]
    rbindlist(
        lapply(
            seq_along(ys),
            function(j) {
                mj <- if (length(ys) == 1) m else m[[j]]
                b <- coef(mj)[paste0('fit_', xs)]
                V <- vcov(mj)[
                    paste0('fit_', xs),
                    paste0('fit_', xs)
                ]
                R <- cbind(-1, diag(K - 1))
                rb <- R %*% b
                W <- drop(t(rb) %*% solve(R %*% V %*% t(R)) %*% rb)
                Fw <- W / (K - 1)
                data.table(
                    moderator = g,
                    group = lv,
                    outcome = ys[j],
                    b = unname(b),
                    se = sqrt(diag(V)),
                    n = ns[lv, N, on = 'gg'],
                    hh = nh[lv, V1, on = 'gg'],
                    fs_F = fsF,
                    wald_F = Fw,
                    wald_p = pf(Fw, K - 1, HET_G - 1, lower.tail = FALSE)
                )
            }
        )
    )
}

# Continuous gradient: T2 and T2 x (m - median), instrumented by z and z x (m -
# median).
het_grad <- function(
    d, m, ys = names(HET_Y), fe = 'yq',
    weights = NULL
) {
    dd <- d[!is.na(get(m))]
    dd[, mc := get(m) - median(get(m))]
    dd[, `:=`(x_m = infl_exposure * mc, z_m = z * mc)]
    fm <- as.formula(
        sprintf(
            'c(%s) ~ mc | %s | infl_exposure + x_m ~ z + z_m',
            paste(ys, collapse = ', '),
            fe
        )
    )
    w <- if (is.null(weights)) NULL else as.formula(paste0('~', weights))
    mm <- feols(fm, dd, cluster = ~state, weights = w, notes = FALSE)
    rbindlist(
        lapply(
            seq_along(ys),
            function(j) {
                mj <- if (length(ys) == 1) mm else mm[[j]]
                ct <- coeftable(mj)
                data.table(
                    moderator = m,
                    outcome = ys[j],
                    b_med = ct['fit_infl_exposure', 1],
                    se_med = ct['fit_infl_exposure', 2],
                    b_int = ct['fit_x_m', 1],
                    se_int = ct['fit_x_m', 2],
                    p_int = 2 * pt(
                        abs(ct['fit_x_m', 3]),
                        HET_G - 1,
                        lower.tail = FALSE
                    ),
                    n = nobs(mj)
                )
            }
        )
    )
}


# Measurement and sample variants ----------------------------------------------

# The f1 gate (src/data_prep/07_controls/03_sample_panel.sql, est_gate), rebuilt
# from its flags so that each condition can be varied: cov is the labelled-share
# floor in t and t-4, items the base-basket floor (T2 validity already needs 10
# items).
ms_gate <- function(
    d, cov = 0.5, items = 10, y2020 = FALSE,
    core = 'both', partial = FALSE
) {
    ok <- (y2020 | d$year != 2020) &
        !is.na(d$cov_lab) & !is.na(d$cov_lab_b) &
        round(d$cov_lab, 9) >= cov & round(d$cov_lab_b, 9) >= cov &
        !is.na(d$kcal_per_usd_real) & !is.na(d$kpu_b) &
        d$kcal_per_usd_real > 0 & d$kpu_b > 0 & d$n_base_items >= items
    ok <- ok & switch(
        core,
        both = d$core_a & d$core_b,
        t = d$core_a,
        any = TRUE
    )
    if (!partial) ok <- ok & !d$partial_a & !d$partial_b
    ok
}

# Leave-household-out mean of x within the groups g, optionally weighted
loo_mean <- function(x, g, w = NULL) {
    if (is.null(w)) w <- rep(1, length(x))
    dt <- data.table(x = x, g = g, w = w)
    dt[, `:=`(sx = sum(w * x), sw = sum(w)), by = g]
    out <- (dt$sx - dt$w * dt$x) / (dt$sw - dt$w)
    out[!is.finite(out)] <- NA_real_
    out
}

# 2SLS of each outcome on T2 (x) instrumented by z; several outcomes can be
# fitted at once, and one row per outcome is returned. fe is a fixest FE string;
# w the name of a weight column, or NULL.
ms_iv <- function(
    d, ys, fe = 'yq', x = 'infl_exposure', z = 'z',
    w = NULL, cl = 'state', ctrl = NULL, label = ''
) {
    rhs <- if (is.null(ctrl)) '1' else paste(ctrl, collapse = ' + ')
    # Same as iv(), but one outcome per fit: the estimates are the same as from
    # a multi-outcome fit, but only one model's n-length vectors are in memory
    # at a time
    rbindlist(
        lapply(
            ys,
            function(y) {
                f <- as.formula(
                    sprintf(
                        '%s ~ %s | %s | %s ~ %s',
                        y,
                        rhs,
                        fe,
                        x,
                        z
                    )
                )
                mk <- feols(
                    f,
                    d,
                    cluster = as.formula(paste0('~', cl)),
                    weights = if (is.null(w)) {
                        NULL
                    } else {
                        as.formula(paste0('~', w))
                    },
                    notes = FALSE,
                    warn = FALSE
                )
                o <- obs(mk)
                out <- data.table(
                    spec = label,
                    outcome = y,
                    b = unname(coef(mk)[paste0('fit_', x)]),
                    se = unname(se(mk)[paste0('fit_', x)]),
                    n = nobs(mk),
                    n_hh = uniqueN(d$household_code[o]),
                    n_cl = uniqueN(d[[cl]][o]),
                    f = fitstat(mk, ~ivf1)[[1]]$stat
                )
                rm(mk)
                invisible(gc())
                out
            }
        )
    )
}

# Wild cluster restricted (null-imposed) bootstrap of the Anderson-Rubin test of
# H0: beta = b0, with quarter FE partialled out.
ms_wcr_ar <- function(y, x, z, fe, cl, grid, B = 9999, seed = 1) {
    ok <- is.finite(y) & is.finite(x) & is.finite(z)
    y <- y[ok]
    x <- x[ok]
    z <- z[ok]
    fe <- fe[ok]
    cl <- cl[ok]
    r <- fixest::demean(cbind(y, x, z), f = list(fe))
    yd <- r[, 1]
    xd <- r[, 2]
    zd <- r[, 3]
    gi <- as.integer(factor(cl))
    qi <- as.integer(factor(fe))
    G <- max(gi)
    Q <- max(qi)
    Cg <- as.numeric(tapply(zd * zd, gi, sum))
    C <- sum(Cg)
    Ay <- as.numeric(tapply(zd * yd, gi, sum))
    Ax <- as.numeric(tapply(zd * xd, gi, sum))
    sum_gq <- function(v) {
        m <- matrix(0, G, Q)
        s <- tapply(v, list(gi, qi), sum)
        m[!is.na(s)] <- s[!is.na(s)]
        m
    }
    Uy <- sum_gq(yd)
    Ux <- sum_gq(xd)
    Zgq <- sum_gq(zd)
    Nq <- as.numeric(table(qi))
    set.seed(seed)
    wv <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
    W <- matrix(sample(wv, B * G, replace = TRUE), B, G)
    tstat <- function(Wm, A, U) {
        g <- as.numeric(Wm %*% A) / C
        mq <- sweep(Wm %*% U, 2, Nq, '/') # bootstrap quarter means
        S <- sweep(Wm, 2, A, '*') - mq %*% t(Zgq) - outer(g, Cg)
        g / sqrt(rowSums(S^2) / C^2)
    }
    one <- matrix(1, 1, G)
    p <- vapply(
        grid,
        function(b0) {
            A <- Ay - b0 * Ax
            U <- Uy - b0 * Ux
            t0 <- tstat(one, A, U)
            mean(abs(tstat(W, A, U)) >= abs(t0))
        },
        0
    )
    acc <- grid[p >= 0.05]
    list(
        p = data.table(b0 = grid, p = p),
        ci = if (length(acc)) range(acc) else c(NA_real_, NA_real_),
        G = G,
        edge = length(acc) > 0 &&
            (min(acc) == min(grid) || max(acc) == max(grid))
    )
}


# Coefficient rows for tables --------------------------------------------------

coef_row <- function(m, design, outcome, var = 'fit_infl_exposure') {
    ct <- coeftable(m)
    i <- which(rownames(ct) == var)
    stopifnot(length(i) == 1)
    f <- tryCatch(fitstat(m, ~ivwald1)[[1]]$stat, error = function(e) NA)
    data.table(
        design = design,
        outcome = outcome,
        b = ct[i, 1],
        se = ct[i, 2],
        p = ct[i, 4],
        n = nobs(m),
        f_wald = f
    )
}
