# An instrument built only from data outside NielsenIQ: BLS CPI inflation for
# food at home in the CPI area the household belongs to.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
set.seed(20261001)
setFixest_nthreads(2)

root <- Sys.getenv('LOCAL_DATA', file.path(wp2_paths()$data, 'intermediate'))
ctx <- file.path(root, 'local_context')
con <- DBI::dbConnect(duckdb::duckdb())
d <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            "SELECT household_code, yq, year, state, cpi_area, area_type,
          infl_exposure, d_cpi, d_lkpu, d_lspend, d_lkcal, d_ed, d_hs
   FROM '%s'",
            file.path(ctx, 'local_panel.parquet')
        )
    )
)
us <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            paste0(
                "SELECT yq, d_cpi FROM '%s' WHERE ",
                "area = '0000' AND d_cpi IS NOT NULL"
            ),
            file.path(ctx, 'cpi_q.parquet')
        )
    )
)
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(nrow(d) == F1_N, !anyNA(d$d_cpi), !anyNA(d$cpi_area))
d[, z := (sum(infl_exposure) - infl_exposure) / (.N - 1), by = .(state, yq)]
cat(
    'sample:',
    nrow(d),
    'household-quarters,',
    uniqueN(d$household_code),
    'households,',
    uniqueN(d$cpi_area),
    'CPI areas,',
    uniqueN(d[, .(cpi_area, yq)]),
    'area-quarters\n'
)
print(d[, .(n = .N, hh = uniqueN(household_code)), keyby = area_type])


# A. Validation: Nielsen exposure inflation vs BLS food at home ----------------

nat <- merge(
    d[, .(t2 = mean(infl_exposure)), by = yq],
    us[, .(yq, bls = d_cpi)],
    by = 'yq'
)
cat(
    'national quarters:',
    nrow(nat),
    '| corr(Nielsen mean T2, BLS US FAH)',
    cor(nat$t2, nat$bls),
    '| slope',
    coef(lm(t2 ~ bls, nat))[[2]],
    '| mean gap (T2 - BLS)',
    mean(nat$t2 - nat$bls),
    '\n'
)
cell <- d[, .(
    t2 = mean(infl_exposure),
    bls = d_cpi[1],
    n = .N,
    area_type = area_type[1]
), by = .(cpi_area, yq)]
stopifnot(d[, uniqueN(d_cpi), by = .(cpi_area, yq)]$V1 == 1)
vf <- feols(t2 ~ bls | yq, cell, weights = ~n, cluster = ~cpi_area)
vr <- demean(
    as.matrix(cell[, .(t2, bls)]),
    f = cell[, .(yq)],
    weights = cell$n
)
cell[, `:=`(t2_r = vr[, 1], bls_r = vr[, 2])]
wcor <- function(x, y, w) cov.wt(cbind(x, y), w, cor = TRUE)$cor[1, 2]
val <- rbindlist(
    list(
        data.table(
            level = 'National, time series',
            cells = nrow(nat),
            r = cor(nat$t2, nat$bls),
            slope = coef(lm(t2 ~ bls, nat))[[2]],
            se = NA_real_
        ),
        data.table(
            level = 'CPI area x quarter, raw',
            cells = nrow(cell),
            r = wcor(cell$t2, cell$bls, cell$n),
            slope = coef(feols(t2 ~ bls, cell, weights = ~n))[[2]],
            se = NA_real_
        ),
        data.table(
            level = 'CPI area x quarter, net of quarter FE',
            cells = nrow(cell),
            r = wcor(cell$t2_r, cell$bls_r, cell$n),
            slope = coef(vf)[[1]],
            se = se(vf)[[1]]
        )
    )
)
for (k in c('Metro', 'Region B/C')) {
    ck <- cell[area_type == k]
    m <- feols(t2 ~ bls | yq, ck, weights = ~n, cluster = ~cpi_area)
    rk <- demean(
        as.matrix(ck[, .(t2, bls)]),
        f = ck[, .(yq)],
        weights = ck$n
    )
    val <- rbind(
        val,
        data.table(
            level = paste0(k, ' areas, net of quarter FE'),
            cells = nrow(ck),
            r = wcor(rk[, 1], rk[, 2], ck$n),
            slope = coef(m)[[1]],
            se = se(m)[[1]]
        )
    )
}
print(val)


# B. 2SLS with the BLS instrument (micro data, CPI-area clusters) --------------

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
        d_cpi = 'BLS food-at-home inflation (pp)',
        z = 'Nielsen state leave-out mean T2 (pp)',
        yq = 'Quarter', cpi_area = 'CPI area'
    )
)
lhs <- paste(names(ys), collapse = ', ')
f_iv <- function(inst) {
    as.formula(
        sprintf(
            'c(%s) ~ 1 | yq | infl_exposure ~ %s',
            lhs,
            inst
        )
    )
}
m_f1 <- feols(f_iv('z'), d, cluster = ~state)
m_bls <- feols(f_iv('d_cpi'), d, cluster = ~cpi_area)
m_met <- feols(f_iv('d_cpi'), d[area_type == 'Metro'], cluster = ~cpi_area)
m_both <- feols(f_iv('z + d_cpi'), d, cluster = ~state)
print(etable(m_bls, stage = 1:2))
fs_bls <- fitstat(m_bls[[1]], ~ ivf1 + ivwald1)
fs_met <- fitstat(m_met[[1]], ~ ivf1 + ivwald1)
print(fs_bls)
print(fs_met)
print(fitstat(m_both[[1]], ~ ivf1 + sargan))
grab <- function(mm, lab, cl) {
    rbindlist(
        lapply(
            seq_along(ys),
            function(j) {
                m <- mm[[j]]
                data.table(
                    model = lab,
                    outcome = names(ys)[j],
                    b = coef(m)[['fit_infl_exposure']],
                    se = se(m)[['fit_infl_exposure']],
                    n = nobs(m),
                    f = fitstat(m, ~ivf1)[[1]]$stat,
                    sargan_p = if (lab == 'Both instruments') {
                        fitstat(m, ~sargan)[[1]]$p
                    } else {
                        NA_real_
                    },
                    cluster = cl
                )
            }
        )
    )
}
est <- rbind(
    grab(m_f1, 'Main instrument (state leave-out)', 'state'),
    grab(m_bls, 'BLS CPI, all areas', 'CPI area'),
    grab(m_met, 'BLS CPI, 14 metros', 'CPI area'),
    grab(m_both, 'Both instruments', 'state')
)
print(est)
local_bls_iv_tex <- etable(
    m_bls,
    tex = TRUE,
    style.tex = style.tex(
        'aer',
        tablefoot = FALSE,
        tabular = '*',
        yesNo = c('Yes', '')
    ),
    signif.code = c('***' = .001, '**' = .01, '*' = .05),
    fitstat = ~ n + ivf1,
    fixef_sizes = FALSE,
    digits = 3,
    digits.stats = 3,
    postprocess.tex = tex_fit_apa
)


# C. Small-cluster inference on CPI-area x quarter cell means ------------------

# When the instrument and FE vary at the cell level or coarser, regressions on
# cell means weighted by cell size give the same first stage, reduced form and
# 2SLS ratio as the microdata (checked against m_bls below).
crv_t <- function(yd, xd, w, g) { # slope and CR1 t-stat, one regressor
    sxx <- sum(w * xd^2)
    b <- sum(w * xd * yd) / sxx
    e <- yd - b * xd
    s <- rowsum(w * xd * e, g)
    G <- length(s)
    v <- sum(s^2) / sxx^2 * G / (G - 1)
    c(b = b, t = b / sqrt(v), se = sqrt(v))
}
webb <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
wild_p <- function(yd, xd, w, g, q, B = 999) {
    # The null is imposed (slope 0): with the FE partialled out, the restricted
    # residual is yd.
    t0 <- crv_t(yd, xd, w, g)[['t']]
    ug <- unique(g)
    gi <- match(g, ug)
    uq <- unique(q)
    qi <- match(q, uq)
    sw <- rowsum(w, qi, reorder = FALSE)[, 1]
    tb <- vapply(
        seq_len(B),
        function(r) {
            ys <- yd * sample(webb, length(ug), TRUE)[gi]
            ys <- ys - (rowsum(w * ys, qi, reorder = FALSE)[, 1] / sw)[qi]
            crv_t(ys, xd, w, g)[['t']]
        },
        0
    )
    mean(abs(tb) >= abs(t0))
}
inf <- rbindlist(
    lapply(
        c('all', 'metro'),
        function(smp) {
            rbindlist(
                lapply(
                    names(ys),
                    function(v) {
                        in_sample <- smp == 'all' | d$area_type == 'Metro'
                        dd <- d[is.finite(get(v)) & in_sample]
                        cm <- dd[, .(
                            y = mean(get(v)),
                            t = mean(infl_exposure),
                            x = d_cpi[1],
                            n = .N
                        ), by = .(cpi_area, yq)]
                        # FWL with quarter FE, regressions weighted by n
                        X <- demean(
                            as.matrix(cm[, .(y, t, x)]),
                            f = cm[, .(yq)],
                            weights = cm$n
                        )
                        r <- list(yd = X[, 1], td = X[, 2], xd = X[, 3])
                        fs <- crv_t(r$td, r$xd, cm$n, cm$cpi_area)
                        rf <- crv_t(r$yd, r$xd, cm$n, cm$cpi_area)
                        b_iv <- rf[['b']] / fs[['b']]
                        p_rf <- wild_p(r$yd, r$xd, cm$n, cm$cpi_area, cm$yq)
                        # Pairs cluster bootstrap, re-estimating the 2SLS ratio
                        # in each draw
                        ua <- unique(cm$cpi_area)
                        sp <- split(cm, by = 'cpi_area')
                        bs <- vapply(
                            seq_len(999),
                            function(k) {
                                cb <- rbindlist(sp[sample(
                                    ua,
                                    length(ua),
                                    TRUE
                                )])
                                Xb <- demean(
                                    as.matrix(cb[, .(y, t, x)]),
                                    f = cb[, .(yq)],
                                    weights = cb$n
                                )
                                # Columns are y, t (exposure) and x (instrument)
                                sum(cb$n * Xb[, 3] * Xb[, 1]) /
                                    sum(cb$n * Xb[, 3] * Xb[, 2])
                            },
                            0
                        )
                        # Anderson-Rubin set: the values of beta0 not rejected
                        # by a wild bootstrap test of H0: beta = beta0,
                        # regressing (y - beta0 t) on x in each draw.
                        grid <- b_iv + seq(-1, 1, length.out = 201) * 6 * sd(bs)
                        keep <- vapply(
                            grid,
                            function(b0) {
                                wild_p(
                                    r$yd - b0 * r$td,
                                    r$xd,
                                    cm$n,
                                    cm$cpi_area,
                                    cm$yq,
                                    B = 399
                                ) > .05
                            },
                            TRUE
                        )
                        data.table(
                            sample = smp,
                            outcome = v,
                            clusters = length(ua),
                            cells = nrow(cm),
                            b_fs = fs[['b']],
                            t_fs = fs[['t']],
                            b_rf = rf[['b']],
                            se_rf = rf[['se']],
                            wild_p_rf = p_rf,
                            b_iv = b_iv,
                            ar_lo = if (any(keep)) min(grid[keep]) else NA,
                            ar_hi = if (any(keep)) max(grid[keep]) else NA,
                            ar_gaps = any(diff(which(keep)) > 1),
                            ar_at_edge = keep[1] | keep[length(keep)],
                            pairs_se = sd(bs),
                            pairs_lo = quantile(
                                bs,
                                .025
                            )[[1]],
                            pairs_hi = quantile(bs, .975)[[1]]
                        )
                    }
                )
            )
        }
    )
)
print(inf)
# The cell-mean ratio must equal the micro 2SLS up to floating-point precision,
# since both are exact closed forms (in practice the difference is around 1e-10)
chk <- merge(
    inf[sample == 'all', .(outcome, b_iv)],
    est[model == 'BLS CPI, all areas', .(outcome, b)],
    by = 'outcome'
)
print(chk[, .(outcome, b_iv, b, diff = b_iv - b)])
stopifnot(max(abs(chk$b_iv - chk$b)) < 1e-6)
est <- merge(
    est,
    rbind(
        inf[sample == 'all', .(
            model = 'BLS CPI, all areas',
            outcome,
            ar_lo,
            ar_hi,
            pairs_lo,
            pairs_hi,
            wild_p_rf
        )],
        inf[sample == 'metro', .(
            model = 'BLS CPI, 14 metros',
            outcome,
            ar_lo,
            ar_hi,
            pairs_lo,
            pairs_hi,
            wild_p_rf
        )]
    ),
    by = c('model', 'outcome'),
    all.x = TRUE
)
print(est[order(outcome, model)])

save_results(
    '16_bls_cpi',
    est = est,
    val = val,
    inf = inf,
    local_bls_iv_tex = local_bls_iv_tex,
    first_stage = data.table(
        sample = c('all', 'metro'),
        f = c(fs_bls$ivf1$stat, fs_met$ivf1$stat),
        wald = c(
            fs_bls$ivwald1$stat,
            fs_met$ivwald1$stat
        )
    )
)
print(sessionInfo())
