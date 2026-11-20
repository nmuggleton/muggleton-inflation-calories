# Sensitivity of the headline to the exact measure of the outcomes, the
# treatment and the instrument.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
set.seed(20261001)
setFixest_nthreads(4)

ys_all <- c(
    'd_lkpu', 'd_lkpu_nom', 'd_lkpu_w', 'd_arc_kpu', 'd_lkpu_fb',
    'd_lkcal', 'd_lkcal_tot', 'd_lkcal_fb', 'd_lkcal_sc', 'd_lvol',
    'd_lgrams', 'd_lspend', 'd_lspend_nom', 'd_lspend_lab',
    'd_lspend_unlab', 'd_lspend_tot', 'd_ed', 'd_hs', 'd_cov',
    'd_lspend_all', 'd_sh_mag', 'd_sh_fv_all'
)
cols <- c(
    'household_code', 'yq', 'year', 'state', 'county',
    'projection_factor', 'projection_factor_magnet', 'f_magnet_hh',
    'core_a', 'core_b', 'partial_a', 'partial_b', 'cov_lab',
    'cov_lab_b', 'kcal_per_usd_real', 'kpu_b', 'n_base_items',
    'infl_exposure', 'infl_laspeyres', 'f_idx_valid', 'idx_exposure',
    ys_all
)
d <- f1_read(cols)
d[, projection_factor := as.numeric(projection_factor)]
d[, projection_factor_magnet := as.numeric(projection_factor_magnet)]
d[, gate := ms_gate(d)]
stopifnot(sum(d$gate) == F1_N) # f1 N
d[, sq := paste(state, yq)]

# Population instrument: the leave-out mean over every T2-valid
# household-quarter with a base quarter (any core / coverage status), t != 2020
d[year != 2020, z_pop := loo_mean(infl_exposure, sq)]
g <- d[gate == TRUE]
rm(d)
invisible(gc())
g[, z := loo_mean(infl_exposure, sq)]
stopifnot(all(is.finite(g$z)), all(is.finite(g$z_pop)))


# A. Outcome definitions (headline instrument and sample) ----------------------

lab_y <- c(
    d_lkpu = 'kcal per $, winsorized p1/p99 by year (headline)',
    d_lkpu_nom = 'kcal per $, not winsorized',
    d_arc_kpu = 'kcal per $ (winsorized), arc % change',
    d_lkpu_fb = 'kcal per $ incl. module-imputed kcal',
    d_lkcal = 'Labeled kcal per AE-day (headline)',
    d_lkcal_tot = 'Labeled kcal per household-quarter',
    d_lkcal_fb = 'kcal incl. module-imputed, per AE-day',
    d_lkcal_sc = 'kcal scaled by 1/coverage, per AE-day',
    d_lvol = 'Labeled grams + ml per AE-day',
    d_lgrams = 'Labeled grams (foods) per AE-day',
    d_lspend = 'Real non-Magnet spend per AE-day (headline)',
    d_lspend_nom = 'Nominal non-Magnet spend per AE-day',
    d_lspend_lab = 'Labeled spend per AE-day',
    d_lspend_unlab = 'Unlabeled non-Magnet spend per AE-day',
    d_lspend_tot = 'Non-Magnet spend per household-quarter',
    d_ed = 'Energy density, foods (kcal/100 g)',
    d_hs = 'Health score',
    d_cov = 'Labeled share of spend (coverage)'
)
grp_y <- c(
    rep('Calories per dollar', 4), rep('Quantity', 6),
    rep('Spend', 5), rep('Nutrition and coverage', 3)
)
names(grp_y) <- names(lab_y)
oa <- ms_iv(g, names(lab_y), label = 'gate')
oa[, `:=`(group = grp_y[outcome], label = lab_y[outcome])]
print(oa)
# Nominal vs. real: the two differ only by the national deflator, which quarter
# FE absorb, so the results are identical up to rounding.
ow <- ms_iv(g, 'd_lkpu_w', label = 'gate')
cat(
    'headline vs winsorized kcal/$:',
    oa[outcome == 'd_lkpu', b],
    ow$b,
    '| raw:',
    oa[outcome == 'd_lkpu_nom', b],
    '\n'
)
stopifnot(
    abs(oa[outcome == 'd_lkpu', b] - ow$b) < 1e-8,
    abs(
        oa[outcome == 'd_lspend', b] -
            oa[outcome == 'd_lspend_nom', b]
    ) < 1e-8
)

# Exact budget identity on the common sample: kcal = labelled spend x kcal per
# labelled $, and since 2SLS is linear in y the coefficients add up exactly
ids <- g[
    is.finite(d_lkcal) & is.finite(d_lspend_lab) &
        is.finite(d_lkpu_nom) & is.finite(d_lspend) &
        is.finite(d_lspend_unlab)
]
ia <- ms_iv(
    ids,
    c(
        'd_lkcal', 'd_lspend_lab', 'd_lkpu_nom', 'd_lspend',
        'd_lspend_unlab'
    ),
    label = 'identity sample'
)
print(ia)
gap <- ia[outcome == 'd_lspend_lab', b] + ia[outcome == 'd_lkpu_nom', b] -
    ia[outcome == 'd_lkcal', b]
cat('identity: labeled spend + kcal/$ - kcal =', gap, '\n')
stopifnot(abs(gap) < 1e-6) # exact up to floating point
# 2SLS of labelled spend minus all spend (same sample, same instrument), to
# check whether the two spend responses differ
ids[, d_lab_minus_all := d_lspend_lab - d_lspend]
dl <- ms_iv(ids, 'd_lab_minus_all', label = 'labeled - all spend')
print(dl)


# B. Treatment / instrument construction and support (kcal/$, kcal, spend) -----

y3 <- c('d_lkpu', 'd_lkcal', 'd_lspend')
g[, cty := paste(state, county)]
g[, `:=`(sx = sum(infl_exposure), nn = .N), by = sq]
g[, `:=`(sxc = sum(infl_exposure), nc = .N), by = .(sq, cty)]
g[, z_lco := fifelse(nn > nc, (sx - sxc) / (nn - nc), NA_real_)]
g[, z_w := loo_mean(infl_exposure, sq, projection_factor)]
g[, big := as.numeric(n_base_items >= 50)]
g[, `:=`(sxb = sum(infl_exposure * big), nb = sum(big)), by = sq]
g[, z_big := fifelse(
    nb - big > 0,
    (sxb - infl_exposure * big) /
        (nb - big),
    NA_real_
)]
# Log points: idx_exposure is the index ratio, so its scale is picked from the
# identity with infl_exposure (pp)
sc <- if (abs(median(g$idx_exposure, na.rm = TRUE) - 1) < 0.5) 1 else 100
chk <- max(
    abs(100 * (g$idx_exposure / sc - 1) - g$infl_exposure),
    na.rm = TRUE
)
cat('idx scale', sc, '| max |100(idx-1) - T2|', chk, '\n')
stopifnot(chk < 1e-6)
g[, lt2 := 100 * log(idx_exposure / sc)]
g[, z_log := loo_mean(lt2, sq)]
# support diagnostics for the instrument
zr <- fixest::demean(g$z, g$yq)
g[, zres := as.numeric(zr)]
q_z <- quantile(g$zres, c(.025, .975))
cat(
    'instrument net of quarter FE: SD',
    sd(g$zres),
    '| p2.5/p97.5',
    q_z,
    '\n'
)
cellN <- g[, .N, by = sq]
cat(
    'state-quarter cells:',
    nrow(cellN),
    '| households per cell p10/p50',
    quantile(cellN$N, c(.1, .5)),
    '| cells < 100:',
    sum(cellN$N < 100),
    '\n'
)
big5 <- g[, .N, by = state][order(-N)][1:5, state]
cat('five largest states (FIPS):', big5, '\n')

tq <- g[, .(
    lo = quantile(infl_exposure, .01),
    hi = quantile(infl_exposure, .99)
), by = yq]
g[tq, `:=`(t_lo = i.lo, t_hi = i.hi), on = 'yq']
# Memory: the subsets are built one at a time inside the loop (hence the
# function() entries), since holding all ten subsets of the 2.8m-row panel at
# once exceeds 15 GB
B <- list(
    list(
        'Headline: leave-household-out state-quarter mean',
        function() g,
        'z',
        'infl_exposure'
    ),
    list(
        'Population instrument (all T2-valid households)',
        function() g,
        'z_pop',
        'infl_exposure'
    ),
    list(
        'Leave-county-out state-quarter mean',
        function() g[is.finite(z_lco)],
        'z_lco',
        'infl_exposure'
    ),
    list(
        'Projection-weighted leave-out mean',
        function() g,
        'z_w',
        'infl_exposure'
    ),
    list(
        'Instrument from baskets with 50+ items',
        function() g[is.finite(z_big)],
        'z_big',
        'infl_exposure'
    ),
    list('Log-point treatment, 100 log index', function() g, 'z_log', 'lt2'),
    list(
        'Treatment T2 trimmed p1/p99 by quarter',
        # The leave-out instrument is recomputed on the subsample
        function() {
            dd <- copy(g[infl_exposure >= t_lo & infl_exposure <= t_hi])
            dd[, z := loo_mean(infl_exposure, sq)]
            dd[is.finite(z)]
        },
        'z',
        'infl_exposure'
    ),
    list(
        'Instrument support: drop outer 5% of residual z',
        function() g[zres >= q_z[1] & zres <= q_z[2]],
        'z',
        'infl_exposure'
    ),
    list(
        'Drop state-quarters with < 100 households',
        function() g[sq %in% cellN[N >= 100, sq]],
        'z',
        'infl_exposure'
    ),
    list(
        'Drop the five largest states',
        # The leave-out instrument is recomputed on the subsample
        function() {
            dd <- copy(g[!state %in% big5])
            dd[, z := loo_mean(infl_exposure, sq)]
            dd[is.finite(z)]
        },
        'z',
        'infl_exposure'
    ),
    list(
        'Experienced inflation T1 instrumented by z',
        function() g[f_idx_valid == TRUE & is.finite(infl_laspeyres)],
        'z',
        'infl_laspeyres'
    )
)
ib <- rbindlist(
    lapply(
        B,
        function(s) {
            r <- ms_iv(s[[2]](), y3, x = s[[4]], z = s[[3]], label = s[[1]])
            invisible(gc())
            r
        }
    )
)
rm(B)
invisible(gc())
print(ib)

# Outcome-tail trims: each outcome is trimmed at its own p1/p99 within quarter
ot <- rbindlist(
    lapply(
        y3,
        function(v) {
            qq <- g[is.finite(get(v)), .(
                lo = quantile(get(v), .01),
                hi = quantile(get(v), .99)
            ), by = yq]
            dd <- g[qq, on = 'yq']
            dd <- dd[is.finite(get(v)) & get(v) >= lo & get(v) <= hi]
            ms_iv(dd, v, label = 'Outcome trimmed p1/p99 by quarter')
        }
    )
)
print(ot)
ib <- rbind(ib, ot)


# C. Magnet households (random-weight fresh food observed), t >= 2013 so -------

# that both quarters follow the 2012 Magnet module split. Instrument: the gate z
# (the state-quarter price shock from all gate households).

mg <- g[
    f_magnet_hh == TRUE & year >= 2013 &
        is.finite(projection_factor_magnet)
]
cat(
    'Magnet sample:',
    nrow(mg),
    'household-quarters,',
    uniqueN(mg$household_code),
    'households\n'
)
ym <- c(
    'd_lspend_all', 'd_sh_mag', 'd_sh_fv_all', 'd_lspend', 'd_lkpu',
    'd_lkcal'
)
lab_m <- c(
    d_lspend_all = 'All food spend incl. random weight, per AE-day',
    d_sh_mag = 'Random-weight share of food spend',
    d_sh_fv_all = 'Fruit and vegetable share incl. random weight',
    d_lspend = 'Non-Magnet spend per AE-day',
    d_lkpu = 'kcal per $ (labeled)',
    d_lkcal = 'Labeled kcal per AE-day'
)
mc <- rbind(
    ms_iv(mg, ym, label = 'Unweighted'),
    ms_iv(
        mg,
        ym,
        w = 'projection_factor_magnet',
        label = 'Magnet projection weights'
    )
)
print(mc)

res <- rbind(
    oa[, .(
        block = 'A outcome',
        spec,
        outcome,
        b,
        se,
        n,
        n_hh,
        n_cl,
        f
    )],
    ia[, .(
        block = 'A identity',
        spec,
        outcome,
        b,
        se,
        n,
        n_hh,
        n_cl,
        f
    )],
    dl[, .(
        block = 'A identity',
        spec,
        outcome,
        b,
        se,
        n,
        n_hh,
        n_cl,
        f
    )],
    ib[, .(
        block = 'B instrument',
        spec,
        outcome,
        b,
        se,
        n,
        n_hh,
        n_cl,
        f
    )],
    mc[, .(
        block = 'C magnet',
        spec,
        outcome,
        b,
        se,
        n,
        n_hh,
        n_cl,
        f
    )]
)

save_results(
    '12_measurement',
    oa = oa,
    ia = ia,
    dl = dl,
    ib = ib,
    mc = mc,
    ym = ym,
    lab_m = lab_m
)
print(sessionInfo())
