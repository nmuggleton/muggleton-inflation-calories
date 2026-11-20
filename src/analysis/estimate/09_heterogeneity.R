# Does the response weaken where food budgets bind? If tight budgets forced
# households to trade down, the poorest should pay the least.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)

d <- het_load(
    c(
        'household_code', 'yq', 'state', 'infl_exposure', 'z',
        'projection_factor', names(HET_Y), 'fpl_b', 'fpl_bin',
        'fpl_rank', 'snap_b', 'burden', 'burden_q',
        'kcal_100g_food_b', 'health_score_b', 'topcode_b', 'year'
    )
)
cat(
    'sample flow: gate',
    nrow(d),
    '| with FPL',
    d[!is.na(fpl_bin), .N],
    '| with burden',
    d[!is.na(burden_q), .N],
    '\n'
)

# The pooled estimate must match the headline saved by 01_main.R (a tolerance of
# 1e-6 allows for the order of floating-point sums)
ref <- feols(d_lkpu ~ 1 | yq | infl_exposure ~ z, d, cluster = ~state)
cat('headline replication (kcal/$):', coef(ref), se(ref), '\n')
stopifnot(abs(coef(ref) - headline_estimate()) < 1e-6)


# 1. Group-specific 2SLS (fully interacted: quarter x group FE) ----------------

d[, burden_q := factor(burden_q, labels = paste0('Q', 1:5))]
d[, snap_f := factor(
    fifelse(snap_b, 'SNAP-eligible (proxy)', 'Not eligible'),
    levels = c('SNAP-eligible (proxy)', 'Not eligible')
)]
res <- rbindlist(
    list(
        het_grp(d, 'fpl_bin'),
        het_grp(d, 'snap_f'),
        het_grp(d, 'burden_q')
    )
)
print(res)
stopifnot(all(res$fs_F > 1000)) # weak-IV check by group (f1 F ~ 197k)

# Wald tests that effects are equal across groups, with Holm correction within
# the family (3 moderators x 5 outcomes = 15 tests)
wt <- unique(res[, .(moderator, outcome, wald_F, wald_p)])
wt[, p_holm := p.adjust(wald_p, 'holm')]
print(wt)


# 2. Continuous gradient in FPL rank (pre-specified summary test) --------------

# FE: quarter x FPL bin, so that aggregate shocks hitting particular income
# bands (such as the Great Recession hitting the poor harder) cannot load on the
# z x rank term

gr <- het_grad(d, 'fpl_rank', fe = 'yq^fpl_bin')
gr[, p_holm := p.adjust(p_int, 'holm')]
gr[, spec := 'Unweighted (primary)']
grw <- het_grad(
    d,
    'fpl_rank',
    fe = 'yq^fpl_bin',
    weights = 'projection_factor'
)
grw[, `:=`(p_holm = p.adjust(p_int, 'holm'), spec = 'Projection-weighted')]
grq <- het_grad(d, 'fpl_rank', fe = 'yq')
grq[, `:=`(p_holm = p.adjust(p_int, 'holm'), spec = 'Quarter FE only')]
ga <- rbind(gr, grw, grq)
print(ga)

# The implied effect at the 5th and 95th FPL percentiles (plug in rank .05 and
# .95)
med_r <- median(d$fpl_rank)
gr[, `:=`(
    b_p05 = b_med + b_int * (.05 - med_r),
    b_p95 = b_med + b_int * (.95 - med_r)
)]
print(gr[, .(outcome, b_p05, b_p95, b_int, se_int, p_int, p_holm)])

# An equivalence-style statement for the poorest band: the share of the price
# rise that reaches calories bought, with its lower 95% bound
pb <- res[moderator == 'fpl_bin' & group == '<=100%']
pb[, `:=`(lo = b - 1.96 * se, hi = b + 1.96 * se)]
print(pb[, .(outcome, b, se, lo, hi, n, fs_F)])


# 2b. Room to move ------------------------------------------------------------

# Is the composition gradient really a baseline-level gradient?

base_of <- c(d_ed = 'kcal_100g_food_b', d_hs = 'health_score_b')
gb <- rbindlist(
    lapply(
        names(base_of),
        function(y) {
            dd <- d[!is.na(fpl_rank) & !is.na(get(base_of[[y]]))]
            dd[, `:=`(
                mc = fpl_rank - median(fpl_rank),
                bc = get(base_of[[y]]) - median(get(base_of[[y]]))
            )]
            dd[, `:=`(
                x_m = infl_exposure * mc,
                z_m = z * mc,
                x_b = infl_exposure * bc,
                z_b = z * bc
            )]
            m <- feols(
                as.formula(
                    sprintf(
                        paste0(
                            '%s ~ mc + bc | yq^fpl_bin | ',
                            'infl_exposure + x_m + x_b ~ z + z_m + z_b'
                        ),
                        y
                    )
                ),
                dd,
                cluster = ~state,
                notes = FALSE
            )
            ct <- coeftable(m)
            data.table(
                outcome = y,
                b_int = ct['fit_x_m', 1],
                se_int = ct['fit_x_m', 2],
                p_int = 2 * pt(
                    abs(ct['fit_x_m', 3]),
                    HET_G - 1,
                    lower.tail = FALSE
                ),
                b_base = ct['fit_x_b', 1],
                se_base = ct['fit_x_b', 2],
                n = nobs(m)
            )
        }
    )
)
gb <- merge(
    gb,
    gr[
        outcome %in% names(base_of),
        .(
            outcome,
            b_int0 = b_int,
            se_int0 = se_int,
            p_int0 = p_int
        )
    ],
    by = 'outcome'
)
cat('\nFPL-rank gradient with and without base-level x T2 controls:\n')
print(gb, digits = 6)


# 2c. Top-coded incomes --------------------------------------------------------

# NielsenIQ's top income category changes across panel years (the top-coded base
# share is ~9% in 2005-06, ~1% in 2007-10 and 14-23% in 2011-21), so the FPL
# rank and the top FPL bins are measured differently by era.

cat('\nbase income top-coded share by year:\n')
print(d[, .(topcoded = mean(topcode_b, na.rm = TRUE)), by = year][order(year)])
dn <- d[topcode_b == FALSE]
cat('sample without top-coded base income:', nrow(dn), '\n')
rt <- het_grp(dn, 'fpl_bin')
grt <- het_grad(dn, 'fpl_rank', fe = 'yq^fpl_bin')
grt[, p_holm := p.adjust(p_int, 'holm')]
cat('FPL-bin effects, no top-coded households:\n')
print(rt)
cat('FPL-rank gradient, no top-coded households:\n')
print(grt)
tc <- merge(
    gr[, .(outcome, b0 = b_int, se0 = se_int, p0 = p_int)],
    grt[, .(outcome, b1 = b_int, se1 = se_int, p1 = p_int)],
    by = 'outcome'
)
rm(dn)

save_results(
    '09_heterogeneity',
    res = res,
    wt = wt,
    ga = ga,
    gr = gr,
    pb = pb,
    gb = gb,
    rt = rt,
    grt = grt,
    tc = tc,
    fpl_levels = levels(d$fpl_bin)
)
print(sessionInfo())
