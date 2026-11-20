# Robustness of the headline to the sample, the weights, the controls and the
# method of inference.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
set.seed(20261001)
setFixest_nthreads(4)

y5 <- c('d_lkpu', 'd_lkcal', 'd_lspend', 'd_ed', 'd_hs100')
lab5 <- c(
    d_lkpu = 'kcal per $', d_lkcal = 'kcal', d_lspend = 'Spend',
    d_ed = 'Energy density', d_hs100 = 'Health score x100'
)
d <- f1_read(
    c(
        'household_code', 'yq', 'year', 'state', 'region_code', 'income_cat',
        'projection_factor', 'household_size', 'household_size_b', 'head_age',
        'head_college', 'race_f', 'hispanic', 'children_present',
        'first_panel_year', 'n_years_in_panel', 'in_next_year', 'bal_2007_2012',
        'core_a', 'core_b', 'partial_a', 'partial_b', 'spend_outlier_ab',
        'cov_lab', 'cov_lab_b', 'kcal_per_usd_real', 'kpu_b', 'n_base_items',
        'spend_base', 'infl_exposure', 'base_lkpu', 'base_lspend', 'base_lkcal',
        'd_lkpu', 'd_lkcal', 'd_lspend', 'd_ed', 'd_hs'
    )
)
d[, projection_factor := as.numeric(projection_factor)]
d[, d_hs100 := 100 * d_hs]
d[, sq := paste(state, yq)]
# Census division of each state (Census Bureau regions and divisions)
division_of_state <- c(
    `9` = 1, `23` = 1, `25` = 1, `33` = 1, `44` = 1, `50` = 1,
    `34` = 2, `36` = 2, `42` = 2,
    `17` = 3, `18` = 3, `26` = 3, `39` = 3, `55` = 3,
    `19` = 4, `20` = 4, `27` = 4, `29` = 4, `31` = 4, `38` = 4,
    `46` = 4,
    `10` = 5, `11` = 5, `12` = 5, `13` = 5, `24` = 5, `37` = 5,
    `45` = 5, `51` = 5, `54` = 5,
    `1` = 6, `21` = 6, `28` = 6, `47` = 6,
    `5` = 7, `22` = 7, `40` = 7, `48` = 7,
    `4` = 8, `8` = 8, `16` = 8, `30` = 8, `32` = 8, `35` = 8,
    `49` = 8, `56` = 8,
    `2` = 9, `6` = 9, `15` = 9, `41` = 9, `53` = 9
)
d[, division := unname(division_of_state[as.character(as.integer(state))])]
stopifnot(!anyNA(d$division))
d[, `:=`(
    div_q = paste(division, yq),
    state_y = paste(state, year),
    age_bin = cut(head_age, c(-Inf, 35, 45, 55, 65, Inf)),
    size_bin = pmin(household_size, 5L),
    l_items = log(n_base_items)
)]
d[, gate := ms_gate(d)]
stopifnot(sum(d$gate) == F1_N)
cat('panel rows', nrow(d), '| gate', sum(d$gate), '\n')

# The instrument is rebuilt within each sample as a leave-one-out mean: the
# average exposure of the sample's other households in the state-quarter
g <- copy(d[gate == TRUE])
g[, z := loo_mean(infl_exposure, sq)]
g <- g[is.finite(z)]
stopifnot(nrow(g) == F1_N)


# A. Gate thresholds and sample path -------------------------------------------

# Logical vectors that select rows from the panel (the samples are built one at
# a time to keep memory bounded)
G <- function(...) ms_gate(d, ...)
gt <- d$gate
A <- list(
    `Headline gate` = gt,
    `Include t in 2020` = G(y2020 = TRUE),
    `Exclude t in 2021 (base in 2020)` = gt & d$year != 2021,
    `Coverage floor .3` = G(cov = 0.3),
    `Coverage floor .4` = G(cov = 0.4),
    `Coverage floor .6` = G(cov = 0.6),
    `Coverage floor .7` = G(cov = 0.7),
    `Coverage floor .8` = G(cov = 0.8),
    `Base basket 25+ items` = G(items = 25),
    `Base basket 50+ items` = G(items = 50),
    `Base basket 100+ items` = G(items = 100),
    `Core in t only` = G(core = 't'),
    `No core requirement` = G(core = 'any'),
    `Allow partial quarters` = G(partial = TRUE),
    `Same household size t and t-4` =
        gt & d$household_size == d$household_size_b,
    `Exclude spend outliers` = gt & !d$spend_outlier_ab,
    `Base quarter not in first panel year` =
        gt & d$year - 1 > d$first_panel_year,
    `Households with 8+ panel years` = gt & d$n_years_in_panel >= 8,
    `Balanced 2007-2012 panel, t in 2008-2012` =
        gt & fcoalesce(d$bal_2007_2012, FALSE) & d$year %between% c(2008, 2012),
    `2005-2010` = gt & d$year <= 2010,
    `2011-2015` = gt & d$year %between% c(2011, 2015),
    late = gt & d$year >= 2016
)
names(A)[names(A) == 'late'] <- sprintf('2016-%d', END_YEAR)
ra <- rbindlist(
    lapply(
        names(A),
        function(k) {
            sel <- which(fcoalesce(A[[k]], FALSE))
            cat('A:', k, length(sel), '\n')
            if (k == 'Headline gate') {
                return(ms_iv(g, y5, label = k))
            }
            dd <- copy(d[sel])
            dd[, z := loo_mean(infl_exposure, sq)]
            ms_iv(dd[is.finite(z)], y5, label = k)
        }
    )
)
ra[, block := 'Sample and thresholds']
rm(A)
invisible(gc())


# B. Weights -------------------------------------------------------------------

g[, w_eq := 1 / .N, by = sq]
# Inverse probability of entering the gate, from cell means over every T2-valid
# household-quarter with a base quarter (excluding t = 2020).
pop <- d[year != 2020]
pop[, cell := paste(
    income_cat,
    year,
    age_bin,
    size_bin,
    children_present,
    division
)]
ps <- pop[, .(p_gate = mean(gate), n_cell = .N), by = cell]
g[, cell := paste(
    income_cat,
    year,
    age_bin,
    size_bin,
    children_present,
    division
)]
g[ps, `:=`(p_gate = i.p_gate, n_cell = i.n_cell), on = 'cell']
stopifnot(!anyNA(g$p_gate), all(g$p_gate > 0))
cat(
    'gate propensity: p1/p50/p99',
    quantile(g$p_gate, c(.01, .5, .99)),
    '| cells',
    nrow(ps),
    '\n'
)
g[, w_ipw := projection_factor / p_gate]
bal <- rbind(
    pop[, .(
        sample = 'All T2-valid, projection wt',
        income_lt25k = weighted.mean(
            income_cat == '<25k',
            projection_factor
        ),
        college = weighted.mean(
            head_college,
            projection_factor,
            na.rm = TRUE
        ),
        children = weighted.mean(
            children_present,
            projection_factor,
            na.rm = TRUE
        ),
        age = weighted.mean(head_age, projection_factor, na.rm = TRUE),
        size = weighted.mean(household_size, projection_factor)
    )],
    g[, .(
        sample = 'Gate, projection wt',
        income_lt25k = weighted.mean(
            income_cat == '<25k',
            projection_factor
        ),
        college = weighted.mean(
            head_college,
            projection_factor,
            na.rm = TRUE
        ),
        children = weighted.mean(
            children_present,
            projection_factor,
            na.rm = TRUE
        ),
        age = weighted.mean(head_age, projection_factor, na.rm = TRUE),
        size = weighted.mean(household_size, projection_factor)
    )],
    g[, .(
        sample = 'Gate, IPW',
        income_lt25k = weighted.mean(income_cat == '<25k', w_ipw),
        college = weighted.mean(head_college, w_ipw, na.rm = TRUE),
        children = weighted.mean(children_present, w_ipw, na.rm = TRUE),
        age = weighted.mean(head_age, w_ipw, na.rm = TRUE),
        size = weighted.mean(household_size, w_ipw)
    )]
)
cat('income categories:', sort(unique(g$income_cat)), '\n')
print(bal)
rm(pop)
invisible(gc())
rb <- rbind(
    ms_iv(g, y5, w = 'projection_factor', label = 'Projection weights'),
    ms_iv(
        g,
        y5,
        w = 'spend_base',
        label = 'Base-basket expenditure weights'
    ),
    ms_iv(g, y5, w = 'w_eq', label = 'Equal weight per state-quarter'),
    ms_iv(
        g,
        y5,
        w = 'w_ipw',
        label = 'Inverse probability of gate x projection'
    )
)
rb[, block := 'Weights']


# C. Controls and fixed effects ------------------------------------------------

gc_ <- g[
    is.finite(base_lkpu) & is.finite(base_lspend) &
        is.finite(base_lkcal)
]
rc <- rbind(
    ms_iv(
        g,
        y5,
        fe = paste(
            'yq + income_cat + age_bin + size_bin +',
            'head_college + race_f + children_present'
        ),
        label = 'Demographic FE'
    ),
    ms_iv(
        gc_,
        y5,
        ctrl = c('base_lkpu', 'base_lspend', 'base_lkcal'),
        label = 'Base-quarter outcome levels'
    ),
    ms_iv(
        g,
        y5,
        ctrl = c('cov_lab_b', 'l_items'),
        label = 'Base coverage and log basket size'
    ),
    ms_iv(g, y5, fe = 'yq + state', label = 'State FE (state trends)'),
    ms_iv(
        g,
        y5,
        fe = 'div_q + state',
        label = 'State FE + census division x quarter FE'
    )
)
rc[, block := 'Controls']


# D. Inference -----------------------------------------------------------------

rd <- rbindlist(
    list(
        ms_iv(g, y5, label = 'Cluster: state')[, cl := 'state'],
        ms_iv(
            g,
            y5,
            cl = 'division',
            label = 'Cluster: census division'
        )[, cl := 'division'],
        ms_iv(
            g,
            y5,
            cl = 'state_y',
            label = 'Cluster: state x year'
        )[, cl := 'state_y']
    )
)
tw <- feols(
    as.formula(
        sprintf(
            'c(%s) ~ 1 | yq | infl_exposure ~ z',
            paste(y5, collapse = ', ')
        )
    ),
    g,
    cluster = ~ state + yq,
    notes = FALSE
)
rd <- rbind(
    rd,
    data.table(
        spec = 'Cluster: state and quarter',
        outcome = y5,
        b = sapply(tw, function(m) coef(m)[[1]]),
        se = sapply(tw, function(m) se(m)[[1]]),
        n = sapply(tw, nobs),
        n_hh = NA_integer_,
        n_cl = NA_integer_,
        f = NA_real_,
        cl = 'state+yq'
    )
)
print(rd)
wcr <- list()
for (v in y5) {
    h <- rd[cl == 'state' & outcome == v]
    for (clv in c('state', 'division')) {
        span <- if (clv == 'state') 8 else 30
        grid <- seq(h$b - span * h$se, h$b + span * h$se, length.out = 401)
        if (v == 'd_lkpu') grid <- sort(unique(c(grid, -1, 0)))
        if (v != 'd_lkpu') grid <- sort(unique(c(grid, 0)))
        r <- ms_wcr_ar(
            g[[v]],
            g$infl_exposure,
            g$z,
            g$yq,
            g[[clv]],
            grid,
            B = 9999,
            seed = 20261001
        )
        wcr[[paste(v, clv)]] <- data.table(
            outcome = v,
            cl = clv,
            G = r$G,
            ci_lo = r$ci[1],
            ci_hi = r$ci[2],
            edge = r$edge,
            p0 = r$p[b0 == 0, p],
            pm1 = if (v == 'd_lkpu') r$p[b0 == -1, p] else NA_real_
        )
        cat(
            'WCR-AR',
            v,
            clv,
            'G =',
            r$G,
            '| 95% CI',
            r$ci,
            '| p(b=0)',
            r$p[b0 == 0, p],
            if (v == 'd_lkpu') {
                paste('| p(b=-1)', r$p[b0 == -1, p])
            },
            '| edge',
            r$edge,
            '\n'
        )
    }
}
wcr <- rbindlist(wcr)
print(wcr)


# E. Attrition: does exposure predict leaving the panel next year? -------------

# Note that in_next_year is undefined for t in 2021 (the last panel year), and
# 2020 is not in the gate, so the attrition sample is t <= 2019
ge <- g[year <= 2019]
ge[, exit_next := 100 * as.numeric(!fcoalesce(in_next_year, FALSE))]
cat(
    'exit rate (pct of household-quarters, t <= 2019):',
    mean(ge$exit_next),
    '\n'
)
re <- ms_iv(ge, 'exit_next', label = 'Exit next year (x100), t <= 2019')
rm(ge)
print(re)


# F. By year -------------------------------------------------------------------

ry <- rbindlist(
    lapply(
        sort(unique(g$year)),
        function(yr) {
            ms_iv(
                g[year == yr],
                c('d_lkpu', 'd_lkcal', 'd_lspend'),
                label = as.character(yr)
            )
        }
    )
)
print(ry)

res <- rbind(
    ra,
    rb,
    rc,
    copy(rd)[, cl := NULL][, block := 'Inference'],
    re[, block := 'Attrition'],
    ry[, block := 'By year'],
    fill = TRUE
)
cat(
    'range of kcal/$ 2SLS over A-C:',
    range(rbind(ra, rb, rc)[outcome == 'd_lkpu', b]),
    '\n'
)
cat(
    'range of kcal 2SLS over A-C:',
    range(rbind(ra, rb, rc)[outcome == 'd_lkcal', b]),
    '\n'
)
cat(
    'range of spend 2SLS over A-C:',
    range(rbind(ra, rb, rc)[outcome == 'd_lspend', b]),
    '\n'
)

save_results(
    '13_sample',
    ra = ra,
    rb = rb,
    rc = rc,
    rd = rd,
    wcr = wcr,
    re = re,
    ry = ry,
    bal = bal,
    lab5 = lab5,
    y5 = y5
)
print(sessionInfo())
