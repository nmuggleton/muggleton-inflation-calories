# The calorie-price ladder: were cheaper calories available on the shelf, and
# did local inflation make them relatively dearer? Each household's base basket
# is compared with its state's ladder of price per calorie within the same
# product modules (built by data_prep/09_analysis_dataset/01_ladder.sh).

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
set.seed(20261001)
setFixest_nthreads(4)
int <- file.path(wp2_paths()$data, 'intermediate')


# 0. Estimation sample with ladder positions -----------------------------------

lad <- c(
    'b_spend_lab', 'b_spend', 'b_kcal', 'sav25', 'sav50', 'cost_cheap',
    'own_sugar', 'cf_sugar', 'own_satfat', 'cf_satfat', 'own_sodium',
    'cf_sodium', 'own_fiber', 'cf_fiber', 'own_protein', 'cf_protein',
    'own_kcal_g', 'own_grams', 'cf_grams', 'f_cheap', 'f_all',
    'f_prem', 'f_cheap_own', 'f_prem_own', 'fx_spend', 'f_cheap_b2',
    'f_prem_b2', 'e_own', 'n_e_items', 'kcal_cheap1', 'kcal_tier1',
    'kcal_cheap0', 'kcal_tier0', 'kcal_prem1', 'kcal_prem0', 'spend1',
    'kcal1', 'spend0', 'kcal0', 'dec_w', 'dec_m', 'dec_l',
    'n_mod_both', 'dec_t'
)
d <- f1_gate(
    c(
        'income_cat', 'projection_factor', 'n_base_items', 'd_lkpu',
        'd_lspend', 'd_lkcal', 'd_ed', 'd_hs', lad
    )
)
cat(
    'f1 rows',
    nrow(d),
    '| with shelf metrics',
    sum(!is.na(d$b_spend)),
    '\n'
)

# The constructed shelf variables (shares in %, changes in pp or log points
# x100)
d[, `:=`(
    sh_valid = b_spend / b_spend_lab,
    sav25_pct = 100 * sav25 / b_spend,
    sav50_pct = 100 * sav50 / b_spend,
    cheap_net_pct = 100 * (1 - cost_cheap / b_spend),
    d_cheap = 100 * (kcal_cheap1 / kcal_tier1 - kcal_cheap0 / kcal_tier0),
    d_prem = 100 * (kcal_prem1 / kcal_tier1 - kcal_prem0 / kcal_tier0),
    dec_mix = dec_t - dec_w,
    f_rel = f_cheap - f_prem,
    f_rel_b2 = f_cheap_b2 - f_prem_b2,
    f_rel_own = f_cheap_own - f_prem_own,
    ed_own = 100 * own_kcal_g / own_grams,
    ed_cf = 100 * own_kcal_g / cf_grams
)]
for (v in c('sugar', 'satfat', 'sodium', 'fiber', 'protein')) {
    d[, (paste0(v, '_own')) := 1000 * get(paste0('own_', v)) / b_kcal]
    d[, (paste0(v, '_cf')) := 1000 * get(paste0('cf_', v)) / b_kcal]
}
# These are flags, not drops: the shelf metrics need a base basket whose valid
# ladders cover most of its labelled spend, and outliers in the index are
# flagged
d[, f_shelf := !is.na(b_spend) & b_spend > 0 & sh_valid >= .5]
# I require |index| < 50 pp; household T2 itself stays within about +-20 pp
# (see src/data_prep/07_controls/03_sample_panel.sql)
d[
    ,
    f_idx := f_shelf & !is.na(f_cheap) & !is.na(e_own) &
        abs(f_cheap) < 50 & abs(f_prem) < 50 & abs(e_own) < 50
]
d[
    ,
    f_idx2 := f_idx & !is.na(f_rel_b2) & abs(f_cheap_b2) < 50 &
        abs(f_prem_b2) < 50
]
d[, f_dec := f_shelf & !is.na(dec_w) & !is.na(dec_t) & n_mod_both >= 5]
d[, f_tier := f_shelf & kcal_tier1 > 0 & kcal_tier0 > 0]
flow <- data.table(
    step = c(
        'f1 gate sample with instrument', 'shelf metrics (base basket)',
        'valid-ladder share of labeled spend >= .5',
        'tier price indices defined, |index| < 50',
        'decomposition (>= 5 modules both quarters)',
        'tier calorie shares both quarters'
    ),
    hh_quarters = c(
        nrow(d), sum(!is.na(d$b_spend)), sum(d$f_shelf),
        sum(d$f_idx), sum(d$f_dec), sum(d$f_tier)
    ),
    households = c(
        uniqueN(d$household_code),
        uniqueN(d[!is.na(b_spend), household_code]),
        uniqueN(d[(f_shelf), household_code]),
        uniqueN(d[(f_idx), household_code]),
        uniqueN(d[(f_dec), household_code]),
        uniqueN(d[(f_tier), household_code])
    )
)
print(flow)
# A shelf sample this thin would not represent f1 (set before the run; the f1
# gate already requires a labelled share of at least .5)
stopifnot(sum(d$f_shelf) / nrow(d) > .8)


# 1. The shelf: savings at fixed calories against a year's price rise ----------

s <- d[(f_shelf)]
wm <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])
shelf <- data.table(
    measure = c(
        'Valid-ladder share of labeled base spend (%)',
        'Savings buying module calories at state p25 (%)',
        'Savings buying module calories at state p50 (%)',
        'Net saving, all calories at cheap-tier mean $/kcal (%)',
        'Exposure inflation T2 (pp)',
        'T2 among households with T2 > 0 (pp)'
    ),
    mean = c(
        100 * mean(s$sh_valid), mean(s$sav25_pct), mean(s$sav50_pct),
        mean(s$cheap_net_pct, na.rm = TRUE), mean(s$infl_exposure),
        mean(s[infl_exposure > 0, infl_exposure])
    ),
    median = c(
        100 * median(s$sh_valid), median(s$sav25_pct),
        median(s$sav50_pct), median(s$cheap_net_pct, na.rm = TRUE),
        median(s$infl_exposure),
        median(s[infl_exposure > 0, infl_exposure])
    ),
    wmean = c(
        100 * wm(s$sh_valid, s$projection_factor),
        wm(s$sav25_pct, s$projection_factor),
        wm(s$sav50_pct, s$projection_factor),
        wm(s$cheap_net_pct, s$projection_factor),
        wm(s$infl_exposure, s$projection_factor),
        s[infl_exposure > 0, wm(infl_exposure, projection_factor)]
    )
)
print(shelf, digits = 6)
# Shock-to-slack ratios: how much of the slack at the 25th percentile a 1 pp
# shock (or the household's own positive shock) would use up
cat(
    'share of f1 household-quarters with sav25 > own T2 (T2 > 0):',
    s[infl_exposure > 0, mean(sav25_pct > infl_exposure)],
    '\n'
)
cat(
    'calories to move to the p25 rung to offset 1 pp, % (median):',
    median(100 / s[sav25_pct > 0, sav25_pct]),
    '\n'
)
# How much of the price rise households avoid (1 + the calories-per-dollar
# response from 01_main), set against the saving on offer at the p25 rung of the
# ladder
offset <- 1 + load_results('01_main')$headline[outcome == 'd_lkpu', b_iv]
say(
    'share of the rise avoided: %.3f | p25 saving on offer: %.1f percent',
    offset,
    mean(s$sav25_pct)
)
s[, inc := factor(
    income_cat,
    levels = c(
        '<25k', '25-50k', '50-70k',
        '70-100k', '100k+'
    )
)]
stopifnot(!anyNA(s$inc))
print(
    s[, .(
        n = .N,
        sav25 = mean(sav25_pct),
        sav50 = mean(sav50_pct),
        cheap_net = mean(cheap_net_pct, na.rm = TRUE)
    ), keyby = inc]
)


# 2. The main design on ladder outcomes ----------------------------------------

# Did the cheap rungs inflate faster, and did households move down the ladder?
lab <- c(
    f_cheap = 'Cheap-tier $/kcal inflation (pp)',
    f_all = 'Module $/kcal inflation, all tiers (pp)',
    f_prem = 'Premium-tier $/kcal inflation (pp)',
    f_rel = 'Cheap minus premium tier inflation (pp)',
    f_rel_b2 = 'Cheap minus premium, tiers ranked at t-8 (pp)',
    f_rel_own = 'Cheap minus premium, own-quarter tiers (pp)',
    e_own = 'Own base items $/kcal inflation (pp)',
    d_cheap = 'Change cheap-tier kcal share (pp)',
    d_prem = 'Change premium-tier kcal share (pp)',
    dec_t = 'Change log $/kcal, labeled (x100)',
    dec_m = 'Market ladder: module p50 (x100)',
    dec_l = 'Own rung relative to p50 (x100)',
    dec_mix = 'Between-module mix and entry (x100)',
    d_lkpu = 'Change log kcal per real $ (x100)'
)
setFixest_dict(
    c(
        lab,
        infl_exposure = 'Exposure inflation T2 (pp)',
        fit_infl_exposure = 'Exposure inflation T2 (pp)',
        yq = 'Quarter'
    )
)
# Lean fits: coefficients, SEs, N and the first-stage F are all etable() needs
# with fitstat = ~ n + ivf1, at a fraction of the memory
ivm <- function(y, dat) {
    feols(
        as.formula(
            sprintf(
                '%s ~ 1 | yq | infl_exposure ~ z',
                y
            )
        ),
        dat,
        cluster = ~state,
        lean = TRUE
    )
}
m_idx <- lapply(
    c('f_cheap', 'f_all', 'f_prem', 'f_rel', 'e_own'),
    ivm,
    dat = d[(f_idx)]
)
# Checks on the tier definition: tiers ranked at t-8 (independent of noise in
# the base quarter) and the mean-reverting own-quarter ranking (for reference
# only)
m_chk <- lapply(c('f_rel_b2', 'f_rel_own'), ivm, dat = d[(f_idx2)])
m_dec <- lapply(
    c('dec_t', 'dec_m', 'dec_l', 'dec_mix', 'd_lkpu'),
    ivm,
    dat = d[(f_dec)]
)
m_tier <- lapply(c('d_cheap', 'd_prem'), ivm, dat = d[(f_tier)])
allm <- c(m_idx, m_chk, m_dec, m_tier)
cat('mean tier inflation, f_idx2 sample (lso / t-8 / own-quarter):\n')
print(
    d[(f_idx2), lapply(.SD, mean), .SDcols = c(
        'f_cheap', 'f_prem',
        'f_cheap_b2', 'f_prem_b2', 'f_cheap_own', 'f_prem_own', 'e_own',
        'infl_exposure'
    )]
)
print(etable(allm, digits = 4, fitstat = ~ n + ivf1))
ivt <- rbindlist(
    lapply(
        allm,
        function(m) {
            data.table(
                outcome = as.character(m$fml[[2]]),
                b = coef(m)[[1]],
                se = se(m)[[1]],
                n = nobs(m),
                f = fitstat(m, ~ivf1)[[1]]$stat
            )
        }
    )
)
ivt[, p := 2 * pnorm(-abs(b / se))]
print(ivt, digits = 6)
# The decomposition is exact by construction within the f_dec sample
stopifnot(
    abs(
        ivt[outcome == 'dec_t', b] - ivt[outcome == 'dec_m', b] -
            ivt[outcome == 'dec_l', b] - ivt[outcome == 'dec_mix', b]
    )
    < 1e-6
)
cat(
    'labeled dln $/kcal vs -d_lkpu (f_dec):',
    ivt[outcome == 'dec_t', b],
    'vs',
    -ivt[outcome == 'd_lkpu', b],
    '\n'
)
sty <- style.tex(
    'aer',
    tablefoot = FALSE,
    tabular = '*',
    yesNo = c('Yes', '')
)
ladder_iv_tex <- etable(
    m_idx,
    tex = TRUE,
    style.tex = sty,
    signif.code = c('***' = .001, '**' = .01, '*' = .05),
    fitstat = ~ n + ivf1,
    fixef_sizes = FALSE,
    digits = 3,
    digits.stats = 3,
    postprocess.tex = tex_fit_apa
)
ladder_dec_tex <- etable(
    c(m_dec[1:4], m_tier),
    tex = TRUE,
    style.tex = sty,
    signif.code = c('***' = .001, '**' = .01, '*' = .05),
    fitstat = ~ n + ivf1,
    fixef_sizes = FALSE,
    digits = 3,
    digits.stats = 3,
    postprocess.tex = tex_fit_apa
)
rm(allm, m_idx, m_chk, m_dec, m_tier)
invisible(gc())


# 3. Response by the room there was to save ------------------------------------

# Households split into thirds by the p50 saving in their base basket (set at
# t-4, so before the shock), ranked within each quarter
d[
    (f_shelf),
    slack3 := cut(
        frank(sav50_pct, ties.method = 'first') / .N,
        c(0, 1 / 3, 2 / 3, 1),
        labels = c('T1', 'T2', 'T3')
    ),
    by = yq
]
print(
    d[(f_shelf), .(
        n = .N,
        sav50 = mean(sav50_pct),
        t2 = mean(infl_exposure)
    ), keyby = slack3]
)
# A narrow copy of the data for the slack fits: the full frame carries about 60
# ladder columns, and one copy per fit exhausts a 16 GB machine
invisible(gc())
ds <- d[(f_shelf), .(
    yq,
    state,
    z,
    infl_exposure,
    d_lkpu,
    d_lspend,
    d_lkcal,
    slack3
)]
het <- rbindlist(
    lapply(
        c('d_lkpu', 'd_lspend', 'd_lkcal'),
        function(y) {
            rbindlist(
                lapply(
                    c('T1', 'T2', 'T3'),
                    function(k) {
                        m <- feols(
                            as.formula(
                                sprintf(
                                    '%s ~ 1 | yq | infl_exposure ~ z',
                                    y
                                )
                            ),
                            ds[slack3 == k],
                            cluster = ~state,
                            lean = TRUE
                        )
                        data.table(
                            outcome = y,
                            slack3 = k,
                            b = coef(m)[[1]],
                            se = se(m)[[1]],
                            n = nobs(m),
                            f = fitstat(m, ~ivf1)[[1]]$stat
                        )
                    }
                )
            )
        }
    )
)
print(het, digits = 6)
# Pooled interaction test (T3 vs T1) for calories per dollar
ds[, `:=`(
    zt3 = z * (slack3 == 'T3'),
    xt3 = infl_exposure *
        (slack3 == 'T3'),
    slq = paste(slack3, yq)
)]
hm3 <- feols(
    d_lkpu ~ 1 | slq | infl_exposure + xt3 ~ z + zt3,
    ds[slack3 %in% c('T1', 'T3')],
    cluster = ~state
)
print(summary(hm3))
rm(ds)
invisible(gc())
cat(
    'T3 - T1 difference in kcal/$ pass-through:',
    coef(hm3)[['fit_xt3']],
    'SE',
    se(hm3)[['fit_xt3']],
    '\n'
)


# 4. Figure 4 data: the distribution of savings --------------------------------

# Densities of the p25 and p50 savings on a grid, as geom_density would draw
# them (savings up to 80 percent, bandwidth bw.nrd0 x 1.2), plus counts in
# 1-point bins
mt2 <- mean(s$infl_exposure)
dd <- rbind(
    data.table(x = s$sav50_pct, k = 'State median price per calorie'),
    data.table(
        x = s$sav25_pct,
        k = 'State 25th percentile price per calorie'
    )
)
dd[, k := factor(k, levels = unique(k))]
dd <- dd[x <= 80]
dens <- dd[
    ,
    {
        f <- density(x, adjust = 1.2, n = 512, from = min(x), to = max(x))
        .(x = f$x, density = f$y, bw = f$bw, n = .N)
    },
    by = .(series = k)
]
hist <- dd[, .(count = .N), keyby = .(
    series = k,
    bin_lo = pmax(0, floor(x))
)]
hist[, `:=`(bin_hi = bin_lo + 1, n = sum(count)), by = series]
hist[, `:=`(share = count / n, mean_shock = mt2)]
rm(dd)


# 5. National calorie cheapflation (SI Fig. S4) --------------------------------

# Spend-weighted mean of state x module fixed-set tier inflation, by quarter:
# the cheap, middle and premium tiers, and cheap and premium with tiers ranked
# two years earlier (t-8)
tier_sql <- "
    SELECT
        t,
        sum(spend0 * l_cheap) / sum(spend0) AS cheap,
        sum(spend0 * l_mid) / sum(spend0) AS mid,
        sum(spend0 * l_prem) / sum(spend0) AS prem,
        sum(spend0 * l_cheap_b2) FILTER (l_prem_b2 IS NOT NULL)
            / sum(spend0) FILTER (
                l_cheap_b2 IS NOT NULL AND l_prem_b2 IS NOT NULL
            ) AS cheap_b2,
        sum(spend0 * l_prem_b2) FILTER (l_cheap_b2 IS NOT NULL)
            / sum(spend0) FILTER (
                l_cheap_b2 IS NOT NULL AND l_prem_b2 IS NOT NULL
            ) AS prem_b2,
        count(*) AS n
    FROM read_parquet('%s')
    WHERE l_cheap IS NOT NULL AND l_mid IS NOT NULL AND l_prem IS NOT NULL
    GROUP BY t
    ORDER BY t
"
tr <- f1_sql(sprintf(tier_sql, file.path(int, 'ladder_tier', '*.parquet')))
tr[, date := as.Date(sprintf('%d-%02d-15', t %/% 10, 3 * (t %% 10) - 1))]
print(
    tr[, .(
        t,
        cheap = 100 * (cheap - 1),
        mid = 100 * (mid - 1),
        prem = 100 * (prem - 1),
        cheap_b2 = 100 * (cheap_b2 - 1),
        prem_b2 = 100 * (prem_b2 - 1),
        n
    )],
    digits = 4
)
cat(
    'mean cheap - premium, t-8 tiers (pp):',
    100 * mean(tr$cheap_b2 - tr$prem_b2, na.rm = TRUE),
    '\n'
)
cat(
    'mean cheap - premium tier inflation (pp):',
    100 * mean(tr$cheap - tr$prem),
    '| corr with mean tier inflation:',
    cor(tr$cheap - tr$prem, (tr$cheap + tr$mid + tr$prem) / 3),
    '\n'
)


# Save -------------------------------------------------------------------------

save_results(
    '08_mechanism',
    flow = flow,
    shelf = shelf,
    ivt = ivt,
    ladder_iv_tex = ladder_iv_tex,
    ladder_dec_tex = ladder_dec_tex,
    het = het,
    slack_t3_t1 = data.table(
        b = coef(hm3)[['fit_xt3']],
        se = se(hm3)[['fit_xt3']]
    ),
    dens = dens,
    hist = hist,
    mean_shock = mt2,
    tr = tr
)
print(sessionInfo())
