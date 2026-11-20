# The "ladder of reach" on panel-observed shelves: how much a household could
# save on its base basket by buying each module's calories lower down the shelf,
# as the shelf widens from the retailer where the calories were bought to the
# household's own retailers, then all stores in its 3-digit zip code, its
# county, and its state. Unlike a ladder built on Retail Scanner shelves, it
# covers every store type and store brands.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
int <- file.path(wp2_paths()$data, 'intermediate')


# 0. f1 sample joined to the panel-shelf rungs (no drops: flags) ---------------

d <- f1_gate(
    c(
        'projection_factor', 'income_cat', 'inc_real_b', 'fpl_bin',
        'b_spend', 'sav25', 'sav50'
    )
)
r <- wp2_sql(
    sprintf(
        "SELECT * EXCLUDE (state) FROM read_parquet('%s')",
        file.path(int, 'panel_shelf', 'hh', '*.parquet')
    )
)
stopifnot(!anyDuplicated(r, by = c('household_code', 'yq')))
d <- merge(d, r, by = c('household_code', 'yq'), all.x = TRUE)
stopifnot(nrow(d) == F1_N)

d[, sh_common := pb_spend / pb_spend_lab]
# Compared only where the common base makes up most of the labelled basket; the
# share is rounded before the cut-off, as est_gate does
d[, f_reach := !is.na(pb_spend) & pb_spend > 0 & round(sh_common, 9) >= .5]
d[, f_hs := !is.na(b_spend) & b_spend > 0]
flow <- data.table(
    step = c(
        'f1 gate sample', 'target year has a panel base year',
        'base basket with valid ladders at all eight rungs',
        'common base >= .5 of labeled base spend'
    ),
    hh_quarters = c(
        nrow(d), sum(!is.na(d$pb_spend_lab)),
        sum(d$pb_spend > 0, na.rm = TRUE), sum(d$f_reach)
    ),
    households = c(
        uniqueN(d$household_code),
        uniqueN(d[!is.na(pb_spend_lab), household_code]),
        uniqueN(d[pb_spend > 0, household_code]),
        uniqueN(d[(f_reach), household_code])
    )
)
print(flow)
by_year <- d[
    !is.na(pb_spend_lab),
    .(
        n = .N,
        sh_common = mean(sh_common, na.rm = TRUE),
        f_reach = mean(f_reach)
    ),
    keyby = year
]
print(by_year, digits = 3)


# 1. Savings by rung, % of the common-base spend -------------------------------

s <- d[(f_reach)]
# Two ladders on the same common base (07b): the pooled shelf, and the best
# single retailer (cross-fitted). They are not one nested ladder.
rungs <- data.table(
    ladder = rep(c('pooled', 'best retailer'), c(4, 4)),
    rung = c('ret', 'zip3', 'cty', 'st', 'ret', 'setx', 'bz3', 'bcty'),
    label = c(
        'Retailer where the calories were bought',
        "Everything bought in the household's zip3",
        "Everything bought in the household's county",
        "Everything bought in the household's state",
        'Retailer where the calories were bought',
        "Best of the household's own retailers",
        "Best retailer used in the household's zip3",
        "Best retailer used in the household's county"
    )
)
rungs <- rbind(
    rungs,
    data.table(
        ladder = 'best retailer',
        rung = 'bst',
        label = "Best retailer in the household's state"
    )
)
wm <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])
reach <- rbindlist(
    lapply(
        seq_len(nrow(rungs)),
        function(i) {
            rbindlist(
                lapply(
                    c(25, 50),
                    function(p) {
                        x <- 100 * s[[sprintf(
                            'sav%d_%s',
                            p,
                            rungs$rung[i]
                        )]] / s$pb_spend
                        data.table(
                            ladder = rungs$ladder[i],
                            rung = rungs$rung[i],
                            label = rungs$label[i],
                            quantile = p,
                            mean = mean(x),
                            median = median(x),
                            wmean = wm(x, s$projection_factor),
                            n = length(x)
                        )
                    }
                )
            )
        }
    )
)
# The panellist-price state shelf of Figure 4 (01_ladder.sh, item prices pooled
# across households), on the same household-quarters
ref <- s[(f_hs), {
    a <- 100 * sav25 / b_spend
    b <- 100 * sav50 / b_spend
    data.table(
        ladder = 'reference',
        rung = 'fig3_state',
        label = 'State, Figure 4 panellist-price shelf',
        quantile = c(25, 50),
        mean = c(mean(a), mean(b)),
        median = c(median(a), median(b)),
        wmean = c(wm(a, projection_factor), wm(b, projection_factor)),
        n = .N
    )
}]
reach <- rbind(reach, ref)
print(reach, digits = 4)

# The retailer rung as a share of each wider rung, for both ladders
kept <- s[, .(
    p25_zip3 = sum(sav25_ret) / sum(sav25_zip3),
    p25_cty = sum(sav25_ret) / sum(sav25_cty),
    p25_st = sum(sav25_ret) / sum(sav25_st),
    p50_st = sum(sav50_ret) / sum(sav50_st),
    p25_best_own = sum(sav25_ret) / sum(sav25_setx),
    p25_best_zip3 = sum(sav25_ret) / sum(sav25_bz3),
    p25_best_cty = sum(sav25_ret) / sum(sav25_bcty),
    p25_best_st = sum(sav25_ret) / sum(sav25_bst)
)]
print(kept, digits = 3)

s[, inc := factor(
    income_cat,
    levels = c(
        '<25k', '25-50k', '50-70k',
        '70-100k', '100k+'
    )
)]
stopifnot(!anyNA(s$inc))
by_inc <- s[, .(
    n = .N,
    retailer = 100 * mean(sav25_ret / pb_spend),
    zip3 = 100 * mean(sav25_zip3 / pb_spend),
    county = 100 * mean(sav25_cty / pb_spend),
    state = 100 * mean(sav25_st / pb_spend),
    best_own = 100 * mean(sav25_setx / pb_spend),
    best_zip3 = 100 * mean(sav25_bz3 / pb_spend),
    best_cty = 100 * mean(sav25_bcty / pb_spend),
    best_st = 100 * mean(sav25_bst / pb_spend),
    n_retailers = mean(n_retailers)
), keyby = inc]
print(by_inc, digits = 4)

# 95% CIs around the mean p25 saving, with SEs clustered by household_code (a
# household can appear up to 4 times a year, over many years)
cat('clusters (households):', uniqueN(s$household_code), '\n')
reach_ci <- rbindlist(
    lapply(
        c('ret', 'setx', 'zip3', 'cty', 'st', 'bz3', 'bcty', 'bst'),
        function(r) {
            s[, x := 100 * get(sprintf('sav25_%s', r)) / pb_spend]
            m <- feols(x ~ 1, data = s, cluster = ~household_code)
            data.table(rung = r, mean = coef(m)[[1]], se = se(m)[[1]])
        }
    )
)
reach_ci[, `:=`(lo = mean - 1.96 * se, hi = mean + 1.96 * se)]
# The point estimates should match the reach table
stopifnot(
    all.equal(
        reach_ci$mean,
        reach[quantile == 25 & ladder != 'reference'][
            match(reach_ci$rung, rung), mean
        ]
    )
)
print(reach_ci, digits = 4)

inc_ci <- rbindlist(
    lapply(
        c(retailer = 'ret', state = 'st', best_zip3 = 'bz3'),
        function(r) {
            s[, x := 100 * get(sprintf('sav25_%s', r)) / pb_spend]
            m <- feols(x ~ 0 + inc, data = s, cluster = ~household_code)
            data.table(
                rung = r,
                inc = sub('^inc', '', names(coef(m))),
                mean = unname(coef(m)),
                se = unname(se(m))
            )
        }
    ),
    idcol = 'series'
)
inc_ci[, `:=`(lo = mean - 1.96 * se, hi = mean + 1.96 * se)]
print(inc_ci, digits = 4)

# The same three rungs under three income definitions, to choose the one for
# Figure 4C: (i) nominal brackets (income_cat, current year; fixed dollar
# cut-offs drift with ~40% price growth over 2005-2021), (ii) real base-year
# income in 2019 dollars cut at the same points, and (iii) the base-year ratio
# to the poverty line (the bins of the "poorest pay too" section,
# 09_heterogeneity)
s[, inc_real := cut(
    inc_real_b,
    breaks = c(-Inf, 25000, 50000, 70000, 100000, Inf),
    labels = levels(s$inc),
    right = FALSE
)]
s[, fpl := factor(fpl_bin, levels = HET_LEVELS$fpl_bin)]
stopifnot(s[!is.na(fpl_bin), !anyNA(fpl)])
inc_def <- rbindlist(
    lapply(
        c(
            nominal = 'inc', real2019 = 'inc_real',
            poverty_line = 'fpl'
        ),
        function(g) {
            rbindlist(
                lapply(
                    c(retailer = 'ret', state = 'st', best_zip3 = 'bz3'),
                    function(r) {
                        s[, x := 100 * get(sprintf('sav25_%s', r)) / pb_spend]
                        m <- feols(
                            as.formula(sprintf('x ~ 0 + %s', g)),
                            data = s,
                            cluster = ~household_code
                        )
                        n <- s[!is.na(get(g)), .N, keyby = g]
                        data.table(
                            rung = r,
                            group = sub(
                                paste0('^', g),
                                '',
                                names(coef(m))
                            ),
                            mean = unname(coef(m)),
                            se = unname(se(m)),
                            n = n$N,
                            n_missing = s[is.na(get(g)), .N]
                        )
                    }
                ),
                idcol = 'series'
            )
        }
    ),
    idcol = 'definition'
)
inc_def[, `:=`(lo = mean - 1.96 * se, hi = mean + 1.96 * se)]
s[, x := NULL]
print(inc_def, digits = 4)


# 2. Diagnostics ---------------------------------------------------------------

# (a) The plain minimum p25 across the household's retailers (set) is a minimum
# of noisy quantile estimates and tends to rise with the number of retailers;
# the cross-fitted rung (setx) should not
s[, n_ret_cat := fcase(
    n_retailers <= 1,
    '1',
    n_retailers == 2,
    '2',
    n_retailers == 3,
    '3',
    n_retailers <= 5,
    '4-5',
    default = '6+'
)]
set_by_n <- s[, .(
    n = .N,
    retailer = 100 * mean(sav25_ret / pb_spend),
    set_naive = 100 * mean(sav25_set / pb_spend),
    set_crossfit = 100 * mean(sav25_setx / pb_spend),
    best_st = 100 * mean(sav25_bst / pb_spend),
    state = 100 * mean(sav25_st / pb_spend)
),
keyby = n_ret_cat
]
print(set_by_n, digits = 4)
# (b) A household's own calories are part of its zip3 and county shelves;
# restrict to rungs where they are under a tenth of the ladder
own <- rbind(
    s[, .(
        sample = 'all',
        n = .N,
        zip3 = 100 * mean(sav25_zip3 / pb_spend),
        county = 100 * mean(sav25_cty / pb_spend),
        med_own_cty = median(own_sh_cty)
    )],
    s[
        own_sh_cty < .1 & own_sh_zip3 < .1,
        .(
            sample = 'own share < .1 of zip3 and county',
            n = .N,
            zip3 = 100 * mean(sav25_zip3 / pb_spend),
            county = 100 * mean(sav25_cty / pb_spend),
            med_own_cty = median(own_sh_cty)
        )
    ]
)
print(own, digits = 4)

save_results(
    '22_panel_reach',
    flow = flow,
    by_year = by_year,
    reach = reach,
    kept = kept,
    by_inc = by_inc,
    reach_ci = reach_ci,
    inc_ci = inc_ci,
    inc_def = inc_def,
    set_by_n = set_by_n,
    own = own
)
print(sessionInfo())
