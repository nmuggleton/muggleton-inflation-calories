# Where does the gap between own and state prices sit? On the products a
# household buys in both years, its prices rise by less per point of exposure
# than the prices other households in the state pay (27_relative_prices.R).

rm(list = ls())
library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

int <- Sys.getenv('WP2_INT')
src <- file.path(int, 'price_gap_sources')
stopifnot(nzchar(int), dir.exists(file.path(src, 'groups')))

# Sample: that of 27_relative_prices.R under gross10 ---------------------------

gate <- f1_gate(c('infl_laspeyres', 'f_idx_valid'))
gate <- gate[
    f_idx_valid %in% TRUE & is.finite(infl_laspeyres) &
        infl_laspeyres > -100
]
own_all <- wp2_sql(
    sprintf(
        "SELECT household_code, yq
   FROM read_parquet('%s/price_own_rules/index/year=*.parquet')
   WHERE n_pairs >= 5 AND rule = 'gross10'",
        int
    )
)
rel <- wp2_sql(
    sprintf(
        "SELECT household_code, yq, own, own_g, i0, i1, i3, i3_g
   FROM read_parquet('%s/price_rel_gross10/year=*.parquet')",
        int
    )
)
pct <- function(x) 100 * log(x) # log changes x 100
d0 <- merge(gate, own_all, by = c('household_code', 'yq'))
d0 <- merge(d0, rel, by = c('household_code', 'yq'))
d0 <- d0[
    is.finite(pct(own) - pct(i0)) &
        is.finite((pct(own) - pct(own_g)) - (pct(i3) - pct(i3_g))) &
        is.finite(pct(own_g) - pct(i3_g))
]
d0[, gap_27 := pct(own) - pct(i0)]
d0 <- d0[, .(household_code, yq, year, state, z, infl_exposure, gap_27)]
rm(gate, own_all, rel)
cat('analysis sample:', nrow(d0), 'household-quarters\n')

# 2SLS of an outcome on exposure: quarter FE, SEs clustered by state
fit <- function(data, y) {
    data <- data[is.finite(get(y))]
    m <- feols(
        as.formula(sprintf('%s ~ 1 | yq | infl_exposure ~ z', y)),
        data,
        cluster = ~state,
        notes = FALSE
    )
    data.table(outcome = y, b = coef(m)[[1]], se = se(m)[[1]], n = nobs(m))
}

# Household-quarter sums for the groups whose name starts with `prefix`, on the
# analysis sample, with the gap and its parts (one split at a time, so the data
# fit in memory)
grp_read <- function(prefix) {
    g <- wp2_sql(
        sprintf(
            "SELECT * FROM read_parquet('%s/groups/year=*.parquet')
       WHERE starts_with(grp, '%s')",
            src,
            prefix
        )
    )
    g <- merge(d0, g, by = c('household_code', 'yq'))
    g[, `:=`(
        base = pct(a0 / b0),
        now = pct(a1 / b1),
        own_m = pct(a1 / a0),
        state_m = pct(b1 / b0),
        ret_m = pct(r11 / r00)
    )]
    g[, `:=`(
        gap = now - base,
        within = own_m - ret_m,
        ret_step = ret_m - state_m
    )]
    g[]
}

# The total, two ways ----------------------------------------------------------

whole <- grp_read('all')
stopifnot(
    nrow(whole) == nrow(d0),
    whole[, max(abs(gap - gap_27))] < 1e-8 # the pairs of 27_relative_prices.R
)
whole[, agap := 100 * (a1 / a0 - b1 / b0)] # index points: this adds up
tot <- whole[, sum(a0)]
total <- rbindlist(
    lapply(
        c('gap', 'base', 'now', 'own_m', 'state_m', 'agap'),
        fit,
        data = whole
    )
)
print(total, digits = 3)

# Own gaps and contributions by split ------------------------------------------

splits <- c(
    channel = 'ch:', coverage = 'cov:', coverage_channel = 'covch:',
    store = 'sw:'
)
by_split <- lapply(
    names(splits),
    function(k) {
        cat('\n==', k, '\n')
        g <- grp_read(splits[[k]])
        own <- g[, cbind(
            sh_spend = sum(a0) / tot,
            sh_hq = .N / nrow(d0),
            rbindlist(
                lapply(
                    c('gap', 'base', 'now', 'own_m', 'state_m'), fit,
                    data = .SD
                )
            )
        ), by = grp]

        # Contributions: with base weights w (own spending) and v (spending at
        # state prices), the own index is the sum of w_g x own_g and the state
        # index the sum of v_g x state_g, so a group's part of the difference is
        # 100 x [(a1_g - a0_g) / a0 - (b1_g - b0_g) / b0]
        x <- merge(
            g[
                ,
                .(
                    household_code, yq, grp, ga0 = a0, gb0 = b0, ga1 = a1,
                    gb1 = b1
                )
            ],
            whole[, .(household_code, yq, a0, b0)],
            by = c('household_code', 'yq')
        )
        x[, cg := 100 * ((ga1 - ga0) / a0 - (gb1 - gb0) / b0)]
        stopifnot(
            merge(
                x[, .(s = sum(cg)), by = .(household_code, yq)],
                whole[, .(household_code, yq, agap)],
                by = c('household_code', 'yq')
            )[, .N == nrow(whole) & max(abs(s - agap)) < 1e-6]
        )
        con <- rbindlist(
            lapply(
                sort(unique(x$grp)),
                function(gk) {
                    w <- merge(
                        whole[
                            ,
                            .(
                                household_code, yq, state, z,
                                infl_exposure
                            )
                        ],
                        x[grp == gk, .(household_code, yq, cg)],
                        by = c('household_code', 'yq'),
                        all.x = TRUE
                    )
                    w[is.na(cg), cg := 0] # no pair of this group
                    cbind(grp = gk, fit(w, 'cg'))
                }
            )
        )
        con[, share_of_gap := b / total[outcome == 'agap', b]]
        say(
            'contributions sum to %.5f; total %.5f',
            con[, sum(b)],
            total[outcome == 'agap', b]
        )

        # products another household prices at the same retailer: the own
        # gap = household against those buyers + retailer against the state
        parts <- if (grepl('coverage', k)) {
            g[grepl('1$', grp), rbindlist(
                lapply(c('gap', 'within', 'ret_step'), fit, data = .SD)
            ), by = grp]
        }
        list(
            own = cbind(split = k, own),
            con = cbind(split = k, con),
            parts = parts
        )
    }
)
names(by_split) <- names(splits)
own <- rbindlist(lapply(by_split, `[[`, 'own'))
contrib <- rbindlist(lapply(by_split, `[[`, 'con'))
parts <- rbindlist(lapply(by_split, `[[`, 'parts'))
contrib <- merge(
    contrib,
    unique(own[, .(split, grp, sh_spend)]),
    by = c('split', 'grp'),
    sort = FALSE
)
print(
    dcast(own, split + grp + sh_spend ~ outcome, value.var = 'b'),
    digits = 3
)
print(dcast(own, split + grp ~ outcome, value.var = 'se'), digits = 3)
print(contrib, digits = 3)
print(parts, digits = 3)

# A second route to the channel contributions: summed in DuckDB from the
# coverage-by-channel groups (a different grouping of the same pairs), rather
# than from the channel groups in R
alt <- wp2_sql(
    sprintf(
        "SELECT household_code, yq,
            'ch:' || split_part(substr(grp, 7), '|', 1) AS grp,
            sum(a1 - a0) AS da, sum(b1 - b0) AS db
   FROM read_parquet('%s/groups/year=*.parquet')
   WHERE starts_with(grp, 'covch:')
   GROUP BY ALL",
        src
    )
)
route <- rbindlist(
    lapply(
        sort(unique(alt$grp)),
        function(gk) {
            w <- merge(
                whole[
                    ,
                    .(
                        household_code, yq, state, z, infl_exposure, a0,
                        b0
                    )
                ],
                alt[grp == gk],
                by = c('household_code', 'yq'),
                all.x = TRUE
            )
            w[, cg := fcoalesce(100 * (da / a0 - db / b0), 0)]
            cbind(grp = gk, fit(w, 'cg'))
        }
    )
)
route <- merge(
    contrib[split == 'channel', .(grp, b_r = b, se_r = se)],
    route[, .(grp, b_sql = b, se_sql = se)],
    by = 'grp'
)
print(route, digits = 6)
stopifnot(route[, max(abs(b_r - b_sql), abs(se_r - se_sql))] < 1e-9)
rm(alt)

# Store status from 2011, when the store is recorded for most purchases --------

g <- grp_read('sw:')
store_2011 <- rbind(
    cbind(grp = 'all', fit(whole[year >= 2011], 'gap')),
    g[year >= 2011, fit(.SD, 'gap'), by = grp]
)
print(store_2011, digits = 3)

# Retail Scanner stores, 2007 to 2019 ------------------------------------------
# rms:1 = both purchases in a store that is in the Retail Scanner data

g <- grp_read('rms:')[year %between% c(2007, 2019)]
rms <- rbind(
    cbind(
        grp = 'all', sh_spend = 1, fit(
            whole[year %between% c(2007, 2019)],
            'gap'
        )
    ),
    g[, cbind(
        sh_spend = sum(a0) / whole[year %between% c(2007, 2019), sum(a0)],
        rbindlist(lapply(c('gap', 'base', 'now'), fit, data = .SD))
    ), by = grp]
)
g[, k := as.integer(grp == 'rms:0')]
g[, `:=`(ek = infl_exposure * k, zk = z * k)]
m <- feols(
    gap ~ 1 | yq^grp | infl_exposure + ek ~ z + zk, g,
    cluster = ~state, notes = FALSE
)
rms <- rbind(
    rms,
    data.table(
        grp = 'rms:0 minus rms:1', sh_spend = NA, outcome = 'gap',
        b = coef(m)[['fit_ek']], se = se(m)[['fit_ek']], n = nobs(m)
    )
)
print(rms, digits = 3)

# Changes of retailer priced in all four cells ---------------------------------
# at_base: the retailer moved to against the one left, at t-4 prices

g <- grp_read('cov4:')
g[, `:=`(at_base = pct(r10 / r00), at_now = pct(r11 / r01))]
moved <- cbind(
    sh_spend = g[, sum(a0)] / tot,
    rbindlist(lapply(c('gap', 'at_base', 'at_now'), fit, data = g))
)
print(moved, digits = 3)
rm(g)

# Does switching respond to exposure? ------------------------------------------
# Shares of base spending on rebought products, in percentage points

s <- wp2_sql(
    sprintf("SELECT * FROM read_parquet('%s/switch/year=*.parquet')", src)
)
s <- merge(d0, s, by = c('household_code', 'yq'))
stopifnot(nrow(s) == nrow(d0))
s[, `:=`(
    sh_same = 100 * s_same / s0,
    sh_within = 100 * s_within / s0,
    sh_ret = 100 * s_ret / s0,
    sh_unk = 100 * s_unk / s0,
    d_deal = 100 * (s_deal1 - s_deal0) / s0
)]
ys <- c('sh_same', 'sh_within', 'sh_ret', 'sh_unk', 'd_deal')
switching <- rbind(
    cbind(
        years = 'all',
        rbindlist(lapply(ys, fit, data = s)),
        mean = s[, sapply(.SD, mean), .SDcols = ys]
    ),
    cbind(
        years = '2011 on',
        rbindlist(lapply(ys, fit, data = s[year >= 2011])),
        mean = s[year >= 2011, sapply(.SD, mean), .SDcols = ys]
    )
)
print(switching, digits = 3)

save_results(
    '37_price_gap_sources',
    total = total,
    own = own,
    contrib = contrib,
    parts = parts,
    route = route,
    store_2011 = store_2011,
    rms = rms,
    moved = moved,
    switching = switching,
    flow = data.table(gate = F1_N, analysis = nrow(d0))
)
