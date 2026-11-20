# Diagnostics behind the rule for extreme price changes in the household's own
# price index (35_own_price_rules.R gives the index under each rule). The
# published rule drops a product whose net price changes by more than a factor
# of three (Kaplan and Schulhofer-Wohl 2017).

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

src <- file.path(Sys.getenv('WP2_INT'), 'outlier_diagnostics')
stopifnot(dir.exists(src))
read_part <- function(part) {
    wp2_sql(
        sprintf(
            "SELECT * FROM read_parquet('%s/%s/year=*.parquet')",
            src,
            part
        )
    )
}
bands <- read_part('bands')
expo <- read_part('exposure')
pairs <- read_part('pairs')
key <- c('household_code', 'yq')
stopifnot(!anyDuplicated(bands, by = key), !anyDuplicated(expo, by = key))

sizes <- c('r2', 'b1', 'b2', 'b3', 'b4', 'b5')
size_lab <- c('2 to 3', '3 to 3.5', '3.5 to 5', '5 to 10', '10 to 20', '20+')
band_names <- c(paste0('up_', sizes), paste0('dn_', sizes))

d <- f1_gate(
    c('d_lspend', 'd_lkcal', 'd_lkpu', 'infl_laspeyres', 'f_idx_valid')
)
v <- d[
    f_idx_valid %in% TRUE & is.finite(infl_laspeyres) &
        infl_laspeyres > -100
]
v <- merge(v, bands, by = key)
cat('gate', nrow(d), '| published own-price sample', nrow(v), '\n')
stopifnot(
    nrow(v) == d[
        f_idx_valid %in% TRUE & is.finite(infl_laspeyres) &
            infl_laspeyres > -100, .N
    ]
)

# 2SLS: quarter effects, state clusters
fit <- function(data, y, label, x = 'infl_exposure', z = 'z') {
    m <- feols(
        as.formula(sprintf('%s ~ 1 | yq | %s ~ %s', y, x, z)),
        data,
        cluster = ~state,
        notes = FALSE
    )
    data.table(
        label = label,
        outcome = y,
        b = coef(m)[[1]],
        se = se(m)[[1]],
        p = pvalue(m)[[1]],
        n = nobs(m)
    )
}

# (1) bands against exposure ---------------------------------------------------
band_slopes <- rbindlist(
    lapply(
        band_names,
        function(k) {
            nb <- v[[paste0(k, '_nb')]]
            sb <- v[[paste0(k, '_sb')]]
            v[, `:=`(
                pairs = 100 * nb / n_pairs,
                spend = 100 * sb / s0_all
            )]
            cbind(
                rbind(fit(v, 'pairs', k), fit(v, 'spend', k)),
                mean = v[, c(mean(pairs), mean(spend))],
                pooled = 100 * c(
                    sum(nb) / v[, sum(n_pairs)],
                    sum(sb) / v[, sum(s0_all)]
                )
            )
        }
    )
)
setnames(band_slopes, c('label', 'outcome'), c('band', 'share_of'))
cat('\n== (1) share in band (pp) per point of exposure\n')
print(
    band_slopes[, .(band, share_of, b, se, t = b / se, mean, pooled)],
    digits = 3
)

# Beyond threefold and beyond tenfold, rises and falls
count_in <- function(side, from) {
    Reduce(`+`, lapply(paste0(side, '_', from, '_nb'), function(k) v[[k]]))
}
v[, `:=`(
    up3 = 100 * count_in('up', sizes[-1]) / n_pairs,
    dn3 = 100 * count_in('dn', sizes[-1]) / n_pairs,
    up10 = 100 * count_in('up', sizes[5:6]) / n_pairs,
    dn10 = 100 * count_in('dn', sizes[5:6]) / n_pairs
)]
beyond <- rbindlist(
    lapply(
        c('up3', 'dn3', 'up10', 'dn10'),
        function(k) cbind(fit(v, k, k), mean = mean(v[[k]]))
    )
)
cat('\n== share of pairs beyond threefold and tenfold (net prices)\n')
print(beyond[, .(label, b, se, t = b / se, mean, n)], digits = 3)
# The same shares, calculated separately by 05a_own_price_rules.sql
pub <- load_results('35_own_price_rules')$dropped
chk <- merge(
    beyond[, .(label, b, se)],
    pub[, .(
        label = sub('net_', '', bound),
        b35 = b,
        se35 = se
    )],
    by = 'label'
)
print(chk, digits = 6)
stopifnot(
    nrow(chk) == 4,
    chk[, max(abs(b - b35))] < 1e-8,
    chk[, max(abs(se - se35))] < 1e-8
)

# (2) rises against falls ------------------------------------------------------
natq <- d[, .(nat = mean(infl_exposure)), by = yq]
natq[, high := nat > median(nat)]
v[natq, on = 'yq', high := i.high]
# The ratio of rises to falls: pooled, and an interval from the 49 state ratios
symmetry <- rbindlist(
    lapply(
        list(
            `all quarters` = v,
            `high national exposure` = v[high == TRUE],
            `low national exposure` = v[high == FALSE]
        ),
        function(data) {
            rbindlist(
                lapply(
                    sizes,
                    function(k) {
                        s <- data[, .(
                            up = sum(get(paste0('up_', k, '_nb'))),
                            dn = sum(get(paste0('dn_', k, '_nb')))
                        ), by = state][up > 0 & dn > 0]
                        lr <- s[, log(up / dn)]
                        half <- qt(.975, length(lr) - 1) * sd(lr) /
                            sqrt(length(lr))
                        data.table(
                            band = k,
                            rises = s[, sum(up)],
                            falls = s[, sum(dn)],
                            ratio = s[, sum(up) / sum(dn)],
                            lo = exp(mean(lr) - half),
                            hi = exp(mean(lr) + half),
                            states = length(lr)
                        )
                    }
                )
            )
        }
    ),
    idcol = 'quarters'
)
cat('\n== (2) rises / falls by band\n')
print(symmetry, digits = 3)

# (3) bands by quarter: mean and SE over states of the pooled state share ------
by_quarter <- rbindlist(
    lapply(
        band_names,
        function(k) {
            s <- v[, .(
                share = 100 * sum(get(paste0(k, '_nb'))) / sum(n_pairs)
            ), by = .(state, yq)]
            s[, .(
                band = k,
                share = mean(share),
                se = sd(share) / sqrt(.N)
            ), keyby = yq]
        }
    )
)
by_quarter[natq, on = 'yq', nat_exposure := i.nat]

# (4) pairs outside [1/2, 2], all households -----------------------------------
counts <- setdiff(names(pairs), 'band')
pair_shares <- pairs[, lapply(.SD, sum), by = band, .SDcols = counts]
pair_shares[, `:=`(
    coupon_cheap = 100 * n_coupon_cheap / n,
    under_25c = 100 * n_under_25c / n,
    gross_in3 = 100 * n_gross_in3 / n,
    classified = 100 * n_classified / n,
    state_moved = 100 * n_state_moved / n_classified,
    odd2 = 100 * n_odd2 / n_classified,
    odd3 = 100 * n_odd3 / n_classified
)]
pair_shares[, band := factor(band, band_names)]
setorder(pair_shares, band)
cat('\n== (4) pairs outside [1/2, 2] (percent)\n')
print(
    pair_shares[, .(
        band, n, coupon_cheap, under_25c, gross_in3, classified,
        state_moved, odd2, odd3
    )],
    digits = 3
)
dropped3 <- pair_shares[!band %in% c('up_r2', 'dn_r2')]
cat(
    'pairs beyond threefold:', dropped3[, sum(n)],
    '| coupon on the cheap side, %:',
    dropped3[, 100 * sum(n_coupon_cheap) / sum(n)],
    '| pre-coupon change within threefold, %:',
    dropped3[, 100 * sum(n_gross_in3) / sum(n)], '\n'
)

# (5) exposure and instrument under other screens ------------------------------
e <- merge(d, expo, by = key, all.x = TRUE)
stopifnot(nrow(e) == nrow(d))
# The published screen reproduces exposure, from both sets of state totals
e[, `:=`(
    x_t3 = 100 * (enum_t3 / eden_t3 - 1),
    x_c3 = 100 * (enum_c3 / eden_c3 - 1),
    x_t10 = 100 * (enum_t10 / eden_t10 - 1),
    x_g10 = 100 * (enum_g10 / eden_g10 - 1)
)]
cat(
    '\nrebuilt against published exposure, largest difference:',
    e[, max(abs(x_t3 - infl_exposure))],
    e[, max(abs(x_c3 - infl_exposure))], '\n'
)
stopifnot(
    e[, max(abs(x_t3 - infl_exposure))] < 1e-8,
    e[, max(abs(x_c3 - infl_exposure))] < 1e-8
)
screens <- c(
    x_t3 = 'net, threefold (published)',
    x_t10 = 'net, tenfold',
    x_g10 = 'pre-coupon, tenfold'
)
exposure <- rbindlist(
    lapply(
        names(screens),
        function(k) {
            dd <- e[is.finite(get(k))]
            dd[, x := get(k)]
            # Instrument: the leave-household-out mean by state and quarter
            dd[, zx := (sum(x) - x) / (.N - 1), by = .(state, yq)]
            fs <- feols(x ~ zx | yq, dd, cluster = ~state, notes = FALSE)
            cbind(
                rbindlist(
                    lapply(
                        c('d_lspend', 'd_lkcal', 'd_lkpu'),
                        function(y) fit(dd, y, screens[[k]], 'x', 'zx')
                    )
                ),
                lost = nrow(e) - nrow(dd),
                sd_x = dd[, sd(x)],
                cor_published = dd[, cor(x, infl_exposure)],
                first_stage = coef(fs)[[1]]
            )
        }
    )
)
setnames(exposure, 'label', 'screen')
cat('\n== (5) headline with exposure rebuilt under each screen\n')
print(exposure, digits = 3)
# The published screen gives the published headline
main <- load_results('01_main')$headline
stopifnot(
    abs(
        exposure[screen == screens[['x_t3']] & outcome == 'd_lspend', b] -
            main[outcome == 'd_lspend', b_iv]
    ) < 1e-6
)

save_results(
    '38_outlier_diagnostics',
    band_slopes = band_slopes,
    beyond = beyond,
    symmetry = symmetry,
    by_quarter = by_quarter,
    pair_shares = pair_shares,
    exposure = exposure,
    labels = data.table(band = sizes, relative = size_lab),
    flow = data.table(
        gate = nrow(d),
        own_sample = nrow(v),
        pairs = v[, sum(n_pairs)],
        pairs_outside_2 = pairs[, sum(n)]
    )
)
