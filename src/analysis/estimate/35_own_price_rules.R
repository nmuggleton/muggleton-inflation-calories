# Which price changes should the household's own price index leave out? The
# published index (01a_price_indices.sql) drops a product when its net price
# changes by more than a factor of three between t-4 and t (Kaplan and
# Schulhofer-Wohl 2017).

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

MAIN_RULE <- 'gross10'
rules <- c('net3', 'gross10', 'net10', 'gross3', 'none')
src <- file.path(Sys.getenv('WP2_INT'), 'price_own_rules')
stopifnot(dir.exists(src))
hand_file <- 'results/35_own_price_rules/hand_codes.csv'
hand <- if (file.exists(hand_file)) fread(hand_file) else NULL

read_part <- function(part) {
    wp2_sql(
        sprintf(
            "SELECT * FROM read_parquet('%s/%s/year=*.parquet')",
            src,
            part
        )
    )
}
idx <- read_part('index')
tails <- read_part('tails')
coupons <- read_part('coupons')
stopifnot(
    !anyDuplicated(idx, by = c('household_code', 'yq', 'rule')),
    !anyDuplicated(tails, by = c('household_code', 'yq')),
    !anyDuplicated(coupons, by = c('household_code', 'yq')),
    setequal(unique(idx$rule), rules)
)

d <- f1_gate(c('d_lkpu', 'infl_laspeyres', 'f_idx_valid'))
v <- d[
    f_idx_valid %in% TRUE & is.finite(infl_laspeyres) &
        infl_laspeyres > -100
]
cat('gate', nrow(d), '| published own-price sample', nrow(v), '\n')

# (1) the published rule reproduces the published index ------------------------
chk <- merge(v, idx[rule == 'net3'], by = c('household_code', 'yq'))
stopifnot(
    nrow(chk) == nrow(v),
    chk[, max(abs(100 * (num / den - 1) - infl_laspeyres))] < 1e-8
)
cat(
    'net3 against infl_laspeyres, largest difference:',
    chk[, max(abs(100 * (num / den - 1) - infl_laspeyres))], '\n'
)
rm(chk)

# 2SLS on exposure: quarter effects, SEs clustered by state
fit <- function(data, y, label) {
    m <- feols(
        as.formula(sprintf('%s ~ 1 | yq | infl_exposure ~ z', y)),
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
        n = nobs(m),
        f1 = fitstat(m, ~ivf1)[[1]]$stat
    )
}

# (2) own prices and composition under each rule -------------------------------
# own = pre-coupon own prices + coupon discount; d_lkpu = -own + comp
ys <- c('d_lkpu', 'own', 'own_gross', 'coupon', 'comp')
est <- rbindlist(
    lapply(
        rules,
        function(r) {
            dd <- merge(v, idx[rule == r], by = c('household_code', 'yq'))
            dd <- dd[n_pairs >= 5]
            dd[, `:=`(
                own = 100 * log(num / den),
                own_gross = 100 * log(gnum / gden)
            )]
            dd[, `:=`(coupon = own - own_gross, comp = d_lkpu + own)]
            stopifnot(dd[, all(is.finite(own) & is.finite(own_gross))])
            out <- rbindlist(lapply(ys, function(y) fit(dd, y, r)))
            out[, `:=`(
                pairs = dd[, sum(n_pairs)],
                lost = nrow(v) - nrow(dd) # fewer than 5 products kept
            )]
            out
        }
    )
)
setnames(est, 'label', 'rule')
cat('\n== own prices and composition by rule\n')
print(
    dcast(est, rule + n + lost + pairs ~ outcome, value.var = 'b'),
    digits = 3
)
print(dcast(est, rule ~ outcome, value.var = 'se'), digits = 3)
stopifnot(
    abs(est[rule == 'net3' & outcome == 'own', b] - 0.849) < 5e-4,
    abs(est[rule == 'net3' & outcome == 'comp', b] - 0.031) < 5e-4
)

# (3) pairs beyond each bound, and their response to exposure ------------------
tv <- merge(v, tails, by = c('household_code', 'yq'))
stopifnot(nrow(tv) == nrow(v))
bounds <- setdiff(names(tails), c('household_code', 'yq', 'n_pairs'))
dropped <- rbindlist(
    lapply(
        bounds,
        function(k) {
            tv[, share := 100 * get(k) / n_pairs]
            cbind(
                fit(tv, 'share', k),
                pooled = tv[, 100 * sum(get(k)) / sum(n_pairs)]
            )
        }
    )
)
setnames(dropped, 'label', 'bound')
cat('\n== pairs beyond each bound: pooled %, and pp per point of exposure\n')
print(dropped[, .(bound, pooled, b, se, t = b / se, n)], digits = 3)

# (4) coupon use against exposure ----------------------------------------------
cu <- merge(d, coupons, by = c('household_code', 'yq'))
cu[, `:=`(
    d_value = 100 * (spend_base / gross_base - spend / gross),
    d_items = 100 * (n_coupon / n_items - n_coupon_base / n_items_base)
)]
use <- rbind(
    fit(cu, 'd_value', 'coupon value, % of pre-coupon spending'),
    fit(cu, 'd_items', 'products bought with a coupon, %')
)
use[, base_mean := cu[, c(
    100 * mean(1 - spend_base / gross_base),
    100 * mean(n_coupon_base / n_items_base)
)]]
cat('\n== coupon use (pp per point of exposure); gate rows', nrow(cu), '\n')
print(use, digits = 3)

# (5) hand coding of 200 pairs dropped by the published rule -------------------
if (!is.null(hand)) {
    cat('\n== hand coding\n')
    print(hand[, .N, keyby = code])
    print(dcast(hand, band ~ code, fun.aggregate = length, value.var = 'code'))
}

out <- list(
    est = est,
    dropped = dropped,
    coupon_use = use,
    flow = data.table(
        gate = nrow(d),
        own_sample = nrow(v),
        main_rule = MAIN_RULE
    )
)
if (!is.null(hand)) out$hand_codes <- hand
do.call(save_results, c(list('35_own_price_rules'), out))
