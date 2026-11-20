# Where does the avoided share come from? On the products a household buys in
# both years, its own prices rise by less than its exposure
# (05_identification.R, step f).

rm(list = ls())
library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

OWN_RULE <- 'gross10'
rel_dir <- c(net3 = 'price_rel', gross10 = 'price_rel_gross10')

int <- Sys.getenv('WP2_INT')
stopifnot(nzchar(int), all(dir.exists(file.path(int, rel_dir))))

gate <- f1_gate(c('infl_laspeyres', 'f_idx_valid'))
cat('sample flow: gate', nrow(gate), 'household-quarters\n')
# The sample of the own-price decomposition (05_identification.R, step f)
gate <- gate[
    f_idx_valid %in% TRUE & is.finite(infl_laspeyres) &
        infl_laspeyres > -100
]
cat('  with a valid own-price index:', nrow(gate), '\n')

# Own prices on all products bought in both years, under each rule
own_all <- wp2_sql(
    sprintf(
        "SELECT household_code, yq, rule, 100 * ln(num / den) AS t1_log
   FROM read_parquet('%s/price_own_rules/index/year=*.parquet')
   WHERE n_pairs >= 5 AND rule IN ('net3', 'gross10')",
        int
    )
)

# 2SLS of each price index and each step on exposure: quarter FE, SEs clustered
# by state
fit <- function(data, y, sample) {
    m <- feols(
        as.formula(sprintf('%s ~ 1 | yq | infl_exposure ~ z', y)),
        data,
        cluster = ~state
    )
    data.table(
        sample = sample,
        outcome = y,
        b = coef(m)[[1]],
        se = se(m)[[1]],
        n = nobs(m)
    )
}
pct <- function(x) 100 * log(x) # log changes x 100
steps <- c('where', 'when', 'deal', 'coupon', 'rest')
ys <- c('t1_log', 'own_matched', 'state_same', 'gap', steps)

by_rule <- lapply(
    names(rel_dir),
    function(rule_k) {
        cat('\n== own price relatives screened by', rule_k, '\n')
        rel <- wp2_sql(
            sprintf(
                "SELECT * FROM read_parquet('%s/%s/year=*.parquet')",
                int,
                rel_dir[[rule_k]]
            )
        )
        stopifnot(!anyDuplicated(rel, by = c('household_code', 'yq')))
        d <- merge(
            gate,
            own_all[rule == rule_k, !'rule'],
            by = c('household_code', 'yq')
        )
        d <- merge(d, rel, by = c('household_code', 'yq'))
        cat('  with own and state prices on the same products:', nrow(d), '\n')

        # Each step is the change in going from one price index to the next
        d[, `:=`(
            own_matched = pct(own),
            state_same = pct(i0),
            gap = pct(own) - pct(i0),
            where = pct(i1) - pct(i0),
            when = pct(i2) - pct(i1),
            deal = pct(i3) - pct(i2),
            coupon = (pct(own) - pct(own_g)) - (pct(i3) - pct(i3_g)),
            rest = pct(own_g) - pct(i3_g)
        )]
        d <- d[is.finite(gap) & is.finite(coupon) & is.finite(rest)]
        cat('  analysis sample:', nrow(d), 'household-quarters\n')
        stopifnot(
            d[, max(abs(gap - where - when - deal - coupon - rest))] < 1e-8
        )

        coverage <- d[, .(
            rule = rule_k,
            retailer = mean(cov1),
            month = mean(cov2),
            deal = mean(cov3),
            products = mean(n_valid)
        )]
        print(coverage, digits = 3)

        est <- rbindlist(lapply(ys, function(y) fit(d, y, 'all')))
        print(est, digits = 3)
        say(
            'check: gap = sum of steps, %.3f = %.3f',
            est[outcome == 'gap', b],
            est[outcome %in% steps, sum(b)]
        )

        # For most household-quarters, transactions are priced at the month
        # level, but some fall back to coarser prices.
        dense <- d[cov2 >= 0.8]
        est_dense <- rbindlist(lapply(ys, function(y) fit(dense, y, 'dense')))
        print(est_dense, digits = 3)

        list(
            est = cbind(rule = rule_k, rbind(est, est_dense)),
            coverage = coverage,
            flow = data.table(
                rule = rule_k,
                gate = F1_N,
                analysis = nrow(d),
                dense = nrow(dense)
            )
        )
    }
)

save_results(
    '27_relative_prices',
    est = rbindlist(lapply(by_rule, `[[`, 'est')),
    coverage = rbindlist(lapply(by_rule, `[[`, 'coverage')),
    flow = rbindlist(lapply(by_rule, `[[`, 'flow')),
    own_rule = data.table(own_rule = OWN_RULE)
)
