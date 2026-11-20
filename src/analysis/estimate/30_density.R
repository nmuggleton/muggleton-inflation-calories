# Do the headline responses depend on how completely a household's purchases can
# be priced against other panel households? Noticed while building
# 27_relative_prices.R: for households whose base spending is mostly on products
# that other households in their state bought at the same retailer in the same
# month ("densely priced"), calories per dollar fall almost one for one with
# exposure and calories bought fall.

rm(list = ls())
library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

int <- Sys.getenv('WP2_INT')
stopifnot(nzchar(int), dir.exists(file.path(int, 'price_rel')))

outcomes <- c('d_lkpu', 'd_lspend', 'd_lkcal')
d <- f1_gate(outcomes)
cat('sample flow: gate', nrow(d), 'household-quarters\n')

con <- wp2_con()
rel <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            paste(
                'SELECT household_code, yq, cov2',
                "FROM read_parquet('%s/price_rel/year=*.parquet')"
            ),
            int
        )
    )
)
idx <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            paste(
                'SELECT household_code, yq, n_base_priced,',
                'n_base_items AS n_base_all',
                "FROM read_parquet('%s/price_idx/year=*.parquet')"
            ),
            int
        )
    )
)
DBI::dbDisconnect(con, shutdown = TRUE)

d <- merge(d, rel, by = c('household_code', 'yq'))
d <- merge(d, idx, by = c('household_code', 'yq'))
cat('  with a density measure:', nrow(d), '\n')

# The household's density in its other quarters
d[, `:=`(sum_cov = sum(cov2), n_q = .N), by = household_code]
d[, cov_other := (sum_cov - cov2) / (n_q - 1)]
# Panel households in the state and quarter, and the household's density rank
# within them
d[, n_sq := .N, by = state_q]
d[, rank_sq := frank(cov2) / .N, by = state_q]
d[, priced_share := n_base_priced / n_base_all]

cuts <- c(-Inf, 0.5, 0.65, 0.8, Inf)
labs <- c('<50%', '50-65%', '65-80%', '80%+')
third <- function(x) {
    cut(
        x,
        quantile(x, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE),
        labels = c('low', 'middle', 'high'),
        include.lowest = TRUE
    )
}
d[, `:=`(
    g_current = cut(cov2, cuts, labels = labs),
    g_trait = cut(cov_other, cuts, labels = labs),
    g_state = third(n_sq),
    g_within = third(rank_sq),
    g_priced = third(priced_share)
)]

# 2SLS within a group: quarter FE, SEs clustered at the state level
fit <- function(data, y) {
    m <- feols(
        as.formula(sprintf('%s ~ 1 | yq | infl_exposure ~ z', y)),
        data,
        cluster = ~state
    )
    data.table(
        outcome = y,
        b = coef(m)[[1]],
        se = se(m)[[1]],
        n = nobs(m)
    )
}
# every group of every split
splits <- c('g_current', 'g_trait', 'g_state', 'g_within', 'g_priced')
rows <- list()
for (sv in splits) {
    for (g in levels(d[[sv]])) {
        dg <- d[get(sv) == g]
        est_g <- rbindlist(lapply(outcomes, function(y) fit(dg, y)))
        rows[[length(rows) + 1]] <- cbind(
            split = sv,
            group = g,
            est_g,
            density = dg[, mean(cov2)],
            sd_x = dg[, sd(infl_exposure)],
            hh_per_sq = dg[, median(n_sq)]
        )
    }
}
est <- rbindlist(rows)
rm(rows, dg, est_g)
for (s in splits) {
    cat('\n==', s, '\n')
    print(
        dcast(
            est[split == s],
            group + n + density + hh_per_sq ~ outcome,
            value.var = c('b', 'se')
        )[match(levels(d[[s]]), group)],
        digits = 3
    )
}

# (6) trait density within thirds of state size --------------------------------
d[, dense_trait := cov_other >= 0.8]
two_way <- rbindlist(
    lapply(
        levels(d$g_state),
        function(g) {
            rbindlist(
                lapply(
                    c(FALSE, TRUE),
                    function(k) {
                        dg <- d[g_state == g & dense_trait == k]
                        est <- rbindlist(
                            lapply(outcomes, function(y) fit(dg, y))
                        )
                        cbind(state_size = g, dense = k, est)
                    }
                )
            )
        }
    )
)
cat('\n== trait density within thirds of state size\n')
print(
    dcast(two_way, state_size + dense + n ~ outcome, value.var = c('b', 'se')),
    digits = 3
)

# (7) calories in the dense group ----------------------------------------------
dense <- d[cov_other >= 0.8]
m <- feols(d_lkcal ~ 1 | yq | infl_exposure ~ z, dense, cluster = ~state)
b <- coef(m)[[1]]
s <- se(m)[[1]]
crit <- qt(0.975, 48)
calories_dense <- data.table(
    n = nobs(m),
    b = b,
    se = s,
    lo = b - crit * s,
    hi = b + crit * s,
    # Equivalence: can I rule out a fall as large as 0.25 percent per point?
    p_tost_025 = pt((b + 0.25) / s, 48, lower.tail = FALSE)
)
cat('\n== calories, trait-dense households\n')
print(calories_dense, digits = 3)

save_results(
    '30_density',
    est = est,
    two_way = two_way,
    calories_dense = calories_dense,
    flow = data.table(gate = F1_N, analysis = nrow(d))
)
