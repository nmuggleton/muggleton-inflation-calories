# Do households fund food inflation by cutting the other purchases the panel
# records? Non-food spending is all recorded spending less food spending
# (household goods, health and beauty, general merchandise), in dollars.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

# (1) changes in recorded non-food spending (t against t-4) --------------------
ch <- wp2_sql(
    sprintf(
        "WITH h AS (
       SELECT household_code,
              yq,
              sum(spend_all) AS spend_all,
              sum(spend_food) AS spend_food
       FROM read_parquet('%s/intermediate/hhq/*.parquet')
       GROUP BY ALL
   )
   SELECT a.household_code,
          a.yq,
          a.spend_all,
          a.spend_all - a.spend_food AS nonfood,
          b.spend_all AS spend_all_b,
          b.spend_all - b.spend_food AS nonfood_b
   FROM h a
   JOIN h b
       ON a.household_code = b.household_code AND b.yq = a.yq - 10",
        wp2_paths()$data
    )
)
d <- f1_gate(c('d_lspend', 'fpl_bin'))
d <- merge(d, ch, by = c('household_code', 'yq'))
rm(ch)
stopifnot(nrow(d) == F1_N)
d[, `:=`(
    d_lnonfood = fifelse(
        nonfood > 0 & nonfood_b > 0,
        100 * log(nonfood / nonfood_b),
        NA_real_
    ),
    d_lall = 100 * log(spend_all / spend_all_b),
    d_sh_nonfood = 100 * (nonfood / spend_all - nonfood_b / spend_all_b)
)]
flow <- data.table(
    gate = nrow(d),
    with_nonfood = d[is.finite(d_lnonfood), .N],
    base_share = d[, mean(nonfood_b / spend_all_b)]
)
print(flow)

# (2) and (3) 2SLS responses ---------------------------------------------------
fit <- function(data, y, sample) {
    m <- feols(
        as.formula(sprintf('%s ~ 1 | yq | infl_exposure ~ z', y)),
        data,
        cluster = ~state
    )
    ct <- coeftable(m)
    data.table(
        sample = sample,
        outcome = y,
        b = ct[1, 1],
        se = ct[1, 2],
        p = 2 * pt(abs(ct[1, 3]), 48, lower.tail = FALSE),
        n = nobs(m)
    )
}
low <- d$fpl_bin %in% c('<=100%', '100-130%')
est <- rbind(
    fit(d, 'd_lnonfood', 'all'),
    fit(d, 'd_sh_nonfood', 'all'),
    fit(d, 'd_lall', 'all'),
    fit(d, 'd_lspend', 'all'),
    fit(d[low], 'd_lnonfood', 'up to 130% FPL'),
    fit(d[!low & !is.na(fpl_bin)], 'd_lnonfood', 'above 130% FPL'),
    fit(d[abs(d_lnonfood) < 150], 'd_lnonfood', 'trimmed')
)
print(est, digits = 3)

save_results('32_nonfood', est = est, flow = flow)
