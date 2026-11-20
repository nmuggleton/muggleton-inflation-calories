# Does the instrument move because other households changed where they shop?
# State prices in T2 are unit values (other households' spending over packages),
# so a shift of shoppers between retailers moves them even when no retailer
# changes a price.

rm(list = ls())
library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

int <- Sys.getenv('WP2_INT')
stopifnot(nzchar(int), dir.exists(file.path(int, 'price_idx_fw')))

outcomes <- c('d_lkpu', 'd_lspend', 'd_lkcal', 'd_ed', 'd_hs')
d <- f1_gate(outcomes)
cat('sample flow: gate', nrow(d), 'household-quarters\n')

con <- wp2_con()
fw <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            "SELECT household_code, yq, n_priced_fw, t2_uv, t2_fw
   FROM read_parquet('%s/price_idx_fw/year=*.parquet')",
            int
        )
    )
)
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(fw, by = c('household_code', 'yq')))

d[fw, on = .(household_code, yq), `:=`(
    t2_uv = i.t2_uv,
    t2_fw = i.t2_fw,
    n_fw = i.n_priced_fw
)]
cat(
    '  with a fixed-retailer exposure (>= 10 products):',
    d[!is.na(t2_fw), .N],
    '(',
    round(100 * d[, mean(!is.na(t2_fw))], 1),
    '% )\n'
)
d <- d[!is.na(t2_fw)]

# Leave-household-out state x quarter means on this sample: the main exposure
# (so its instrument is built on the same rows), UV and FW
loo <- function(x, g) (ave(x, g, FUN = sum) - x) / (ave(x, g, FUN = length) - 1)
d[, n_sq := .N, by = state_q]
d <- d[n_sq >= 2]
d[, `:=`(
    z_main = loo(infl_exposure, state_q),
    z_uv = loo(t2_uv, state_q),
    z_fw = loo(t2_fw, state_q)
)]
d[, z_mix := z_uv - z_fw]
cat(
    '  analysis sample:',
    nrow(d),
    'household-quarters,',
    uniqueN(d$state_q),
    'state-quarters\n'
)

# State x quarter: how much of the instrument is retailer mix?
cells <- d[, .(
    n = .N,
    z_main = mean(infl_exposure),
    z_uv = mean(t2_uv),
    z_fw = mean(t2_fw)
), by = .(state, yq)]
cells[, `:=`(z_mix = z_uv - z_fw)]
for (v in c('z_main', 'z_uv', 'z_fw', 'z_mix')) {
    cells[, paste0(v, '_net') := get(v) - weighted.mean(get(v), n), by = yq]
}
wcor <- function(a, b, w) cov.wt(cbind(a, b), wt = w, cor = TRUE)$cor[1, 2]
wsd <- function(a, w) sqrt(cov.wt(cbind(a), wt = w)$cov[1, 1])
decomp <- cells[, .(
    cells = .N,
    sd_main = wsd(z_main_net, n),
    sd_uv = wsd(z_uv_net, n),
    sd_fw = wsd(z_fw_net, n),
    sd_mix = wsd(z_mix_net, n),
    r_main_uv = wcor(z_main_net, z_uv_net, n),
    r_main_fw = wcor(z_main_net, z_fw_net, n),
    r_uv_fw = wcor(z_uv_net, z_fw_net, n),
    share_mix = wsd(z_mix_net, n)^2 / wsd(z_uv_net, n)^2
)]
print(decomp, digits = 3)

# The headline 2SLS with each instrument: T2 instrumented, quarter FE, SEs
# clustered at the state level.
grid <- CJ(
    y = outcomes,
    inst = c('z', 'z_main', 'z_uv', 'z_fw'),
    sorted = FALSE
)
iv <- rbindlist(
    Map(
        function(y, inst) {
            m <- feols(
                as.formula(
                    sprintf('%s ~ 1 | yq | infl_exposure ~ %s', y, inst)
                ),
                d,
                cluster = ~state
            )
            data.table(
                outcome = y,
                instrument = inst,
                b = coef(m)[[1]],
                se = se(m)[[1]],
                n = nobs(m),
                fs = coef(summary(m, stage = 1))[[inst]],
                f = fitstat(m, 'ivwald1')[[1]]$stat
            )
        },
        grid$y, grid$inst
    )
)
print(dcast(iv, outcome ~ instrument, value.var = 'b'), digits = 3)
print(dcast(iv, outcome ~ instrument, value.var = 'se'), digits = 3)

# Reduced form with both parts: does the retailer-mix part move the outcomes?
rf <- rbindlist(
    lapply(
        outcomes,
        function(y) {
            m <- feols(
                as.formula(paste(y, '~ z_fw + z_mix | yq')),
                d,
                cluster = ~state
            )
            ct <- coeftable(m)
            data.table(
                outcome = y,
                term = rownames(ct),
                b = ct[, 1],
                se = ct[, 2],
                p = ct[, 4],
                n = nobs(m)
            )
        }
    )
)
print(rf, digits = 3)

save_results(
    '26_fixed_retailer',
    decomp = decomp,
    iv = iv,
    rf = rf,
    cells = cells[, .(state, yq, n, z_main, z_uv, z_fw, z_mix)],
    flow = data.table(gate = F1_N, analysis = nrow(d))
)
