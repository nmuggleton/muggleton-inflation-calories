# Is the response to food prices, or does the instrument pick up other local
# prices that move with them? This adds local BLS inflation for food away from
# home, shelter, energy, and all items less food and energy as controls.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)
setFixest_nthreads(2)

root <- Sys.getenv('LOCAL_DATA', file.path(wp2_paths()$data, 'intermediate'))
cu <- fread(
    file.path(root, 'local_raw', 'cu.data.0.Current'),
    sep = '\t',
    strip.white = TRUE,
    colClasses = list(character = 'value')
)
cu[, series_id := trimws(series_id)]
items <- c(
    SEFV = 'fafh', SAH1 = 'shelter', SA0E = 'energy',
    SA0L1E = 'core'
)
cu <- cu[
    grepl('^CUUR', series_id) & year %in% 2003:END_YEAR &
        grepl('^M(0[1-9]|1[0-2])$', period) &
        substr(series_id, 9, 20) %in% names(items)
]
cu[, `:=`(
    area = substr(series_id, 5, 8),
    item = items[substr(series_id, 9, 20)],
    m = as.integer(substr(period, 2, 3)),
    value = as.numeric(value)
)]
cu[, yq := year * 10L + (m - 1L) %/% 3L + 1L]
# Same-month year-on-year changes, averaged within the quarter
lag <- cu[, .(area, item, m, year = year + 1L, v0 = value)]
cu <- merge(cu, lag, by = c('area', 'item', 'm', 'year'))
cq <- cu[
    ,
    .(yoy = mean(100 * (value / v0 - 1)), n_m = .N),
    by = .(area, item, yq)
]
cq <- dcast(cq, area + yq ~ item, value.var = 'yoy')
setnames(
    cq,
    c('fafh', 'shelter', 'energy', 'core'),
    paste0('p_', c('fafh', 'shelter', 'energy', 'core'))
)
rm(cu, lag)
gc(verbose = FALSE)

con <- DBI::dbConnect(duckdb::duckdb())
d <- setDT(
    DBI::dbGetQuery(
        con,
        sprintf(
            "SELECT household_code, yq, state, cpi_area, infl_exposure, d_cpi,
          d_lkpu, d_lspend, d_lkcal, d_ed, d_hs FROM '%s'",
            file.path(root, 'local_context', 'local_panel.parquet')
        )
    )
)
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(nrow(d) == F1_N)
d[, z := (sum(infl_exposure) - infl_exposure) / (.N - 1), by = .(state, yq)]
n0 <- nrow(d)
d <- merge(
    d,
    cq,
    by.x = c('cpi_area', 'yq'),
    by.y = c('area', 'yq'),
    all.x = TRUE
)
stopifnot(nrow(d) == n0)
pv <- c('p_fafh', 'p_shelter', 'p_energy', 'p_core')
d[, ok := complete.cases(d[, ..pv])]
cat(
    'sample flow: f1 gate',
    n0,
    '-> all four BLS item controls',
    d[ok == TRUE, .N],
    '| areas',
    uniqueN(d[ok == TRUE, cpi_area]),
    '\n'
)
print(
    d[
        ok == TRUE, lapply(.SD, function(x) round(c(mean(x), sd(x)), 3)),
        .SDcols = c('infl_exposure', 'z', 'd_cpi', pv)
    ]
)
dc <- d[ok == TRUE]
print(round(cor(dc[, c('z', 'd_cpi', pv), with = FALSE]), 3))

# Balance: z regressed on each item's inflation (quarter FE, SEs clustered by
# state)
lab <- c(
    p_fafh = 'Food away from home', p_shelter = 'Shelter',
    p_energy = 'Energy', p_core = 'All items less food and energy'
)
bal <- rbindlist(
    lapply(
        pv,
        function(v) {
            m <- feols(as.formula(paste(v, '~ z | yq')), dc, cluster = ~state)
            data.table(
                var = v,
                b = coef(m)[[1]],
                se = se(m)[[1]],
                p = pvalue(m)[[1]]
            )
        }
    )
)
print(bal)

ys <- c(
    d_lkpu = 'Change log kcal per real $ (x100)',
    d_lspend = 'Change log real spend per AE-day (x100)',
    d_lkcal = 'Change log kcal per AE-day (x100)',
    d_ed = 'Change energy density (kcal/100 g)',
    d_hs = 'Change health score'
)
setFixest_dict(
    c(
        ys,
        infl_exposure = 'Exposure inflation T2 (pp)',
        fit_infl_exposure = 'Exposure inflation T2 (pp)',
        p_fafh = 'CPI food away from home (pp)',
        p_shelter = 'CPI shelter (pp)', p_energy = 'CPI energy (pp)',
        p_core = 'CPI less food and energy (pp)', yq = 'Quarter'
    )
)
specs <- list(
    `Baseline, item sample` = c(),
    `+ food away from home` = 'p_fafh',
    `+ shelter and energy` = c('p_shelter', 'p_energy'),
    `+ less food and energy` = 'p_core',
    `+ all four` = pv
)
fits <- lapply(
    specs,
    function(ctl) {
        rhs <- if (length(ctl)) paste(ctl, collapse = ' + ') else '1'
        feols(
            as.formula(
                sprintf(
                    'c(%s) ~ %s | yq | infl_exposure ~ z',
                    paste(names(ys), collapse = ', '),
                    rhs
                )
            ),
            dc,
            cluster = ~state
        )
    }
)
est <- rbindlist(
    lapply(
        names(fits),
        function(k) {
            rbindlist(
                lapply(
                    seq_along(ys),
                    function(j) {
                        m <- fits[[k]][[j]]
                        data.table(
                            spec = k,
                            outcome = names(ys)[j],
                            b = coef(m)[['fit_infl_exposure']],
                            se = se(m)[['fit_infl_exposure']],
                            n = nobs(m),
                            f = fitstat(m, ~ivf1)[[1]]$stat
                        )
                    }
                )
            )
        }
    )
)
print(dcast(est, spec ~ outcome, value.var = 'b'))
print(dcast(est, spec ~ outcome, value.var = 'se'))
print(etable(fits[['+ all four']]))
save_results(
    '17_other_prices',
    est = est,
    bal = bal,
    lab = lab,
    spec_names = names(specs)
)
print(sessionInfo())
