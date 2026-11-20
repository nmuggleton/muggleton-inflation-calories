# Two checks of the identifying assumption.

rm(list = ls())

library(data.table)
library(fixest)
library(readxl)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

YS <- c(
    d_lkpu = 'Calories per dollar', d_lspend = 'Real food spend',
    d_lkcal = 'Calories bought'
)


# 0. Effective minimum wage by state x quarter (end of quarter) ----------------

mw_dir <- file.path(wp2_paths()$data, 'intermediate', 'minwage_raw')
st <- as.data.table(
    read_excel(
        file.path(
            mw_dir,
            'StateMinimumWage_Changes.xlsx'
        )
    )
)
fe <- as.data.table(
    read_excel(
        file.path(
            mw_dir,
            'FederalMinimumWage_Changes.xlsx'
        )
    )
)
st <- st[!is.na(mw), .(
    state = as.integer(statefips),
    date = as.IDate(
        sprintf(
            '%d-%02d-%02d',
            year,
            month,
            day
        )
    ),
    mw
)]
fe <- fe[, .(
    date = as.IDate(sprintf('%d-%02d-%02d', year, month, day)),
    fed = Fed_mw
)]
qs <- CJ(state = sort(unique(st$state)), year = 2003:END_YEAR, qtr = 1:4)
qs[, qend := as.IDate(sprintf(
    '%d-%02d-01',
    year + (qtr == 4),
    qtr %% 4 * 3 + 1
)) - 1L]
setkey(st, state, date)
setkey(fe, date)
qs[, mw_state := st[.(qs$state, qs$qend), mw, roll = TRUE]]
qs[, mw_fed := fe[.(qs$qend), fed, roll = TRUE]]
qs[, mw_eff := pmax(mw_state, mw_fed, na.rm = TRUE)]
qs[, yq := year * 10L + qtr]
setkey(qs, state, year, qtr)
qs[, mw_eff_l4 := shift(mw_eff, 4L), by = state]
qs[, dlmw := 100 * (log(mw_eff) - log(mw_eff_l4))]
qs[, dlmw_l := shift(dlmw, 4L), by = state]
cat(
    'effective minimum wage, state-quarters:',
    nrow(qs),
    '| missing:',
    qs[year >= 2005, sum(is.na(dlmw))],
    '\n'
)
print(
    qs[year >= 2005, .(
        mean_mw = mean(mw_eff),
        sh_change = mean(dlmw > 0),
        mean_dlmw = mean(dlmw)
    ), by = year]
)

d <- f1_gate(names(YS))
d <- merge(
    d,
    qs[, .(state, yq, dlmw, dlmw_l, mw_eff)],
    by = c('state', 'yq'),
    all.x = TRUE
)
cat(
    'gate rows:',
    nrow(d),
    '| with minimum wage:',
    d[!is.na(dlmw), .N],
    "| state's minimum wage changed t-4 -> t:",
    d[dlmw != 0, .N],
    '\n'
)


# 1. Minimum wages: balance, control, exclusion --------------------------------

bal <- feols(dlmw ~ z | yq, d, cluster = ~state)
cat('\nbalance: d log effective minimum wage (x100) on z\n')
print(coeftable(bal))

fit <- function(dd, rhs = '1', lab) {
    m <- feols(
        as.formula(
            sprintf(
                'c(%s) ~ %s | yq | infl_exposure ~ z',
                paste(names(YS), collapse = ', '),
                rhs
            )
        ),
        dd,
        cluster = ~state,
        notes = FALSE
    )
    rbindlist(
        lapply(
            seq_along(YS),
            function(j) {
                ct <- coeftable(m[[j]])
                data.table(
                    spec = lab,
                    outcome = names(YS)[j],
                    b = ct['fit_infl_exposure', 1],
                    se = ct['fit_infl_exposure', 2],
                    n = nobs(m[[j]])
                )
            }
        )
    )
}
mw <- rbind(
    fit(d, lab = 'Headline (gate)'),
    fit(
        d[!is.na(dlmw_l)],
        'dlmw + dlmw_l',
        '+ minimum-wage change, current and lagged'
    ),
    fit(d[dlmw == 0], lab = 'Drop if state minimum wage changed t-4 to t'),
    fit(
        d[dlmw == 0 & (dlmw_l == 0 | is.na(dlmw_l))],
        lab = 'Drop if it changed t-8 to t'
    )
)
print(mw, digits = 6)
# Must reproduce the headline saved by 01_main.R
stopifnot(
    abs(
        mw[spec == 'Headline (gate)' & outcome == 'd_lkpu', b] -
            headline_estimate()
    ) < 1e-6
)


# 2. Leave-one-state-out jackknife (delete-one-cluster) ------------------------

states <- sort(unique(d$state))
G <- length(states)
jk <- rbindlist(
    lapply(
        states,
        function(s) {
            m <- feols(
                as.formula(
                    sprintf(
                        'c(%s) ~ 1 | yq | infl_exposure ~ z',
                        paste(names(YS), collapse = ', ')
                    )
                ),
                d,
                subset = d$state != s,
                notes = FALSE,
                vcov = 'iid'
            )
            data.table(
                state = s,
                outcome = names(YS),
                b = vapply(
                    seq_along(YS),
                    function(j) {
                        coef(m[[j]])[['fit_infl_exposure']]
                    },
                    0
                )
            )
        }
    )
)
hd <- mw[spec == 'Headline (gate)']
jks <- jk[, .(
    b_min = min(b),
    b_max = max(b),
    se_jk = sqrt((G - 1) / G * sum((b - mean(b))^2))
), by = outcome]
jks <- merge(hd[, .(outcome, b, se_cl = se)], jks, by = 'outcome')
jks[, `:=`(
    lo_jk = b - qt(.975, G - 1) * se_jk,
    hi_jk = b + qt(.975, G - 1) * se_jk
)]
cat('\nleave-one-state-out jackknife (G =', G, ')\n')
print(jks, digits = 6)
save_results(
    '18_methods_audit',
    mw = mw,
    jks = jks,
    YS = YS,
    mw_balance = data.table(
        b = coef(bal)[['z']],
        se = se(bal)[['z']],
        p = pvalue(bal)[['z']]
    )
)
print(sessionInfo())
