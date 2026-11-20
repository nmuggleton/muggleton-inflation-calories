# Is the headline driven by a few states, years, extreme shocks, or outlying
# outcomes?

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999, datatable.print.nrows = 1000)

d <- rob_load()
ys <- ROB_YS
fml <- as.formula(
    sprintf(
        'c(%s) ~ 1 | yq | infl_exposure ~ z',
        paste(names(ys), collapse = ', ')
    )
)
fit <- function(dd, spec, group = NA_character_) {
    m <- feols(fml, dd, cluster = ~state, notes = FALSE, warn = FALSE)
    rbindlist(
        lapply(
            seq_along(ys),
            function(k) {
                data.table(
                    spec = spec,
                    group = group,
                    outcome = names(ys)[k],
                    b = coef(m[[k]])[[1]],
                    se = se(m[[k]])[[1]],
                    n = nobs(m[[k]]),
                    f1 = fitstat(m[[k]], ~ivf1)[[1]]$stat
                )
            }
        )
    )
}
head_r <- fit(d, 'Headline')
# Must reproduce the headline saved by 01_main.R
stopifnot(abs(head_r[outcome == 'd_lkpu', b] - headline_estimate()) < 1e-6)

# (a) leave-one-out ------------------------------------------------------------
loso <- rbindlist(
    lapply(
        sort(unique(d$state)),
        function(s) {
            fit(d[state != s], 'Leave one state out', as.character(s))
        }
    )
)
loyo <- rbindlist(
    lapply(
        sort(unique(d$year)),
        function(y) {
            fit(d[year != y], 'Leave one year out', as.character(y))
        }
    )
)
loo_sum <- rbind(loso, loyo)[, .(
    min = min(b),
    max = max(b),
    which_min = group[which.min(b)],
    which_max = group[which.max(b)],
    share_sig = mean(abs(b / se) > 1.96)
), by = .(spec, outcome)]
print(loo_sum)

# (b) regime windows -----------------------------------------------------------
# The annual mean of the instrument picks out the high-inflation years
ym <- d[, .(z_mean = mean(z)), keyby = year]
print(ym)
hi <- ym[z_mean > median(z_mean), year]
cat('high-inflation years (above-median mean z):', hi, '\n')
win <- rbind(
    fit(d[!year %in% 2007:2009], 'Drop 2007-09'),
    fit(d[year <= 2012], '2005-12 only'),
    fit(d[year >= 2013], sprintf('2013-%02d only', END_YEAR %% 100)),
    fit(d[year %in% hi], 'High-inflation years'),
    fit(d[!year %in% hi], 'Low-inflation years')
)

# (c) support ------------------------------------------------------------------
cells <- unique(d[, .(state_q, yq, state, z_sq, n_sq)])
cells[, z_dev := z_sq - mean(z_sq), by = yq]
qs <- quantile(cells$z_dev, c(.05, .95))
cells[, trim := z_dev < qs[[1]] | z_dev > qs[[2]]]
d[cells, `:=`(trim = i.trim), on = 'state_q']
st_n <- d[, .(hh = uniqueN(household_code)), keyby = state]
big <- st_n[hh >= median(hh), state]
cat(
    'cells trimmed:',
    sum(cells$trim),
    'of',
    nrow(cells),
    '| cells < 200 households:',
    sum(cells$n_sq < 200),
    '| large states:',
    length(big),
    '\n'
)
sup <- rbind(
    fit(d[trim == FALSE], 'Trim 5% tails of state-quarter shock'),
    fit(d[n_sq >= 200], 'State-quarter cells >= 200 households'),
    fit(d[state %in% big], 'Large states (above-median households)')
)

# (d) winsorised outcomes ------------------------------------------------------
dw <- copy(d)
for (v in names(ys)) {
    q <- quantile(dw[[v]], c(.01, .99), na.rm = TRUE)
    set(dw, j = v, value = pmin(pmax(dw[[v]], q[[1]]), q[[2]]))
}
wins <- fit(dw, 'Outcomes winsorized 1/99')
rm(dw)
invisible(gc())

# (e) state-quarter influence (kcal/$), FWL after quarter demeaning ------------
s <- d[is.finite(d_lkpu)]
m <- demean(
    X = as.matrix(s[, .(y = d_lkpu, x = infl_exposure, z)]),
    f = s[, .(yq)]
)
cs <- data.table(
    state_q = s$state_q,
    A = m[, 3] * m[, 1],
    C = m[, 3] * m[, 2]
)[
    ,
    .(A = sum(A), C = sum(C)),
    by = state_q
]
b_all <- sum(cs$A) / sum(cs$C)
cs[, b_drop := (sum(cs$A) - A) / (sum(cs$C) - C)]
cs[, infl := b_drop - b_all]
setorder(cs, -infl)
cat(
    'headline from cell sums:',
    b_all,
    '| max |change| from dropping one',
    'state-quarter:',
    max(abs(cs$infl)),
    '\n'
)
print(head(cs[order(-abs(infl))], 10))
# Adversarial: drop the 1% of cells whose removal moves the estimate most toward
# 0 (i.e. the cells most supportive of a large negative response)
k1 <- ceiling(.01 * nrow(cs))
top <- cs[order(-infl)][seq_len(k1), state_q]
inf_r <- fit(
    d[!state_q %in% top],
    'Drop 1% of state-quarters most supportive of headline'
)

all_r <- rbind(head_r, win, sup, wins, inf_r)
print(all_r)

save_results(
    '11_leverage',
    all_r = all_r,
    loo_sum = loo_sum,
    loso = loso,
    loyo = loyo,
    year_z = ym
)
print(sessionInfo())
