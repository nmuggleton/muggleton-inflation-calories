# Do poorer households respond less to short-lived price changes? One mechanism
# for the fall in calories under within-year deviations of exposure
# (07_dynamics.R, step 2) is purchase timing: buying ahead when prices are
# temporarily low.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)

outcomes <- c('d_lkcal', 'd_lspend', 'd_lkpu')
d <- het_load(c(outcomes, 'fpl_bin'))
cat('sample flow: gate', nrow(d), 'household-quarters\n')

# (1) persistent and transient parts of the instrument -------------------------
# On the full sample, before any household is dropped, as in 07_dynamics.R
d[, state_y := paste(state, year)]
d[, z_ann := mean(z), by = state_y]
d[, z_dev := z - z_ann]

d <- d[!is.na(fpl_bin)]
cat('  with base-quarter income:', nrow(d), '\n')
d[, low := fpl_bin %in% c('<=100%', '100-130%')]
print(d[, .N, keyby = .(fpl_bin, low)])

# (2) response to each part, by income group -----------------------------------
# Separate 2SLS regressions by income group, outcome and part of the instrument
rows <- list()
for (g in levels(d$fpl_bin)) {
    dg <- d[fpl_bin == g]
    for (y in outcomes) {
        for (inst in c('z_ann', 'z_dev')) {
            m <- feols(
                as.formula(
                    sprintf('%s ~ 1 | yq | infl_exposure ~ %s', y, inst)
                ),
                dg,
                cluster = ~state
            )
            rows[[length(rows) + 1]] <- data.table(
                group = g,
                outcome = y,
                part = inst,
                b = coef(m)[[1]],
                se = se(m)[[1]],
                n = nobs(m),
                f = fitstat(m, 'ivwald1')[[1]]$stat
            )
        }
    }
}
by_group <- rbindlist(rows)
rm(rows, dg, m)
print(
    dcast(by_group, group + part ~ outcome, value.var = 'b'),
    digits = 3
)
print(
    dcast(by_group, group + part ~ outcome, value.var = 'se'),
    digits = 3
)

# (3) low-income against other households --------------------------------------
# One stacked model per outcome and part: exposure and the instrument are
# interacted with the two income groups, with group-specific quarter effects
d[, `:=`(
    x_low = infl_exposure * low,
    x_oth = infl_exposure * !low
)]
two_group <- rbindlist(
    lapply(
        outcomes,
        function(y) {
            rbindlist(
                lapply(
                    c('z_ann', 'z_dev'),
                    function(inst) {
                        d[, `:=`(
                            i_low = get(inst) * low,
                            i_oth = get(inst) * !low
                        )]
                        m <- feols(
                            as.formula(
                                sprintf(
                                    paste(
                                        '%s ~ 1 | yq^low |',
                                        'x_low + x_oth ~ i_low + i_oth'
                                    ),
                                    y
                                )
                            ),
                            d,
                            cluster = ~state
                        )
                        b <- coef(m)
                        v <- vcov(m)
                        diff <- b[['fit_x_low']] - b[['fit_x_oth']]
                        se_diff <- sqrt(
                            v['fit_x_low', 'fit_x_low'] +
                                v['fit_x_oth', 'fit_x_oth'] -
                                2 * v['fit_x_low', 'fit_x_oth']
                        )
                        data.table(
                            outcome = y,
                            part = inst,
                            b_low = b[['fit_x_low']],
                            se_low = se(m)[['fit_x_low']],
                            b_oth = b[['fit_x_oth']],
                            se_oth = se(m)[['fit_x_oth']],
                            diff = diff,
                            se_diff = se_diff,
                            p = 2 * pt(-abs(diff / se_diff), 48)
                        )
                    }
                )
            )
        }
    )
)
print(two_group, digits = 3)

save_results(
    '29_persistence_income',
    by_group = by_group,
    two_group = two_group,
    flow = data.table(gate = F1_N, with_income = nrow(d))
)
