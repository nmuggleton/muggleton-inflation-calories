# Fig. S6: responses to the persistent (annual state) and transient
# (within-state-year) components of exposure.

rm(list = ls())

source('src/lib/format.R')

per <- load_results('07_dynamics')$per
per # one row per component x outcome: 2SLS estimate and SE

core <- c('d_lkpu', 'd_lspend', 'd_lkcal')
comp <- c(
    'Annual state component (z_ann)' = 'Persistent: annual state component',
    'Within-state-year deviation (z_dev)' = 'Transient: within-year deviation'
)
fp <- per[spec %in% names(comp) & outcome %in% core]
fp[, `:=`(
    spec = factor(comp[spec], levels = comp),
    outcome = factor(
        c(
            d_lkpu = 'Calories per dollar', d_lspend = 'Real spending',
            d_lkcal = 'Calories bought'
        )[outcome],
        levels = c('Calories per dollar', 'Real spending', 'Calories bought')
    )
)]
stopifnot(nrow(fp) == 6, !anyNA(fp$spec), !anyNA(fp$outcome))

dodge <- position_dodge(width = 0.4)
g <- ggplot(
    fp,
    aes(outcome, b, fill = spec)
) +
    geom_hline(yintercept = 0, colour = '#8C8C8C', linewidth = pt2lw(0.5)) +
    geom_errorbar(
        aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se),
        width = 0.1,
        linewidth = pt2lw(0.5),
        position = dodge
    ) +
    geom_point(pch = 21, colour = 'black', size = 1.5, position = dodge) +
    scale_fill_manual(values = c('#4D4D4D', 'white'), name = NULL) +
    labs(x = NULL, y = '2SLS effect of a 1-point\nrise in exposure') +
    guides(fill = guide_legend(ncol = 1)) + # two rows: one column wide
    theme_classic(base_size = 7) +
    theme(
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin = margin(0, 0, 0, 0)
    )

g

save_fig(g, 'persistence', w = 87, h = 65) # one column: three outcomes
