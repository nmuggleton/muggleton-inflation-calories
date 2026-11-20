# Fig. S3: the OLS and 2SLS calories-per-dollar responses plotted by the number
# of products in the base basket.

rm(list = ls())

source('src/lib/format.R')

main <- load_results('01_main')
rel <- copy(main$reliability)
rel # one row per basket size x estimator: estimate, SE, N

lev <- c('OLS, household exposure', '2SLS, state instrument')
rel[, est_b := est]
rel[, items := factor(items, levels = main$item_levels)]
rel[, est := factor(
    fifelse(startsWith(est_type, 'OLS'), lev[1], lev[2]),
    levels = lev
)]
stopifnot(!anyNA(rel$est), !anyNA(rel$items))

dodge <- position_dodge(width = 0.4)
g <- ggplot(
    rel,
    aes(items, est_b, fill = est)
) +
    geom_hline(yintercept = 0, colour = '#8C8C8C', linewidth = pt2lw(0.5)) +
    geom_hline(
        yintercept = -1,
        colour = '#8C8C8C',
        linewidth = pt2lw(0.5),
        linetype = '22'
    ) +
    geom_errorbar(
        aes(ymin = est_b - 1.96 * se, ymax = est_b + 1.96 * se),
        width = 0.1,
        linewidth = pt2lw(0.5),
        position = dodge
    ) +
    geom_point(pch = 21, colour = 'black', size = 1.5, position = dodge) +
    scale_fill_manual(values = c('white', '#4D4D4D'), name = NULL) +
    labs(
        x = 'Products in the base basket',
        y = 'Calories-per-dollar response\n(per point of exposure)'
    ) +
    theme_classic(base_size = 7) +
    theme(
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin = margin(0, 0, 0, 0)
    )

g

save_fig(g, 'reliability', w = 87, h = 65) # one column: five groups
