# Fig. S7: 2SLS estimates by year of the target quarter, for calories per
# dollar, real spending and calories bought.

rm(list = ls())

source('src/lib/format.R')

ry <- copy(load_results('13_sample')$ry)
ry # one row per year x outcome: 2SLS estimate and SE

out <- c(
    d_lkpu = 'Calories per dollar',
    d_lspend = 'Real spending',
    d_lkcal = 'Calories bought'
)
ry <- ry[outcome %in% names(out)]
ry[, `:=`(series = factor(out[outcome], levels = out), yr = as.integer(spec))]
stopifnot(!anyNA(ry$yr), nrow(ry) == 3 * uniqueN(ry$yr))

dodge <- position_dodge(width = 0.6)
g <- ggplot(
    ry,
    aes(yr, b, fill = series)
) +
    geom_hline(yintercept = 0, colour = '#8C8C8C', linewidth = pt2lw(0.5)) +
    geom_hline(
        yintercept = -1,
        colour = '#8C8C8C',
        linewidth = pt2lw(0.5),
        linetype = '22'
    ) +
    geom_errorbar(
        aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se),
        width = 0.3,
        linewidth = pt2lw(0.5),
        position = dodge
    ) +
    geom_point(pch = 21, colour = 'black', size = 1.2, position = dodge) +
    scale_fill_manual(
        values = c('#4D4D4D', '#B2182B', '#2166AC'),
        name = NULL
    ) +
    scale_x_continuous(
        breaks = seq(2006, END_YEAR, 3),
        expand = expansion(mult = 0.02)
    ) +
    labs(x = NULL, y = '2SLS effect of a 1-point\nrise in exposure') +
    theme_classic(base_size = 7) +
    theme(
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin = margin(0, 0, 0, 0)
    )

g

save_fig(g, 'by_year_sample', w = 178, h = 70) # SI text width (505.7 pt)
