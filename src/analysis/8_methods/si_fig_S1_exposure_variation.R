# Fig. S1: how far state exposure inflation departs from the national rate,
# quarter by quarter.

rm(list = ls())

source('src/lib/format.R')

q <- load_results('03_descriptive')$q
# One row per quarter: percentiles across states of state minus national
# exposure
q

# 2020 is outside the estimation sample, so its rows are NA and the bands and
# line break there
gap <- data.table(year = 2020L, qtr = 1:4)
gap[, yq := 10L * year + qtr]
q <- rbind(q, gap, fill = TRUE)
q[, date := year + (qtr - 0.5) / 4]
setorder(q, date)

# A legend rather than text on the plot: the line and the two bands, in one row
# on top
lab <- c(mid = '90% of states', wide = '95% of states')
g <- ggplot(
    q,
    aes(date)
) +
    geom_ribbon(
        aes(ymin = p025, ymax = p975, fill = '95% of states'),
        na.rm = TRUE
    ) +
    geom_ribbon(
        aes(ymin = p05, ymax = p95, fill = '90% of states'),
        na.rm = TRUE
    ) +
    geom_hline(
        yintercept = 0,
        colour = 'black',
        linewidth = pt2lw(0.5),
        linetype = '22'
    ) +
    geom_line(
        aes(y = p50, colour = 'Median state'),
        linewidth = pt2lw(0.75),
        na.rm = TRUE
    ) +
    scale_fill_manual(
        values = setNames(c('#8C8C8C', '#BDBDBD'), unname(lab)),
        breaks = unname(lab),
        name = NULL
    ) +
    scale_colour_manual(values = c(`Median state` = 'black'), name = NULL) +
    guides(
        colour = guide_legend(order = 1),
        fill = guide_legend(order = 2)
    ) +
    scale_x_continuous(
        breaks = seq(2006, END_YEAR, 6),
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
        breaks = seq(-4, 4, 2),
        labels = scales::label_number(
            accuracy = 1,
            suffix = '%',
            style_negative = 'minus'
        )
    ) +
    labs(x = NULL, y = 'State minus national\nexposure inflation') +
    theme_classic(base_size = 7) +
    theme(
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin = margin(0, 0, 0, 0),
        legend.key.size = grid::unit(3, 'mm')
    )

g

save_fig(g, 'exposure_variation', w = 114, h = 70) # 1.5 columns: one time series
