# Fig. S5: the national year-on-year change in the price per calorie of fixed
# products, by tier of each module's calories (cheapest 25%, middle 50%, dearest
# 25%).

rm(list = ls())

source('src/lib/format.R')

tr <- load_results('08_mechanism')$tr
tr # one row per quarter: price index of each tier, a year earlier = 1

tiers <- c(cheap = 'Cheapest 25%', mid = 'Middle 50%', prem = 'Dearest 25%')
tl <- melt(
    tr[, .(date, cheap, mid, prem)],
    id.vars = 'date',
    variable.factor = FALSE
)
tl[, `:=`(
    value = 100 * (value - 1),
    tier = factor(tiers[variable], levels = tiers)
)]
stopifnot(!anyNA(tl$tier))

g <- ggplot(
    tl,
    aes(date, value, colour = tier, linetype = tier)
) +
    geom_hline(yintercept = 0, colour = '#8C8C8C', linewidth = pt2lw(0.5)) +
    geom_line(linewidth = pt2lw(0.75), na.rm = TRUE) +
    scale_colour_manual(
        values = c('#2166AC', '#4D4D4D', '#B2182B'),
        name = NULL
    ) +
    scale_linetype_manual(values = c('solid', '22', 'solid'), name = NULL) +
    scale_x_date(expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(
        labels = scales::label_number(
            accuracy = 1,
            suffix = '%',
            style_negative = 'minus'
        )
    ) +
    labs(x = NULL, y = 'Year-on-year change in\nprice per calorie') +
    theme_classic(base_size = 7) +
    theme(
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin = margin(0, 0, 0, 0),
        legend.key.width = grid::unit(5, 'mm')
    )

g

save_fig(g, 'cheapflation', w = 114, h = 70) # 1.5 columns: one time series
