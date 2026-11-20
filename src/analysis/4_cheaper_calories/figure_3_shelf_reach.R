# Figure 3 (fig:shelf): (A) the savings available within households' own product
# modules at the state shelf; (B) the ladder of reach: the same saving on
# shelves widening from the retailer used to the zip3, county and state, pooled
# or best single retailer; (C) by real income. B and C use panel-observed
# shelves (every store type, store brands included; validated against Retail
# Scanner shelves).

rm(list = ls())

library(patchwork)
source('src/lib/format.R')

mech <- load_results('08_mechanism')
pr <- load_results('22_panel_reach')

dens <- copy(mech$dens)
dens # density of the saving at fixed calories, one curve per series

shock <- mech$mean_shock # mean yearly basket price increase (%)
shock

reach <- pr$reach[quantile == 25 & ladder != 'reference']
reach # mean saving (% of common-base spend), one row per ladder x rung

reach_ci <- pr$reach_ci
reach_ci # 95% CI on each rung's mean, SEs clustered on household

# Real base-year income (2019 dollars), chosen over nominal brackets, whose
# fixed cut-offs drift with prices, and over the poverty-line ratio
inc <- pr$inc_def[definition == 'real2019']
inc # mean p25 saving and 95% CI by real income, three rungs

pr$flow # household-quarters behind B and C (last row)

# data -------------------------------------------------------------------------
# Panel a: one key for both curves and the price-rise line
key_a <- c(
    'Median',
    '25th percentile',
    sprintf('Price rise (%.1f%%)', shock)
)
dens[, lab := factor(
    fifelse(series == 'State median price per calorie', key_a[1], key_a[2]),
    levels = key_a
)]
stopifnot(
    dens[, all(
        series %in% c(
            'State median price per calorie',
            'State 25th percentile price per calorie'
        )
    )]
)
shock_line <- data.table(x = shock, lab = factor(key_a[3], levels = key_a))

# Panel b: the rungs of reach from nearest (top) to widest (bottom); the
# retailer used is the same in both ladders, so it is drawn once
reach_lab <- c(
    ret = 'Retailer used',
    setx = 'Own retailers',
    zip3 = 'Zip3', bz3 = 'Zip3',
    cty = 'County', bcty = 'County',
    st = 'State', bst = 'State'
)
reach[, level := factor(
    reach_lab[rung],
    levels = rev(c('Retailer used', 'Own retailers', 'Zip3', 'County', 'State'))
)]
reach[, series := fifelse(
    rung == 'ret',
    'Both',
    fifelse(ladder == 'pooled', 'All stores in area', 'Best single retailer')
)]
reach <- unique(reach, by = c('level', 'series'))
reach <- merge(reach, reach_ci[, .(rung, lo, hi)], by = 'rung')
stopifnot(nrow(reach) == 8, !anyNA(reach$lo))
ret_used <- reach[rung == 'ret', mean]

# Panel c: three rungs by income
inc[, series := factor(
    series,
    levels = c('best_zip3', 'state', 'retailer'),
    labels = c('Best retailer in zip3', 'All stores in state', 'Retailer used')
)]
inc[, inc := factor(group, levels = unique(group))]
stopifnot(nrow(inc) == 15, !anyNA(inc$inc))

# The figure data as CSV, for use outside R (see
# manuscript/figures/data/README.md)
FIG_DATA <- 'manuscript/figures/data'
dir.create(FIG_DATA, showWarnings = FALSE)
fwrite(mech$dens, file.path(FIG_DATA, 'fig3_density.csv'))
fwrite(mech$hist, file.path(FIG_DATA, 'fig3_hist.csv'))
fwrite(
    reach[, .(ladder, rung, level, series, mean, lo, hi, median, n)],
    file.path(FIG_DATA, 'fig3_reach.csv')
)
fwrite(inc, file.path(FIG_DATA, 'fig3_reach_income.csv'))

# Figure 3 ---------------------------------------------------------------------
# Full width, three equal panels; styled as Figures 1-3 (theme_classic, 7 pt).
panel_a <- ggplot(
    dens,
    aes(x, density, linetype = lab, colour = lab)
) +
    geom_vline(
        data = shock_line,
        aes(xintercept = x, linetype = lab, colour = lab),
        linewidth = pt2lw(0.5),
        key_glyph = 'path' # a horizontal line in the key, as the curves
    ) +
    geom_line(linewidth = pt2lw(0.75)) +
    scale_linetype_manual(
        values = setNames(c('solid', '42', '22'), key_a),
        breaks = key_a,
        name = NULL
    ) +
    scale_colour_manual(
        values = setNames(c('#4D4D4D', '#4D4D4D', '#B2182B'), key_a),
        breaks = key_a,
        name = NULL
    ) +
    scale_x_continuous(
        breaks = seq(0, 80, 20),
        labels = scales::label_number(suffix = '%'),
        expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
        x = 'Saving at fixed calories\n(share of base spending)',
        y = 'Density',
        tag = 'A'
    ) +
    theme_classic(base_size = 7) +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'inside',
        legend.position.inside = c(1, 1), # top right
        legend.justification = c(1, 1),
        legend.key.width = grid::unit(6, 'mm'),
        legend.key.height = grid::unit(3, 'mm'),
        legend.text = element_text(size = 6),
        legend.background = element_blank()
    )

panel_a

cols <- c(
    'All stores in area' = '#1F4E79',
    'Best single retailer' = '#C8553D',
    'Both' = '#4D4D4D'
)
nudge <- c('All stores in area' = 0.15, 'Best single retailer' = -0.15, 'Both' = 0)
reach[, y := as.numeric(level) + nudge[series]]

panel_b <- ggplot(
    reach,
    aes(x = mean, y = y, colour = series)
) +
    geom_vline(
        xintercept = ret_used,
        colour = '#8C8C8C',
        linewidth = pt2lw(0.5),
        linetype = '22'
    ) +
    geom_segment(
        data = reach[series != 'Both'],
        aes(x = ret_used, xend = mean, yend = y),
        linewidth = pt2lw(1.5)
    ) +
    geom_errorbar( # CIs as Figure 2: caps 0.1 of the row spacing, 0.5 pt
        aes(xmin = lo, xmax = hi),
        orientation = 'y',
        width = 0.1,
        linewidth = pt2lw(0.5)
    ) +
    geom_point(
        data = reach[series != 'Both'],
        aes(x = ret_used), # each bar's start: the retailer used
        fill = cols[['Both']],
        colour = 'black',
        pch = 21,
        size = 1.5
    ) +
    geom_point(
        aes(fill = series), # points as Figure 2: series fill, black outline
        colour = 'black',
        pch = 21,
        size = 1.5
    ) +
    scale_colour_manual(
        values = cols,
        breaks = c('Both', 'All stores in area', 'Best single retailer'),
        labels = c('Retailer used', 'All stores in area', 'Best single retailer'),
        name = NULL
    ) +
    scale_fill_manual(
        values = cols,
        breaks = c('Both', 'All stores in area', 'Best single retailer'),
        labels = c('Retailer used', 'All stores in area', 'Best single retailer'),
        name = NULL
    ) +
    scale_y_continuous(
        breaks = seq_along(levels(reach$level)),
        labels = levels(reach$level),
        expand = expansion(add = 0.4)
    ) +
    scale_x_continuous(
        limits = c(25, 45),
        breaks = seq(25, 45, 5),
        labels = scales::label_number(suffix = '%'),
        expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
        x = 'Saving at fixed calories\n(share of base spending)',
        y = NULL,
        tag = 'B'
    ) +
    theme_classic(base_size = 7) +
    theme(
        axis.ticks.y = element_blank(),
        legend.position = 'inside',
        legend.position.inside = c(1, 1), # top right, beside the retailer row
        legend.justification = c(1, 1),
        legend.key.width = grid::unit(4, 'mm'),
        legend.key.height = grid::unit(3, 'mm'),
        legend.text = element_text(size = 6),
        legend.background = element_blank()
    )

panel_b

cols_inc <- c(
    'Best retailer in zip3' = '#C8553D',
    'All stores in state' = '#1F4E79',
    'Retailer used' = '#4D4D4D'
)

panel_c <- ggplot(
    inc,
    aes(x = inc, y = mean, colour = series, group = series)
) +
    geom_line(linewidth = pt2lw(0.75)) +
    geom_errorbar( # as Figure 2
        aes(ymin = lo, ymax = hi),
        width = 0.1,
        linewidth = pt2lw(0.5)
    ) +
    geom_point(
        aes(fill = series), # points as Figure 2: series fill, black outline
        colour = 'black',
        pch = 21,
        size = 1.5
    ) +
    # No key: the colours are B's (grey for the retailer used, navy pooled, red
    # best retailer); the caption names the three rungs
    scale_colour_manual(values = cols_inc, guide = 'none') +
    scale_fill_manual(values = cols_inc, guide = 'none') +
    scale_x_discrete(expand = expansion(add = 0.3)) +
    scale_y_continuous(
        limits = c(25, 45),
        breaks = seq(25, 45, 5),
        labels = scales::label_number(suffix = '%')
    ) +
    labs(
        x = 'Household income, base year (2019 $)',
        y = 'Saving at fixed calories',
        tag = 'C'
    ) +
    theme_classic(base_size = 7)

panel_c

# Panel tags in bold upper case, as in Figures 1-2 (theme_print: bold tags, top
# left)
fig3 <- panel_a + panel_b + panel_c + plot_layout(widths = c(1, 1, 1)) &
    theme(plot.tag = element_text(face = 'bold'))

fig3

# Each plot is three-quarters as tall as a third of the two-column width, plus 6
# mm for the two-line x-axis titles
fig_h <- 0.75 * fig_width[['double']] / 3 + 6

save_print(
    fig3,
    'manuscript/figures/shelf_reach.pdf',
    width = 178,
    height = fig_h
)
