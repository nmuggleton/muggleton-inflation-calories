# Introduction figure: households face different inflation, and how the
# instrument is built. (C) is part illustrative: the same baskets inflate faster
# in the highest-inflation state (TN) than the lowest (NJ), and the state
# instrument (real) tracks them.

rm(list = ls())

library(patchwork)
source('src/lib/format.R')

quarter <- 20214 # 2021 Q4, the same year as (A)

# state FIPS code -> postal abbreviation, for labelling states in (D)
state_abb <- c(
    `1` = 'AL', `4` = 'AZ', `5` = 'AR', `6` = 'CA', `8` = 'CO', `9` = 'CT',
    `10` = 'DE',
    `11` = 'DC',
    `12` = 'FL',
    `13` = 'GA',
    `16` = 'ID',
    `17` = 'IL',
    `18` = 'IN',
    `19` = 'IA',
    `20` = 'KS',
    `21` = 'KY',
    `22` = 'LA',
    `23` = 'ME',
    `24` = 'MD',
    `25` = 'MA',
    `26` = 'MI',
    `27` = 'MN',
    `28` = 'MS',
    `29` = 'MO',
    `30` = 'MT',
    `31` = 'NE',
    `32` = 'NV',
    `33` = 'NH',
    `34` = 'NJ',
    `35` = 'NM',
    `36` = 'NY',
    `37` = 'NC',
    `38` = 'ND',
    `39` = 'OH',
    `40` = 'OK',
    `41` = 'OR',
    `42` = 'PA',
    `44` = 'RI',
    `45` = 'SC',
    `46` = 'SD',
    `47` = 'TN',
    `48` = 'TX',
    `49` = 'UT',
    `50` = 'VT',
    `51` = 'VA',
    `53` = 'WA',
    `54` = 'WV',
    `55` = 'WI',
    `56` = 'WY'
)

# C: the same baskets in two states ------------------------------------------
# Each point is a household: the basket it bought last year (shape), priced at
# other households' prices in its state (fill), gives its exposure (y).
state_cols <- c(Tennessee = '#4D4D4D', `New Jersey` = 'white')
ends <- load_results('02_cells')$cells[
    yq == quarter & state %in% c(47, 34),
    .(state = fifelse(state == 47, 'Tennessee', 'New Jersey'), z = x)
]
ends # TN 7.6%, NJ 3.6% in 2021 Q4

states_c <- rbind(
    ends[, .(basket = 'Bread, milk, eggs', z, t2 = z - 1), by = state],
    ends[, .(basket = 'Chicken, fruit', z, t2 = z + 1), by = state]
)
# own-price inflation, either side of exposure (no systematic direction)
states_c[, t1 := t2 + fcase(
    state == 'New Jersey' & basket == 'Bread, milk, eggs', -2.2,
    state == 'New Jersey' & basket == 'Chicken, fruit', 2.4,
    state == 'Tennessee' & basket == 'Bread, milk, eggs', -2.6,
    default = 2.4
)]
# baskets nudged either side of the orange line so the points show (fixed
# offsets, not random, so the arrows and grey lines stay attached); each arrow
# runs straight up or down from cross to point, stopping short of it
states_c[, xj := z + fifelse(basket == 'Bread, milk, eggs', -0.35, 0.35)]
states_c[, t2_end := t2 - 0.3 * sign(t2 - t1)]
states_c[, state := factor(state, levels = c('Tennessee', 'New Jersey'))]
states_c

panel_a <- ggplot(
    states_c,
    aes(xj, t2)
) +
    geom_abline(
        slope = 1,
        intercept = 0,
        linetype = '22',
        colour = '#8C8C8C',
        linewidth = pt2lw(0.5)
    ) +
    geom_line(
        aes(group = basket),
        colour = '#8C8C8C',
        linewidth = pt2lw(0.75)
    ) +
    geom_segment(
        aes(xend = xj, y = t1, yend = t2_end),
        colour = '#009E73',
        linewidth = pt2lw(0.5),
        arrow = grid::arrow(length = grid::unit(1, 'mm'), type = 'closed')
    ) +
    geom_point(
        aes(y = t1, shape = basket, fill = state, colour = 'paid'),
        size = 2,
        stroke = 0.5
    ) +
    geom_point(
        aes(shape = basket, fill = state, colour = 'exposure'),
        size = 2,
        stroke = 0.5
    ) +
    # each state's instrument, shared by its households
    geom_point(
        data = ends,
        aes(z, z, fill = state, colour = 'instrument'),
        shape = 21,
        size = 2.5,
        stroke = 0.5
    ) +
    # circles are the state averages, as in (D); baskets are diamonds and
    # triangles
    scale_shape_manual(values = c(23, 24), name = NULL) +
    scale_colour_manual(
        values = c(paid = '#009E73', exposure = 'black', instrument = 'black'),
        breaks = c('paid', 'exposure', 'instrument'),
        labels = c(
            'Price paid by household',
            "Household's exposure",
            'State instrument'
        ),
        name = NULL
    ) +
    scale_fill_manual(
        values = state_cols,
        labels = c('Tennessee (TN)', 'New Jersey (NJ)'), # as labelled in (D)
        name = NULL
    ) +
    guides(
        fill = guide_legend(
            order = 1,
            override.aes = list(shape = 22, size = 2.5, stroke = 0.5)
        ),
        shape = guide_legend(
            order = 2,
            override.aes = list(fill = 'white', stroke = 0.5)
        ),
        colour = guide_legend(
            order = 3,
            override.aes = list(
                shape = c(22, 22, 21),
                fill = 'white',
                stroke = 0.5
            )
        )
    ) +
    scale_x_continuous(
        breaks = 3:8,
        labels = scales::label_number(suffix = '%')
    ) +
    scale_y_continuous(
        limits = c(0, 11.5),
        breaks = seq(0, 10, 2),
        labels = scales::label_number(suffix = '%')
    ) +
    labs(
        x = "State instrument (others' average)",
        y = "Household's inflation",
        tag = 'C'
    ) +
    theme_classic(base_size = 7) +
    theme(
        plot.title = element_text(size = 7, face = 'bold'),
        legend.position = 'right',
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.y = grid::unit(1, 'mm'),
        legend.key.size = grid::unit(3, 'mm'),
        plot.margin = margin(0, 0, 0, 0)
    )

panel_a

# D: state averages in one quarter (real data) ---------------------------------
# the cell mean of the instrument equals the cell mean of exposure, so each
# point is the average exposure, and the average instrument, in that state
states <- load_results('02_cells')$cells[yq == quarter, .(state, n, x)]
states[, abb := state_abb[as.character(state)]]
states[, end := x == min(x) | x == max(x)] # name the lowest and highest
# TN and NJ coloured as in (C); every other state light grey
states[, col := fcase(
    abb == 'TN',
    'Tennessee',
    abb == 'NJ',
    'New Jersey',
    default = 'other'
)]
stopifnot(states[(end), sort(abb)] == c('NJ', 'TN')) # (C) assumes these ends
states <- states[order(col != 'other', decreasing = TRUE)] # TN and NJ on top
states

panel_b <- ggplot(
    states,
    aes(x, 0)
) +
    geom_point(
        aes(fill = col),
        pch = 21,
        colour = 'black',
        size = 2,
        position = position_jitter(width = 0, height = 0.15, seed = 1)
    ) +
    geom_text(
        data = states[(end)],
        aes(label = sprintf('%s %s%%', abb, fmt_num(x, 1))),
        vjust = -1.6,
        size = pt2size(6)
    ) +
    scale_fill_manual(
        values = c(state_cols, other = '#BDBDBD'),
        guide = 'none'
    ) +
    scale_x_continuous(
        breaks = 3:8,
        labels = scales::label_number(suffix = '%'),
        expand = expansion(mult = 0.08) # room for the end labels
    ) +
    scale_y_continuous(limits = c(-0.3, 0.6), breaks = NULL) +
    labs(
        # title = "Across states, the average rise differs too (2021 Q4)",
        x = "Average rise in last year's basket, by state",
        y = NULL,
        tag = 'D'
    ) +
    theme_classic(base_size = 7) +
    theme(
        plot.title = element_text(size = 7, face = 'bold'),
        axis.line.y = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
    )

panel_b

# A: household inflation against the CPI, the four quarters of 2021 ------------
# Histograms of household exposure (0.25-point bins), with the CPI for food at
# home marked.
desc <- load_results('03_descriptive')
hist_q <- desc$hist_yr
spread <- desc$spread_yr
spread # per quarter: CPI, and the shares below, close to, and above it

close <- 1 # within 1 point of the CPI counts as close (as in 03_descriptive.R)
hist_q[, mid := (bin_lo + bin_hi) / 2]
hist_q[spread, on = 'yq', cpi := i.cpi]
hist_q[, side := fcase(
    mid < cpi - close,
    'below',
    mid > cpi + close,
    'above',
    default = 'close'
)]
side_cols <- c(below = '#2166AC', close = '#BDBDBD', above = '#B2182B')
x_rng <- c(-6, 14) # 2021: at least the middle 98% of households in each quarter

# one small histogram per quarter (four, so a function)
quarter_hist <- function(q) {
    s <- spread[yq == q]
    ggplot(
        hist_q[yq == q],
        aes(mid, 100 * share, fill = side)
    ) +
        geom_col(width = 0.25) +
        geom_vline(xintercept = s$cpi, linewidth = pt2lw(0.75)) +
        annotate(
            'text',
            x = x_rng[1] + 0.5,
            y = Inf,
            # Share below (blue); the caption explains
            label = sprintf('%.0f%%', 100 * s$below),
            hjust = 0,
            vjust = 1.3,
            size = pt2size(6),
            colour = side_cols[['below']]
        ) +
        annotate(
            'text',
            x = x_rng[2] - 0.5,
            y = Inf,
            label = sprintf('%.0f%%', 100 * s$above), # share above (red)
            hjust = 1,
            vjust = 1.3,
            size = pt2size(6),
            colour = side_cols[['above']]
        ) +
        scale_fill_manual(values = side_cols, guide = 'none') +
        scale_x_continuous(
            breaks = seq(-5, 10, 5),
            labels = scales::label_number(
                suffix = '%',
                style_negative = 'minus'
            )
        ) +
        coord_cartesian(xlim = x_rng) + # tails beyond are cut from view
        labs(
            subtitle = sprintf('%s Q%s: CPI %.1f%%', q %/% 10, q %% 10, s$cpi),
            x = "Rise in the price of last year's basket",
            y = NULL
        ) +
        theme_classic(base_size = 7) +
        theme(
            plot.title = element_text(size = 7, face = 'bold'),
            plot.subtitle = element_text(size = 6, margin = margin(b = 0)),
            plot.margin = margin(r = 0, l = 0),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )
}

no_x <- theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
)
quarters <- sort(unique(hist_q$yq))
c_q1 <- quarter_hist(quarters[1]) + no_x +
    labs(tag = 'A')
c_q2 <- quarter_hist(quarters[2]) + no_x
c_q3 <- quarter_hist(quarters[3]) + no_x
c_q4 <- quarter_hist(quarters[4])

# no margin between the four histograms; 3 mm above the first leaves room for
# the tag A little space above each histogram
tight <- theme(plot.margin = margin(1.5, 0, 0, 0, 'mm'))
panel_c <- (c_q1 + theme(plot.margin = margin(3, 0, 0, 0, 'mm'))) /
    (c_q2 + tight) / (c_q3 + tight) / (c_q4 + tight)

panel_c

# B: national average against the CPI, 2005 on ---------------------------------
# no gap: 2020 is included (outside the estimation sample only because of its
# year; shaded)
national <- melt(
    desc$q_all[, .(date = year + (qtr - 0.5) / 4, ours = nat, cpi)],
    id.vars = 'date',
    variable.name = 'series'
)
national[, series := factor(
    series,
    levels = c('ours', 'cpi'),
    labels = c(
        'Households (mean)',
        'CPI, food at home'
    )
)]
r_national <- desc$q_all[, cor(nat, cpi)]
r_national

panel_d <- ggplot(
    national,
    aes(date, value, colour = series, linetype = series)
) +
    annotate(
        'rect',
        xmin = 2020,
        xmax = 2021,
        ymin = -Inf,
        ymax = Inf,
        fill = '#F0F0F0'
    ) +
    geom_hline(yintercept = 0, colour = '#8C8C8C', linewidth = pt2lw(0.5)) +
    geom_line(linewidth = pt2lw(0.75)) +
    annotate(
        'text',
        x = 2005,
        y = Inf,
        label = sprintf('r = %s', sub('^0', '', fmt_num(r_national, 2))),
        hjust = 0,
        vjust = 1.3,
        size = pt2size(6)
    ) +
    scale_colour_manual(values = c('#4D4D4D', 'black'), name = NULL) +
    scale_linetype_manual(values = c('solid', '22'), name = NULL) +
    scale_x_continuous(breaks = c(2006, 2015, END_YEAR)) +
    scale_y_continuous(labels = scales::label_number(
        suffix = '%',
        style_negative = 'minus'
    )) +
    labs(
        x = NULL,
        y = 'Year-on-year change',
        tag = 'B'
    ) +
    theme_classic(base_size = 7) +
    theme(
        plot.title = element_text(size = 7, face = 'bold'),
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin = margin(0, 0, 0, 0),
        legend.key.width = grid::unit(5, 'mm'),
        plot.margin = margin(0, 0, 0, 0)
    )

panel_d

# Figure -----------------------------------------------------------------------
# One column wide: A and B side by side (half width each), then C, then D, so
# the panels are lettered in the order the text cites them. 3 mm above each
# panel leaves room for its tag; 3 mm left of B is the gap from A.
tag_style <- theme(
    plot.tag = element_text(face = 'bold', hjust = 0, vjust = 1),
    plot.tag.location = 'margin', # in the 3 mm top margin, clear of the content
    plot.tag.position = 'topleft'
)
# left 1.94 mm = ggplot's default 5.5 pt, so C's tag lines up with A's and D's
top_gap <- theme(plot.margin = margin(3, 0, 0, 1.94, 'mm'))

panel_c[[1]] <- panel_c[[1]] + tag_style # the tag is on the first histogram
# The caption says what the axis is
panel_c[[4]] <- panel_c[[4]] + theme(axis.title.x = element_blank())
x_c <- range(states_c$xj)
x_c <- x_c + c(-1, 1) * 0.08 * diff(x_c)
# One grid, so the panels line up: the four histograms of A in the left column
# with B beside them, then C with its key (wrapped, so the key sits beside it
# and sets the row height), then D across both columns, its axis running from
# A's left edge to B's right edge.
p_b <- panel_d + tag_style + theme(
    plot.margin = margin(3, 0, 0, 3, 'mm'),
    legend.position = 'none'
)
p_c <- wrap_elements(
    full = panel_a + tag_style +
        coord_cartesian(xlim = x_c, expand = FALSE, clip = 'off') +
        top_gap
)
p_d <- panel_b + tag_style + coord_cartesian(clip = 'off') +
    theme(plot.margin = margin(3, 0, 0, 0, 'mm'))
fig0 <- panel_c[[1]] + panel_c[[2]] + panel_c[[3]] + panel_c[[4]] +
    p_b + p_c + p_d +
    plot_layout(
        design = 'AE\nBE\nCE\nDE\nFF\nGG',
        heights = grid::unit(
            c(1, 1, 1, 1, 31, 7),
            c('null', 'null', 'null', 'null', 'mm', 'mm')
        )
    )

fig0

save_print(
    fig0,
    'manuscript/figures/schematic.pdf',
    width = 87, # one column
    height = 92
)
