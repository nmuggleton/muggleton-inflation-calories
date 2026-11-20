# Figure 2: how diets differ by income (A), against what basket inflation does
# to spending and calories (B) and to the nutrients in the basket (C), across
# 3,136 state-quarters. Built from the state-quarter cells and income-group
# levels saved by estimate/02_cells.R (results/02_cells/); also writes the
# figure data to manuscript/figures/data/.

rm(list = ls())

library(patchwork)
source('src/lib/format.R')

n_bins <- 100 # number of exposure bins in each facet

cells <- load_results('02_cells')
cells$cells # one row per state-quarter: households (n), and each variable
# The _net columns are net of quarter fixed effects

# Bins -------------------------------------------------------------------------
# The nutrients behind the health score: same cells, same x_net
cells$nutrients # one row per state-quarter: nutrient changes net of quarter FE

# Sort the state-quarters by exposure, then cut them into bins holding equal
# numbers of households
cl <- merge(
    cells$cells,
    cells$nutrients[, !c('n', 'x_net')],
    by = c('state', 'yq')
)
setorder(cl, x_net)
cl[, bin := pmin(n_bins, ceiling(n_bins * cumsum(n) / sum(n)))]

nutrients <- c(
    'protein',
    'fiber',
    'sugar',
    'satfat',
    'sodium',
    'fv',
    'sweet',
    'snack'
)
long <- melt(
    cl,
    id.vars = c('bin', 'n', 'x_net'),
    measure.vars = paste0(c('spend', 'kcal', 'kpu', 'ed', nutrients), '_net'),
    variable.name = 'outcome'
)
long[, outcome := sub('_net$', '', outcome)]

# 02_cells.R saved energy density in SD units of its level; multiply back by
# that SD (Table S1) to get kcal per 100 g
ed_sd <- load_results('04_summary')$s[variable == 'kcal_100g_food', sd]
long[outcome == 'ed', value := value * ed_sd]

# Household-weighted bin means.
bins <- long[
    ,
    {
        m <- weighted.mean(value, n)
        se <- sqrt(sum(n^2 * (value - m)^2)) / sum(n)
        .(
            x = weighted.mean(x_net, n),
            value = m,
            se = se,
            n_hhq = sum(n),
            n_cells = .N
        )
    },
    keyby = .(outcome, bin)
]
bins[, `:=`(lo = value - 1.96 * se, hi = value + 1.96 * se)]

# Each outcome's group, facet label and units: "budget" (spend, calories,
# calories per dollar) or "diet" (energy density, nutrients, calorie shares)
labels <- data.table(
    outcome = c('spend', 'kcal', 'kpu', 'ed', nutrients),
    panel = c(rep('budget', 3), rep('diet', 9)),
    label = c(
        'Real spend', 'Calories bought', 'Calories per $',
        'Energy density (kcal per 100 g)',
        'Protein (g per 1,000 kcal)', 'Fiber (g per 1,000 kcal)',
        'Sugar (g per 1,000 kcal)', 'Saturated fat (g per 1,000 kcal)',
        'Sodium (mg per 1,000 kcal)', 'Fruit and veg (% of spend)',
        'Sweet drinks (% of calories)', 'Snacks and sweets (% of calories)'
    ),
    units = c(
        rep('% (log x 100)', 3), 'kcal per 100 g', rep('g per 1,000 kcal', 4),
        'mg per 1,000 kcal', 'pp of spend', 'pp of calories', 'pp of calories'
    )
)
bins <- merge(bins, labels, by = 'outcome')
bins[, label := factor(label, levels = labels$label)]
setorder(bins, label, bin)
bins

# The correlation between exposure and each outcome across the 3,136
# state-quarters (weighted by households), so it does not depend on n_bins
r_lab <- long[
    ,
    .(r = cov.wt(cbind(x_net, value), wt = n, cor = TRUE)$cor[1, 2]),
    by = outcome
]
r_lab <- merge(r_lab, labels, by = 'outcome')
r_lab[, label := factor(label, levels = labels$label)]
r_lab[, r := round(r, 2) + 0] # + 0 turns -0.00 into 0.00
# Label text, e.g. 'r = .91': no leading zero, a true minus sign, and an italic
# r (plotmath)
r_lab[, r_text := sub('^-', '-', fmt_bounded(r, 2))]
r_lab[, text := sprintf('italic(r)~"= %s"', r_text)]
r_lab

# The figure data as CSV, for use outside R (see
# manuscript/figures/data/README.md)
FIG_DATA <- 'manuscript/figures/data'
dir.create(FIG_DATA, showWarnings = FALSE)
fwrite(cells$cells, file.path(FIG_DATA, 'fig2_cells.csv'))
fwrite(
    bins[, .(
        panel,
        outcome,
        label,
        units,
        bin,
        x,
        value,
        se,
        lo,
        hi,
        n_hhq,
        n_cells
    )],
    file.path(FIG_DATA, 'fig2_bins.csv')
)
fwrite(cells$fit, file.path(FIG_DATA, 'fig2_fit.csv'))

cols <- pal$okabe
# Exposure inflation, net of quarter FE; the caption says so
x_lab <- 'Inflation'
# X ticks: -2%, 0%, 2%
pct <- scales::label_number(suffix = '%', style_negative = 'minus')

# The unit of each diet outcome's change, shown on the y tick labels in panel C
unit_suffix <- c(
    ed = ' kcal', sugar = 'g', satfat = 'g', sodium = 'mg',
    protein = 'g', fiber = 'g', fv = '%', sweet = '%', snack = '%'
)
# (For shares, a change is the difference between two percentages, so 10% to
# 10.05% shows as 0.05%; the caption says so)

# One colour per outcome: grey for the budget outcomes in A, and a colour of its
# own for each diet outcome, the same in B and C
outcome_cols <- c(
    spend = '#4D4D4D', kcal = '#4D4D4D', kpu = '#4D4D4D',
    ed = '#666666', protein = cols[1], fiber = cols[2],
    sugar = cols[3], satfat = cols[4], sodium = cols[5], fv = cols[6],
    sweet = '#882255', snack = '#999933'
) # wine and olive (Tol muted)
title_of <- setNames(labels$label, labels$outcome)

# Panels B and C share one colour key, in the order of their 3 x 3 grids: the
# plots there have no titles, so the colour is what links each bar to its
# scatter
grid_order <- c(
    'ed', 'sugar', 'satfat', 'sweet', 'snack', 'sodium',
    'protein', 'fiber', 'fv'
)
key_scale <- scale_fill_manual(
    values = setNames(unname(outcome_cols[grid_order]), title_of[grid_order]),
    # Every measure appears in every key, so that the keys merge
    limits = unname(title_of[grid_order]),
    drop = FALSE,
    name = NULL,
    # 3 x 3, matching the plots below it
    guide = guide_legend(nrow = 3, byrow = TRUE)
)

# The unit of each diet outcome's level, shown on the y tick labels in panel B
level_suffix <- c(
    unit_suffix[c('ed', 'sugar', 'satfat', 'sodium', 'protein', 'fiber')],
    fv = '%', sweet = '%', snack = '%'
)
r_text <- setNames(r_lab$text, r_lab$outcome)

# Panel A: the slope of each outcome on exposure across the state-quarters
# (quarter FE, weighted by households, SEs clustered by state; 02_cells.R)
slope_text <- cells$gs[
    ,
    setNames(
        sprintf( # plotmath: italic b and SE
            'italic(b)~"= %s ("*italic(SE)~"%s)"',
            sub('^-', '-', fmt_num(b, 3)),
            fmt_num(se, 3)
        ),
        y
    )
]
slope_text

# Income groups: average levels ------------------------------------------------
# NielsenIQ household income (the bracket midpoint) over the HHS poverty line
# for the household's size, in the base quarter; the bottom, middle and top
# fifth of that ratio, ranked within each year.
income <- cells$nut_by_income[sample == 'All households']
income[, income := factor(
    sub(' fifth', '', income),
    levels = c('Bottom', 'Middle', 'Top')
)]
income

# The income gap for the bands in panel C: top minus bottom fifth, in each unit
income_gap <- income[
    ,
    .(gap = mean[income == 'Top'] - mean[income == 'Bottom']),
    by = outcome
][, setNames(gap, outcome)]
income_gap

# Building blocks: one plot per outcome ----------------------------------------
# Each function returns an ordinary ggplot, so any single plot can be customised
# further with +. ylim sets the y-axis range, e.g. income_bar("sodium", ylim =
# c(1500, 2200)) or diet_scatter("sugar", ylim = c(-0.5, 0.5)); NULL leaves it
# automatic.

# B: the average level by income fifth, with a 95% CI.
income_bar <- function(y, ylim = NULL) {
    # CI range by default
    if (is.null(ylim)) ylim <- income[outcome == y, range(lo, hi)]
    ggplot(
        income[outcome == y][, key := title_of[[y]]],
        aes(income, mean)
    ) +
        annotate( # the income gap, shaded as in the scatter above it
            'rect',
            xmin = -Inf,
            xmax = Inf,
            ymin = income[outcome == y, min(mean[income %in% c(
                'Bottom',
                'Top'
            )])],
            ymax = income[outcome == y, max(mean[income %in% c(
                'Bottom',
                'Top'
            )])],
            fill = outcome_cols[[y]],
            alpha = 0.2
        ) +
        geom_col(
            aes(fill = key),
            width = 0.7,
            colour = 'black',
            key_glyph = 'rect',
            show.legend = TRUE # draw all nine swatches, not just this plot's
        ) +
        geom_errorbar(
            aes(ymin = lo, ymax = hi),
            width = 0.1,
            linewidth = pt2lw(0.5)
        ) +
        key_scale +
        scale_y_continuous(
            breaks = function(l) { # 2 or 3 round breaks inside the axis
                b <- pretty(l, n = 2)
                b <- b[b >= l[1] & b <= l[2]]
                if (length(b) > 3) b <- b[c(TRUE, FALSE)]
                b
            },
            # Decimals from the tick spacing, the same on every tick: 16.0, 16.1
            labels = function(b) {
                bb <- b[!is.na(b)]

                # Tolerance of 1e-9, since 16.1 - 16.0 is 0.0999... in floating
                # point
                acc <- 1
                if (length(bb) > 1) {
                    acc <- 10^floor(log10(min(diff(bb))) + 1e-9)
                }
                scales::label_number(
                    accuracy = min(acc, 1),
                    suffix = level_suffix[[y]],
                    big.mark = ''
                )(b)
            }
        ) +
        coord_cartesian(ylim = ylim) +
        labs(y = 'Average\nintake') +
        theme_classic(base_size = 7) +
        theme(
            plot.title = element_text(size = 7, hjust = 0.5),
            axis.title.x = element_blank(), # "income fifth" is in the caption
            # aspect.ratio = 1 / 3   # a short strip under its scatter
        )
}

# Where to put a scatter's label (b or r): bottom right, unless a point or the
# fitted line falls in the label's box there, in which case top right. width is
# the label's width as a share of the x range; yr the panel's y range (points,
# income-gap band, ylim); the box is taken to be 15% of that range tall.
label_spot <- function(d, width, yr = range(d$value)) {
    xr <- range(d$x)
    in_x <- d$x >= xr[2] - width * diff(xr)
    fitted <- predict(lm(value ~ x, d))
    box_hi <- yr[1] + 0.15 * diff(yr)
    clear <- !any(in_x & (d$value <= box_hi | fitted <= box_hi))
    if (clear) {
        c(
            x = xr[2],
            y = yr[1],
            vjust = 0
        )
    } else {
        c(x = xr[2], y = yr[2], vjust = 1)
    }
}

# A: bin means against exposure, a linear fit, and the slope (slope_text).
budget_scatter <- function(y, slopes, label = slope_text[[y]], ylim = NULL) {
    d <- bins[outcome == y]
    spot <- label_spot(
        d,
        width = 0.55,
        yr = if (is.null(ylim)) range(d$value) else ylim
    )
    ggplot(
        bins[outcome == y],
        aes(x, value)
    ) +
        geom_abline(
            slope = slopes,
            intercept = 0,
            colour = '#8C8C8C',
            linewidth = pt2lw(0.5),
            linetype = '22'
        ) +
        # geom_point(size = 1, colour = outcome_cols[[y]], alpha = .5) +
        geom_point(
            # Outside aes(): a fixed colour, not a mapping
            fill = outcome_cols[[y]],
            colour = 'black',
            pch = 21,
            size = 1.5
        ) +
        geom_smooth(method = 'lm', formula = y ~ x, colour = 'black') +
        annotate(
            'text',
            # Bottom right, or top right if that spot is taken (label_spot())
            x = spot[['x']],
            y = spot[['y']],
            label = label,
            parse = TRUE, # plotmath, for the italic b, r and SE
            hjust = 1,
            vjust = spot[['vjust']],
            size = pt2size(6)
        ) +
        scale_x_continuous(breaks = -2:2, labels = pct) +
        scale_y_continuous(
            limits = ylim,
            oob = scales::oob_keep,
            expand = expansion(mult = 0.05)
        ) +
        labs(
            title = paste0(title_of[[y]], '\n(%, log x 100)'),
            x = x_lab,
            y = 'Change'
        ) +
        theme_classic(base_size = 7) +
        theme(plot.title = element_text(size = 7, hjust = 0.5, face = 'bold'))
}

# C: bin means against exposure, a linear fit, and r.
diet_scatter <- function(y, label = r_text[[y]], ylim = NULL, show_gap = TRUE) {
    d <- bins[outcome == y]
    spot <- label_spot( # the panel also spans the income-gap band
        d,
        width = 0.3,
        yr = range(c(d$value, 0, if (show_gap) income_gap[[y]], ylim))
    )
    ggplot(
        bins[outcome == y][, key := title_of[[y]]],
        aes(x, value)
    ) +
        (
            if (show_gap) {
                annotate(
                    'rect',
                    xmin = -Inf,
                    xmax = Inf,
                    ymin = min(0, income_gap[[y]]),
                    ymax = max(0, income_gap[[y]]),
                    fill = outcome_cols[[y]],
                    alpha = 0.2
                )
            }) +
        geom_hline(
            yintercept = 0,
            colour = '#8C8C8C',
            linewidth = pt2lw(0.5),
            linetype = '22'
        ) +
        # geom_point(size = 1, colour = outcome_cols[[y]], alpha = .5) +
        geom_point(
            aes(fill = key), # the measure, coloured by the shared key
            key_glyph = 'rect',
            show.legend = TRUE, # draw all nine swatches, not just this plot's
            colour = 'black',
            pch = 21,
            size = 1.5
        ) +
        geom_smooth(method = 'lm', formula = y ~ x, colour = 'black') +
        annotate(
            'text',
            # Bottom right, or top right if that spot is taken (label_spot())
            x = spot[['x']],
            y = spot[['y']],
            label = label,
            parse = TRUE, # plotmath, for the italic b, r and SE
            hjust = 1,
            vjust = spot[['vjust']],
            size = pt2size(6)
        ) +
        scale_x_continuous(breaks = c(-2, 0, 2), labels = pct) +
        scale_y_continuous(
            limits = ylim,
            oob = scales::oob_keep,
            expand = expansion(mult = 0.05),
            labels = scales::label_number(
                suffix = unit_suffix[[y]],
                big.mark = '',
                style_negative = 'minus'
            )
        ) +
        key_scale +
        # Units on a second line
        labs(
            title = sub(' \\(', '\n(', title_of[[y]]),
            x = x_lab,
            y = 'Change'
        ) +
        theme_classic(base_size = 7) +
        theme(
            plot.title = element_text(size = 7, hjust = 0.5, face = 'bold'),
            # aspect.ratio = 1   # square
        )
}

# Panel A: spending and calories -----------------------------------------------
# Dashed lines: +1 for spend (households pay the whole price rise), 0 for
# calories (no change).
a_spend <- budget_scatter(
    'spend',
    slopes = 1,
    ylim = c(-2, 2)
) + labs(tag = 'A')
a_kcal <- budget_scatter('kcal', slopes = 0, ylim = c(-2, 2))

panel_a <- a_spend + a_kcal +
    plot_layout(ncol = 2, axis_titles = 'collect')

panel_a

# Panel B: diets by income -----------------------------------------------------
# The average level of each diet outcome in the bottom, middle and top income
# fifths, in the same units as panel C.
b_ed <- income_bar('ed', ylim = c(250, 265))
b_sugar <- income_bar('sugar', ylim = c(60, 66))
b_satfat <- income_bar('satfat', c(16, 16.2))
b_sweet <- income_bar('sweet', ylim = c(4.5, 6.5))
b_snack <- income_bar('snack', c(28, 29.2))
b_sodium <- income_bar('sodium', c(2050, 2150))
b_protein <- income_bar('protein', ylim = c(25, 30))
b_fiber <- income_bar('fiber', ylim = c(7, 10))
b_fv <- income_bar('fv', c(9, 13))

panel_b <- b_ed + b_sugar + b_satfat +
    b_sweet + b_snack + b_sodium +
    b_protein + b_fiber + b_fv +
    plot_layout(ncol = 3, axis_titles = 'collect')

panel_b

# Panel C: energy density, nutrients and calorie shares ------------------------
# Dashed line at 0 (no change); the same 3 x 3 order as panel B
c_ed <- diet_scatter('ed')
c_sugar <- diet_scatter('sugar') + labs(tag = 'B')
c_satfat <- diet_scatter('satfat')
c_sweet <- diet_scatter('sweet')
c_snack <- diet_scatter('snack')
c_sodium <- diet_scatter('sodium')
c_protein <- diet_scatter('protein')
c_fiber <- diet_scatter('fiber')
c_fv <- diet_scatter('fv')

panel_c <- c_ed + c_sugar + c_satfat +
    c_sweet + c_snack + c_sodium +
    c_protein + c_fiber + c_fv +
    plot_layout(ncol = 3, axis_titles = 'collect')

panel_c

# Figure 2: A (spend, calories, energy density), then B ------------------------
# Each diet measure is a pair: a square scatter (change against inflation) with
# a short strip of income bars (3:1) directly below it.
no_x <- theme(axis.title.x = element_blank()) # labs(x = NULL) is not reliable
no_y <- theme(axis.title.y = element_blank())
row_a_margin <- theme(plot.margin = margin(0, 1, 1, 1, 'mm'))
upper <- theme(plot.margin = margin(1, 1, -1, 1, 'mm')) # scatter: space above
# Bars: tight below scatter
lower <- theme(plot.margin = margin(0, 1, 1, 1, 'mm'))

pair_ed <- (c_ed + no_x + row_a_margin) / (b_ed + lower) +
    plot_layout(heights = c(3, 1))
# Widths: spend and calories share the width of three B columns, and the
# energy-density pair takes one, so it is the same size as the pairs in B
row_a <- (a_spend + row_a_margin) + (a_kcal + no_y + row_a_margin) + pair_ed +
    plot_layout(ncol = 3, widths = c(1.5, 1.5, 1))

pairs <- (c_sugar + no_x + upper) + (c_satfat + no_x + no_y + upper) +
    (c_sweet + no_x + no_y + upper) + (c_snack + no_x + no_y + upper) +
    (b_sugar + lower) + (b_satfat + no_y + lower) +
    (b_sweet + no_y + lower) + (b_snack + no_y + lower) +
    (c_sodium + no_x + upper) + (c_protein + no_x + no_y + upper) +
    (c_fiber + no_x + no_y + upper) + (c_fv + no_x + no_y + upper) +
    (b_sodium + lower) + (b_protein + no_y + lower) +
    (b_fiber + no_y + lower) + (b_fv + no_y + lower) +
    plot_layout(ncol = 4, heights = c(3, 1, 3, 1))

# Row A is a little shorter than a row of B pairs, so the energy-density pair
# has no spare height; tags are drawn in the plot's top-left corner with no
# margin of their own; no margin around the whole figure
fig1 <- row_a / pairs +
    plot_layout(heights = c(0.9, 2)) +
    plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0))) &
    theme(
        # Anchored by its top-left corner, so it stays inside
        plot.tag = element_text(face = 'bold', hjust = 0, vjust = 1),
        plot.tag.location = 'plot',
        plot.tag.position = c(0, 1),
        legend.position = 'none'
    )
fig1

save_print(
    fig1,
    'manuscript/figures/budget_identity.pdf',
    width = 178,
    height = 150
)
