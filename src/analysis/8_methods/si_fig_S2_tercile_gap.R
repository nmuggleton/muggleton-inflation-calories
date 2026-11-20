# Fig. S2: the design's variation in raw form.

rm(list = ls())

source('src/lib/format.R')

tw <- load_results('02_cells')$tw
tw # one row per quarter: high minus low third of states

# 2020 is outside the estimation sample, so its rows are NA and the lines break
# there
gap <- data.table(year = 2020L, qtr = 1:4)
tt <- rbind(
    tw[, .(year, qtr, gx, gspend, gkcal, se_gx, se_gspend, se_gkcal, df)],
    gap,
    fill = TRUE
)
tt[, date := year + (qtr - 0.5) / 4]
setorder(tt, date)

series <- c(
    gx     = 'Exposure inflation',
    gspend = 'Real spending',
    gkcal  = 'Calories bought'
)
tl <- melt(
    tt,
    id.vars = c('date', 'df'),
    measure.vars = list(
        value = names(series),
        se = paste0('se_', names(series))
    ),
    variable.factor = FALSE
)
tl[, s := factor(series[as.integer(variable)], levels = series)]
tl[, `:=`(lo = value - qt(0.975, df) * se, hi = value + qt(0.975, df) * se)]
tl

# Numbers for the note: the mean gaps over the sample quarters
tw[, .(
    gx = mean(gx),
    gspend = mean(gspend),
    gkcal = mean(gkcal),
    share_spend_pos = mean(gspend > 0)
)]

# One panel per series, a third of the width each, on a common y axis, so that
# the spending gap reads against the exposure gap and calories against zero
g <- ggplot(
    tl,
    aes(date, value)
) +
    # The band is see-through, and the zero line is dashed and drawn over it, so
    # the line can be read against zero
    geom_ribbon(
        aes(ymin = lo, ymax = hi),
        fill = '#4D4D4D',
        alpha = 0.25,
        na.rm = TRUE
    ) +
    geom_hline(
        yintercept = 0,
        colour = 'black',
        linewidth = pt2lw(0.5),
        linetype = '22'
    ) +
    geom_line(linewidth = pt2lw(0.75), na.rm = TRUE) +
    facet_wrap(~s, nrow = 1) +
    scale_x_continuous(
        breaks = c(2006, 2015, END_YEAR),
        expand = expansion(mult = c(0.03, 0.03))
    ) +
    scale_y_continuous(
        labels = scales::label_number(
            accuracy = 1,
            suffix = '%',
            style_negative = 'minus'
        )
    ) +
    labs(x = NULL, y = 'High minus low third\nof states') +
    theme_classic(base_size = 7) +
    theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 7, hjust = 0),
        panel.spacing = grid::unit(3, 'mm')
    )

g

save_fig(g, 'tercile_gap', w = 178, h = 60) # SI text width (505.7 pt)
