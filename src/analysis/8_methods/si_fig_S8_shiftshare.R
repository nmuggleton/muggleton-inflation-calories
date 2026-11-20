# Fig. S8: the shift-share alternative fails its placebo.

rm(list = ls())

source('src/lib/format.R')

dyn <- copy(load_results('14_shiftshare')$dyn)
dyn # one row per instrument x outcome x window

out <- c(
    d_lkpu = 'Calories per dollar',
    d_lspend = 'Real spending',
    d_lkcal = 'Calories bought'
)
inst <- c(z = 'Main instrument (state mean)', B = 'Shift-share instrument')
pd <- dyn[outcome %in% names(out)]
pd[, `:=`(
    outcome = factor(out[outcome], levels = out),
    inst = factor(inst[instrument], levels = inst)
)]
stopifnot(!anyNA(pd$inst), all(pd$horizon %in% -1:1))

dodge <- position_dodge(width = 0.4)
g <- ggplot(
    pd,
    aes(horizon, b_scaled, fill = inst)
) +
    geom_hline(yintercept = 0, colour = '#8C8C8C', linewidth = pt2lw(0.5)) +
    geom_vline(
        xintercept = 0.5,
        colour = '#8C8C8C',
        linewidth = pt2lw(0.5),
        linetype = '22'
    ) +
    geom_errorbar(
        aes(
            ymin = b_scaled - 1.96 * se_scaled,
            ymax = b_scaled + 1.96 * se_scaled
        ),
        width = 0.1,
        linewidth = pt2lw(0.5),
        position = dodge
    ) +
    geom_point(pch = 21, colour = 'black', size = 1.5, position = dodge) +
    facet_wrap(~outcome, nrow = 1) +
    scale_x_continuous(
        breaks = -1:1,
        labels = c('t-8 to t-4', 't-4 to t', 't to t+4\n(placebo)')
    ) +
    scale_fill_manual(values = c('#4D4D4D', 'white'), name = NULL) +
    labs(
        x = 'Window of the price shock',
        y = 'Response per point\n(scaled by first stage)'
    ) +
    theme_classic(base_size = 7) +
    theme(
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin = margin(0, 0, 0, 0),
        strip.background = element_blank(),
        strip.text = element_text(size = 7, hjust = 0),
        panel.spacing = grid::unit(3, 'mm')
    )

g

save_fig(g, 'shiftshare_dynamic', w = 178, h = 65) # SI text width (505.7 pt)
