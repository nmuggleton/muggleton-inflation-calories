# Fig. S4: estimates with an instrument built from official price indices.

rm(list = ls())

library(patchwork)
source('src/lib/format.R')

pe <- copy(load_results('16_bls_cpi')$est)
pe # one row per instrument x outcome: 2SLS b, SE, pairs-bootstrap CI

# 95% CIs are Wald, except for the BLS rows (21 or 14 clusters), which use the
# pairs cluster bootstrap
pe[, `:=`(lo = b - 1.96 * se, hi = b + 1.96 * se)]
pe[!is.na(pairs_lo), `:=`(lo = pairs_lo, hi = pairs_hi)]
pe[, ci_method := fifelse(
    !is.na(pairs_lo),
    'pairs cluster bootstrap',
    'Wald, clustered SE'
)]

# Instruments from top to bottom; short facet titles, to fit a 3-column grid
pe[, model := factor(
    model,
    levels = rev(
        c(
            'Main instrument (state leave-out)', 'BLS CPI, all areas',
            'BLS CPI, 14 metros', 'Both instruments'
        )
    )
)]
ys_short <- c(
    d_lkpu = 'Calories per $ (%)', d_lspend = 'Real spend (%)',
    d_lkcal = 'Calories (%)', d_ed = 'Energy density',
    d_hs = 'Health score'
)
pe[, out_lab := factor(ys_short[outcome], levels = ys_short)]

# Benchmarks: full pass-through to kcal/$ (-1) and to spending (+1)
pe[, benchmark := fcase(
    outcome == 'd_lkpu',
    -1,
    outcome == 'd_lspend',
    1,
    default = NA_real_
)]
setorder(pe, out_lab, -model)

# The figure data as CSV, for use outside R (see
# manuscript/figures/data/README.md)
FIG_DATA <- 'manuscript/figures/data'
dir.create(FIG_DATA, showWarnings = FALSE)
fwrite(
    pe[, .(outcome, out_lab, model, b, se, lo, hi, ci_method, benchmark)],
    file.path(FIG_DATA, 'figS4_iv.csv')
)

# Fig. S4: one row, one plot per outcome (style as Figure 2) ------------------
# Each plot shows the 2SLS estimate and 95% CI under each instrument; dashed
# lines mark 0 and the benchmarks (-1 for calories per $, +1 for spend).
titles <- c(
    d_lkpu = 'Calories per $\n(%)', d_lspend = 'Real spend\n(%)',
    d_lkcal = 'Calories bought\n(%)', d_ed = 'Energy density\n(kcal per 100 g)',
    d_hs = 'Health score\n(SD)'
)

iv_plot <- function(y) {
    d <- pe[outcome == y]
    ggplot(
        d,
        aes(b, model)
    ) +
        geom_vline(xintercept = 0, colour = '#8C8C8C', linewidth = pt2lw(0.5)) +
        geom_vline(
            # NA (no line) except for calories per $ and spend
            xintercept = d$benchmark[1],
            colour = '#8C8C8C',
            linewidth = pt2lw(0.5),
            linetype = '22',
            na.rm = TRUE
        ) +
        geom_errorbar( # CIs as Figure 2: caps 0.1 of the row spacing, 0.5 pt
            aes(xmin = lo, xmax = hi),
            orientation = 'y',
            width = 0.1,
            linewidth = pt2lw(0.5)
        ) +
        geom_point(
            fill = '#4D4D4D',
            colour = 'black',
            pch = 21,
            size = 1.5
        ) +
        scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
        labs(title = titles[[y]], x = NULL, y = NULL) +
        theme_classic(base_size = 7) +
        theme(plot.title = element_text(size = 7, hjust = 0.5, face = 'bold'))
}

no_y <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
fig_s4 <- iv_plot('d_lkpu') +
    (iv_plot('d_lspend') + no_y) +
    (iv_plot('d_lkcal') + no_y) +
    (iv_plot('d_ed') + no_y) +
    (iv_plot('d_hs') + no_y) +
    plot_layout(nrow = 1) +
    plot_annotation(
        caption = '2SLS effect of a 1 pp rise in exposure inflation (95% CI)',
        theme = theme(plot.caption = element_text(size = 7, hjust = 0.5))
    )

fig_s4

save_print(
    fig_s4,
    'manuscript/figures/local_iv_compare.pdf',
    width = 178,
    height = 30
)
