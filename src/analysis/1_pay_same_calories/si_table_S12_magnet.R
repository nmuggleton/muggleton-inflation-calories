# Table S12: Magnet (random-weight) households, 2013-2021, with and without the
# Magnet projection weights.

rm(list = ls())

source('src/lib/format.R')

msr <- load_results('12_measurement')
mc <- copy(msr$mc)
mc # one row per outcome x weighting

# Shares are shown x100 so that 3 decimals carry
x100 <- c('d_sh_mag', 'd_sh_fv_all')
mc[outcome %in% x100, `:=`(b = 100 * b, se = 100 * se)]
labels <- msr$lab_m
labels[x100] <- paste(labels[x100], 'x100')

w <- dcast(mc, outcome ~ spec, value.var = c('b', 'se', 'n'))
w <- w[match(msr$ym, outcome)]
tab <- w[, .(
    Outcome = labels[outcome],
    # Stars: t, 48 df
    b_unw = paste0(
        fmt_num(b_Unweighted, 3),
        stars(2 * pt(-abs(b_Unweighted / se_Unweighted), 48))
    ),
    se_unw = parens(fmt_num(se_Unweighted, 3)),
    b_wt = paste0(
        fmt_num(`b_Magnet projection weights`, 3),
        stars(2 * pt(
            -abs(
                `b_Magnet projection weights` /
                    `se_Magnet projection weights`
            ),
            48
        ))
    ),
    se_wt = parens(fmt_num(`se_Magnet projection weights`, 3)),
    N = fmt_num(n_Unweighted, 0)
)]
tab

write_tex_table(
    tab,
    'manuscript/tables/magnet.tex',
    header = c('Outcome', 'b', 'SE', 'b', 'SE', 'N'),
    spanners = c('Unweighted' = 2, 'Magnet weights' = 2, ' ' = 1)
)
