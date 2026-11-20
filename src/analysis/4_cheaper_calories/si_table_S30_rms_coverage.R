# Table S30: where panel food spend takes place, and how much of it Retail
# Scanner (RMS) stores cover, by channel.

rm(list = ls())

source('src/lib/format.R')

chk <- load_results('21_panel_shelf_check')
by_kind <- chk$by_kind
by_kind # share of labelled food spend by store kind and year
by_chan <- chk$by_chan
by_chan # share of spend by channel, and the share of it at RMS stores

years <- c(2008, 2013, 2018, 2023) # RMS years of the shelf check
stopifnot(setequal(unique(by_chan$year), years))

# Channels with at least 0.5 percent of spend in some year; the rest are pooled
top_share <- by_chan[, .(top = max(sh_spend)), by = channel_type]
main <- top_share[top >= .005, channel_type]
by_chan[, channel := fifelse(
    channel_type %in% main,
    channel_type,
    'Other channels'
)]
ch <- by_chan[, .(
    sh_spend = sum(sh_spend),
    sh_rms = sum(sh_spend * sh_rms) / sum(sh_spend)
),
by = .(year, channel)
]
order_ch <- ch[year == 2018][order(-sh_spend), channel]
order_ch <- c(setdiff(order_ch, 'Other channels'), 'Other channels')

tab_ch <- dcast(ch, channel ~ year, value.var = c('sh_spend', 'sh_rms'))
tab_ch <- tab_ch[match(order_ch, channel)]
tab_ch <- tab_ch[, c(
    'channel', as.vector(
        rbind(
            paste0('sh_spend_', years),
            paste0('sh_rms_', years)
        )
    )
), with = FALSE]
for (j in names(tab_ch)[-1]) {
    set(
        tab_ch,
        j = j,
        value = fmt_bounded(tab_ch[[j]], 2)
    )
}

kind_lab <- c(
    rms = 'Store in RMS', coded_non_rms = 'Store coded, not in RMS',
    uncoded = 'No store code'
)
tab_k <- dcast(by_kind, kind ~ year, value.var = 'sh')
tab_k <- tab_k[match(names(kind_lab), kind)]
tab_k <- data.table(channel = kind_lab[tab_k$kind])
for (y in years) {
    tab_k[, paste0(
        'sh_spend_',
        y
    ) := fmt_bounded(by_kind[year == y][match(names(kind_lab), kind), sh], 2)]
    tab_k[, paste0('sh_rms_', y) := NA_character_]
}

tab <- rbind(tab_ch, tab_k, use.names = FALSE)
tab

write_tex_table(
    tab,
    'manuscript/tables/rms_coverage.tex',
    header = c('', rep(c('Share', 'At RMS'), length(years))),
    spanners = setNames(rep(2, length(years)), years),
    panels = c('A. By channel' = 1, 'B. By store' = nrow(tab_ch) + 1),
    indent = TRUE
)
