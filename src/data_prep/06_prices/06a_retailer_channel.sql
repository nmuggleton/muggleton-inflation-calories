-- The channel of each retailer: the channel type recorded most often on its
-- purchase lines, over all panel years. Run once by 06_price_gap_sources.sh.

COPY (
    SELECT
        retailer_code AS ret,
        mode(channel_type) AS channel
    FROM read_parquet('${WP2_FINAL}/purchases/panel_year=*/data.parquet')
    WHERE retailer_code IS NOT NULL
    GROUP BY ALL
) TO '${WP2_INT}/price_gap_sources/retailer_channel.parquet' (FORMAT parquet);
