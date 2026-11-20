-- One panel year of purchases -> additive household x calendar-quarter
-- components, a household x UPC x quarter price-quantity table, and a state x
-- UPC x quarter price table. Run once per panel year by
-- src/data_prep/05_household_quarter/01_stream.sh (YEAR set).
--
-- Line-level rules
--   human food      xwalk is_human_food (food depts 1-6 minus pet food,
--                   ice, yeast)
--   label UPC       2004-2020 private label: the 2021 masked code from the
--                   Kilts PL crosswalk; otherwise the purchased UPC
--   label used      only if f_kcal_implausible is false and basis known
--   pack amount     purchase-time size (size1_amount x unit factor) x multi
--                   x quantity, in the label basis (g or ml). OZ is read as
--                   weight or fluid ounces according to the LABEL basis.
--                   Fallbacks: label product_size, then servings x serving.
--   kcal            kcal_100 x amount / 100 (same for each nutrient)
--   fallback kcal   module median density (dominant basis of the module)
--                   x amount, for unlabelled lines with a usable size
--   size check      f_size_disagree: purchase-time and label sizes differ
--                   by more than 10% (resized / reused UPC)

CREATE TEMP TABLE fbm AS                 -- dominant basis per module
SELECT * EXCLUDE (rk) FROM (
    SELECT
        *,
        row_number() OVER (
            PARTITION BY product_module_code
            ORDER BY n_upc DESC
        ) AS rk
    FROM read_parquet('${WP2_FINAL}/module_nutrient_fallback.parquet')
)
WHERE rk = 1;

CREATE TEMP TABLE hy AS
SELECT
    household_code,
    panel_year,
    fips_state_code
FROM read_parquet('${WP2_FINAL}/household_year.parquet')
WHERE panel_year = ${YEAR};

CREATE TEMP VIEW l AS                    -- a view, not a table: each output
-- Streams the year (a full-year table spills more than 25 GB from 2007 on)
WITH p AS (
    SELECT
        p.*,
        fg.is_human_food,
        fg.food_group,
        CASE
            WHEN p.is_private_label AND ${YEAR} <= 2020
                THEN coalesce(x.upc_2021_masked, p.upc)
            ELSE p.upc
        END AS label_upc,
        (
            p.is_private_label AND ${YEAR} <= 2020
            AND x.upc_2021_masked IS NOT NULL
        ) AS pl_mapped,
        (year(p.purchase_date) * 10 + quarter(p.purchase_date)) AS yq
    FROM
        read_parquet('${WP2_FINAL}/purchases/panel_year=${YEAR}/data.parquet')
            AS p
    LEFT JOIN read_parquet('${WP2_FINAL}/xwalk_module_foodgroup.parquet') AS fg
        USING (product_module_code)
    LEFT JOIN read_parquet('${WP2_INT}/ref/pl_crosswalk/data.parquet') AS x
        ON p.upc = x.upc_2020
),

j AS (
    SELECT
        p.*,
        n.basis,
        n.kcal_100,
        n.sugar_100,
        n.satfat_100,
        n.sodium_mg_100,
        n.fiber_100,
        n.protein_100,
        n.fat_100,
        n.carb_100,
        n.unit_size,
        n.pack_size_srv,
        (
            n.kcal_100 IS NOT NULL AND n.basis IS NOT NULL
            AND NOT n.f_kcal_implausible
        ) AS has_label,
        f.basis AS fb_basis,
        f.kcal_100 AS fb_kcal_100
    FROM p
    LEFT JOIN read_parquet('${WP2_FINAL}/upc_nutrients.parquet') AS n
        ON n.upc = p.label_upc
    LEFT JOIN fbm AS f ON f.product_module_code = p.product_module_code
),

s AS (
    SELECT
        j.*,
        coalesce(basis, fb_basis) AS use_basis,
        -- Purchase-time size of one item, in the use basis
        CASE upper(trim(size1_units))
            WHEN 'OZ'
                THEN
                    size1_amount * CASE coalesce(basis, fb_basis)
                        WHEN 'g' THEN 28.3495 WHEN 'ml' THEN 29.5735
                    END
            WHEN 'PO'
                THEN
                    CASE
                        WHEN coalesce(basis, fb_basis) = 'g'
                            THEN size1_amount * 453.592
                    END
            WHEN 'QT'
                THEN
                    CASE
                        WHEN coalesce(basis, fb_basis) = 'ml'
                            THEN size1_amount * 946.353
                    END
            WHEN 'LI'
                THEN
                    CASE
                        WHEN coalesce(basis, fb_basis) = 'ml'
                            THEN size1_amount * 1000
                    END
            WHEN 'ML'
                THEN
                    CASE
                        WHEN coalesce(basis, fb_basis) = 'ml'
                            THEN size1_amount
                    END
            WHEN 'GA' THEN CASE
                WHEN coalesce(basis, fb_basis) = 'ml'
                    THEN size1_amount * 3785.41
            END
        END AS size_line
    FROM j
)

SELECT
    s.*,
    coalesce(size_line, CASE WHEN has_label THEN coalesce(
        unit_size,
        pack_size_srv
    ) END) * coalesce(nullif(multi, 0), 1) * quantity
        AS amount,
    (
        has_label AND size_line > 0 AND unit_size > 0
        AND abs(size_line / unit_size - 1) > 0.10
    ) AS f_size_disagree
FROM s;

-- 1. household x quarter additive components --------------------------------
COPY (
    SELECT
        household_code,
        CAST(${YEAR} AS SMALLINT) AS panel_year,
        yq,
        count(DISTINCT trip_code_uc) AS n_trips_all,
        count(DISTINCT purchase_date) AS n_days_all,
        sum(net_price_paid) AS spend_all,
        -- human food
        sum(net_price_paid) FILTER (is_human_food) AS spend_food,
        sum(net_price_paid) FILTER (is_human_food AND NOT is_magnet_product)
            AS spend_food_nomag,
        sum(net_price_paid) FILTER (is_human_food AND is_magnet_product)
            AS spend_food_mag,
        count(*) FILTER (is_human_food) AS n_lines_food,
        count(DISTINCT trip_code_uc) FILTER (is_human_food) AS n_trips_food,
        -- Labelled: non-Magnet human food with a usable label and amount
        sum(
            net_price_paid) FILTER (
            is_human_food AND NOT is_magnet_product
            AND has_label AND amount > 0
        ) AS spend_lab,
        sum(
            net_price_paid) FILTER (
            is_human_food AND NOT is_magnet_product
            AND has_label AND amount > 0 AND is_private_label
        ) AS spend_lab_pl,
        sum(
            net_price_paid) FILTER (
            is_human_food AND NOT is_magnet_product
            AND is_private_label
        ) AS spend_pl,
        count(*) FILTER (is_human_food AND has_label AND f_size_disagree)
            AS n_lines_size_disagree,
        count(*) FILTER (is_human_food AND has_label AND amount > 0)
            AS n_lines_lab,
        -- labelled nutrient totals
        sum(
            kcal_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
        ) AS kcal_lab,
        sum(
            sugar_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND sugar_100 IS NOT NULL
        ) AS sugar_g,
        sum(
            kcal_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND sugar_100 IS NOT NULL
        ) AS kcal_w_sugar,
        sum(
            satfat_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND satfat_100 IS NOT NULL
        ) AS satfat_g,
        sum(
            kcal_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND satfat_100 IS NOT NULL
        ) AS kcal_w_satfat,
        sum(
            sodium_mg_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND sodium_mg_100 IS NOT NULL
        ) AS sodium_mg,
        sum(
            kcal_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND sodium_mg_100 IS NOT NULL
        ) AS kcal_w_sodium,
        sum(
            fiber_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND fiber_100 IS NOT NULL
        ) AS fiber_g,
        sum(
            kcal_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND fiber_100 IS NOT NULL
        ) AS kcal_w_fiber,
        sum(
            protein_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND protein_100 IS NOT NULL
        ) AS protein_g,
        sum(
            kcal_100 * amount / 100) FILTER (
            is_human_food AND NOT
            is_magnet_product AND has_label AND amount > 0
            AND protein_100 IS NOT NULL
        ) AS kcal_w_protein,
        -- Energy density components (foods on a g basis, drinks on ml)
        sum(
            amount) FILTER (
            is_human_food AND NOT is_magnet_product
            AND has_label AND basis = 'g' AND amount > 0
        ) AS grams_lab,
        sum(
            kcal_100 * amount / 100) FILTER (is_human_food AND NOT
        is_magnet_product AND has_label AND basis = 'g' AND amount > 0)
            AS kcal_lab_g,
        sum(
            net_price_paid) FILTER (
            is_human_food AND NOT is_magnet_product
            AND has_label AND basis = 'g' AND amount > 0
        ) AS spend_lab_g,
        sum(
            amount) FILTER (
            is_human_food AND NOT is_magnet_product
            AND has_label AND basis = 'ml' AND amount > 0
        ) AS ml_lab,
        sum(
            kcal_100 * amount / 100) FILTER (is_human_food AND NOT
        is_magnet_product AND has_label AND basis = 'ml' AND amount > 0)
            AS kcal_lab_ml,
        -- Fallback-inclusive kcal: labelled plus module-median imputed
        sum(
            CASE WHEN has_label THEN kcal_100 ELSE fb_kcal_100 END
            * amount / 100) FILTER (
            is_human_food AND NOT is_magnet_product
            AND amount > 0
        ) AS kcal_fb,
        sum(
            net_price_paid) FILTER (is_human_food AND NOT is_magnet_product
        AND amount > 0 AND (has_label OR fb_kcal_100 IS NOT NULL))
            AS spend_fb,
        -- Food-group spend (all human food, Magnet groups included) and kcal
        -- (non-Magnet, fallback-inclusive)
        sum(net_price_paid) FILTER (food_group = 'fv') AS spend_fg_fv,
        sum(net_price_paid) FILTER (food_group = 'juice100')
            AS spend_fg_juice100,
        sum(net_price_paid) FILTER (food_group = 'ssb') AS spend_fg_ssb,
        sum(net_price_paid) FILTER (food_group = 'diet_water')
            AS spend_fg_diet_water,
        sum(net_price_paid) FILTER (food_group = 'coffee_tea')
            AS spend_fg_coffee_tea,
        sum(net_price_paid) FILTER (food_group = 'dairy') AS spend_fg_dairy,
        sum(net_price_paid) FILTER (food_group = 'protein') AS spend_fg_protein,
        sum(net_price_paid) FILTER (food_group = 'grains') AS spend_fg_grains,
        sum(net_price_paid) FILTER (food_group = 'sweets') AS spend_fg_sweets,
        sum(net_price_paid) FILTER (food_group = 'salty_snacks')
            AS spend_fg_salty_snacks,
        sum(net_price_paid) FILTER (food_group = 'prepared')
            AS spend_fg_prepared,
        sum(net_price_paid) FILTER (food_group = 'takeout') AS spend_fg_takeout,
        sum(net_price_paid) FILTER (food_group = 'fats_condiments')
            AS spend_fg_fats_condiments,
        sum(net_price_paid) FILTER (food_group = 'baby') AS spend_fg_baby,
        sum(net_price_paid) FILTER (food_group = 'fv' AND NOT is_magnet_product)
            AS spend_fg_fv_nomag,
        sum(
            CASE WHEN has_label THEN kcal_100 ELSE fb_kcal_100 END * amount
            / 100) FILTER (
            food_group = 'fv' AND NOT is_magnet_product
            AND amount > 0
        ) AS kcal_fg_fv,
        sum(
            CASE WHEN has_label THEN kcal_100 ELSE fb_kcal_100 END * amount
            / 100) FILTER (
            food_group IN ('ssb', 'juice100') AND NOT
            is_magnet_product AND amount > 0
        ) AS kcal_fg_drinks_sweet,
        sum(
            CASE WHEN has_label THEN kcal_100 ELSE fb_kcal_100 END * amount
            / 100) FILTER (
            food_group IN ('sweets', 'salty_snacks') AND NOT
            is_magnet_product AND amount > 0
        ) AS kcal_fg_snacks_sweets,
        -- shopping-behaviour margins (human food)
        sum(net_price_paid) FILTER (is_human_food AND deal_flag_uc = 1)
            AS spend_deal,
        sum(coalesce(coupon_value, 0)) FILTER (is_human_food) AS coupon_value,
        sum(net_price_paid) FILTER (is_human_food AND channel_type = 'Grocery')
            AS spend_ch_grocery,
        sum(
            net_price_paid) FILTER (
            is_human_food AND channel_type IN
            ('Discount Store', 'Hypermarket')
        ) AS spend_ch_discount,
        sum(
            net_price_paid) FILTER (
            is_human_food AND channel_type
            = 'Warehouse Club'
        ) AS spend_ch_club,
        sum(
            net_price_paid) FILTER (
            is_human_food AND channel_type IN
            ('Dollar Store', 'Close Out Store')
        ) AS spend_ch_dollar,
        sum(
            net_price_paid) FILTER (is_human_food AND channel_type IN
        ('Convenience Store', 'Gas Mini Mart', 'Service Station'))
            AS spend_ch_convenience,
        sum(
            net_price_paid) FILTER (
            is_human_food AND channel_type IN
            (
                'Restaurant', 'Quick Serve Restaurants', 'Pizzeria',
                'Coffee Store/Gourmet Coffee Sh'
            )
        ) AS spend_ch_restaurant,
        sum(
            net_price_paid) FILTER (is_human_food AND channel_type IN
        ('Online Shopping', 'Mail Order', 'Home Delivery'))
            AS spend_ch_online,
        sum(net_price_paid) FILTER (is_human_food AND store_code_uc > 0)
            AS spend_food_store_id,
        -- Gasoline (the Magnet reference card; Magnet households, 2012 on)
        sum(net_price_paid) FILTER (product_module_code = 454) AS spend_gas_card
    FROM l
    GROUP BY household_code, yq
) TO '${WP2_INT}/hhq/panel_year=${YEAR}.parquet' (FORMAT parquet);

-- 2. household x UPC x quarter prices (non-Magnet human food) ------------
COPY (
    SELECT
        household_code,
        CAST(${YEAR} AS SMALLINT) AS panel_year,
        yq,
        upc,
        coalesce(upc_ver_uc, 0)::SMALLINT AS upc_ver_uc,
        sum(quantity) AS qty,
        sum(net_price_paid) AS spend,
        sum(total_price_paid) AS spend_gross
    FROM l
    WHERE
        is_human_food AND NOT is_magnet_product AND quantity > 0
        AND net_price_paid > 0
    GROUP BY ALL
) TO '${WP2_INT}/hh_upc_q/panel_year=${YEAR}.parquet'
(FORMAT parquet, COMPRESSION zstd);

-- 3. state x UPC x quarter totals for leave-one-out prices -------------------
COPY (
    SELECT
        hy.fips_state_code,
        l.yq,
        l.upc,
        coalesce(l.upc_ver_uc, 0)::SMALLINT AS upc_ver_uc,
        CAST(${YEAR} AS SMALLINT) AS panel_year,
        sum(l.quantity) AS qty,
        sum(l.net_price_paid) AS spend,
        count(DISTINCT l.household_code) AS n_hh
    FROM l JOIN hy USING (household_code)
    WHERE
        l.is_human_food AND NOT l.is_magnet_product AND l.quantity > 0
        AND l.net_price_paid > 0
    GROUP BY ALL
) TO '${WP2_INT}/state_upc_q/panel_year=${YEAR}.parquet'
(FORMAT parquet, COMPRESSION zstd);
