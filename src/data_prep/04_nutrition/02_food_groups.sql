-- Module -> food-group crosswalk. Run with run_sql (src/lib/paths.sh), which
-- fills ${WP2_FINAL} and ${FG_OUT} (default:
-- final/xwalk_module_foodgroup.parquet).

CREATE TEMP TABLE group_default (
    product_group_code INTEGER, food_group VARCHAR
);
INSERT INTO group_default VALUES
(1501, 'grains'), (1503, 'ssb'), (503, 'sweets'), (1006, 'coffee_tea'),
(507, 'ssb'), (1005, 'grains'), (1007, 'fats_condiments'),
(1508, 'ssb'), (1505, 'sweets'), (1011, 'protein'), (511, 'prepared'),
(513, 'prepared'), (510, 'prepared'), (514, 'fv'), (1004, 'grains'),
(1506, 'grains'), (1020, 'coffee_tea'), (1012, 'dairy'),
(1015, 'fats_condiments'), (506, 'sweets'), (1017, 'fats_condiments'),
(1002, 'fats_condiments'), (1016, 'fats_condiments'), (1021, 'grains'),
(1008, 'sweets'), (1018, 'sweets'), (1010, 'fv'),
(1014, 'fats_condiments'), (512, 'protein'), (1013, 'grains'),
(1001, 'sweets'), (501, 'baby'), (504, 'fv'), (505, 'sweets'),
(1009, 'grains'), (1019, 'sweets'), (1507, 'salty_snacks'),
(2009, 'protein'), (2008, 'prepared'), (2005, 'sweets'),
(2007, 'prepared'), (2010, 'fv'), (2002, 'grains'),
(2003, 'sweets'), (2001, 'grains'), (2006, 'juice100'),
(2502, 'dairy'), (2506, 'dairy'), (2510, 'dairy'),
(2501, 'fats_condiments'), (2505, 'protein'), (2503, 'dairy'),
(2508, 'salty_snacks'), (2504, 'grains'), (2507, 'sweets'),
(3001, 'prepared'), (3002, 'protein'), (3501, 'protein'),
(4001, 'fv');

-- Module-description overrides: exceptions first, and the lowest priority wins
CREATE TEMP TABLE module_override (
    priority INTEGER, pattern VARCHAR, food_group VARCHAR
);
INSERT INTO module_override VALUES
(1, 'POT PIES|IN PASTRY|ASPIC', 'prepared'),
(
    2,
    'ENGLISH MUFFIN|BISCUITS/ROLLS/MUFFINS|WAFFLES|PANCAKE|RICE CAKES',
    'grains'
),
(3, 'CHEESECAKE', 'sweets'),
(4, 'SNACKS - PUFFED CHEESE', 'salty_snacks'),
(5, 'CRACKERS - CHEESE', 'grains'),
(6, 'SAUCE MIX - CHEESE|SALAD DRESSING', 'fats_condiments'),
(7, 'CANDY-DIETETIC|PUDDING - DIET|GELATIN - DIET', 'sweets'),
(8, 'REFERENCE CARD TAKE OUT', 'takeout'),
(9, 'REFERENCE CARD FOUNTAIN', 'ssb'),
(10, 'REFERENCE CARD (FRUITS|VEGETABLES)', 'fv'),
(11, 'REFERENCE CARD (MEAT|POULTRY|SEAFOOD|COLD CUTS)', 'protein'),
(12, 'REFERENCE CARD CHEESE', 'dairy'),
(13, 'REFERENCE CARD (BAKED GOODS - (CAKES|COOKIES|PIES)|CANDY)', 'sweets'),
(14, 'REFERENCE CARD BAKED GOODS', 'grains'),
(15, 'REFERENCE CARD (PREPARED FOODS|MEAL KIT)', 'prepared'),
(16, 'REFERENCE CARD COFFEE', 'coffee_tea'),
(17, 'LOW CALORIE|WATER-BOTTLED|WATER - |SELTZER|DIET', 'diet_water'),
(
    18,
    '^FRUIT JUICE|VEGETABLE JUICE|JUICE.*100|JUICES? - FROZEN|JUICE-FROZEN',
    'juice100'
),
(
    19,
    'CAKES|PIES|DOUGHNUT|SWEET ROLL|PASTR|BROWNIE|MUFFIN|TOASTER PASTR|COOKIE',
    'sweets'
),
(20, 'PEANUT BUTTER', 'protein'),
(21, 'CREAMER', 'coffee_tea'),
(22, 'PRECUT FRESH SALAD|FRUIT-REFRIGERATED|SALAD MIX', 'fv'),
(
    23,
    'BEANS - DRY|BEANS-DRY|DRY BEANS|DRIED BEANS|LENTIL|PEAS - DRY',
    'protein'
),
(24, 'TORTILLAS|RICE -? ?(MIXES)?$|^RICE', 'grains'),
(25, 'SNACKS? - MEAT|MEAT SNACK', 'salty_snacks'),
(26, 'HEALTH BARS|GRANOLA|BREAKFAST BARS', 'sweets'),
(27, 'DIP', 'fats_condiments'),
(28, 'HONEY|JAM|JELL|PRESERVES|MARMALADE', 'sweets'),
(29, 'POTATOES? - (MASHED|DEHYDRATED)', 'prepared'),
(30, 'BABY|INFANT|FORMULA', 'baby');

CREATE TEMP TABLE m AS
SELECT
    *,
    is_food_department
    AND product_group_code NOT IN (508, 2004, 2509) AS is_human_food
FROM '${WP2_FINAL}/xwalk_module.parquet';

COPY (
    WITH hit AS (
        SELECT
            m.product_module_code,
            arg_min(o.food_group, o.priority) AS food_group,
            arg_min(o.pattern, o.priority) AS pattern
        FROM m JOIN module_override AS o
            ON
                m.is_human_food
                AND regexp_matches(m.product_module_descr, o.pattern)
        GROUP BY 1
    )

    SELECT
        m.product_module_code,
        m.product_module_descr,
        m.product_group_code,
        m.product_group_descr,
        m.department_code,
        m.is_human_food,
        m.is_magnet_product,
        CASE
            WHEN NOT m.is_human_food THEN NULL
            WHEN h.food_group IS NOT NULL THEN h.food_group
            WHEN g.food_group IS NOT NULL THEN g.food_group
            WHEN m.is_human_food THEN 'other_food'
        END AS food_group,
        CASE
            WHEN NOT m.is_human_food THEN 'not food'
            WHEN h.food_group IS NOT NULL THEN 'module: ' || h.pattern
            WHEN g.food_group IS NOT NULL THEN 'group default'
            WHEN m.is_human_food THEN 'unmatched'
        END AS fg_rule
    FROM m
    LEFT JOIN hit AS h USING (product_module_code)
    LEFT JOIN group_default AS g USING (product_group_code)
) TO '${FG_OUT}' (FORMAT parquet);
