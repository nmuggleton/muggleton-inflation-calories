"""src/lib/py/schema.py -- canonical column names and types for staged tables.

Raw Kilts files change header case and naming across years (e.g. 2019
panelists use 'Household_Cd'; 2021 uses 'household_code'), and the 2021
release drops some columns. Staging reads every raw column as VARCHAR,
normalises names (lower-case), maps aliases to canonical names, adds
missing canonical columns as typed NULLs and TRY_CASTs to the types below.
Columns not listed are kept as VARCHAR so nothing is silently dropped.

Types follow the Kilts Consumer Panel Dataset Manual. UPCs, ZIP codes and
payment codes stay VARCHAR to preserve leading zeros.
"""

ALIASES = {
    "household_cd": "household_code",
    "wic_indicator_ever_not_current": "wic_indicator_ever_notcurrent",
    # 2004-2020 panelists geography names -> 2021+ / manual names
    "panelist_zipcd": "panelist_zip_code",
    "fips_state_cd": "fips_state_code",
    "fips_state_desc": "fips_state_descr",
    "fips_county_cd": "fips_county_code",
    "fips_county_desc": "fips_county_descr",
    "region_cd": "region_code",
    "scantrack_market_identifier_cd": "scantrack_market_code",
    "scantrack_market_identifier_desc": "scantrack_market_descr",
    "dma_cd": "dma_code",
    "dma_name": "dma_descr",
}

SCHEMA = {
    "purchases": {
        "trip_code_uc": "BIGINT",
        "upc": "VARCHAR",
        "upc_ver_uc": "SMALLINT",
        "quantity": "INTEGER",
        "total_price_paid": "DOUBLE",
        "coupon_value": "DOUBLE",
        "deal_flag_uc": "TINYINT",
        # 2021+ purchase.tsv carries legacy (HMS) product attributes and
        # panel_year, and has NO upc_ver_uc (products keyed by upc + year).
        "product_module_code_hms": "INTEGER",
        "size1_amount_hms": "VARCHAR",
        "size1_unit_hms": "VARCHAR",
        "brand_descr_hms": "VARCHAR",
        "multi_hms": "INTEGER",
        "panel_year": "SMALLINT",
    },
    "productdesc": {
        "upc": "VARCHAR",
        "product_descr": "VARCHAR",
        "product_module_code": "INTEGER",
        "product_module_descr": "VARCHAR",
        "multi": "INTEGER",
        "year": "SMALLINT",
    },
    "producthierarchy": {
        "upc": "VARCHAR",
        "year": "SMALLINT",
        "super_category": "VARCHAR",
        "super_category_cd": "BIGINT",
        "category": "VARCHAR",
        "category_cd": "BIGINT",
        "sub_category": "VARCHAR",
        "sub_category_cd": "BIGINT",
        "segment": "VARCHAR",
        "segment_cd": "BIGINT",
        "department": "VARCHAR",
        "department_cd": "BIGINT",
    },
    "trips": {
        "trip_code_uc": "BIGINT",
        "household_code": "INTEGER",
        "purchase_date": "DATE",
        "retailer_code": "INTEGER",
        "store_code_uc": "INTEGER",
        "panel_year": "SMALLINT",
        "store_zip3": "VARCHAR",
        "total_spent": "DOUBLE",
        "method_of_payment_cd": "VARCHAR",
    },
    "panelists": {
        "household_code": "INTEGER",
        "panel_year": "SMALLINT",
        "projection_factor": "INTEGER",
        "projection_factor_magnet": "INTEGER",
        "household_income": "SMALLINT",
        "household_size": "SMALLINT",
        "type_of_residence": "SMALLINT",
        "household_composition": "SMALLINT",
        "age_and_presence_of_children": "SMALLINT",
        "male_head_age": "SMALLINT",
        "female_head_age": "SMALLINT",
        "male_head_employment": "SMALLINT",
        "female_head_employment": "SMALLINT",
        "male_head_education": "SMALLINT",
        "female_head_education": "SMALLINT",
        "male_head_occupation": "SMALLINT",
        "female_head_occupation": "SMALLINT",
        "male_head_birth": "SMALLINT",
        "female_head_birth": "SMALLINT",
        "marital_status": "SMALLINT",
        "race": "SMALLINT",
        "hispanic_origin": "SMALLINT",
        "panelist_zip_code": "VARCHAR",
        "fips_state_code": "SMALLINT",
        "fips_state_descr": "VARCHAR",
        "fips_county_code": "SMALLINT",
        "fips_county_descr": "VARCHAR",
        "region_code": "SMALLINT",
        "scantrack_market_code": "SMALLINT",
        "scantrack_market_descr": "VARCHAR",
        "dma_code": "SMALLINT",
        "dma_descr": "VARCHAR",
        "kitchen_appliances": "SMALLINT",
        "tv_items": "SMALLINT",
        "household_internet_connection": "SMALLINT",
        "wic_indicator_current": "SMALLINT",
        "wic_indicator_ever_notcurrent": "SMALLINT",
        **{
            f"member_{i}_{v}": "SMALLINT"
            for i in range(1, 8)
            for v in ("birth", "relationship_sex", "employment")
        },
    },
    "products_master": {
        "upc": "VARCHAR",
        "upc_ver_uc": "SMALLINT",
        "upc_descr": "VARCHAR",
        "product_module_code": "INTEGER",
        "product_module_descr": "VARCHAR",
        "product_group_code": "INTEGER",
        "product_group_descr": "VARCHAR",
        "department_code": "INTEGER",
        "department_descr": "VARCHAR",
        "brand_code_uc": "INTEGER",
        "brand_descr": "VARCHAR",
        "multi": "INTEGER",
        "size1_code_uc": "INTEGER",
        "size1_amount": "DOUBLE",
        "size1_units": "VARCHAR",
        "dataset_found_uc": "VARCHAR",
        "size1_change_flag_uc": "SMALLINT",
    },
    "retailers_master": {
        "retailer_code": "INTEGER",
        "channel_type": "VARCHAR",
    },
    "retailers": {"retailer_code": "INTEGER", "channel_type": "VARCHAR"},
    "brand_variations": {
        "brand_code_uc": "INTEGER",
        "brand_descr": "VARCHAR",
        "brand_descr_alternative": "VARCHAR",
        "start_date": "DATE",
        "end_date": "DATE",
        "datasets_found_uc": "VARCHAR",
    },
    "stores": {
        "store_code_uc": "INTEGER",
        "year": "SMALLINT",
        "parent_code": "INTEGER",
        "retailer_code": "INTEGER",
        "channel_code": "VARCHAR",  # letter code (F, D, M, C, L)
        "store_zip3": "VARCHAR",
        "fips_state_code": "SMALLINT",
        "fips_state_descr": "VARCHAR",
        "fips_county_code": "SMALLINT",
        "fips_county_descr": "VARCHAR",
        "dma_code": "SMALLINT",
        "dma_descr": "VARCHAR",
        "acv": "DOUBLE",
    },
    # products_extra and the 2021+ product files: codes cast where the name
    # says so; everything else VARCHAR (see RULE_TYPES).
}

# Name-based fallback typing for tables without an explicit schema.
RULE_TYPES = [
    (r"^upc$", "VARCHAR"),
    (r"_descr$|_desc$|description|_name$|_cd$", "VARCHAR"),
    (r"^panel_year$|^year$", "SMALLINT"),
    (r"^upc_ver_uc$", "SMALLINT"),
    (r"_code$|_code_uc$|_id$", "BIGINT"),
    (r"_amount$|_amo$", "DOUBLE"),
]
