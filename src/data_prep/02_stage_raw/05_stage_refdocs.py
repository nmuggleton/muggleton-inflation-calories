"""src/data_prep/02_stage_raw/05_stage_refdocs.py

Stage two small Kilts reference spreadsheets
that carry information not present in the TSV data files.

  Product_Hierarchy 2004-2020 data.xlsx
      -> intermediate/ref/module_flags/data.parquet
         one row per product_module_code with the documentation flags
         (deferred module; HMS-only; RMS-only; no data). Manual sec. E:
         "Data for ALL deferred modules should be used with caution".
  Corrected_UPC_Version_Files_2006-2017.xlsx
      -> intermediate/ref/upc_version_corrections/data.parquet
         (upc, upc_ver_uc, panel_year) -> corrected (upc, upc_ver_uc);
         one sheet per year, header spellings vary across sheets.

Reads from $NIELSEN_RAW/kilts_panel/Reference_Documentation/
2004-2020_Documentation/. Polars + fastexcel (calamine engine).
"""

import os
import polars as pl

RAW = os.environ["NIELSEN_RAW"]
INT = os.path.join(os.environ["WP2_DATA"], "intermediate", "ref")
DOC = os.path.join(
    RAW, "kilts_panel", "Reference_Documentation", "2004-2020_Documentation"
)


def out(table: pl.DataFrame, dirname: str) -> None:
    d = os.path.join(INT, dirname)
    os.makedirs(d, exist_ok=True)
    tmp = os.path.join(d, "data.parquet.tmp")
    table.write_parquet(tmp)
    os.replace(tmp, os.path.join(d, "data.parquet"))
    print(f"{dirname}: {table.height} rows")


def flag(c: str) -> pl.Expr:
    return pl.col(c).is_not_null() & (
        pl.col(c).cast(pl.Utf8).str.strip_chars() != ""
    )


# 1. module flags ---------------------------------------------------------
h = pl.read_excel(
    os.path.join(DOC, "Product_Hierarchy 2004-2020 data.xlsx"),
    infer_schema_length=0,
)
h = h.select(h.columns[:11])
h.columns = [
    "product_module_code",
    "product_module_descr",
    "product_group_code",
    "product_group_descr",
    "department_code",
    "department_descr",
    "deferred",
    "hms_only_note",
    "rms_only",
    "rms_years_no_data",
    "no_data",
]
mods = (
    h.filter(pl.col("product_module_code").is_not_null())
    .select(
        pl.col("product_module_code").cast(pl.Int32),
        flag("deferred").alias("is_deferred_module"),
        pl.col("hms_only_note")
        .cast(pl.Utf8)
        .str.strip_chars()
        .alias("hms_only_note"),
        flag("rms_only").alias("is_rms_only_module"),
        pl.col("rms_years_no_data")
        .cast(pl.Utf8)
        .str.strip_chars()
        .alias("rms_years_no_data"),
        flag("no_data").alias("is_no_data_module"),
    )
    # module 8621 (cellular phone, non-food) appears twice with two
    # group codes (2006-13 vs 2014+): collapse to one row per module
    .group_by("product_module_code")
    .agg(
        pl.col("is_deferred_module").any(),
        pl.col("hms_only_note").drop_nulls().str.join("; "),
        pl.col("is_rms_only_module").any(),
        pl.col("rms_years_no_data").drop_nulls().str.join("; "),
        pl.col("is_no_data_module").any(),
    )
    .with_columns(
        pl.col(["hms_only_note", "rms_years_no_data"]).replace("", None)
    )
    .sort("product_module_code")
)
assert mods["product_module_code"].n_unique() == mods.height, "dup modules"
out(mods, "module_flags")

# 2. corrected UPC versions -------------------------------------------------
f = os.path.join(DOC, "Corrected_UPC_Version_Files_2006-2017.xlsx")
sheets = pl.read_excel(f, sheet_id=0, infer_schema_length=0)
parts = []
for name, df in sheets.items():
    df = df.select(df.columns[:5])
    df.columns = [
        "upc",
        "upc_ver_uc",
        "panel_year",
        "corrected_upc",
        "corrected_upc_ver_uc",
    ]
    parts.append(df)
cor = (
    pl.concat(parts)
    .filter(pl.col("upc").is_not_null())
    .select(
        pl.col("upc").cast(pl.Utf8).str.zfill(12),
        pl.col("upc_ver_uc").cast(pl.Float64).cast(pl.Int16),
        pl.col("panel_year").cast(pl.Float64).cast(pl.Int16),
        pl.col("corrected_upc").cast(pl.Utf8).str.zfill(12),
        pl.col("corrected_upc_ver_uc").cast(pl.Float64).cast(pl.Int16),
    )
)
out(cor, "upc_version_corrections")
