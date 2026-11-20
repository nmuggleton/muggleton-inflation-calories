"""src/data_prep/02_stage_raw/02a_classify.py

Map raw NielsenIQ files to (source, table, year).

Reads the inventory written by 02_inventory.sh and writes the manifest the
staging scripts use to find their inputs, so the build does not depend on
the exact folder layout of the raw copy.

usage: python 02a_classify.py inventory.tsv manifest.tsv [rms_inventory.tsv]
"""

import re
import sys

import polars as pl

# (source, table, regex). First match wins. Year captured as group 'y'.
RULES = [
    # Pilot-derived outputs sitting next to the raw data (not raw)
    ("derived_pilot", "calculations", r"^calculations/"),
    ("census", "acs_gini", r"^gini/"),
    # Retail Scanner 2021+ ("File Select Supercategory" layout)
    (
        "rms_b2",
        "annual",
        r"(?P<y>20[2-9]\d)_File_Select_Supercategory_MovementData/01_Annual_Files/"
        r"[^/]+\.tsv$",
    ),
    (
        "rms_b2",
        "movement",
        r"(?P<y>20[2-9]\d)_File_Select_Supercategory_MovementData/\d+_[^/]+/.+\.tsv$",
    ),
    (
        "rms_b2",
        "movement_tgz",
        r"(?P<y>20[2-9]\d)_File_Select_Supercategory_MovementData/.+\.tgz$",
    ),
    ("rms_b2", "full_tgz", r"SCANNER_DATA_(?P<y>20[2-9]\d)\.tgz$"),
    # Consumer Panel (HMS), 2004-2020 layout
    (
        "panel",
        "purchases",
        r"(?:^|/)(?P<y>\d{4})/Annual_Files/purchases_\d{4}\.tsv$",
    ),
    ("panel", "trips", r"(?:^|/)(?P<y>\d{4})/Annual_Files/trips_\d{4}\.tsv$"),
    (
        "panel",
        "panelists",
        r"(?:^|/)(?P<y>\d{4})/Annual_Files/panelists_\d{4}\.tsv$",
    ),
    (
        "panel",
        "products_extra",
        r"HMS/(?P<y>\d{4})/Annual_Files/products_extra_\d{4}\.tsv$",
    ),
    # Consumer Panel, 2021-onward layout (no year suffix in file names)
    (
        "panel",
        "purchases",
        r"(?:^|/)(?P<y>20[2-9]\d)/Annual_Files/purchase\.tsv$",
    ),
    ("panel", "trips", r"(?:^|/)(?P<y>20[2-9]\d)/Annual_Files/trip\.tsv$"),
    (
        "panel",
        "panelists",
        r"(?:^|/)(?P<y>20[2-9]\d)/Annual_Files/panelist\.tsv$",
    ),
    (
        "panel",
        "productdesc",
        r"(?P<y>20[2-9]\d)/Annual_Files/productdesc\.tsv$",
    ),
    (
        "panel",
        "producthierarchy",
        r"(?P<y>20[2-9]\d)/Annual_Files/producthierarchy\.tsv$",
    ),
    (
        "panel",
        "productattributes_bp",
        r"(?P<y>20[2-9]\d)/Annual_Files/productattributes_bp\.tsv$",
    ),
    (
        "panel",
        "productattributes",
        r"(?P<y>20[2-9]\d)/Annual_Files/productattributes\.tsv$",
    ),
    ("panel", "retailers", r"(?P<y>20[2-9]\d)/Annual_Files/retailer\.tsv$"),
    # Consumer Panel master files (2004-2020)
    ("panel", "products_master", r"HMS/Master_Files/Latest/products\.tsv$"),
    ("panel", "retailers_master", r"HMS/Master_Files/Latest/retailers\.tsv$"),
    (
        "panel",
        "brand_variations",
        r"HMS/Master_Files/Latest/brand_variations\.tsv$",
    ),
    # Retail Scanner (RMS)
    (
        "rms",
        "movement",
        r"RMS/(?P<y>\d{4})/Movement_Files/\d+_\d{4}/\d+_\d{4}\.tsv$",
    ),
    ("rms", "stores", r"RMS/(?P<y>\d{4})/Annual_Files/stores_\d{4}\.tsv$"),
    (
        "rms",
        "rms_versions",
        r"RMS/(?P<y>\d{4})/Annual_Files/rms_versions_\d{4}\.tsv$",
    ),
    (
        "rms",
        "products_extra",
        r"RMS/(?P<y>\d{4})/Annual_Files/products_extra_\d{4}\.tsv$",
    ),
    (
        "rms",
        "products_master",
        r"(?:RMS/Master_Files|Scanner_Data/Master_Files_\d{4}-\d{4})/Latest/products\.tsv$",
    ),
    (
        "rms",
        "brand_variations",
        r"(?:RMS/Master_Files|Scanner_Data/Master_Files_\d{4}-\d{4})/Latest/brand_variations\.tsv$",
    ),
    # PanelViews health-care surveys
    (
        "panelviews",
        "health_parsed",
        r"(?P<y>\d{4})/[^/]*(?:Health|Ailment|HCS)[^/]*Parsed[^/]*\.xlsx?$",
    ),
    (
        "panelviews",
        "health_parsed",
        r"(?P<y>\d{4})/[^/]*Health Shopper Data File[^/]*\.xlsx?$",
    ),
    (
        "panelviews",
        "health_format",
        r"(?P<y>\d{4})/[^/]*(?:Health|HCS|format|Layout)[^/]*\.(?:xlsx?|docx?)$",
    ),
    # Archives (Globus downloads that may not be unpacked yet)
    ("archive", "tgz", r"\.(?:tgz|tar\.gz|tar)$"),
    ("archive", "zip", r"\.zip$"),
    # Documentation
    ("docs", "doc", r"\.(?:pdf|docx?|xlsx?|xls|txt|csv|md|html?)$"),
]
COMPILED = [(s, t, re.compile(p, re.I)) for s, t, p in RULES]


def classify(path: str):
    for src, tab, rx in COMPILED:
        m = rx.search(path)
        if m:
            y = m.groupdict().get("y")
            return src, tab, int(y) if y else None
    return "other", "other", None


def read_classified(inv_path: str) -> pl.DataFrame:
    inv = pl.read_csv(inv_path, separator="\t", infer_schema_length=0)
    inv = inv.with_columns(pl.col("bytes").cast(pl.Int64))
    rows = [classify(p) for p in inv["relpath"].to_list()]
    return inv.with_columns(
        pl.Series("source", [r[0] for r in rows], dtype=pl.Utf8),
        pl.Series("table", [r[1] for r in rows], dtype=pl.Utf8),
        pl.Series("year", [r[2] for r in rows], dtype=pl.Int32),
    )


def main(inv_path: str, out_path: str, rms_inv_path: str | None) -> None:
    man = read_classified(inv_path)
    # RMS in its own tree (RMS_RAW): its rms rows replace any in the main
    # tree, and nothing else is taken from it. Staging resolves rms paths
    # against RMS_RAW, so relpaths stay relative to their own root.
    # Consumer Panel years missing from the main tree (2022+, which were
    # never copied locally) are read in place from the same shared tree.
    # Column `root` (last, so the shell scripts' column numbers hold) says
    # which tree a relpath is relative to: raw (NIELSEN_RAW) or rms_raw.
    man = man.with_columns(pl.lit("raw").alias("root"))
    if rms_inv_path:
        other = read_classified(rms_inv_path).with_columns(
            pl.lit("rms_raw").alias("root")
        )
        have = man.filter(
            (pl.col("source") == "panel") & pl.col("year").is_not_null()
        )["year"].unique()
        extra = other.filter(
            (pl.col("source") == "panel")
            & pl.col("year").is_not_null()
            & ~pl.col("year").is_in(have.implode())
        )
        if extra.height:
            print(
                f"panel years read from RMS_RAW: "
                f"{sorted(extra['year'].unique().to_list())}"
            )
        man = pl.concat(
            [
                man.filter(pl.col("source") != "rms"),
                other.filter(pl.col("source") == "rms"),
                extra,
            ]
        )
    # Retail Scanner 2021+ annual files: table = file stem (stores, ...).
    stem = man["relpath"].str.extract(r"([^/]+)\.tsv$", 1)
    man = man.with_columns(
        pl.when((pl.col("source") == "rms_b2") & (pl.col("table") == "annual"))
        .then(stem)
        .otherwise(pl.col("table"))
        .alias("table")
    )
    # RMS movement files: keep product group / module ids from the path.
    grp = man["relpath"].str.extract(r"Movement_Files/(\d+)_\d{4}/", 1)
    mod = man["relpath"].str.extract(
        r"Movement_Files/\d+_\d{4}/(\d+)_\d{4}", 1
    )
    man = man.with_columns(
        grp.cast(pl.Int32).alias("rms_group"),
        mod.cast(pl.Int32).alias("rms_module"),
    ).select(pl.exclude("root"), "root")
    man.write_csv(out_path, separator="\t")
    summ = (
        man.group_by("source", "table")
        .agg(
            pl.len().alias("n_files"),
            (pl.col("bytes").sum() / 1e9).round(2).alias("gb"),
            pl.col("year").min().alias("y0"),
            pl.col("year").max().alias("y1"),
        )
        .sort("source", "table")
    )
    with pl.Config(tbl_rows=100, tbl_width_chars=120):
        print(summ)
    dups = (
        man.filter(
            pl.col("year").is_not_null()
            & pl.col("source").is_in(["panel", "rms"])
            & (pl.col("table") != "movement")
        )
        .group_by("source", "table", "year")
        .agg(pl.len().alias("n"))
        .filter(pl.col("n") > 1)
    )
    if dups.height:
        print("WARNING duplicate (source, table, year) entries:")
        print(dups)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else None)
