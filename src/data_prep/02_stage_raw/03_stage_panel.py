"""src/data_prep/02_stage_raw/03_stage_panel.py -- stage raw Kilts TSVs as
Parquet.

One raw file -> one Parquet file. No rows are dropped and no analysis
variables are created. Column names are normalised and aliased
(desc_schema.ALIASES); canonical columns are TRY_CAST to their types;
canonical columns absent in a given year are added as typed NULLs.

Layout ($WP2_INT = <WP2_DATA>/intermediate):
  panel/<table>/panel_year=<YYYY>/data.parquet     annual tables
  panel/<table>/data.parquet                       master tables
Idempotent: skips outputs newer than their input unless --force.
Writes to a temp file then renames, so a crash never leaves a partial
Parquet in place. Row counts go to the run log.

usage:
  python src/data_prep/02_stage_raw/03_stage_panel.py      every stageable table
  python src/data_prep/02_stage_raw/03_stage_panel.py --table purchases --year 2019
"""

from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[3]))  # repo root

import polars as pl  # noqa: E402

from src.lib.py.common import (
    INT,
    RAW,
    RMS_RAW,
    Log,  # noqa: E402
    connect,
    header,
    manifest,
    read_raw_sql,
    run_with_encoding,
    select_list,
)

STAGE_TABLES = [
    "purchases",
    "trips",
    "panelists",
    "products_extra",
    "products_master",
    "retailers_master",
    "retailers",
    "brand_variations",
    "productdesc",
    "producthierarchy",
    "productattributes_bp",
    "productattributes",
    "stores",
    "rms_versions",
]


def out_path(source: str, table: str, year) -> Path:
    base = INT / source / table
    return (
        base / f"panel_year={year}" / "data.parquet"
        if year
        else base / "data.parquet"
    )


def stage_one(con, r: dict, force: bool, log) -> dict | None:
    # relpaths are relative to their tree: RMS_RAW for the scanner and for
    # panel years read in place from the shared drive (02a_classify.py)
    in_rms_tree = r["source"] == "rms" or r.get("root") == "rms_raw"
    src = (RMS_RAW if in_rms_tree else RAW) / r["relpath"]
    dst = out_path(r["source"], r["table"], r["year"])
    if (
        dst.exists()
        and not force
        and dst.stat().st_mtime > src.stat().st_mtime
    ):
        log(f"skip (up to date) {dst.relative_to(INT)}")
        return None
    dst.parent.mkdir(parents=True, exist_ok=True)
    tmp = dst.with_suffix(".parquet.tmp")
    cols = header(src)
    sel = select_list(r["table"], cols, r["year"])

    def build(encoding):
        return (
            f"COPY (SELECT\n  {sel}\nFROM {read_raw_sql(src, cols, encoding)})"
            f"\nTO '{tmp}' (FORMAT parquet, COMPRESSION zstd, "
            f"ROW_GROUP_SIZE 1000000)"
        )

    log(f"staging {r['relpath']} ({r['bytes'] / 1e9:.2f} GB)")
    _, enc = run_with_encoding(con, build, log, r["relpath"])
    n_out = con.execute(
        f"SELECT count(*) FROM read_parquet('{tmp}')"
    ).fetchone()[0]
    os.replace(tmp, dst)
    log(
        f"  -> {dst.relative_to(INT)} rows={n_out:,} "
        f"size={dst.stat().st_size / 1e9:.2f} GB"
    )
    return dict(
        source=r["source"],
        table=r["table"],
        year=r["year"],
        raw=r["relpath"],
        raw_bytes=r["bytes"],
        rows_out=n_out,
        encoding=enc,
        parquet=str(dst.relative_to(INT)),
        parquet_bytes=dst.stat().st_size,
    )


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--table")
    ap.add_argument("--year", type=int)
    ap.add_argument(
        "--source", default="panel", choices=["panel", "rms", "rms_b2"]
    )
    ap.add_argument("--force", action="store_true")
    a = ap.parse_args()
    log = Log("desc_01_stage")
    man = manifest().filter(
        (pl.col("source") == a.source) & pl.col("table").is_in(STAGE_TABLES)
    )
    # Retail Scanner 2021+ product attributes duplicate the panel files; the
    # 2022 ones are the only source of 2022 nutrition facts, so keep them.
    if a.table:
        man = man.filter(pl.col("table") == a.table)
    if a.year:
        man = man.filter(pl.col("year") == a.year)
    # Small files first so failures surface early.
    man = man.sort("bytes")
    if man.height == 0:
        sys.exit(
            f"no {a.source} files to stage in the manifest "
            "(run 02_inventory.sh first)"
        )
    con = connect()
    failed = 0
    for r in man.iter_rows(named=True):
        try:
            stage_one(con, r, a.force, log)
        except Exception as e:
            log(f"FAILED {r['relpath']}: {type(e).__name__}: {e}")
            failed += 1
    if failed:
        sys.exit(f"{failed} file(s) failed to stage; see {log.path}")


if __name__ == "__main__":
    main()
