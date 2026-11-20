"""src/data_prep/02_stage_raw/04_stage_panelviews.py

Stage PanelViews health-care surveys.

The surveys arrive as very wide Excel workbooks (about 750 columns, one
sheet), one per survey year, with question wording and coding that change
across years. This script does NOT harmonise ailments (that is analysis
work). It:
  1. reads every sheet as strings (no type guessing),
  2. finds the household-ID column and casts it to household_code,
  3. normalises column names (the original header text is kept in a
     per-year dictionary so nothing is lost),
  4. writes intermediate/panelviews/health/survey_year=<Y>/data.parquet.
Python/polars is used here because DuckDB cannot read .xlsx natively.

usage: python src/data_prep/02_stage_raw/04_stage_panelviews.py [--year 2019] [--force]
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[3]))  # repo root

import polars as pl  # noqa: E402

from src.lib.py.common import INT, RAW, Log, manifest  # noqa: E402

ID_RX = re.compile(r"panelist\s*id|hhid|house\w*\s*id|household", re.I)
SURVEY_RX = re.compile(r"survey\s*number", re.I)


def norm_unique(names: list[str]) -> list[str]:
    seen, out = {}, []
    for n in names:
        b = re.sub(r"[^0-9a-z]+", "_", str(n).lower()).strip("_") or "col"
        k = b
        i = 1
        while k in seen:
            i += 1
            k = f"{b}__{i}"
        seen[k] = 1
        out.append(k)
    return out


def stage(path: Path, year: int, force: bool, log) -> dict | None:
    dst = (
        INT / "panelviews" / "health" / f"survey_year={year}" / "data.parquet"
    )
    if (
        dst.exists()
        and not force
        and dst.stat().st_mtime > path.stat().st_mtime
    ):
        log(f"skip (up to date) {dst.relative_to(INT)}")
        return None
    log(f"reading {path.name} ({path.stat().st_size / 1e6:.0f} MB)")
    sheets = pl.read_excel(
        path, sheet_id=0, engine="calamine", infer_schema_length=0
    )
    if isinstance(sheets, pl.DataFrame):
        sheets = {"sheet1": sheets}
    # Use the largest sheet (the parsed-data sheet); log the others.
    name, df = max(sheets.items(), key=lambda kv: kv[1].height)
    for k, v in sheets.items():
        log(
            f"  sheet '{k}': {v.height} rows x {v.width} cols"
            + ("  <- used" if k == name else "")
        )
    raw_names = df.columns
    new_names = norm_unique(raw_names)
    df.columns = new_names
    id_col = next(
        (n for n, r in zip(new_names, raw_names) if ID_RX.search(r)), None
    )
    sv_col = next(
        (n for n, r in zip(new_names, raw_names) if SURVEY_RX.search(r)), None
    )
    if id_col is None:
        raise ValueError(f"no household-id column found in {path.name}")
    df = df.with_columns(
        pl.col(id_col)
        .str.strip_chars()
        .str.replace(r"\.0$", "")
        .cast(pl.Int64, strict=False)
        .alias("household_code"),
        pl.lit(year, dtype=pl.Int16).alias("survey_year"),
    )
    n_bad_id = df.filter(pl.col("household_code").is_null()).height
    dst.parent.mkdir(parents=True, exist_ok=True)
    tmp = dst.with_suffix(".parquet.tmp")
    df.write_parquet(tmp, compression="zstd")
    tmp.replace(dst)
    n_hh = df["household_code"].n_unique()
    rec = dict(
        survey_year=year,
        file=path.name,
        sheet=name,
        rows=df.height,
        cols=df.width,
        households=n_hh,
        id_col=id_col,
        survey_col=sv_col,
        rows_bad_id=n_bad_id,
        max_rows_per_hh=int(df.group_by("household_code").len()["len"].max()),
    )
    log(f"  -> {dst.relative_to(INT)} {rec}")
    return rec


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--year", type=int)
    ap.add_argument("--force", action="store_true")
    a = ap.parse_args()
    log = Log("desc_01_stage_panelviews")
    man = manifest().filter(
        (pl.col("source") == "panelviews")
        & (pl.col("table") == "health_parsed")
    )
    if a.year:
        man = man.filter(pl.col("year") == a.year)
    failed = 0
    for r in man.sort("year").iter_rows(named=True):
        try:
            stage(RAW / r["relpath"], r["year"], a.force, log)
        except Exception as e:
            log(f"FAILED {r['relpath']}: {type(e).__name__}: {e}")
            failed += 1
    if failed:
        sys.exit(f"{failed} survey file(s) failed to stage; see {log.path}")


if __name__ == "__main__":
    main()
