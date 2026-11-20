"""src/lib/py/common.py

Helpers shared by the staging scripts in src/data_prep/02_stage_raw/.

All heavy work is done by DuckDB SQL; Python only reads file headers,
builds the harmonising SELECT list, and writes logs.
"""

from __future__ import annotations

import os
import re
import time
from pathlib import Path

import duckdb
import polars as pl

from src.lib.py.schema import ALIASES, RULE_TYPES, SCHEMA

ENV = os.environ
RAW = Path(ENV["NIELSEN_RAW"])
RMS_RAW = Path(ENV.get("RMS_RAW") or ENV["NIELSEN_RAW"])
INT = Path(ENV["WP2_INT"])
FINAL = Path(ENV["WP2_FINAL"])
LOGS = Path(ENV["WP2_LOGS"])


def connect(db: str = ":memory:") -> duckdb.DuckDBPyConnection:
    con = duckdb.connect(db)
    con.execute(f"SET threads = {ENV.get('DUCKDB_THREADS', '8')}")
    con.execute(f"SET memory_limit = '{ENV.get('DUCKDB_MEMORY', '20GB')}'")
    con.execute(f"SET temp_directory = '{ENV['DUCKDB_TMP']}'")
    con.execute(
        "SET max_temp_directory_size = "
        f"'{ENV.get('DUCKDB_MAX_TEMP', '20GiB')}'"
    )
    con.execute("SET preserve_insertion_order = false")
    return con


def manifest() -> pl.DataFrame:
    """Manifest rows; if WP2_YEARS is set (e.g. "2019 2021"), annual files
    are restricted to those years (master/undated files always kept)."""
    man = pl.read_csv(
        INT / "raw_manifest.tsv", separator="\t", infer_schema_length=0
    ).with_columns(
        pl.col("bytes").cast(pl.Int64), pl.col("year").cast(pl.Int32)
    )
    yrs = ENV.get("WP2_YEARS", "").split()
    if yrs:
        man = man.filter(
            pl.col("year").is_null()
            | pl.col("year").is_in([int(y) for y in yrs])
        )
    return man


def norm(name: str) -> str:
    n = re.sub(r"[^0-9a-z]+", "_", name.strip().lower()).strip("_")
    return ALIASES.get(n, n)


def header(path: Path) -> list[str]:
    with open(path, "r", encoding="utf-8", errors="replace") as fh:
        first = fh.readline()
    return [norm(c) for c in first.rstrip("\r\n").split("\t")]


def rule_type(col: str) -> str:
    for rx, typ in RULE_TYPES:
        if re.search(rx, col):
            return typ
    return "VARCHAR"


def target_types(table: str, cols: list[str]) -> dict[str, str]:
    """Canonical columns (in schema order) plus any extra raw columns."""
    sch = SCHEMA.get(table, {})
    out = dict(sch)
    for c in cols:
        if c not in out:
            out[c] = rule_type(c) if not sch else "VARCHAR"
    return out


def read_raw_sql(path: Path, cols: list[str], encoding: str = "utf-8") -> str:
    names = ", ".join(f"'{c}'" for c in cols)
    return (
        f"read_csv('{path}', delim='\\t', header=true, all_varchar=true, "
        f"quote='', escape='', null_padding=true, strict_mode=false, "
        f"names=[{names}], encoding='{encoding}')"
    )


def q(col: str) -> str:
    return '"' + col.replace('"', '""') + '"'


def cast_expr(col: str, typ: str, present: bool) -> str:
    if not present:
        return f"CAST(NULL AS {typ}) AS {q(col)}"
    raw = f"NULLIF(trim({q(col)}), '')"
    if typ == "VARCHAR":
        return f"{raw} AS {q(col)}"
    return f"TRY_CAST({raw} AS {typ}) AS {q(col)}"


def select_list(table: str, cols: list[str], year: int | None) -> str:
    types = target_types(table, cols)
    parts = [cast_expr(c, t, c in cols) for c, t in types.items()]
    if year is not None and "panel_year" not in types:
        parts.append(f"CAST({year} AS SMALLINT) AS panel_year")
    return ",\n  ".join(parts)


class Log:
    def __init__(self, name: str):
        LOGS.mkdir(parents=True, exist_ok=True)
        self.path = LOGS / f"{name}.log"
        self.fh = open(self.path, "a", encoding="utf-8")
        self.t0 = time.time()

    def __call__(self, *msg):
        line = (
            f"[{time.strftime('%F %T')} +{time.time() - self.t0:7.0f}s] "
            + " ".join(str(m) for m in msg)
        )
        print(line, flush=True)
        self.fh.write(line + "\n")
        self.fh.flush()


def run_with_encoding(con, build_sql, log, what: str):
    """Run SQL built by build_sql(encoding); retry as latin-1 if the file
    is not valid UTF-8 (Kilts product descriptions contain latin-1 bytes).
    Returns (result, encoding_used)."""
    try:
        return con.execute(build_sql("utf-8")), "utf-8"
    except Exception as e:  # duckdb.InvalidInputException etc.
        if "utf" not in str(e).lower() and "unicode" not in str(e).lower():
            raise
        log(f"  {what}: not valid UTF-8, retrying as latin-1")
        return con.execute(build_sql("latin-1")), "latin-1"
