"""Check a data build against the reference checksums in config/checksums.json.

  python src/tools/check_data.py           compare; exit 1 on a mismatch
  python src/tools/check_data.py --write   record the current build

Built tables are fingerprinted, not hashed byte for byte (Parquet bytes vary
with writer and row order): row count plus, for every column, the non-null
count and an order-free sum of value hashes. Floating-point columns instead
record sum, sum of absolute values, min, max and the number of non-finite
values, compared within a relative tolerance (TOL): DuckDB adds doubles in
parallel, so their last digits differ from run to run. Public input files
are hashed with SHA-256. Hash sums depend on the DuckDB version, which is
recorded.

Paths come from src/lib/paths.sh (WP2_DATA); run run_data_prep.sh, or
`source src/lib/paths.sh` first.
"""

import hashlib
import json
import os
import sys
from pathlib import Path

import duckdb

ROOT = Path(__file__).resolve().parents[2]
REF = ROOT / "config" / "checksums.json"
DATA = Path(os.environ["WP2_DATA"])
FIN, INT = DATA / "final", DATA / "intermediate"
SS = Path(os.environ.get("SHIFTSHARE_INT") or INT / "shiftshare")
LOC = Path(os.environ.get("LOCAL_DATA") or INT)

H = "hive_partitioning = true"
TABLES = {
    "household_year": f"'{FIN}/household_year.parquet'",
    "sample_flags": f"'{FIN}/sample_flags.parquet'",
    "upc_nutrients": f"'{FIN}/upc_nutrients.parquet'",
    "xwalk_module_foodgroup": f"'{FIN}/xwalk_module_foodgroup.parquet'",
    "hh_quarter_outcomes": (
        f"read_parquet('{FIN}/hh_quarter_outcomes/*/*.parquet', {H})"
    ),
    "analysis_hh_quarter": (
        f"read_parquet('{FIN}/analysis_hh_quarter/*/*.parquet', {H})"
    ),
    "desc_panel": f"read_parquet('{INT}/desc_panel/*/*.parquet', {H})",
    "ladder_upc_kcal": f"'{INT}/ladder_upc_kcal/data.parquet'",
    "ladder_hh": f"read_parquet('{INT}/ladder_hh/*.parquet')",
    "ladder_tier": f"read_parquet('{INT}/ladder_tier/*.parquet')",
    "f1_analysis": f"read_parquet('{FIN}/f1_analysis/*/*.parquet', {H})",
    "shock_nat": f"read_parquet('{SS}/shock_nat/*.parquet')",
    "shock_st": f"read_parquet('{SS}/shock_st/*.parquet')",
    "shock_div": f"read_parquet('{SS}/shock_div/*.parquet')",
    "bartik": f"read_parquet('{SS}/bartik/*.parquet')",
    "state_controls": f"'{SS}/state_controls.parquet'",
    "state_q": f"'{LOC}/local_context/state_q.parquet'",
    "county_q": f"'{LOC}/local_context/county_q.parquet'",
    "cpi_q": f"'{LOC}/local_context/cpi_q.parquet'",
    "county_cpi": f"'{LOC}/local_context/county_cpi.parquet'",
    "local_panel": f"'{LOC}/local_context/local_panel.parquet'",
}
# Steps whose output may differ between rebuilds; a mismatch here is
# reported but does not fail the check. Empty since the ladder and
# shift-share steps sum in exact decimals: two rebuilds
# on 2026-11-03 matched on every table.
NONDETERMINISTIC: set[str] = set()
INPUTS = [
    LOC / "local_raw",
    SS / "raw",
    INT / "minwage_raw",
    ROOT / "data" / "raw" / "external",
]


FLOATS = {"DOUBLE", "FLOAT", "REAL"}
TOL = 1e-9  # relative to the column's sum of absolute values


def fingerprint(con, rel):
    cols = sorted(con.execute(f"DESCRIBE SELECT * FROM {rel}").fetchall())
    ex, spans = ["count(*)"], []
    for name, typ, *_ in cols:
        q = f'"{name}"'
        if typ in FLOATS:
            # CASE, not FILTER: hundreds of FILTER aggregates in one query
            # run ~100x slower in DuckDB
            f = f"CASE WHEN isfinite({q}) THEN {q} END"
            ex += [
                f"count({q})",
                f"sum({f})",
                f"sum(abs({f}))",
                f"min({f})",
                f"max({f})",
                f"count({q}) - count({f})",
            ]
            spans.append(6)
        else:
            ex += [f"count({q})", f"sum(hash({q}))::HUGEINT::VARCHAR"]
            spans.append(2)
    v = con.execute(f"SELECT {', '.join(ex)} FROM {rel}").fetchone()
    out, i = {}, 1
    for (name, typ, *_), k in zip(cols, spans):
        out[name] = [typ, *v[i : i + k]]
        i += k
    return {"rows": v[0], "columns": out}


def same_column(r, c):
    """Exact match, or for floating-point columns a match within TOL."""
    if r == c:
        return True
    if (
        r is None
        or c is None
        or r[0] != c[0]
        or r[0] not in FLOATS
        or len(r) != 7
        or len(c) != 7
    ):
        return False
    _, n_r, s_r, a_r, lo_r, hi_r, nf_r = r
    _, n_c, s_c, a_c, lo_c, hi_c, nf_c = c
    if n_r != n_c or nf_r != nf_c:
        return False
    if a_r is None or a_c is None:
        return a_r == a_c
    scale = max(abs(a_r), abs(a_c), 1e-300)
    m = max(abs(lo_r), abs(hi_r), abs(lo_c), abs(hi_c), 1e-300)
    return (
        abs(s_r - s_c) <= TOL * scale
        and abs(a_r - a_c) <= TOL * scale
        and abs(lo_r - lo_c) <= TOL * m
        and abs(hi_r - hi_c) <= TOL * m
    )


def sha256(f):
    h = hashlib.sha256()
    with open(f, "rb") as fh:
        for b in iter(lambda: fh.read(1 << 20), b""):
            h.update(b)
    return h.hexdigest()


def build():
    con = duckdb.connect()
    con.execute(
        f"SET memory_limit = '{os.environ.get('DUCKDB_MEMORY', '8GB')}'"
    )
    out = {"duckdb": duckdb.__version__, "tables": {}, "inputs": {}}
    for name, rel in TABLES.items():
        try:
            out["tables"][name] = fingerprint(con, rel)
        except duckdb.IOException:
            out["tables"][name] = None  # not built on this machine
    for d in INPUTS:
        for f in sorted(
            p for p in d.rglob("*") if p.is_file() and p.name != "manifest.tsv"
        ):
            out["inputs"][str(f.relative_to(d.parent))] = sha256(f)
    return out


def main():
    cur = build()
    if "--write" in sys.argv:
        REF.write_text(json.dumps(cur, indent=1, sort_keys=True) + "\n")
        print(f"wrote {REF.relative_to(ROOT)}")
        return 0
    ref = json.loads(REF.read_text())
    if cur["duckdb"] != ref["duckdb"]:
        print(
            f"WARNING: DuckDB {cur['duckdb']}, reference built with "
            f"{ref['duckdb']}: table hashes are not comparable"
        )
    fail, soft = 0, 0
    for name, r in ref["tables"].items():
        c = cur["tables"].get(name)
        if c is None:
            print(f"  skip   {name} (not built here)")
            continue
        cols = [
            k
            for k in set(r["columns"]) | set(c["columns"])
            if not same_column(r["columns"].get(k), c["columns"].get(k))
        ]
        if c["rows"] == r["rows"] and not cols:
            print(f"  same   {name}")
            continue
        tag = "DIFF*" if name in NONDETERMINISTIC else "DIFF"
        fail += name not in NONDETERMINISTIC
        soft += name in NONDETERMINISTIC
        print(
            f"  {tag:6} {name}: rows {r['rows']} -> {c['rows']}; "
            f"columns differ: {', '.join(sorted(cols)[:8])}"
            f"{' ...' if len(cols) > 8 else ''}"
        )
    absent = 0
    for f, h in ref["inputs"].items():
        c = cur["inputs"].get(f)
        if c is None:
            absent += 1
        elif c != h:
            print(f"  DIFF   input {f}")
            fail += 1
    if absent:
        print(
            f"  skip   {absent} of {len(ref['inputs'])} input files "
            "(not downloaded here)"
        )
    if soft:
        print("  (* known non-deterministic step: reported, not failed)")
    print("check: OK" if not fail else f"check: {fail} mismatch(es)")
    return 1 if fail else 0


if __name__ == "__main__":
    sys.exit(main())
