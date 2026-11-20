"""State local-demand controls for the shift-share design: a state x
quarter table of year-on-year changes (t vs t-4) from FRED monthly series:

  {ST}UR    state unemployment rate, SA (BLS LAUS via FRED)
  {ST}PHCI  state coincident economic activity index (Federal Reserve Bank
            of Philadelphia via FRED; none for DC)

Inputs : https://fred.stlouisfed.org/graph/fredgraph.csv?id=<series>,
         cached in $SHIFTSHARE_INT/raw/fred_<series>.csv (downloaded only
         if missing, so a rebuild reuses the vintage the paper used)
Outputs: $SHIFTSHARE_INT/state_controls.parquet
         (state, yq, ur, ci, d_ur, d_lci), 2005Q1 to WP2_END_YEAR Q4
Usage  : python src/data_prep/08_external/02_fred.py
"""

import os
import subprocess
from fractions import Fraction
from pathlib import Path

import polars as pl

XINT = Path(
    os.environ.get("SHIFTSHARE_INT")
    or Path(os.environ["WP2_DATA"]) / "intermediate" / "shiftshare"
)
RAW = XINT / "raw"
END = int(os.environ.get("WP2_END_YEAR", "2024"))  # src/lib/paths.sh
RAW.mkdir(parents=True, exist_ok=True)
FIPS = dict(
    AL=1,
    AK=2,
    AZ=4,
    AR=5,
    CA=6,
    CO=8,
    CT=9,
    DE=10,
    DC=11,
    FL=12,
    GA=13,
    HI=15,
    ID=16,
    IL=17,
    IN=18,
    IA=19,
    KS=20,
    KY=21,
    LA=22,
    ME=23,
    MD=24,
    MA=25,
    MI=26,
    MN=27,
    MS=28,
    MO=29,
    MT=30,
    NE=31,
    NV=32,
    NH=33,
    NJ=34,
    NM=35,
    NY=36,
    NC=37,
    ND=38,
    OH=39,
    OK=40,
    OR=41,
    PA=42,
    RI=44,
    SC=45,
    SD=46,
    TN=47,
    TX=48,
    UT=49,
    VT=50,
    VA=51,
    WA=53,
    WV=54,
    WI=55,
    WY=56,
)


def get(series: str) -> pl.DataFrame:
    f = RAW / f"fred_{series}.csv"
    if not f.exists():
        # curl, like 01_download.sh: it uses the system certificate store,
        # which a python.org Python on macOS does not until its certificates
        # are installed. Download to a temp name so a failure leaves no
        # partial file in the cache.
        tmp = f.with_suffix(".part")
        subprocess.run(
            [
                "curl",
                "-sSf",
                "-m",
                "120",
                "--retry",
                "2",
                "-o",
                str(tmp),
                f"https://fred.stlouisfed.org/graph/fredgraph.csv?id={series}",
            ],
            check=True,
        )
        tmp.rename(f)
    d = pl.read_csv(f, infer_schema=False)
    d.columns = ["date", "value"]
    return d.with_columns(
        date=pl.col("date").str.to_date(),
        value=pl.col("value").cast(pl.Float64, strict=False),
    ).drop_nulls("value")


def mean(c: str) -> pl.Expr:
    """Correctly rounded mean (exact rational sum; null if any value is
    null), as R's extended-precision mean() gives; a plain double sum can
    be 1 ulp off."""

    def f(s: pl.Series) -> pl.Series:
        v = None if s.null_count() else float(sum(map(Fraction, s)) / len(s))
        return pl.Series([v], dtype=pl.Float64)

    return pl.col(c).map_batches(
        f, returns_scalar=True, return_dtype=pl.Float64
    )


out = []
for st, fips in FIPS.items():
    ur = get(f"{st}UR").rename({"value": "ur"})
    # the Philadelphia Fed publishes no coincident index for DC (FRED 404)
    ci = (
        ur.select("date", ci=pl.lit(None, pl.Float64))
        if st == "DC"
        else get(f"{st}PHCI").rename({"value": "ci"})
    )
    m = ur.join(ci, on="date", how="full", coalesce=True).sort("date")
    q = (
        m.with_columns(
            yq=(
                pl.col("date").dt.year() * 10 + pl.col("date").dt.quarter()
            ).cast(pl.Int32)
        )
        .group_by("yq", maintain_order=True)
        .agg(
            ur=mean("ur"), ci=mean("ci"), n_m=pl.col("ur").is_not_null().sum()
        )
        .filter(pl.col("n_m") == 3)  # complete quarters only
        .with_columns(state=pl.lit(float(fips)))
    )
    out.append(q)

res = (
    pl.concat(out)
    .sort("state", "yq")
    .with_columns(
        d_ur=pl.col("ur") - pl.col("ur").shift(4).over("state"),
        d_lci=100
        * (pl.col("ci").log() - pl.col("ci").shift(4).over("state").log()),
        gap=pl.col("yq") - pl.col("yq").shift(4).over("state"),
    )
    # the 4-quarter lag needs consecutive quarters
    .with_columns(
        pl.when(pl.col("gap") == 10).then(pl.col(c)).alias(c)
        for c in ("d_ur", "d_lci")
    )
    .filter(pl.col("yq").is_between(20051, END * 10 + 4))
    .select("state", "yq", "ur", "ci", "d_ur", "d_lci")
)
assert res.select("state", "yq").is_unique().all()
# noinspection PyTypeChecker
assert res["state"].n_unique() == 51 and res["d_ur"].null_count() == 0
# noinspection PyTypeChecker
assert res.filter(pl.col("state") != 11)["d_lci"].null_count() == 0
print(
    res.group_by(year=pl.col("yq") // 10)
    .agg(
        n=pl.len(),
        mean_d_ur=pl.col("d_ur").mean(),
        sd_d_ur=pl.col("d_ur").std(),
        mean_d_lci=pl.col("d_lci").mean(),
        sd_d_lci=pl.col("d_lci").std(),
    )
    .sort("year")
)
res.write_parquet(XINT / "state_controls.parquet")
print("rows:", res.height)
