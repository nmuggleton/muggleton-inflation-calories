"""Quarterly external context tables, built from the public files fetched by
src/data_prep/08_external/01_download.sh:

  state_q     LAUS state unemployment (SA), BEA personal income, FHFA HPI
  county_q    LAUS county unemployment rate and employment (NSA)
  cpi_q       BLS CPI-U food at home (SAF11, NSA) by CPI area
  county_cpi  county -> BLS CPI area (14 published metros, else Census
              region x size class) via the OMB 2013 CBSA delineation

Every table carries year-on-year changes at the analysis horizon (quarter
t vs t-4; yq = year*10 + quarter, so the lag is yq - 10) and the lagged
change (t-4 vs t-8) for pre-trend controls.

Inputs : ${LOCAL_DATA:-$WP2_INT}/local_raw/*
Outputs: ${LOCAL_DATA:-$WP2_INT}/local_context/{state_q,county_q,cpi_q,
         county_cpi}.parquet
Usage  : python src/data_prep/08_external/03_context.py
"""

from __future__ import annotations

import os
from pathlib import Path

import polars as pl

ROOT = Path(
    os.environ.get("LOCAL_DATA")
    or Path(os.environ["WP2_DATA"]) / "intermediate"
)
RAW, OUT = ROOT / "local_raw", ROOT / "local_context"
OUT.mkdir(parents=True, exist_ok=True)
END = int(os.environ.get("WP2_END_YEAR", "2024"))  # src/lib/paths.sh
YR = range(2003, END + 1)  # sample is 2005-END; t-4 and t-8 lags need 2003+
MONTH = r"^M(0[1-9]|1[0-2])$"

# state FIPS for the 48 contiguous states and DC (AK and HI are not sampled)
AB = dict(
    AL=1,
    AZ=4,
    AR=5,
    CA=6,
    CO=8,
    CT=9,
    DE=10,
    DC=11,
    FL=12,
    GA=13,
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
# Census region of each state FIPS
REGION = {
    s: r
    for r, ss in {
        1: [9, 23, 25, 33, 44, 50, 34, 36, 42],
        2: [17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46],
        3: [10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48],
        4: [4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53],
    }.items()
    for s in ss
}


def _mean(s: pl.Series) -> float | None:
    if s.null_count():
        return None
    acc = 0.0
    for v in s:  # sequential double sum, in file order
        acc += v
    return acc / len(s)


def mean(c: str) -> pl.Expr:
    """Group mean as a sequential double-precision sum divided by n, the
    arithmetic the original build used, so results match it bit for bit
    (polars' own mean sums in a different order); null if any value is
    null."""
    return pl.col(c).map_batches(
        lambda s: pl.Series([_mean(s)], dtype=pl.Float64),
        returns_scalar=True,
        return_dtype=pl.Float64,
    )


def write(df: pl.DataFrame, name: str) -> None:
    df.write_parquet(OUT / f"{name}.parquet", compression="zstd")
    print(f"wrote {name} {df.height} rows")


def bls(file: str, pattern: str) -> pl.DataFrame:
    """Monthly rows of a BLS time.series flat file, series matching pattern,
    with numeric value (non-numeric values become null) and yq."""
    d = pl.read_csv(RAW / file, separator="\t", infer_schema=False)
    d = d.rename({c: c.strip() for c in d.columns})
    d = d.with_columns(pl.col(c).str.strip_chars() for c in d.columns)
    return (
        d.filter(
            pl.col("series_id").str.contains(pattern)
            & pl.col("year").cast(pl.Int32).is_in(list(YR))
            & pl.col("period").str.contains(MONTH)
        )
        .with_columns(
            year=pl.col("year").cast(pl.Int32),
            value=pl.col("value").cast(pl.Float64, strict=False),
            q=(pl.col("period").str.slice(1, 2).cast(pl.Int32) - 1) // 3 + 1,
        )
        .with_columns(yq=pl.col("year") * 10 + pl.col("q"))
    )


def add_yoy(
    d: pl.DataFrame, ids: list[str], vars_: list[str], fun
) -> pl.DataFrame:
    """d_<v> = fun(v_t, v_t-4) and dl_<v> = d_<v> at t-4, within ids."""
    lag = d.select(ids + ["yq"] + vars_).with_columns(yq=pl.col("yq") + 10)
    d = d.join(
        lag.rename({v: f"{v}_l4" for v in vars_}), on=ids + ["yq"], how="left"
    )
    d = d.with_columns(
        fun(pl.col(v), pl.col(f"{v}_l4")).alias(f"d_{v}") for v in vars_
    )
    d = d.drop([f"{v}_l4" for v in vars_])
    lag2 = (
        d.select(ids + ["yq"] + [f"d_{v}" for v in vars_])
        .with_columns(yq=pl.col("yq") + 10)
        .rename({f"d_{v}": f"dl_{v}" for v in vars_})
    )
    return d.join(lag2, on=ids + ["yq"], how="left")


def dif(a, b):
    return a - b


def dlog(a, b):
    return 100 * (a.log() - b.log())


def pct(a, b):
    return 100 * (a / b - 1)


# 1. State: LAUS SA unemployment rate, BEA personal income, FHFA HPI --------
ls = bls("la.data.3.AllStatesS", r"^LASST[0-9]{2}0{11}03$").with_columns(
    state=pl.col("series_id").str.slice(5, 2).cast(pl.Int32)
)
st = ls.group_by("state", "yq").agg(ur=mean("value"), n_m=pl.len())
assert (st["n_m"] == 3).all()
st = st.drop("n_m")


def rd_fred(code: str, name: str) -> pl.DataFrame:
    out = []
    for s, f in AB.items():
        d = pl.read_csv(RAW / "fred" / f"{s}{code}.csv", infer_schema=False)
        d.columns = ["date", "v"]
        d = d.with_columns(date=pl.col("date").str.to_date())
        out.append(
            d.filter(pl.col("date").dt.year().is_in(list(YR))).select(
                state=pl.lit(f, pl.Int32),
                yq=(
                    pl.col("date").dt.year() * 10
                    + (pl.col("date").dt.month() - 1) // 3
                    + 1
                ).cast(pl.Int32),
                **{name: pl.col("v").cast(pl.Float64, strict=False)},
            )
        )
    return pl.concat(out)


pi = rd_fred("OTOT", "pinc")
# guard against FRED id collisions (NCOTOT is a bank series): every state's
# income must be in $ mn; the smallest state (VT/WY) is about 35,000 in 2019
assert pi.filter(pl.col("yq") == 20191)["pinc"].min() > 10000
hp = rd_fred("STHPI", "hpi")
st = st.join(pi, on=["state", "yq"]).join(hp, on=["state", "yq"])
assert st["state"].n_unique() == 49 and st.height == 49 * len(YR) * 4
assert st.null_count().sum_horizontal().item() == 0
st = add_yoy(st, ["state"], ["ur"], dif)
st = add_yoy(st, ["state"], ["pinc", "hpi"], dlog)
write(st.sort("state", "yq"), "state_q")

# 2. County: LAUS unemployment rate (03) and employment (05), NSA -----------
lc = bls("la.data.64.County", r"^LAUCN[0-9]{5}0{8}0[35]$").with_columns(
    fips=pl.col("series_id").str.slice(5, 5).cast(pl.Int32),
    m=pl.col("series_id").str.slice(18, 2),
)
print(
    "county LAUS rows with non-numeric value (dropped):",
    lc["value"].null_count(),
)
lc = lc.filter(pl.col("value").is_not_null())
cm = lc.group_by("fips", "yq", "m").agg(v=mean("value"), n_m=pl.len())
cq = (
    cm.filter(pl.col("m") == "03")
    .select("fips", "yq", ur_c="v", n3="n_m")
    .join(
        cm.filter(pl.col("m") == "05").select(
            "fips", "yq", emp_c="v", n5="n_m"
        ),
        on=["fips", "yq"],
    )
    # complete quarters only (3 months of both measures)
    .filter((pl.col("n3") == 3) & (pl.col("n5") == 3))
    .select("fips", "yq", "ur_c", "emp_c")
)
cq = add_yoy(cq, ["fips"], ["ur_c"], dif)
cq = add_yoy(cq, ["fips"], ["emp_c"], dlog)
print("county-quarters:", cq.height, "| counties:", cq["fips"].n_unique())
write(cq.sort("fips", "yq"), "county_q")

# 3. BLS CPI-U food at home by area, quarterly mean of monthly NSA index ----
cu = bls("cu.data.0.Current", r"^CUUR[0-9A-Z]{4}SAF11$").with_columns(
    area=pl.col("series_id").str.slice(4, 4)
)
cp = cu.group_by("area", "yq").agg(cpi=mean("value"), n_m=pl.len())
# areas published monthly over the whole window (every quarter complete)
full = (
    cp.group_by("area")
    .agg(nq=(pl.col("n_m") == 3).sum())
    .filter(pl.col("nq") == len(YR) * 4)["area"]
    .sort()
    .to_list()
)
print(f"CPI areas with complete 2003-{END} quarterly series:", " ".join(full))
cp = cp.filter(pl.col("area").is_in(full)).select("area", "yq", "cpi")
cp = add_yoy(cp, ["area"], ["cpi"], pct)
write(cp.sort("area", "yq"), "cpi_q")

# 4. County -> CPI area. The 14 metros published monthly since 2003 get
#    their own index; other counties get Census region x size class (Size
#    A = the 2018 BLS Size A metros not published separately before 2018;
#    every other county, metro or not, gets the B/C urban index, the
#    closest BLS proxy: the CPI has no rural index after 2017). -----------
cb = pl.read_excel(
    RAW / "cbsa_list1_2013.xls",
    read_options={"header_row": 2},
    infer_schema_length=0,
)
cb = cb.filter(
    pl.col("FIPS State Code").is_not_null()
    & pl.col("FIPS County Code").is_not_null()
).select(
    fips=(
        pl.col("FIPS State Code").cast(pl.Int32) * 1000
        + pl.col("FIPS County Code").cast(pl.Int32)
    ),
    cbsa=pl.col("CBSA Code").cast(pl.Int32),
    title=pl.col("CBSA Title"),
)
assert cb["fips"].is_unique().all()
metro = pl.DataFrame(
    {
        "area": [
            "S11A",
            "S12A",
            "S12B",
            "S23A",
            "S23B",
            "S35A",
            "S35B",
            "S35C",
            "S35E",
            "S37A",
            "S37B",
            "S49A",
            "S49B",
            "S49D",
        ],
        "cbsa": [
            14460,
            35620,
            37980,
            16980,
            19820,
            47900,
            33100,
            12060,
            12580,
            19100,
            26420,
            31080,
            41860,
            42660,
        ],
    },
    schema={"area": pl.String, "cbsa": pl.Int32},
)
SIZE_A = [33460, 41180, 45300, 38060, 19740, 40140, 41740]
# verify the CBSA codes against BLS area names (first word of the title)
an = pl.read_csv(RAW / "cu.area", separator="\t", infer_schema=False)


def first(c):
    return pl.col(c).str.strip_chars().str.replace(r"[-, ].*", "")


chk = (
    metro.join(an.select(area="area_code", area_name="area_name"), on="area")
    .join(cb.select("cbsa", "title").unique(), on="cbsa")
    .with_columns(ok=first("area_name") == first("title"))
)
print(chk.select("area", "cbsa", "area_name", "title", "ok"))
assert chk.height == 14 and chk["ok"].all() and set(metro["area"]) <= set(full)
# union with the OMB list: LAUS now reports Connecticut by planning region
# for all years, so the old CT county codes Nielsen uses are absent there
cc = (
    pl.concat([cq.select("fips"), cb.select("fips")])
    .unique()
    .with_columns(state=pl.col("fips") // 1000)
    .filter(pl.col("state").is_in(list(REGION)))
    .with_columns(
        region=pl.col("state").replace_strict(REGION, return_dtype=pl.Int32)
    )
    .join(cb.select("fips", "cbsa"), on="fips", how="left")
    .join(metro, on="cbsa", how="left")
    .with_columns(
        area=pl.when(pl.col("area").is_not_null())
        .then(pl.col("area"))
        .when(pl.col("cbsa").is_in(SIZE_A))
        .then(pl.format("S{}00", pl.col("region")))
        .otherwise(pl.format("N{}00", pl.col("region")))
    )
    .with_columns(
        area_type=pl.when(pl.col("area").str.slice(2, 2) != "00")
        .then(pl.lit("Metro"))
        .when(pl.col("area").str.starts_with("S"))
        .then(pl.lit("Size A remainder"))
        .otherwise(pl.lit("Region B/C"))
    )
)
assert set(cc["area"]) <= set(full) and cc["fips"].is_unique().all()
print(cc.group_by("area_type", "area").len().sort("area_type", "area"))
write(
    cc.select("fips", "state", "region", "cbsa", "area", "area_type").sort(
        "fips"
    ),
    "county_cpi",
)
