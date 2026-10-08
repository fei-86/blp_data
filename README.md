# blp_data

Pull intraday NYMEX energy futures data (CL, NG, HO, RB) from a Bloomberg Terminal into R: 1‑minute OHLCV bars and raw TRADE/BID/ASK ticks for every listed contract month, stored incrementally on disk with FinancialInstrument metadata for each contract.

## Requirements

- A machine with a logged‑in Bloomberg Terminal (or B‑PIPE). All requests go through that session.
- R packages:

```r
install.packages(c("Rblpapi", "xts", "data.table", "FinancialInstrument"))
```

## Layout

```
R/BloombergHelpers.R               futures chain, contract metadata, chunked getBars() -> xts
R/FinancialInstrument_metadata.R   instrument definitions and .rds/.csv store I/O
data-raw/pull_bbg_minute_energy.R  1-minute bar pull (config + driver)
data-raw/futures_cme.R             tick pull (day-partitioned, memory-bounded)
```

## Quick start

1. Clone the repo and set your working directory to the repo root. The scripts `source()` helpers by relative path.
2. Edit the config block at the top of `data-raw/pull_bbg_minute_energy.R`:
   - `out_dir`: where data is written (default `D:/data/bbg_minute`)
   - `contract_from` / `contract_to`: contract range as month code + 2‑digit year, e.g. `V26` (Oct 2026) to `Z27` (Dec 2027)
   - `universe`: roots to pull. Note RBOB is `XB` on Bloomberg.
3. Run it:

```sh
Rscript data-raw/pull_bbg_minute_energy.R
```

or interactively (sourcing only loads the functions; it does not start the pull):

```r
source("data-raw/pull_bbg_minute_energy.R")
res <- pull_energy_minutes(universe, cfg)
```

## Minute bar output

```
<out_dir>/CL_1min.rds                list of xts, one per contract (CL_Z26, CL_F27, ...)
<out_dir>/csv/CL_Z26_1min.csv        one CSV per contract
<out_dir>/energy_minute_bars.RData   all roots combined
<out_dir>/energy_instruments.RData   FinancialInstrument definitions
```

Columns are `Open, High, Low, Close, Volume, NumEvents, Value`, indexed in UTC.

```r
library(xts); library(FinancialInstrument)
load("D:/data/bbg_minute/energy_minute_bars.RData")
cl <- energy_minute_bars$CL$CL_Z26
tzone(cl) <- "America/Chicago"           # view in exchange time
loadInstruments("energy_instruments", dir = "D:/data/bbg_minute")
getInstrument("CL_Z26")                   # expiry, first notice, bbg ticker, ...
```

## Ticks

Configure `tick_cfg` in `data-raw/futures_cme.R` (default `out_dir` is `D:/data/blp_tick`; the first run pulls 30 days), then:

```sh
Rscript data-raw/futures_cme.R
```

Each contract‑day is fetched, bid/ask forward‑filled, written to `<id>/rds/` and `<id>/csv/`, and freed before the next, so memory stays at one contract‑day. A `manifest.csv` logs each run. Read a contract back with:

```r
source("data-raw/futures_cme.R")
x <- load_ticks("CL_Z26", from = "2026-10-01", trades_only = TRUE)
```

Setting `event_types = "TRADE"` cuts the volume to roughly a fifth.

## Good to know

- Bloomberg serves only about 140 days of intraday history. Earlier start dates are clamped, so run the scripts regularly and history accumulates.
- Re‑running is safe. Minute pulls resume after the last stored bar; tick pulls skip stored days and re‑fetch the most recent one.
- The `.rds` files are the system of record. If a CSV is open in Excel, it is skipped with a warning.
- Pulls count against your Bloomberg data limits, and tick pulls with BID/ASK are large.
