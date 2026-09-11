###############################################################################
#' pull_bbg_minute_energy.R
#'
#' Pull 1-minute OHLCV bars for NYMEX energy futures (all listed months) from
#' the Bloomberg Terminal via \pkg{Rblpapi}; write one CSV per contract, one
#' .rds (list of xts) per root, and a combined .RData, plus FinancialInstrument
#' metadata for each contract.
#'
#' Bloomberg constraint: IntradayBarRequest serves only ~140 trailing calendar
#' days. The script clamps the requested start to that window and appends
#' incrementally to any existing store, so history accumulates across runs.
#'
#' Requires a logged-in Terminal (or B-PIPE) on this machine.
#'
#' @author Billy
###############################################################################

suppressPackageStartupMessages({
  library(Rblpapi)
  library(xts)
  library(FinancialInstrument)
})

## --------------------------------------------------------------------------
## 0. Configuration
## --------------------------------------------------------------------------

#' Universe specification (mirrors the VBA SetA rows)
#'
#' @field primary_id   FinancialInstrument root symbol
#' @field bbg_root     Bloomberg futures root (note RB -> XB)
#' @field months       "ALLM" = every contract month on the chain
#' @field yellow_key   Bloomberg sector key
#' @field description  Human-readable name
#' @field start_year   Desired history start (clamped for intraday bars)
universe <- data.frame(
  primary_id  = c("CL", "NG", "HO", "RB"),
  bbg_root    = c("CL", "NG", "HO", "XB"),
  months      = "ALLM",
  yellow_key  = "Comdty",
  description = c("WTI Crude", "Henry Hub Nat Gas",
                  "ULSD Heating Oil", "RBOB Gasoline"),
  start_year  = 2010L,
  stringsAsFactors = FALSE
)

cfg <- list(
  out_dir       = "D:/AlphaLattice/data/bbg_minute",   # adjust as needed
  contract_from = "V26",         # first contract month to pull (Bloomberg month code + 2-digit yr)
  contract_to   = "Z27",         # last contract month to pull, inclusive
  bar_interval  = 1L,            # minutes
  event_type    = "TRADE",       # TRADE gives OHLC + volume
  chunk_days    = 7L,            # days per getBars() request
  bbg_lookback  = 140L,          # Bloomberg intraday history depth (days)
  tz_store      = "UTC",         # index tz written to disk
  tz_display    = "America/Chicago",
  exchange      = "NYMEX",
  currency      = "USD"
)

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(cfg$out_dir, "csv"), showWarnings = FALSE)

## --------------------------------------------------------------------------
## 1. Bloomberg helpers
## --------------------------------------------------------------------------

source("R/BloombergHelpers.R")

## --------------------------------------------------------------------------
## 2. FinancialInstrument metadata
## --------------------------------------------------------------------------



## --------------------------------------------------------------------------
## 4. Driver
## --------------------------------------------------------------------------

#' Pull all roots in the universe and persist
#'
#' @return named list (by primary_id) of lists of xts (by FI contract id)
pull_energy_minutes <- function(universe, cfg, con = blpConnect()) {
  on.exit(blpDisconnect(con), add = TRUE)
  now_utc <- as.POSIXct(Sys.time(), tz = cfg$tz_store)
  all_roots <- list()
  
  for (r in seq_len(nrow(universe))) {
    spec <- universe[r, ]
    message("==== ", spec$primary_id, " (", spec$bbg_root, ") ====")
    
    ## Window: desired start clamped to Bloomberg's intraday depth
    desired_start <- as.POSIXct(sprintf("%d-01-01", spec$start_year), tz = cfg$tz_store)
    bbg_floor     <- now_utc - cfg$bbg_lookback * 86400
    win_start     <- max(desired_start, bbg_floor)
    if (desired_start < bbg_floor)
      message(sprintf("  start_year %d clamped to %s (Bloomberg intraday depth ~%d days)",
                      spec$start_year, format(win_start, "%Y-%m-%d"), cfg$bbg_lookback))
    
    ## Chain + meta, restricted to contracts alive in the window
    chain <- bbg_fut_chain(spec$bbg_root, spec$yellow_key, con = con)
    meta  <- bbg_contract_meta(chain, con = con)
    meta  <- active_contracts(meta, win_start, now_utc)
    meta  <- meta[order(meta$LAST_TRADEABLE_DT), ]
    cm    <- Map(contract_month, meta$ticker, spec$bbg_root, meta$LAST_TRADEABLE_DT)
    meta$yyyymm    <- vapply(cm, `[[`, integer(1),   "yyyymm")
    meta$fi_suffix <- vapply(cm, `[[`, character(1), "suffix")
    meta  <- meta[!is.na(meta$yyyymm) &
                    meta$yyyymm >= code_yyyymm(cfg$contract_from) &
                    meta$yyyymm <= code_yyyymm(cfg$contract_to), ]
    meta  <- meta[order(meta$yyyymm), ]
    meta$fi_id <- NA_character_
    meta  <- define_energy_instruments(spec, meta, cfg)
    message("  ", nrow(meta), " contracts in window")
    
    store <- load_store(spec$primary_id, cfg)
    
    for (i in seq_len(nrow(meta))) {
      tk <- meta$ticker[i]; id <- meta$fi_id[i]
      old <- store[[id]]
      ## incremental start: last stored bar + 1 minute, else window start
      c_start <- if (!is.null(old) && nrow(old)) end(old) + 60 else win_start
      c_start <- max(c_start, as.POSIXct(meta$FUT_FIRST_TRADE_DT[i], tz = cfg$tz_store))
      c_end   <- min(now_utc, as.POSIXct(meta$LAST_TRADEABLE_DT[i], tz = cfg$tz_store) + 86400)
      if (c_end <= c_start) { message("  ", id, " up to date"); next }
      
      new <- bbg_minute_bars(tk, c_start, c_end,
                             interval = cfg$bar_interval, event_type = cfg$event_type,
                             chunk_days = cfg$chunk_days, tz = cfg$tz_store, con = con)
      if (nrow(new) == 0L && is.null(old)) { message("  ", id, " no bars"); next }
      
      x <- merge_bars(old, new)
      xtsAttributes(x) <- list(bbg_ticker = tk, root = spec$primary_id,
                               expires = as.character(meta$LAST_TRADEABLE_DT[i]),
                               src = "Rblpapi::getBars", bar_min = cfg$bar_interval)
      store[[id]] <- x
      saveRDS(store, rds_path(spec$primary_id, cfg))   # checkpoint per contract
      write_csv_xts(x, file.path(cfg$out_dir, "csv", paste0(id, "_1min.csv")))
    }
    
    all_roots[[spec$primary_id]] <- store
  }
  
  ## combined .RData + instrument env
  energy_minute_bars <- all_roots
  save(energy_minute_bars, file = file.path(cfg$out_dir, "energy_minute_bars.RData"))
  saveInstruments("energy_instruments", dir = cfg$out_dir)
  invisible(all_roots)
}

## --------------------------------------------------------------------------
## 5. Run
## --------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  res <- pull_energy_minutes(universe, cfg)
  str(lapply(res, function(s) sapply(s, nrow)))
}