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

#' Futures chain for a root, including expired contracts
#'
#' @param root        Bloomberg root, e.g. "CL"
#' @param yellow_key  e.g. "Comdty"
#' @param chain_date  Date at which to evaluate the chain
#' @param con         Rblpapi connection
#' @return character vector of Bloomberg tickers, e.g. "CLZ6 Comdty"
bbg_fut_chain <- function(root, yellow_key = "Comdty",
                          chain_date = Sys.Date(), con = defaultConnection()) {
  generic <- paste0(root, "1 ", yellow_key)
  ch <- bds(generic, "FUT_CHAIN",
            overrides = c(INCLUDE_EXPIRED_CONTRACTS = "Y",
                          CHAIN_DATE = format(chain_date, "%Y%m%d")),
            con = con)
  if (is.null(ch) || nrow(ch) == 0L)
    stop("Empty FUT_CHAIN for ", generic)
  unique(as.character(ch[[1L]]))
}

#' Reference data for a set of contract tickers
#'
#' @return data.frame keyed by ticker with expiry / spec fields
bbg_contract_meta <- function(tickers, con = defaultConnection()) {
  flds <- c("FUT_FIRST_TRADE_DT", "LAST_TRADEABLE_DT", "FUT_NOTICE_FIRST",
            "FUT_DLV_DT_LAST", "FUT_CONT_SIZE", "FUT_TICK_SIZE",
            "FUT_TICK_VAL", "CRNCY", "NAME", "FUT_MONTH_YR")
  m <- bdp(tickers, flds, con = con)
  m$ticker <- rownames(m)
  rownames(m) <- NULL
  m
}

#' Restrict chain to contracts that traded inside [start, end]
active_contracts <- function(meta, start, end) {
  ok <- !is.na(meta$FUT_FIRST_TRADE_DT) & !is.na(meta$LAST_TRADEABLE_DT) &
    meta$FUT_FIRST_TRADE_DT <= as.Date(end) &
    meta$LAST_TRADEABLE_DT  >= as.Date(start)
  meta[ok, , drop = FALSE]
}

#' Contract month as an integer YYYYMM from a Bloomberg futures ticker
#'
#' Handles 1- or 2-digit year codes. For 1-digit codes the decade is inferred
#' from LAST_TRADEABLE_DT (contract month is never earlier than last trade).
#'
#' @param ticker      e.g. "CLF7 Comdty" or "CLF27 Comdty"
#' @param root        Bloomberg root, e.g. "CL"
#' @param last_trade  Date, LAST_TRADEABLE_DT for the contract
#' @return list(yyyymm = integer, suffix = "F27")
contract_month <- function(ticker, root, last_trade) {
  m <- regmatches(ticker, regexec(paste0("^", root, "([FGHJKMNQUVXZ])(\\d{1,2}) "), ticker))[[1L]]
  if (length(m) != 3L) return(list(yyyymm = NA_integer_, suffix = NA_character_))
  mon <- match(m[2L], c("F","G","H","J","K","M","N","Q","U","V","X","Z"))
  yc  <- as.integer(m[3L])
  if (nchar(m[3L]) == 2L) {
    yr <- 2000L + yc
  } else {
    base <- as.integer(format(as.Date(last_trade), "%Y"))
    yr   <- (base %/% 10L) * 10L + yc
    if (yr < base) yr <- yr + 10L
  }
  list(yyyymm = yr * 100L + mon, suffix = sprintf("%s%02d", m[2L], yr %% 100L))
}

#' YYYYMM for a short code like "V26" / "Z27"
code_yyyymm <- function(code) {
  mon <- match(substr(code, 1, 1), c("F","G","H","J","K","M","N","Q","U","V","X","Z"))
  (2000L + as.integer(substr(code, 2, 3))) * 100L + mon
}

#' Pull minute bars for one contract, chunked, returned as xts OHLCV
#'
#' @param ticker    Bloomberg ticker
#' @param start     POSIXct start (inclusive)
#' @param end       POSIXct end   (inclusive)
#' @param on_error  "stop" surfaces the first Bloomberg error (default);
#'                  "warn" logs it and continues
#' @return xts with columns Open, High, Low, Close, Volume, NumEvents, Value
#'   (zero-row xts if nothing returned)
#' @note getBars(returnAs = "matrix") returns a data.frame with a `times`
#'   column; "data.frame" is NOT an accepted returnAs value.
bbg_minute_bars <- function(ticker, start, end, interval = 1L,
                            event_type = "TRADE", chunk_days = 7L,
                            tz = "UTC", con = defaultConnection(),
                            verbose = TRUE, on_error = c("stop", "warn")) {
  on_error <- match.arg(on_error)
  cols  <- c("Open", "High", "Low", "Close", "Volume", "NumEvents", "Value")
  empty <- function() xts(matrix(numeric(0), 0, 7, dimnames = list(NULL, cols)),
                          as.POSIXct(character(0), tz = tz))
  start <- as.POSIXct(start, tz = tz)
  end   <- as.POSIXct(end,   tz = tz)
  if (end <= start) return(empty())
  
  breaks <- seq(start, end, by = chunk_days * 86400)
  if (tail(breaks, 1) < end) breaks <- c(breaks, end)
  
  out <- vector("list", length(breaks) - 1L)
  for (k in seq_along(out)) {
    s <- breaks[k]; e <- breaks[k + 1L]
    res <- tryCatch(
      getBars(ticker, eventType = event_type, barInterval = interval,
              startTime = s, endTime = e, tz = tz, con = con,
              returnAs = "matrix"),
      error = function(err) {
        msg <- sprintf("%s [%s -> %s]: %s", ticker, format(s), format(e),
                       conditionMessage(err))
        if (on_error == "stop") stop(msg, call. = FALSE)
        message("  ERROR ", msg); NULL
      })
    if (!is.null(res) && NROW(res) > 0L) out[[k]] <- as.data.frame(res)
    if (verbose) message(sprintf("  %-14s %s -> %s  %6d bars",
                                 ticker, format(s, "%Y-%m-%d"),
                                 format(e, "%Y-%m-%d"),
                                 if (is.null(res)) 0L else NROW(res)))
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(empty())
  
  df <- do.call(rbind, out)
  x  <- xts(cbind(Open = df$open, High = df$high, Low = df$low, Close = df$close,
                  Volume = df$volume, NumEvents = df$numEvents, Value = df$value),
            order.by = as.POSIXct(df$times, tz = tz))
  x <- x[!duplicated(index(x)), ]
  tzone(x) <- tz
  x
}

## --------------------------------------------------------------------------
## 2. FinancialInstrument metadata
## --------------------------------------------------------------------------

#' Define root future and each contract series in the .instrument env
define_energy_instruments <- function(spec, meta, cfg) {
  currency(cfg$currency)
  root_id <- spec$primary_id
  if (!is.instrument(getInstrument(root_id, silent = TRUE))) {
    future(primary_id  = root_id,
           currency    = cfg$currency,
           multiplier  = as.numeric(meta$FUT_CONT_SIZE[1L]),
           tick_size   = as.numeric(meta$FUT_TICK_SIZE[1L]),
           exchange    = cfg$exchange,
           description = spec$description,
           bbg_root    = spec$bbg_root,
           yellow_key  = spec$yellow_key)
  }
  for (i in seq_len(nrow(meta))) {
    fi_suffix <- meta$fi_suffix[i]
    fi_id <- paste(root_id, fi_suffix, sep = "_")
    if (!is.instrument(getInstrument(fi_id, silent = TRUE))) {
      future_series(primary_id       = fi_id,
                    root_id          = root_id,
                    suffix_id        = fi_suffix,
                    first_traded     = as.character(meta$FUT_FIRST_TRADE_DT[i]),
                    expires          = as.character(meta$LAST_TRADEABLE_DT[i]),
                    first_notice     = as.character(meta$FUT_NOTICE_FIRST[i]),
                    identifiers      = list(bbg = meta$ticker[i]))
    }
    meta$fi_id[i] <- fi_id
  }
  meta
}

## --------------------------------------------------------------------------
## 3. Store I/O (incremental)
## --------------------------------------------------------------------------

rds_path <- function(root_id, cfg) file.path(cfg$out_dir, paste0(root_id, "_1min.rds"))

load_store <- function(root_id, cfg) {
  p <- rds_path(root_id, cfg)
  if (file.exists(p)) readRDS(p) else list()
}

#' rbind new bars onto existing xts, de-duplicated on index
merge_bars <- function(old, new) {
  if (is.null(old) || nrow(old) == 0L) return(new)
  if (nrow(new) == 0L) return(old)
  x <- rbind(old, new)
  x[!duplicated(index(x), fromLast = TRUE), ]
}

#' Write an xts to CSV via a temp file; a locked/unwritable target is a
#' warning, not an error (the .rds store is the system of record)
write_csv_xts <- function(x, path) {
  df  <- data.frame(Time = format(index(x), "%Y-%m-%d %H:%M:%S", tz = tzone(x)),
                    coredata(x), check.names = FALSE)
  tmp <- paste0(path, ".tmp")
  ok  <- tryCatch({
    write.csv(df, tmp, row.names = FALSE)
    file.rename(tmp, path)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) {
    if (file.exists(tmp)) unlink(tmp)
    message("  WARN could not write ", basename(path), " (open in Excel?) - rds store still updated")
  }
  invisible(ok)
}

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