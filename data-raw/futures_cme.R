###############################################################################
#' pull_bbg_ticks_energy.R  (v2 — day-partitioned, memory-bounded)
#'
#' Pull tick history (TRADE, BID, ASK) for the NYMEX energy universe from
#' Bloomberg via \pkg{Rblpapi}::getTicks(). One contract-day is fetched,
#' quote-filled, written to disk and freed before the next is requested, so
#' peak memory is a single day of a single contract regardless of history.
#'
#' Store layout (under tick_cfg$out_dir):
#'   <id>/rds/<id>_<yyyymmdd>.rds        data.table: Time, Type, Price, Size,
#'                                       Bid, Ask [, CondCodes]
#'   <id>/rds/<id>_<yyyymmdd>_state.rds  small sidecar: n rows, last Bid/Ask
#'   <id>/csv/<id>_<yyyymmdd>.csv        same rows, via data.table::fwrite
#'   manifest.csv                        per-run log of what was written
#'
#' Incremental: days with a state file are skipped, except the most recent
#' stored day which is re-fetched (it may have been partial). Use
#' load_ticks() to read a contract back into memory for analysis.
#'
#' Depends on helpers in pull_bbg_minute_energy.R.
###############################################################################

suppressPackageStartupMessages({
  library(Rblpapi)
  library(data.table)
})
source("R/pull_bbg_minute_energy.R")   # helpers only; its run guard skips the pull

## --------------------------------------------------------------------------
## 0. Configuration
## --------------------------------------------------------------------------

tick_cfg <- list(
  out_dir       = "D:/AlphaLattice/data/blp_tick",
  event_types   = c("TRADE", "BID", "ASK"),   # drop BID/ASK for ~1/5 the volume
  bbg_lookback  = 140L,                        # Bloomberg tick history depth
  tick_lookback = 30L,                         # days pulled on first run
  contract_from = "V26",
  contract_to   = "Z27",
  tz_store      = "UTC",
  write_csv     = TRUE,
  compress_rds  = FALSE,   # TRUE = smaller files, noticeably slower writes
  gc_every      = 1L       # run gc() after every N contract-days
)

TYPE_LEVELS <- c("TRADE", "BID", "ASK")

## --------------------------------------------------------------------------
## 1. Paths
## --------------------------------------------------------------------------

id_dir     <- function(id, cfg, sub) file.path(cfg$out_dir, id, sub)
day_stamp  <- function(day) format(as.Date(day), "%Y%m%d")
rds_file   <- function(id, day, cfg) file.path(id_dir(id, cfg, "rds"), sprintf("%s_%s.rds", id, day_stamp(day)))
state_file <- function(id, day, cfg) file.path(id_dir(id, cfg, "rds"), sprintf("%s_%s_state.rds", id, day_stamp(day)))
csv_file   <- function(id, day, cfg) file.path(id_dir(id, cfg, "csv"), sprintf("%s_%s.csv", id, day_stamp(day)))

#' Dates already fetched for a contract (from state sidecars)
stored_days <- function(id, cfg) {
  d <- id_dir(id, cfg, "rds")
  if (!dir.exists(d)) return(as.Date(character(0)))
  f <- list.files(d, pattern = "_state\\.rds$")
  if (!length(f)) return(as.Date(character(0)))
  sort(as.Date(sub(sprintf("^%s_(\\d{8})_state\\.rds$", id), "\\1", f), "%Y%m%d"))
}

#' Last known Bid/Ask on or before `day` (looks back up to 10 stored days)
seed_quotes <- function(id, day, cfg) {
  days <- rev(stored_days(id, cfg))
  days <- days[days < as.Date(day)]
  for (d in head(days, 10L)) {
    st <- readRDS(state_file(id, as.Date(d), cfg))
    if (!is.na(st$last_bid) || !is.na(st$last_ask)) return(c(bid = st$last_bid, ask = st$last_ask))
  }
  c(bid = NA_real_, ask = NA_real_)
}

## --------------------------------------------------------------------------
## 2. Fetch + transform one contract-day
## --------------------------------------------------------------------------

#' Fetch ticks for one contract for one UTC day
#'
#' @return data.table Time, Type (factor), Price, Size [, CondCodes]; zero rows
#'   if nothing returned. NULL on caught error when on_error = "warn".
fetch_tick_day <- function(ticker, day, event_types, tz = "UTC", now = Sys.time(),
                           con = defaultConnection(), on_error = c("stop", "warn")) {
  on_error <- match.arg(on_error)
  s <- as.POSIXct(paste(as.Date(day), "00:00:00"), tz = tz)
  e <- min(s + 86400, as.POSIXct(now, tz = tz))
  if (e <= s) return(data.table())
  
  res <- tryCatch(
    getTicks(ticker, eventType = event_types, startTime = s, endTime = e,
             tz = tz, con = con, returnAs = "data.frame"),
    error = function(err) {
      msg <- sprintf("%s [%s]: %s", ticker, as.Date(day), conditionMessage(err))
      if (on_error == "stop") stop(msg, call. = FALSE)
      message("  ERROR ", msg); NULL
    })
  if (is.null(res)) return(NULL)
  dt <- as.data.table(res)
  if (!nrow(dt)) return(dt)
  
  setnames(dt, old = intersect(c("times", "type", "value", "size"), names(dt)),
           new = c("Time", "Type", "Price", "Size")[match(intersect(c("times", "type", "value", "size"), names(dt)),
                                                          c("times", "type", "value", "size"))])
  extra <- setdiff(names(dt), c("Time", "Type", "Price", "Size"))
  if (length(extra) == 1L) setnames(dt, extra, "CondCodes")
  if (!"Type" %in% names(dt)) dt[, Type := event_types[1L]]
  dt[, Type := factor(as.character(Type), levels = TYPE_LEVELS)]
  dt[, Time := as.POSIXct(Time, tz = tz)]
  setorder(dt, Time)          # radix sort is stable: within-second order preserved
  dt
}

#' Forward-fill prevailing Bid/Ask in place, seeded from prior day's close
fill_quotes <- function(dt, seed = c(bid = NA_real_, ask = NA_real_)) {
  if (!nrow(dt)) { dt[, `:=`(Bid = numeric(0), Ask = numeric(0))]; return(dt) }
  dt[, Bid := fifelse(Type == "BID", Price, NA_real_)]
  dt[, Ask := fifelse(Type == "ASK", Price, NA_real_)]
  dt[, Bid := nafill(c(seed[["bid"]], Bid), type = "locf")[-1L]]
  dt[, Ask := nafill(c(seed[["ask"]], Ask), type = "locf")[-1L]]
  setcolorder(dt, intersect(c("Time", "Type", "Price", "Size", "Bid", "Ask", "CondCodes"), names(dt)))
  dt
}

#' Write one contract-day (rds + state + optional csv). Never fatal on csv.
write_tick_day <- function(dt, id, day, cfg) {
  dir.create(id_dir(id, cfg, "rds"), recursive = TRUE, showWarnings = FALSE)
  st <- list(id = id, day = as.Date(day), n = nrow(dt),
             last_bid = if (nrow(dt)) tail(dt$Bid, 1L) else NA_real_,
             last_ask = if (nrow(dt)) tail(dt$Ask, 1L) else NA_real_,
             written  = Sys.time())
  if (nrow(dt)) {
    saveRDS(dt, rds_file(id, day, cfg), compress = cfg$compress_rds)
    if (isTRUE(cfg$write_csv)) {
      dir.create(id_dir(id, cfg, "csv"), recursive = TRUE, showWarnings = FALSE)
      p <- csv_file(id, day, cfg)
      ok <- tryCatch({ fwrite(dt, paste0(p, ".tmp"), dateTimeAs = "write.csv"); file.rename(paste0(p, ".tmp"), p) },
                     error = function(e) FALSE, warning = function(w) FALSE)
      if (!isTRUE(ok)) { unlink(paste0(p, ".tmp")); message("  WARN csv not written: ", basename(p)) }
    }
  } else {
    unlink(rds_file(id, day, cfg)); unlink(csv_file(id, day, cfg))   # day became empty on refetch
  }
  saveRDS(st, state_file(id, day, cfg))
  invisible(st)
}

## --------------------------------------------------------------------------
## 3. Driver
## --------------------------------------------------------------------------

#' Pull ticks for every contract in range; one contract-day in memory at a time
#'
#' @return data.table manifest of contract-days written this run
pull_energy_ticks <- function(universe, cfg = tick_cfg, con = blpConnect()) {
  on.exit(blpDisconnect(con), add = TRUE)
  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  now_utc   <- as.POSIXct(Sys.time(), tz = cfg$tz_store)
  today     <- as.Date(now_utc, tz = cfg$tz_store)
  floor_day <- as.Date(max(now_utc - cfg$bbg_lookback * 86400,
                           now_utc - cfg$tick_lookback * 86400), tz = cfg$tz_store)
  manifest  <- list(); k <- 0L
  
  for (r in seq_len(nrow(universe))) {
    spec <- universe[r, ]
    message("==== ", spec$primary_id, " (", spec$bbg_root, ") ticks ====")
    
    chain <- bbg_fut_chain(spec$bbg_root, spec$yellow_key, con = con)
    meta  <- bbg_contract_meta(chain, con = con)
    meta  <- active_contracts(meta, floor_day, now_utc)
    cm    <- Map(contract_month, meta$ticker, spec$bbg_root, meta$LAST_TRADEABLE_DT)
    meta$yyyymm    <- vapply(cm, `[[`, integer(1),   "yyyymm")
    meta$fi_suffix <- vapply(cm, `[[`, character(1), "suffix")
    meta  <- meta[!is.na(meta$yyyymm) &
                    meta$yyyymm >= code_yyyymm(cfg$contract_from) &
                    meta$yyyymm <= code_yyyymm(cfg$contract_to), ]
    meta  <- meta[order(meta$yyyymm), ]
    meta$fi_id <- paste(spec$primary_id, meta$fi_suffix, sep = "_")
    message("  ", nrow(meta), " contracts")
    
    for (i in seq_len(nrow(meta))) {
      tk <- meta$ticker[i]; id <- meta$fi_id[i]
      first_day <- max(floor_day, as.Date(meta$FUT_FIRST_TRADE_DT[i]))
      last_day  <- min(today,     as.Date(meta$LAST_TRADEABLE_DT[i]))
      if (last_day < first_day) next
      
      have    <- stored_days(id, cfg)
      wanted  <- seq(first_day, last_day, by = "day")
      todo    <- setdiff(wanted, have)
      if (length(have)) todo <- union(todo, max(have))   # refetch most recent (possibly partial) day
      todo    <- sort(as.Date(todo, origin = "1970-01-01"))
      if (!length(todo)) { message("  ", id, " up to date"); next }
      
      for (d in as.list(todo)) {
        dt <- fetch_tick_day(tk, d, cfg$event_types, tz = cfg$tz_store, now = now_utc, con = con)
        if (is.null(dt)) next
        dt <- fill_quotes(dt, seed_quotes(id, d, cfg))
        st <- write_tick_day(dt, id, d, cfg)
        message(sprintf("  %-8s %s  %9d ticks", id, format(as.Date(d)), st$n))
        k <- k + 1L
        manifest[[k]] <- data.table(root = spec$primary_id, id = id, ticker = tk,
                                    day = as.Date(d), n = st$n)
        rm(dt); if (k %% cfg$gc_every == 0L) invisible(gc(verbose = FALSE))
      }
    }
  }
  
  man <- rbindlist(manifest)
  if (nrow(man)) fwrite(man, file.path(cfg$out_dir, "manifest.csv"), append = file.exists(file.path(cfg$out_dir, "manifest.csv")))
  invisible(man)
}

## --------------------------------------------------------------------------
## 4. Reader
## --------------------------------------------------------------------------

#' Load a contract's ticks back from the day-partitioned store
#'
#' @param id           e.g. "CL_Z26"
#' @param from,to      optional Date bounds (inclusive)
#' @param trades_only  keep TRADE rows only
#' @param cols         optional column subset to reduce memory
#' @return data.table
load_ticks <- function(id, from = NULL, to = NULL, trades_only = FALSE,
                       cols = NULL, cfg = tick_cfg) {
  days <- stored_days(id, cfg)
  if (!is.null(from)) days <- days[days >= as.Date(from)]
  if (!is.null(to))   days <- days[days <= as.Date(to)]
  files <- rds_file(id, days, cfg)
  files <- files[file.exists(files)]
  if (!length(files)) return(data.table())
  rbindlist(lapply(files, function(f) {
    x <- readRDS(f)
    if (trades_only) x <- x[Type == "TRADE"]
    if (!is.null(cols)) x <- x[, intersect(cols, names(x)), with = FALSE]
    x
  }), use.names = TRUE, fill = TRUE)
}

## --------------------------------------------------------------------------
## 5. Run
## --------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  man <- pull_energy_ticks(universe, tick_cfg)
  print(man[, .(days = .N, ticks = sum(n)), by = id])
}