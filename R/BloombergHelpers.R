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