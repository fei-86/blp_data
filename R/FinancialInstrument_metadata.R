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