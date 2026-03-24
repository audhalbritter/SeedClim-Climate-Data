################################################################################
# SEEDCLIM CLIMATE DATA — HELPER FUNCTIONS  (R/functions/02_functions.R)
#
# Purpose
# -------
# Lower-level functions for reading, parsing, cleaning, and plotting individual
# climate data files.  Called by the high-level step functions in
# R/functions/pipeline_steps.R, which in turn are called by pipeline_report.qmd.
#
# Original script: Functions_ReadInSeedClimClimateData.R  (left untouched)
#
# Key changes vs. original
# ------------------------
#  - Modernised syntax: pivot_longer, across, dmy_hms, ymd_hms
#  - data.table output from all read functions (rbindlist()-safe)
#  - NULL-safe file assignment in ReadData()
#  - NameCheck(): dual-iconv strategy (UTF-8 + latin1 fallback) so that
#    Norwegian characters (å/ø/æ) are matched regardless of how the calling
#    string was encoded; patterns also accept plain ASCII spellings.
#  - ReadInBodyITAS(): handles ITAS files that have extra unnamed columns
#    (double-tab after "Label" header) by renaming them to X2/X3 before
#    dplyr operations and dropping auxiliary columns afterward.
#    Also walks the directory tree if the filename does not contain a
#    recognisable site name (site rescued from folder path).
#  - ReadInBodyUTL(): robust encoding detection (UTF-8 probe → latin1
#    fallback), Windows line-ending normalisation (\r stripping), Location
#    field lookup by name rather than fixed row index, and a serial-number
#    lookup table for files whose Location/Name header fields are blank.
#  - assign_soil_veg_loggers():
#      Pass 1 — assigns still-unassigned temp1/temp2 pairs by variance ratio.
#      Pass 2 — re-checks ALL assigned tempsoil/tempabove pairs (including
#               manually assigned ones) and auto-corrects any where
#               var(tempsoil) > var(tempabove) × 1.25.
#  - apply_site_overrides(): all per-site manual corrections encoded here;
#    every change written to logger_swap for full auditability.
#  - flag_outliers(): global range, rate-of-change, monthly IQR checks.
#  - plot_*() functions: diagnostic visualisations used in pipeline_report.qmd.
################################################################################

# Suppress R CMD CHECK / lintr "no visible binding" notes from data.table NSE.
utils::globalVariables(c(
  ".", ".N", ".SD",
  "Label", "X1", "X2",
  "site", "logger", "value", "date", "file", "type", "Repo", "source_repo",
  "flag", "proc_flag", "logger_swap",
  "temp1", "temp2", "var_val", "n_obs", "var_ratio", "assignment",
  "delta",
  "month_q25", "month_q75", "month_iqr",
  "pct_clean", "n_clean", "n_total", "n_flagged",
  "year", "n_outliers",
  "roll_med", "roll_mad",
  "swapped_by_variance"
))


#### NAME CHECK ####
# Maps a file path or site string to one of the 12 canonical 3-letter site codes.

NameCheck <- function(textfile) {
  # Normalize to ASCII twice: once treating the string as UTF-8 (the usual case
  # in a modern R session), once treating it as latin1 (for strings that were
  # read with fileEncoding = "latin1" and not re-encoded).  The Norwegian
  # characters ø/Ø, å/Å, æ/Æ all become "q" in both passes.  We then check
  # both normalized versions against the site patterns.
  txt_utf8 <- iconv(textfile, "UTF-8",  "ASCII", sub = "q")
  txt_lat1 <- iconv(textfile, "latin1", "ASCII", sub = "q")

  match_site <- function(txt) {
    dplyr::case_when(
      grepl("ulv|ule",                             txt, ignore.case = TRUE) ~ "ulv",
      grepl("gud",                                 txt, ignore.case = TRUE) ~ "gud",
      grepl("skj",                                 txt, ignore.case = TRUE) ~ "skj",
      grepl("ram",                                 txt, ignore.case = TRUE) ~ "ram",
      grepl("ves",                                 txt, ignore.case = TRUE) ~ "ves",
      grepl("fau",                                 txt, ignore.case = TRUE) ~ "fau",
      grepl("vik",                                 txt, ignore.case = TRUE) ~ "vik",
      grepl("arn|arh",                             txt, ignore.case = TRUE) ~ "arh",
      # ovs: plain ASCII "ovs/ovste" + encoded Øvs variants
      grepl("ovs|ovste|\\.vs|qqvs",               txt, ignore.case = TRUE) ~ "ovs",
      # hog: plain "hog/hogsete" + encoded Høg variants
      grepl("hog|hogsete|h\\.g|hqqg",             txt, ignore.case = TRUE) ~ "hog",
      # alr: plain "alr/alrust" + encoded Ål variants
      grepl("alr|alrust|qqlr|aqqlr|\\.lr|aal|qqlfr|qlr|qlrust", txt,
            ignore.case = TRUE) ~ "alr",
      # lav: plain "lav/lavisdalen" + encoded Lå variants
      grepl("lav|lavisdalen|l\\.v|laqqv|lqqv|lqv|lqqvisdalen", txt,
            ignore.case = TRUE) ~ "lav",
      TRUE ~ "panic"
    )
  }

  site <- match_site(txt_utf8)
  # If the UTF-8 pass didn't match, try the latin1 pass (for strings still in
  # latin1 encoding, e.g. raw folder names on some systems)
  if (site == "panic") site <- match_site(txt_lat1)

  valid_sites <- c("fau", "alr", "ulv", "vik", "hog", "lav",
                   "arh", "ram", "gud", "ovs", "ves", "skj")
  if (!site %in% valid_sites) {
    stop(paste("Unrecognised site from:", textfile))
  }
  return(site)
}


#### READ DATA DISPATCHER ####
# Reads one .txt file, detects logger type (ITAS or UTL) and routes accordingly.
# Returns a data.table or NULL on failure.

ReadData <- function(textfile) {
  first <- tryCatch(
    readLines(textfile, n = 1, encoding = "latin1", warn = FALSE),
    error = function(e) {
      warning(paste(basename(textfile), "— could not read:", conditionMessage(e)))
      return(NULL)
    }
  )
  if (is.null(first) || length(first) == 0) return(NULL)

  dat <- if (grepl("Label", first, fixed = TRUE)) {
    ReadInBodyITAS(textfile)
  } else if (grepl("Versi", first, fixed = TRUE)) {
    ReadInBodyUTL(textfile)
  } else if (grepl("STNR|MND.*DAG.*TIM", first)) {
    warning(paste(basename(textfile),
                  "— MET Norway station format (not supported), skipping"))
    return(NULL)
  } else {
    warning(paste(basename(textfile), "— format not recognised, skipping"))
    return(NULL)
  }

  if (is.null(dat) || nrow(dat) == 0) return(NULL)

  # NULL-safe: only assign file column after confirming dat is non-null
  dat[, file := basename(textfile)]
  return(dat)
}


#### READ ITAS LOGGERS ####
# ITAS files: tab-delimited, first row is header with "Label" column,
# second row is units (removed), then data rows.

ReadInBodyITAS <- function(textfile) {

  raw <- tryCatch({
    readLines(textfile, encoding = "latin1", warn = FALSE) %>%
      gsub(pattern = "\u00f8", replacement = "o", x = .) %>%
      paste(collapse = "\n") %>%
      readr::read_delim(delim = "\t", show_col_types = FALSE,
                        name_repair = "minimal")
  }, error = function(e) {
    warning(paste(basename(textfile), "ITAS read error:", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # Remove "-" from column names (some ITAS files use e.g. "Temp-1")
  names(raw) <- gsub("-", "", names(raw))

  # Some ITAS exports have an extra unnamed column after Label (double-tab in the
  # header, e.g. "Label\t\tJordf1...") and occasionally a second unnamed interval
  # column (e.g. "Label\t\t\tJordf1...").  readr keeps them as "" which breaks
  # dplyr::across(everything()).  Rename them to X2, X3, ... so the existing X2
  # time-column logic works and any remaining auxiliary columns can be dropped.
  empty_idx <- which(is.na(names(raw)) | names(raw) == "")
  if (length(empty_idx) > 0L) {
    names(raw)[empty_idx] <- paste0("X", empty_idx)
  }

  # Replace #-INF / #+INF sentinel values with NA, fix comma decimal separator
  # All columns treated as character at this stage, then converted below
  raw <- raw %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.character),
      dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "#-INF")),
      dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "#+INF")),
      dplyr::across(dplyr::everything(), ~ gsub(",", ".", .))
    ) %>%
    dplyr::slice(-1)  # row 1 is units (e.g. "°C"), not data

  if (nrow(raw) == 0) return(NULL)

  # Some files have a separate time column in X2; others embed time in Label
  if ("X2" %in% names(raw)) {
    raw <- raw %>%
      dplyr::mutate(date = paste(gsub("(.*) .*", "\\1", Label), X2)) %>%
      dplyr::select(-X2, -Label)
  } else {
    raw <- dplyr::rename(raw, date = Label)
  }

  # Drop any leftover unnamed auxiliary columns (e.g. an interval column X3
  # that appears in some files between the time and the first data column).
  aux_cols <- grep("^X[0-9]+$", names(raw), value = TRUE)
  if (length(aux_cols) > 0L) {
    raw <- dplyr::select(raw, -dplyr::all_of(aux_cols))
  }

  # Site detection: try filename first, then walk up the folder tree.
  # ITAS files carry no site field in their headers, so the path is the only
  # source of truth beyond the filename itself.
  site_code <- tryCatch(
    NameCheck(basename(tolower(textfile))),
    error = function(e) {
      parts <- rev(unlist(strsplit(dirname(textfile), "/")))
      found <- NA_character_
      for (p in parts) {
        found <- tryCatch(NameCheck(tolower(p)), error = function(e2) NA_character_)
        if (!is.na(found)) break
      }
      if (!is.na(found)) {
        warning(paste(basename(textfile),
                      "— site from folder path:", found))
        found
      } else {
        stop(conditionMessage(e))
      }
    }
  )

  dat <- raw %>%
    dplyr::mutate(dplyr::across(-date, as.numeric),
                  date = lubridate::dmy_hms(date, truncated = 1,
                                            tz = "Africa/Algiers")) %>%
    tidyr::pivot_longer(cols = -date, names_to = "logger", values_to = "value") %>%
    dplyr::mutate(
      logger = tolower(logger),
      site   = site_code,
      type   = "ITAS"
    ) %>%
    data.table::as.data.table()

  return(dat)
}


#### READ UTL LOGGERS ####
# UTL files: variable-length header ending at a line that reads exactly "Sample",
# followed by 2- or 3-column tab-delimited data.

ReadInBodyUTL <- function(textfile) {

  # Detect file encoding: try UTF-8 first; fall back to latin1 if the header
  # contains invalid UTF-8 byte sequences.  Older UTL logger exports are latin1;
  # newer ones (and most supplemental files) are UTF-8, with or without BOM.
  # Reading a UTF-8 file as latin1 garbles Norwegian characters (å/ø/æ).
  header_probe <- tryCatch(
    readLines(textfile, n = 30L, encoding = "UTF-8", warn = FALSE),
    error = function(e) NULL
  )
  file_enc <- if (!is.null(header_probe) &&
                  !any(is.na(iconv(header_probe, "UTF-8", "UTF-8")))) {
    "UTF-8"
  } else {
    "latin1"
  }

  header_lines <- tryCatch(
    if (file_enc == "UTF-8") header_probe else
      readLines(textfile, n = 30L, encoding = "latin1", warn = FALSE),
    error = function(e) {
      warning(paste(basename(textfile), "UTL read error:", conditionMessage(e)))
      return(NULL)
    }
  )
  if (is.null(header_lines)) return(NULL)

  # Strip Windows carriage returns (\r) — readLines on macOS/Linux keeps them
  # when the file uses \r\n line endings, causing "Sample\r" != "Sample".
  header_lines <- gsub("\r", "", header_lines, fixed = TRUE)

  # UTF-8 BOM (EF BB BF) appears as a prefix on the first element; strip it.
  header_lines[1L] <- gsub("^\xef\xbb\xbf", "", header_lines[1L], useBytes = TRUE)

  skip <- which(header_lines == "Sample")
  if (length(skip) != 1) {
    warning(paste("No 'Sample' header line found:", basename(textfile)))
    return(NULL)
  }

  # Read data body (decimal mark is comma in UTL files)
  dat <- tryCatch(
    readr::read_delim(textfile, delim = "\t", skip = skip, col_names = FALSE,
                      locale = readr::locale(decimal_mark = ",",
                                             encoding = file_enc),
                      show_col_types = FALSE),
    error = function(e) {
      warning(paste(basename(textfile), "UTL body error:", conditionMessage(e)))
      return(NULL)
    }
  )
  if (is.null(dat) || nrow(dat) == 0) return(NULL)

  # Normalise to 2-column format: (date-string, value)
  if (ncol(dat) == 1) {
    # Fallback: space-separated
    dat_raw <- utils::read.table(textfile, header = FALSE, sep = " ",
                                 skip = skip, dec = ",", stringsAsFactors = FALSE)
    dat <- data.frame(X1 = paste(dat_raw[, 1], dat_raw[, 2]),
                      X2 = dat_raw[, 3],
                      stringsAsFactors = FALSE)
  }
  if (ncol(dat) == 3) {
    dat <- tibble::tibble(X1 = paste(dat[[1]], dat[[2]], sep = " "),
                          X2 = dat[[3]])
  }

  dat <- dat %>%
    dplyr::rename(date = X1, value = X2) %>%
    dplyr::mutate(
      date  = lubridate::ymd_hms(date, tz = "Africa/Algiers"),
      value = as.numeric(gsub(",", ".", value))
    )

  # Read header metadata for logger type (30cm / 200cm)
  dat_header <- utils::read.delim(textfile, header = FALSE,
                                  nrows = skip, fileEncoding = file_enc)
  # Strip \r and any BOM artefacts from all header fields
  dat_header[] <- lapply(dat_header, function(x) {
    x <- gsub("\r", "", x, fixed = TRUE)
    gsub("^\xef\xbb\xbf", "", x, useBytes = TRUE)
  })

  # Serial-number lookup for files with blank Location AND Name fields.
  # These are download files where the user forgot to fill in the metadata,
  # but the serial number uniquely identifies the logger and its site.
  # Evidence for each entry is given in the comment.
  UTL_SERIAL_LOOKUP <- list(
    # 1233_30092009.txt: middle of three downloads at Vik/UTL/;
    #   adjacent files 1233_23062009 and 1233_09042010 both show
    #   Location=Vikjesland, Name=30cm.
    "1233" = list(site_code = "vik", logger_name = "temp30cm"),
    # 2488_21092010.txt: found in Hog/UTL/; next download of the same
    #   serial (#002488_20110412) shows Location=Høgsete, Name=200cm.
    "2488" = list(site_code = "hog", logger_name = "temp200cm")
  )

  serial_no   <- trimws(as.character(dat_header$V2[
    which(grepl("^SerialNumber$", dat_header$V1, ignore.case = TRUE))[1L]
  ]))
  loc_raw     <- trimws(as.character(dat_header$V2[
    which(grepl("^Location$|^Place$|^Sted$", dat_header$V1, ignore.case = TRUE))[1L]
  ]))
  name_raw    <- trimws(gsub(" ", "", dat_header$V2[4L], fixed = TRUE))

  serial_entry <- if (!is.na(serial_no) && serial_no %in% names(UTL_SERIAL_LOOKUP) &&
                       (is.na(loc_raw) || loc_raw == "") &&
                       (is.na(name_raw) || name_raw == "")) {
    UTL_SERIAL_LOOKUP[[serial_no]]
  } else {
    NULL
  }

  if (!is.null(serial_entry)) {
    warning(paste0(
      basename(textfile),
      " \u2014 empty Location/Name; assigned from serial lookup: ",
      serial_entry$site_code, " / ", serial_entry$logger_name
    ))
    t_name      <- serial_entry$logger_name
    serial_site <- serial_entry$site_code
  } else {
    # Normal path: read Name field and repair encoding if needed
    temp_logger <- name_raw

    temp_logger_repaired <- tryCatch(
      iconv(iconv(temp_logger, "UTF-8", "latin1"), "latin1", "UTF-8"),
      error = function(e) NA_character_
    )
    if (!is.na(temp_logger_repaired) &&
        nchar(temp_logger_repaired) < nchar(temp_logger)) {
      temp_logger <- temp_logger_repaired
    }

    t_name <- if (temp_logger %in% c("200cm", "30cm")) {
      paste0("temp", temp_logger)
    } else {
      warning(paste(basename(textfile), "— unexpected UTL logger label:", temp_logger))
      temp_logger
    }
    serial_site <- NULL
  }

  # Always impute site from folder path (original grepl condition was always TRUE
  # due to a regex bug — imputing from path is the robust approach)
  path_parts  <- unlist(strsplit(dirname(textfile), "/"))
  folder_site <- dplyr::last(path_parts)
  if (tolower(folder_site) == "utl") {
    folder_site <- path_parts[length(path_parts) - 1]
  }

  # Find site name in UTL header by looking for the Location/Place/Sted field
  # by name rather than assuming a fixed row index.
  header_site_field <- tryCatch({
    location_row <- which(grepl("^Location$|^Place$|^Sted$|^Stasjon$",
                                dat_header$V1, ignore.case = TRUE))
    raw_val <- if (length(location_row) > 0L) {
      as.character(dat_header$V2[location_row[1L]])
    } else {
      as.character(dat_header$V2[3L])  # row-3 fallback for non-standard headers
    }
    # Repair garbled Norwegian characters: UTF-8 bytes misread as latin1 produce
    # two characters (e.g. "Ã¥") where there should be one (e.g. "å").
    # The double-iconv trick re-interprets the string's code points as raw latin1
    # bytes and re-decodes as UTF-8.  We accept the result only if it is shorter
    # (fewer characters = successful multi-byte repair).
    repaired <- tryCatch(
      iconv(iconv(raw_val, "UTF-8", "latin1"), "latin1", "UTF-8"),
      error = function(e) NA_character_
    )
    if (!is.na(repaired) && nchar(repaired) < nchar(raw_val)) repaired else raw_val
  }, error = function(e) NA_character_)

  site_name <- if (!is.null(serial_site)) {
    # Site was determined from the serial-number lookup above
    serial_site
  } else {
    tryCatch(
      NameCheck(tolower(folder_site)),
      error = function(e) {
        # Fall back to the Location field found in the file header
        tryCatch(
          NameCheck(tolower(header_site_field)),
          error = function(e2) stop(paste("Cannot determine site for", basename(textfile)))
        )
      }
    )
  }

  dat <- dat %>%
    dplyr::mutate(logger = t_name, site = site_name, type = "UTL") %>%
    data.table::as.data.table()

  return(dat)
}


#### ASSIGN SOIL vs VEG TEMPERATURE LOGGERS ####
#
# Determines which of temp1/temp2 is soil (lower variance) and which is
# above-ground (higher variance) using a per-site × per-file variance ratio:
#
#   ratio = var(temp1) / var(temp2)
#   ratio < LOW_THRESH  (0.80) → temp1 is soil, temp2 is above-ground
#   ratio > HIGH_THRESH (1.25) → temp2 is soil, temp1 is above-ground
#   0.80 ≤ ratio ≤ 1.25       → ambiguous; flag VarianceProblem_TooSimilar
#
# Two passes are run:
#
#   Pass 1 — processes rows still named temp1/temp2 after apply_site_overrides().
#            Requires both channels to have ≥ MIN_OBS (48) valid observations.
#
#   Pass 2 — re-checks ALL assigned tempsoil/tempabove pairs (including those
#            assigned by manual overrides).  If var(tempsoil)/var(tempabove)
#            exceeds HIGH_THRESH (1.25) after assignment, the assignment is
#            clearly inverted and is automatically corrected.  The original
#            manual assignment reason is preserved in logger_swap and the
#            override is appended ("; AutoOverride_VarRatio:...").
#
# logger_swap values written by this function:
#   AutoSwap_VarRatio:0.43 (temp1→soil)   — Pass 1, temp1 had lower variance
#   AutoSwap_VarRatio:2.10 (temp2→soil)   — Pass 1, temp2 had lower variance
#   AutoOverride_VarRatio:1.43 — var(soil)>var(above), assignment reversed
#                                           — Pass 2 correction

assign_soil_veg_loggers <- function(dt) {

  MIN_OBS     <- 48    # minimum observations for a reliable variance estimate
  LOW_THRESH  <- 0.8   # var(temp1)/var(temp2) < LOW_THRESH  → temp1 is soil
  HIGH_THRESH <- 1.25  # var(temp1)/var(temp2) > HIGH_THRESH → temp2 is soil
  # Same threshold used when re-checking already-assigned pairs:
  # var(tempsoil)/var(tempabove) > HIGH_THRESH → soil has higher variance
  # than above-ground, meaning the current assignment is inverted.

  if (!"logger_swap" %in% names(dt)) dt[, logger_swap := NA_character_]
  if (!"proc_flag"   %in% names(dt)) dt[, proc_flag   := NA_character_]

  # ------------------------------------------------------------------
  # Pass 1: assign still-unassigned temp1/temp2 pairs by variance
  # ------------------------------------------------------------------
  var_dt <- dt[logger %in% c("temp1", "temp2") & !is.na(value),
               .(var_val = var(value, na.rm = TRUE),
                 n_obs   = sum(!is.na(value))),
               by = .(site, file, logger)]

  both_present <- var_dt[, .N, by = .(site, file)][N == 2, .(site, file)]
  var_dt <- var_dt[both_present, on = .(site, file)][n_obs >= MIN_OBS]

  if (nrow(var_dt) > 0L) {
    var_wide <- data.table::dcast(var_dt, site + file ~ logger, value.var = "var_val")
    var_wide <- var_wide[!is.na(temp1) & !is.na(temp2)]
    var_wide[, var_ratio := temp1 / temp2]
    var_wide[, assignment := data.table::fcase(
      var_ratio < LOW_THRESH,  "temp1_is_soil",
      var_ratio > HIGH_THRESH, "temp2_is_soil",
      default = "ambiguous"
    )]

    dt <- merge(dt, var_wide[, .(site, file, var_ratio, assignment)],
                by = c("site", "file"), all.x = TRUE)

    dt[logger == "temp1" & assignment == "temp1_is_soil",
       `:=`(logger      = "tempsoil",
            logger_swap = paste0("AutoSwap_VarRatio:", round(var_ratio, 2),
                                 " (temp1\u2192soil)"))]
    dt[logger == "temp2" & assignment == "temp1_is_soil",
       `:=`(logger      = "tempabove",
            logger_swap = paste0("AutoSwap_VarRatio:", round(var_ratio, 2),
                                 " (temp2\u2192above)"))]
    dt[logger == "temp1" & assignment == "temp2_is_soil",
       `:=`(logger      = "tempabove",
            logger_swap = paste0("AutoSwap_VarRatio:", round(var_ratio, 2),
                                 " (temp1\u2192above)"))]
    dt[logger == "temp2" & assignment == "temp2_is_soil",
       `:=`(logger      = "tempsoil",
            logger_swap = paste0("AutoSwap_VarRatio:", round(var_ratio, 2),
                                 " (temp2\u2192soil)"))]

    dt[logger %in% c("temp1", "temp2") & assignment == "ambiguous",
       proc_flag := data.table::fifelse(
         is.na(proc_flag), "AmbiguousAssignment",
         paste(proc_flag, "AmbiguousAssignment", sep = "|"))]

    dt[, c("var_ratio", "assignment") := NULL]

    n_auto <- dt[!is.na(logger_swap) & grepl("AutoSwap", logger_swap), .N]
    message("  Pass 1 — automated variance assignment: ", n_auto, " rows assigned")
  } else {
    message("  Pass 1 — no unassigned temp1/temp2 pairs found.")
  }

  # ------------------------------------------------------------------
  # Pass 2: re-check ALL assigned tempsoil/tempabove pairs (including
  # manually-assigned ones).  If var(tempsoil) > var(tempabove) by the
  # same HIGH_THRESH, the assignment is clearly inverted — auto-correct
  # and append a note to logger_swap so the override is fully traceable.
  # ------------------------------------------------------------------
  post_var <- dt[
    logger %in% c("tempsoil", "tempabove") & !is.na(value),
    .(var_val = var(value, na.rm = TRUE), n_obs = sum(!is.na(value))),
    by = .(site, file, logger)
  ]
  both_assigned <- post_var[, .N, by = .(site, file)][N == 2, .(site, file)]
  post_var <- post_var[both_assigned, on = .(site, file)][n_obs >= MIN_OBS]

  if (nrow(post_var) > 0L) {
    post_wide <- data.table::dcast(
      post_var, site + file ~ logger, value.var = "var_val"
    )
    post_wide <- post_wide[!is.na(tempsoil) & !is.na(tempabove)]
    post_wide[, var_ratio_post := tempsoil / tempabove]

    # Files where soil clearly has higher variance than above → swap back
    to_correct <- post_wide[var_ratio_post > HIGH_THRESH,
                             .(site, file, var_ratio_post)]

    if (nrow(to_correct) > 0L) {
      message(
        "  Pass 2 — auto-correcting ", nrow(to_correct),
        " file(s) where var(tempsoil) > var(tempabove) \u00d7 ", HIGH_THRESH,
        " (manual assignment overridden by variance test)"
      )

      dt <- merge(dt, to_correct, by = c("site", "file"), all.x = TRUE)

      # Swap tempsoil ↔ tempabove simultaneously (data.table evaluates RHS
      # from the current column values before writing, so both sides are safe)
      dt[!is.na(var_ratio_post) & logger %in% c("tempsoil", "tempabove"),
         `:=`(
           logger = data.table::fifelse(
             logger == "tempsoil", "tempabove", "tempsoil"
           ),
           logger_swap = paste0(
             data.table::fifelse(
               is.na(logger_swap), "",
               paste0(logger_swap, "; ")
             ),
             "AutoOverride_VarRatio:", round(var_ratio_post, 2),
             " \u2014 var(soil)>var(above), assignment reversed by variance test"
           )
         )]

      dt[, var_ratio_post := NULL]
    } else {
      message("  Pass 2 — all assigned pairs confirmed correct by variance test.")
    }
  }

  return(dt)
}


#### APPLY PER-SITE MANUAL OVERRIDES ####
#
# First tier of the four-tier soil/veg assignment ladder.  Applies all
# documented, site-specific knowledge about logger placement.
#
# Runs BEFORE assign_soil_veg_loggers() so explicit knowledge always takes
# precedence over the statistical test.
#
# Two types of changes are made:
#   1. Logger re-assignment: temp1/temp2 → tempsoil/tempabove for known sites.
#      Every change is recorded in logger_swap with a human-readable reason,
#      e.g. "ManualSwap: Fauske — temp1/thermistor2 always soil".
#   2. Data quality flags and NA-ification: values that are physically
#      implausible or known to be wrong are either removed (value := NA) or
#      flagged in the flag column without removal.
#
# Note: assign_soil_veg_loggers() Pass 2 will still re-check these assignments
# and auto-correct if var(tempsoil) > var(tempabove) × 1.25, appending an
# AutoOverride note to logger_swap so the correction is fully traceable.

apply_site_overrides <- function(dt) {

  if (!"flag"        %in% names(dt)) dt[, flag        := NA_character_]
  if (!"proc_flag"   %in% names(dt)) dt[, proc_flag   := NA_character_]
  if (!"logger_swap" %in% names(dt)) dt[, logger_swap := NA_character_]

  # ===== FAUSKE =====
  # temp1 and thermistor 2 are ALWAYS soil; temp2 and thermistor 1 are ALWAYS above.
  # Bad data: remove temp1 for mid-2013 – Oct 2014 (logger not in soil)
  dt[site == "fau" &
       file %in% c("Fauske_ITAS_13.6.6-13.9.22.txt",
                   "Fauske_ITAS_13.9.22-14.5.14.txt",
                   "Fauske_ITAS_14.5.14-14.10.14.txt") &
       logger == "temp1",
     value := NA_real_]

  dt[site == "fau" & logger %in% c("temp1", "thermistor 2"),
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Fauske — temp1/thermistor2 always soil")]
  dt[site == "fau" & logger %in% c("temp2", "thermistor 1"),
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Fauske — temp2/thermistor1 always above")]

  # CORRECTION for the extra logger deployed in spring 2016:
  # temp1 reaches 29–37 °C in May — clearly an above-ground sensor in direct
  # sun, not soil. temp2 (~15 °C) is soil. The site-wide rule above is wrong
  # for this file; overwrite it here (this runs after, so it takes precedence).
  dt[file == "20160706_Fauske_ekstra_logger_met1.txt" & logger == "tempsoil",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Fauske extra 2016 — CORRECTION temp1 above-ground (29-37\u00b0C); overrides site-wide rule")]
  dt[file == "20160706_Fauske_ekstra_logger_met1.txt" & logger == "tempabove",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Fauske extra 2016 — CORRECTION temp2 is soil (~15\u00b0C); overrides site-wide rule")]

  # ===== VIKESLAND =====
  # Remove physically implausible low values Oct–Dec 2018
  dt[site == "vik" &
       date > as.POSIXct("2018-10-01", tz = "Africa/Algiers") &
       date < as.POSIXct("2019-01-01", tz = "Africa/Algiers") &
       logger == "temp2" & !is.na(value) & value < -2,
     value := NA_real_]

  # ===== ARHELLEREN =====
  # July 2009 download file had temp1/temp2 labels reversed
  dt[file == "Arhelleren_08072009.txt" & logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Arhelleren Jul 2009 — labels reversed in download")]
  dt[file == "Arhelleren_08072009.txt" & logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Arhelleren Jul 2009 — labels reversed in download")]

  # ===== OVSTEDAL =====
  dt[site == "ovs" &
       file %in% c("Ovstedal_met1_20160511_20160523.txt",
                   "Ovstedal_met1_Fall2016.txt",
                   "Ovstedal_met1_autumn2017.txt",
                   "Ovstedal_met1.txt",
                   "\u00d8vstedal_met1_20160511_20160523.txt",
                   "\u00d8vstedal_met1_Fall2016.txt",
                   "\u00d8vstedal_met1_autumn2017.txt",
                   "\u00d8vstedal_met1.txt") &
       logger == "temp2",
     logger_swap := data.table::fifelse(
       is.na(logger_swap),
       "KnownIssue: Ovstedal temp2 systematic offset 2016-2017",
       paste(logger_swap,
             "KnownIssue: Ovstedal temp2 systematic offset 2016-2017",
             sep = "; ")
     )]

  dt[site == "ovs" &
       logger %in% c("temp200cm", "temp30cm") &
       date < as.POSIXct("2009-06-28", tz = "Africa/Algiers"),
     value := NA_real_]

  # Two specific UTL files with swapped 30cm/200cm labels
  dt[file == "#002486_20190506_1100.txt" & logger == "temp30cm",
     logger_swap := "ManualSwap: Ovstedal — confirmed temp30cm (no change)"]
  dt[file == "#002473_20190506_1200.txt" & logger == "temp30cm",
     `:=`(logger      = "temp200cm",
          logger_swap = "ManualSwap: Ovstedal — relabelled temp30cm\u2192temp200cm")]

  # ===== VESKRE =====
  veskre_swap_files <- c("Veskre_ITAS_140802_141003.txt",
                         "Veskre_klima_autumn2017.txt",
                         "Veskre_klima_Fall2016.txt",
                         "Veskre_klima.txt")
  dt[site == "ves" & file %in% veskre_swap_files & logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Veskre — temp2\u2192above (2014/2016/2017 files)")]
  dt[site == "ves" & file %in% veskre_swap_files & logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Veskre — temp1\u2192soil (2014/2016/2017 files)")]

  # These files pair "temperature" (soil) with "veg. temp" (above-ground).
  # Both filename variants cover the same logger; oct2021 was missing from the rule.
  dt[file %in% c("Veskre_klima.txt", "Veskre_klima_oct2021.txt") &
       logger == "temperature",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Veskre — temperature\u2192tempsoil (Veskre_klima files)")]

  dt[site == "ves" & lubridate::year(date) == 2021 & logger == "temperature",
     value := NA_real_]

  dt[site == "ves" &
       lubridate::year(date) == 2021 & lubridate::month(date) == 7 &
       logger == "tempabove" & !is.na(value) & value < -10,
     value := NA_real_]

  # ===== RAMBERA =====
  dt[site == "ram" & lubridate::year(date) <= 2013 & logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Rambera — temp1\u2192soil for years \u22642013")]
  dt[site == "ram" & lubridate::year(date) <= 2013 & logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Rambera — temp2\u2192above for years \u22642013")]

  dt[site == "ram" & lubridate::year(date) == 2009 &
       logger == "temp200cm" & !is.na(value) & value > 35,
     value := NA_real_]

  # ===== HOGSETE =====
  # Hogsete 2015-2016: temp2 had physically implausible values < -5 °C
  dt[file == "H\u00f8gsete_met1_2015_2016.txt" &
       logger == "temp2" & !is.na(value) & value < -5,
     value := NA_real_]

  # ===== ALRUST =====
  # Before 2016: loggers were swapped in the field
  dt[site == "alr" & lubridate::year(date) < 2016 & logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Alrust — temp1\u2192soil for years <2016")]
  dt[site == "alr" & lubridate::year(date) < 2016 & logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Alrust — temp2\u2192above for years <2016")]

  dt[site == "alr" & lubridate::year(date) == 2009 &
       logger == "temp200cm" & !is.na(value) & value > 30,
     value := NA_real_]

  # ===== ULVEHAUGEN =====
  # Consistent throughout: temp1 = soil, temp2 = above
  dt[site == "ulv" & logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Ulvehaugen — temp1 always soil")]
  dt[site == "ulv" & logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Ulvehaugen — temp2 always above")]

  # ===== LAVISDALEN =====
  dt <- dt[!grepl("visdalen-met1-20120913\\.txt", file, ignore.case = TRUE)]

  dt[logger == "temp1" & file == "Lavisdalen_25_10_11.txt", value := NA_real_]

  dt[lubridate::year(date) < 2014 &
       grepl("visdalen_met1\\.txt", file, ignore.case = TRUE),
     value := NA_real_]

  dt[site == "lav" & lubridate::year(date) < 2012 & logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Lavisdalen — temp1\u2192soil for years <2012")]
  dt[site == "lav" & lubridate::year(date) < 2012 & logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Lavisdalen — temp2\u2192above for years <2012")]

  dt[site == "lav" & logger == "temperature",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Lavisdalen — temperature\u2192tempabove")]
  dt[site == "lav" & logger == "temperature2",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Lavisdalen — temperature2\u2192tempsoil")]

  dt[site == "lav" &
       logger %in% c("temp1", "temp2", "tempabove", "tempsoil") &
       lubridate::year(date) > 2012 & lubridate::year(date) < 2014,
     value := NA_real_]

  # Remove time-machine observations (date parse error produced year > 2035)
  dt <- dt[lubridate::year(date) < 2035]

  # ===== GUDMEDALEN =====
  dt <- dt[!(site == "gud" & logger == "jord-5cm")]

  dt[site == "gud" &
       logger %in% c("temperature", "temperature2") &
       lubridate::year(date) == 2018,
     value := NA_real_]

  dt[site == "gud" &
       date < as.POSIXct("2015-01-01", tz = "Africa/Algiers") &
       logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Gudmedalen — temp1\u2192soil for dates <2015")]
  dt[site == "gud" &
       date < as.POSIXct("2015-01-01", tz = "Africa/Algiers") &
       logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Gudmedalen — temp2\u2192above for dates <2015")]

  # Specific file had labels reversed relative to the surrounding period
  dt[file == "Gudmedalen_ITAS_140619_141013.txt" & logger == "tempabove",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Gudmedalen_140619_141013 — labels reversed in this download")]
  dt[file == "Gudmedalen_ITAS_140619_141013.txt" & logger == "tempsoil",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Gudmedalen_140619_141013 — labels reversed in this download")]

  # Site-wide rule: all Gudmedalen files that use the "temperature" / "temperature2"
  # column labels follow the same convention — temperature = above, temperature2 = soil.
  # (Supersedes earlier file-specific and year-specific rules for the same mapping.)
  dt[site == "gud" & logger == "temperature",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Gudmedalen — temperature\u2192tempabove (all files)")]
  dt[site == "gud" & logger == "temperature2",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Gudmedalen — temperature2\u2192tempsoil (all files)")]

  # ===== SKJELLINGAHAUGEN =====
  skj_swap_files <- c(
    "Skjellingahaugen_met1 20141016 - 20150121.txt",
    "Skjellingahaugen_met1_20150806 - 20150814.txt",
    "Skjellingahaugen_met1-20150907- 20150910.txt",
    "Skjellingahaugen_met1_Spring2017.txt",
    "Skjellingahaugen_met1_temp_prec_20170712-20170808_Spring2017.txt"
  )
  dt[site == "skj" & file %in% skj_swap_files & logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = "ManualSwap: Skjellingahaugen — temp1\u2192soil (2014\u20132017 download files)")]
  dt[site == "skj" & file %in% skj_swap_files & logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = "ManualSwap: Skjellingahaugen — temp2\u2192above (2014\u20132017 download files)")]

  dt[site == "skj" &
       logger %in% c("temp200cm", "temp30cm") &
       date < as.POSIXct("2009-06-25", tz = "Africa/Algiers"),
     value := NA_real_]

  dt[site == "skj" &
       logger %in% c("temperature", "temperature2") &
       lubridate::year(date) == 2019,
     value := NA_real_]

  # Year-based rule for 2015–2017 (catches any remaining temp1/temp2)
  dt[site == "skj" &
       lubridate::year(date) %in% c(2015, 2016, 2017) &
       logger == "temp1",
     `:=`(logger      = "tempsoil",
          logger_swap = data.table::fifelse(
            is.na(logger_swap),
            "ManualSwap: Skjellingahaugen — temp1\u2192soil 2015\u20132017",
            paste(logger_swap,
                  "ManualSwap: Skjellingahaugen — temp1\u2192soil 2015\u20132017",
                  sep = "|")))]
  dt[site == "skj" &
       lubridate::year(date) %in% c(2015, 2016, 2017) &
       logger == "temp2",
     `:=`(logger      = "tempabove",
          logger_swap = data.table::fifelse(
            is.na(logger_swap),
            "ManualSwap: Skjellingahaugen — temp2\u2192above 2015\u20132017",
            paste(logger_swap,
                  "ManualSwap: Skjellingahaugen — temp2\u2192above 2015\u20132017",
                  sep = "|")))]

  # ===== SPECIFIC UTL FILE LABEL CORRECTIONS (all sites) =====
  # These files had wrong logger labels in the UTL header
  dt[file %in% c("1229_30092009.txt", "#002474_20180607_1000.txt",
                 "#001066_20081030_0000.txt", "1233_30092009.txt"),
     logger := "temp30cm"]
  dt[file %in% c("2488_21092010.txt", "#002482_20180705_0800.txt",
                 "#001047_20081030_0000_Veskre.txt", "1169_30092009.txt"),
     logger := "temp200cm"]

  return(dt)
}


#### FLAG OUTLIERS ####
# Three complementary methods:
#   1. Global range: physically impossible temperatures → flag immediately
#   2. Rate of change: > max_delta °C between consecutive hourly readings → flag
#   3. Monthly IQR: values outside Q1 - k*IQR … Q3 + k*IQR within same site×logger×month

flag_outliers <- function(dt,
                          k_iqr     = 3,    # IQR multiplier
                          max_delta = 5,    # max °C/hour rate of change
                          temp_min  = -40,  # global floor (any temp logger)
                          temp_max  = 50,   # global ceiling (any temp logger)
                          soil_min  = -20,  # stricter floor for soil loggers
                          soil_max  = 35) { # stricter ceiling for soil loggers

  data.table::setkey(dt, site, logger, date)

  # 1. Global range
  dt[logger %in% c("tempabove", "veg_temp", "temp1", "temp2") &
       !is.na(value) & (value < temp_min | value > temp_max),
     flag := data.table::fifelse(is.na(flag), "Outlier_Range",
                                 paste(flag, "Outlier_Range", sep = "|"))]

  dt[logger %in% c("tempsoil", "soil_temp", "temp30cm") &
       !is.na(value) & (value < soil_min | value > soil_max),
     flag := data.table::fifelse(is.na(flag), "Outlier_Range",
                                 paste(flag, "Outlier_Range", sep = "|"))]

  dt[logger == "temp200cm" &
       !is.na(value) & (value < temp_min | value > temp_max),
     flag := data.table::fifelse(is.na(flag), "Outlier_Range",
                                 paste(flag, "Outlier_Range", sep = "|"))]

  # 2. Rate of change (per site × logger, sorted by date)
  dt[, delta := c(NA_real_, abs(diff(value))), by = .(site, logger)]
  dt[!is.na(delta) & delta > max_delta,
     flag := data.table::fifelse(is.na(flag), "Outlier_RateOfChange",
                                 paste(flag, "Outlier_RateOfChange", sep = "|"))]
  dt[, delta := NULL]

  # 3. Monthly IQR per site × logger
  dt[!is.na(value),
     `:=`(
       month_q25 = stats::quantile(value, 0.25, na.rm = TRUE),
       month_q75 = stats::quantile(value, 0.75, na.rm = TRUE)
     ),
     by = .(site, logger, lubridate::year(date), lubridate::month(date))]

  dt[!is.na(value) & !is.na(month_q25),
     month_iqr := month_q75 - month_q25]

  dt[!is.na(value) & !is.na(month_iqr) &
       (value < month_q25 - k_iqr * month_iqr |
        value > month_q75 + k_iqr * month_iqr),
     flag := data.table::fifelse(is.na(flag), "Outlier_IQR",
                                 paste(flag, "Outlier_IQR", sep = "|"))]

  dt[, c("month_q25", "month_q75", "month_iqr") := NULL]

  # TODO: Hampel identifier (rolling MAD) for more localised detection.
  # Deferred — frollapply with median is slow on large datasets.
  # Candidate implementation:
  #   dt[, roll_med := frollapply(value, n = 120, FUN = median, ...)]
  #   dt[, roll_mad := frollapply(value, n = 120, FUN = mad, ...)]
  #   flag where abs(value - roll_med) > k * roll_mad

  return(dt)
}



#### VISUALISATION FUNCTIONS ####

# --- Coverage heatmap: site × year, coloured by % clean data ---
plot_coverage_heatmap <- function(dt) {
  cov_dt <- dt[logger %in% c("veg_temp", "soil_temp"),
               .(n_total   = .N,
                 n_clean   = sum(is.na(flag) & !is.na(value)),
                 n_flagged = sum(!is.na(flag))),
               by = .(site, logger, year = lubridate::year(date))]
  cov_dt[, pct_clean := n_clean / n_total * 100]

  ggplot2::ggplot(cov_dt, ggplot2::aes(x = year, y = site, fill = pct_clean)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradientn(
      colours = c("#d73027", "#fee090", "#1a9850"),
      limits  = c(0, 100), name = "% clean\ndata"
    ) +
    ggplot2::facet_wrap(~ logger) +
    ggplot2::labs(title = "Data coverage by site and year", x = "Year", y = NULL) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# --- Time series for one site, with flagged points highlighted ---
plot_timeseries_site <- function(dt, site_name) {
  plot_dt   <- dt[site == site_name &
                    logger %in% c("veg_temp", "soil_temp") &
                    !is.na(value)]
  flagged_dt <- plot_dt[!is.na(flag)]

  ggplot2::ggplot(plot_dt,
                  ggplot2::aes(x = date, y = value, colour = logger)) +
    ggplot2::geom_line(alpha = 0.5, linewidth = 0.2) +
    ggplot2::geom_point(data = flagged_dt,
                        ggplot2::aes(shape = "flagged"),
                        size = 1.2, alpha = 0.7, colour = "black") +
    ggplot2::scale_colour_manual(
      values = c("veg_temp" = "#d7191c", "soil_temp" = "#2c7bb6")
    ) +
    ggplot2::labs(title = site_name, x = NULL, y = "Temperature (\u00b0C)",
                  colour = "Logger", shape = NULL) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.position = "bottom")
}

# --- Variance ratio diagnostic: one point per file, coloured by assignment ---
plot_variance_ratios <- function(var_summary) {
  if (is.null(var_summary) || nrow(var_summary) == 0) return(NULL)

  ggplot2::ggplot(var_summary,
                  ggplot2::aes(x = seq_len(nrow(var_summary)),
                               y = var_ratio, colour = assignment)) +
    ggplot2::geom_hline(yintercept = c(0.8, 1.25),
                        linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(size = 1.8, alpha = 0.7) +
    ggplot2::scale_colour_manual(
      values = c("temp1_is_soil" = "#1a9850",
                 "temp2_is_soil" = "#d73027",
                 "ambiguous"     = "#f46d43"),
      labels = c("temp1_is_soil" = "temp1 = soil",
                 "temp2_is_soil" = "temp2 = soil",
                 "ambiguous"     = "Ambiguous")
    ) +
    ggplot2::facet_wrap(~ site, scales = "free_x") +
    ggplot2::labs(
      title    = "Automated soil/veg assignment: variance ratios per file",
      subtitle = "Dashed lines: assignment thresholds (0.8 and 1.25)",
      x = "File index (within site)", y = "var(temp1) / var(temp2)",
      colour = "Assignment"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
}

# --- Outlier count by site, logger, year ---
plot_outlier_density <- function(dt) {
  out_dt <- dt[!is.na(flag) & grepl("Outlier", flag),
               .(n_outliers = .N),
               by = .(site, logger, year = lubridate::year(date))]

  ggplot2::ggplot(out_dt,
                  ggplot2::aes(x = year, y = n_outliers, fill = logger)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::facet_wrap(~ site, scales = "free_y") +
    ggplot2::labs(title = "Flagged outliers by site, logger, and year",
                  x = "Year", y = "Number of flagged rows", fill = "Logger") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# --- Combined summary: saves all four plots to output/figures/ ---
plot_climate_summary <- function(dt, var_summary = NULL,
                                 output_dir = "output/figures") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  p_cov <- plot_coverage_heatmap(dt)
  ggplot2::ggsave(file.path(output_dir, "01_coverage_heatmap.png"),
                  p_cov, width = 12, height = 6, dpi = 150)
  message("  Saved 01_coverage_heatmap.png")

  sites     <- as.character(unique(dt[logger %in% c("veg_temp", "soil_temp")]$site))
  ts_plots  <- lapply(sites, function(s) plot_timeseries_site(dt, s))
  names(ts_plots) <- sites
  ts_combined <- patchwork::wrap_plots(ts_plots, ncol = 3) +
    patchwork::plot_annotation(title = "Temperature time series by site")
  ggplot2::ggsave(file.path(output_dir, "02_timeseries_all_sites.png"),
                  ts_combined, width = 18, height = 20, dpi = 150)
  message("  Saved 02_timeseries_all_sites.png")

  if (!is.null(var_summary) && nrow(var_summary) > 0) {
    p_var <- plot_variance_ratios(var_summary)
    if (!is.null(p_var)) {
      ggplot2::ggsave(file.path(output_dir, "03_variance_ratios.png"),
                      p_var, width = 14, height = 8, dpi = 150)
      message("  Saved 03_variance_ratios.png")
    }
  }

  p_out <- plot_outlier_density(dt)
  ggplot2::ggsave(file.path(output_dir, "04_outlier_density.png"),
                  p_out, width = 14, height = 8, dpi = 150)
  message("  Saved 04_outlier_density.png")

  invisible(list(coverage  = p_cov,
                 timeseries = ts_plots,
                 outliers  = p_out))
}
