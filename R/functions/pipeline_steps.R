################################################################################
# pipeline_steps.R
#
# High-level step functions for the SeedClim climate data pipeline.
# Each function handles one processing stage and returns the processed data
# alongside diagnostic metadata used by pipeline_report.qmd.
#
# Dependencies: R/functions/02_functions.R must be sourced first.
################################################################################


#### CONSTANTS ################################################################

CANONICAL_DIR    <- "data/ITAS_UTL_climate_data_2008-2025"
SUPPLEMENTAL_DIR <- "data/supplemental_biofelles"

EXCLUDE_PATTERNS <- paste(c(
  "Notes|Notater|UVB",
  "ITAS\\d{0,4}\\.txt",
  "ITAS-FALL-2014\\.txt",
  "1239_23062009\\.txt",
  "Veskre 1011met1\\.txt",
  "veskre november 2011\\.txt",
  "Veskre_07062011\\.txt"
), collapse = "|")

TEMP_LOGGERS <- c(
  "temp1", "temp2", "temp200cm", "temp30cm", "", "P\u00d8N", "-5cm",
  "thermistor 1", "thermistor 2", "jord-5cm", "2m",
  "temperature", "temperature2",
  "soil temp", "veg. temp", "veg temp", "veg temp ", "soil temp.",
  "veg. temp.", "soil temperature", "soiltemp", "vegtemp",
  "soil_temp", "veg_temp",
  "300cm",
  "P\u00c3\u0098N"
)

PRECIP_LOGGERS <- c(
  "nedbor", "rain", "arg100", "counter", "counter1",
  "counter2", "rain2", "precipitation"
)

SM_LOGGERS <- c(
  "jordf1", "jordf2", "soil.moisture", "soil moisture 2", "soil moisture 1",
  "soil moisture", "sm300 2", "sm300 1", "jordfukt2", "jordfukt1",
  "theta", "soil moist 2", "soil moist 1", "theta2", "soil moist2",
  "jord1", "jord 2", "jordfukt 1"
)

IGNORED_LOGGERS <- c(
  "power",
  "resistance1", "resistance2",
  "voltage1",    "voltage2",
  "uvb_36241",   "uvb_36242"
)

LOGGER_RECODE <- c(
  "2m"               = "temp200cm",
  "P\u00d8N"         = "temp200cm",
  "P\u00c3\u0098N"  = "temp200cm",
  "300cm"            = "temp200cm",
  "-5cm"             = "temp30cm",
  "jord-5cm"         = "temp30cm",
  "soil temp"        = "tempsoil",
  "soil temp."       = "tempsoil",
  "soil temperature" = "tempsoil",
  "soiltemp"         = "tempsoil",
  "veg temp"         = "tempabove",
  "veg temp "        = "tempabove",
  "veg. temp"        = "tempabove",
  "veg. temp."       = "tempabove",
  "vegtemp"          = "tempabove"
)

SITE_NAMES <- c(
  "skj" = "Skjelingahaugen", "gud" = "Gudmedalen",
  "lav" = "Lavisdalen",      "ulv" = "Ulvehaugen",
  "ves" = "Veskre",          "ram" = "Rambera",
  "hog" = "Hogsete",         "alr" = "Alrust",
  "ovs" = "Ovstedalen",      "arh" = "Arhelleren",
  "vik" = "Vikesland",       "fau" = "Fauske"
)

SITE_LEVELS <- c(
  "Skjelingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen",
  "Veskre", "Rambera", "Hogsete", "Alrust",
  "Ovstedalen", "Arhelleren", "Vikesland", "Fauske"
)


#### STEP FUNCTIONS ###########################################################

#' Step 1: Import raw climate data
#'
#' Reads all .txt files from the canonical and supplemental sources,
#' excluding known non-data files. Returns the combined data.table and
#' import diagnostics.
#'
#' @return list: data, n_files_canonical, n_files_supplemental,
#'               n_canonical, n_supplemental, source_summary
step_import <- function(
    canonical_dir    = CANONICAL_DIR,
    supplemental_dir = SUPPLEMENTAL_DIR,
    exclude_patterns = EXCLUDE_PATTERNS
) {
  apply_exclusions <- function(paths) {
    grep(exclude_patterns, paths, invert = TRUE, value = TRUE, ignore.case = TRUE)
  }

  read_files <- function(paths, label) {
    message("  Reading ", length(paths), " files from '", label, "'...")
    issues <- list()

    lst <- lapply(paths, function(f) {
      caught <- character(0)
      result <- withCallingHandlers(
        tryCatch(
          ReadData(f),
          error = function(e) {
            caught <<- c(caught, paste("ERROR:", conditionMessage(e)))
            NULL
          }
        ),
        warning = function(w) {
          caught <<- c(caught, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      if (length(caught) > 0L) {
        issues[[basename(f)]] <<- caught
      }
      result
    })

    if (length(issues) > 0L) {
      issue_dt <- rbindlist(lapply(names(issues), function(f) {
        data.table(file = f, message = issues[[f]])
      }))
      issue_dt[, category := data.table::fcase(
        grepl("MET Norway station format", message, ignore.case = TRUE),
          "MET Norway station format \u2014 different data source, not supported",
        grepl("Unrecognised site",         message, ignore.case = TRUE),
          "Unrecognised site in filename \u2014 file dropped",
        grepl("format not recognised",     message, ignore.case = TRUE),
          "Unrecognised file format \u2014 file dropped",
        grepl("Cannot determine site",     message, ignore.case = TRUE),
          "Cannot determine site \u2014 file dropped",
        grepl("site from folder path",     message, ignore.case = TRUE),
          "Site rescued from folder path \u2014 kept",
        grepl("unexpected UTL logger label", message, ignore.case = TRUE),
          "Unusual UTL logger label \u2014 kept, normalised in Step 4",
        grepl("^ERROR:",                   message),
          "Other read error \u2014 file dropped",
        default = "Other warning \u2014 file kept"
      )]
      issues[[".summary"]] <- issue_dt
    }

    dt <- rbindlist(Filter(Negate(is.null), lst), fill = TRUE, use.names = TRUE)
    if (nrow(dt) > 0L) dt[, source_repo := label]
    list(data = dt, issues = issues)
  }

  canonical_paths <- list.files(
    canonical_dir, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE
  ) |> apply_exclusions()

  supplemental_paths <- list.files(
    supplemental_dir, pattern = "\\.txt$", full.names = TRUE
  ) |> apply_exclusions()

  res_canonical    <- read_files(canonical_paths,    "canonical")
  res_supplemental <- read_files(supplemental_paths, "biofelles_supplement")

  n_canonical    <- nrow(res_canonical$data)
  n_supplemental <- nrow(res_supplemental$data)

  # Collect all per-file issue rows before freeing the large objects
  all_issues <- rbindlist(
    lapply(
      c(res_canonical$issues, res_supplemental$issues),
      function(x) if (is.data.table(x)) x else NULL
    ),
    fill = TRUE
  )

  climate <- rbindlist(
    list(res_canonical$data, res_supplemental$data),
    fill = TRUE, use.names = TRUE
  )
  rm(res_canonical, res_supplemental)
  gc()

  message("Total rows after import: ", format(nrow(climate), big.mark = ","))

  read_warning_summary <- if (nrow(all_issues) > 0L) {
    all_issues[, .(n_files = uniqueN(file), example_files = paste(
      head(unique(file), 3L), collapse = ", "
    )), by = category][order(category)]
  } else {
    NULL
  }

  n_dropped <- if (!is.null(read_warning_summary)) {
    sum(read_warning_summary[grepl("dropped", category), n_files])
  } else 0L

  n_kept_with_warning <- if (!is.null(read_warning_summary)) {
    sum(read_warning_summary[!grepl("dropped", category), n_files])
  } else 0L

  if (!is.null(read_warning_summary)) {
    message(
      n_dropped, " file(s) could not be read and were dropped; ",
      n_kept_with_warning, " file(s) had minor warnings but were kept."
    )
  }

  source_summary <- climate[
    , .(n_rows = .N, n_loggers = uniqueN(logger), n_files = uniqueN(file)),
    by = .(source_repo, site)
  ][order(source_repo, site)]

  list(
    data                 = climate,
    n_files_canonical    = length(canonical_paths),
    n_files_supplemental = length(supplemental_paths),
    n_canonical          = n_canonical,
    n_supplemental       = n_supplemental,
    source_summary       = source_summary,
    read_warning_summary = read_warning_summary
  )
}


#' Step 2: Initial cleaning — trim, date filter, deduplication
#'
#' Two-pass deduplication:
#'   Pass 1 – exact duplicates (same key + value)
#'   Pass 2 – conflicting values; prefer canonical source, then row
#'             closest to the site's monthly median.
#'
#' @return list: data, n_before, n_after, conflict_summary (or NULL)
step_clean <- function(climate) {
  n_before <- nrow(climate)

  climate <- climate[
    , .SD[seq_len(.N) > 2L & seq_len(.N) <= (.N - 2L)], by = file
  ]
  climate <- climate[date >= as.POSIXct("2008-10-01", tz = "Africa/Algiers")]
  climate <- climate[date <  as.POSIXct("2035-01-01", tz = "Africa/Algiers")]
  climate <- unique(climate, by = c("date", "logger", "site", "type", "value"))

  KEY_COLS <- c("date", "logger", "site", "type")
  dups_key <- climate[
    duplicated(climate, by = KEY_COLS) |
      duplicated(climate, by = KEY_COLS, fromLast = TRUE)
  ]

  conflict_summary <- NULL

  if (nrow(dups_key) > 0L) {
    message("Pass-2 conflicts: ", format(nrow(dups_key), big.mark = ","),
            " rows share a key with differing values.")

    conflict_summary <- dups_key[
      , .(
        n_values   = .N,
        files      = paste(sort(unique(file)), collapse = " | "),
        sources    = paste(sort(unique(source_repo)), collapse = " | "),
        value_min  = min(value, na.rm = TRUE),
        value_max  = max(value, na.rm = TRUE),
        value_diff = max(value, na.rm = TRUE) - min(value, na.rm = TRUE)
      ),
      by = KEY_COLS
    ][order(-value_diff)]

    fwrite(conflict_summary, "output/dedup_conflicts.csv")
    message("  Conflict table saved → output/dedup_conflicts.csv")

    data.table::setorder(climate, date, logger, site, type, source_repo)
    climate[, month_med := median(value, na.rm = TRUE),
            by = .(site, logger, data.table::month(date))]
    climate[, med_dist := abs(value - month_med)]
    data.table::setorder(climate, date, logger, site, type, source_repo, med_dist)
    climate <- unique(climate, by = KEY_COLS)
    climate[, c("month_med", "med_dist") := NULL]
  } else {
    climate <- unique(climate, by = KEY_COLS)
    message("No conflicting-value duplicates found.")
  }

  n_after <- nrow(climate)
  message(
    "Rows: ", format(n_before, big.mark = ","), " \u2192 ",
    format(n_after,  big.mark = ","),
    "  (removed ", format(n_before - n_after, big.mark = ","), ")"
  )

  list(
    data             = climate,
    n_before         = n_before,
    n_after          = n_after,
    conflict_summary = conflict_summary
  )
}


#' Step 3: Split into temperature / precipitation / soil moisture and save raw
#'
#' Precipitation and soil moisture are saved immediately as unprocessed CSVs.
#' Unknown logger names are flagged.
#'
#' @return list: temperature, n_temp, n_precip, n_sm, unclassified_summary
step_split <- function(
    climate,
    temp_loggers    = TEMP_LOGGERS,
    precip_loggers  = PRECIP_LOGGERS,
    sm_loggers      = SM_LOGGERS,
    ignored_loggers = IGNORED_LOGGERS
) {
  temperature   <- climate[logger %in% temp_loggers]
  precipitation <- climate[logger %in% precip_loggers]
  soilmoisture  <- climate[logger %in% sm_loggers]

  ALL_KNOWN    <- c(temp_loggers, precip_loggers, sm_loggers, ignored_loggers)
  unclassified <- climate[!logger %in% ALL_KNOWN]

  unclassified_summary <- NULL
  if (nrow(unclassified) > 0L) {
    message("WARNING: Unclassified loggers (not written to any output):")
    unclassified_summary <- unclassified[
      , .(
        n_rows = .N,
        sites  = paste(sort(unique(site)), collapse = ", "),
        files  = paste(sort(unique(file)), collapse = " | ")
      ),
      by = logger
    ][order(logger)]
    print(unclassified_summary)
    message("  Unicode points for diagnosis:")
    for (nm in sort(unique(unclassified$logger))) {
      message("    '", nm, "'  \u2192  ", paste(utf8ToInt(nm), collapse = " "))
    }
    fwrite(unclassified, "output/unclassified_loggers.csv")
    message("  Full rows saved \u2192 output/unclassified_loggers.csv")
  } else {
    message("All logger names accounted for.")
  }

  fwrite(precipitation, "output/Precipitation_raw.csv")
  fwrite(soilmoisture,  "output/Soilmoisture_raw.csv")
  message(
    "Saved Precipitation_raw.csv \u2014 ",
    format(nrow(precipitation), big.mark = ","), " rows"
  )
  message(
    "Saved Soilmoisture_raw.csv  \u2014 ",
    format(nrow(soilmoisture),  big.mark = ","), " rows"
  )

  list(
    temperature          = temperature,
    n_temp               = nrow(temperature),
    n_precip             = nrow(precipitation),
    n_sm                 = nrow(soilmoisture),
    unclassified_summary = unclassified_summary
  )
}


#' Step 4: Normalise logger names to standard intermediate forms
#'
#' Recodes aliases to canonical intermediates. temp1/temp2 are kept at this
#' stage — soil/veg assignment happens in step_assign().
#'
#' @return list: data, loggers_before, loggers_after, recode_map
step_normalise <- function(temperature, logger_recode = LOGGER_RECODE) {
  loggers_before <- sort(unique(temperature$logger))

  temperature[, flag        := NA_character_]
  temperature[, logger_swap := NA_character_]

  temperature[logger == "", logger := "temp30cm"]
  temperature[, logger := dplyr::recode(logger, !!!logger_recode)]

  loggers_after <- sort(unique(temperature$logger))
  recoded       <- setdiff(loggers_before, loggers_after)

  if (length(recoded) > 0L) {
    message("Recoded ", length(recoded), " name(s): ",
            paste(recoded, collapse = ", "))
  } else {
    message("No logger names needed recoding (already normalised).")
  }

  list(
    data           = temperature,
    loggers_before = loggers_before,
    loggers_after  = loggers_after,
    recode_map     = logger_recode
  )
}


#' Steps 5 + 6: Manual site overrides + automated variance-based assignment
#'
#' Includes a pre-assignment variance snapshot (ground truth) and a
#' post-assignment audit that flags pairs where var(soil) > var(above).
#'
#' @return list: data, var_pre_wide, var_post_wide, suspicious, n_manual
step_assign <- function(temperature) {

  # Pre-assignment variance snapshot (objective reference, unaffected by overrides)
  var_pre <- temperature[
    logger %in% c("temp1", "temp2") & !is.na(value),
    .(var_val = var(value, na.rm = TRUE), n_obs = sum(!is.na(value))),
    by = .(site, file, logger)
  ]

  var_pre_wide <- NULL
  if (nrow(var_pre) > 0L) {
    var_pre_wide <- data.table::dcast(
      var_pre, site + file ~ logger, value.var = "var_val"
    )
    if (all(c("temp1", "temp2") %in% names(var_pre_wide))) {
      var_pre_wide[!is.na(temp1) & !is.na(temp2), var_ratio := temp1 / temp2]
      var_pre_wide[, stat_suggestion := data.table::fcase(
        var_ratio < 0.8,   "temp1_is_soil",
        var_ratio > 1.25,  "temp2_is_soil",
        !is.na(var_ratio), "ambiguous",
        default = NA_character_
      )]
    }
  }

  # Manual site overrides
  temperature <- apply_site_overrides(temperature)
  n_manual <- temperature[
    !is.na(logger_swap) & grepl("ManualSwap", logger_swap), .N
  ]
  message("Rows with manual site override: ", format(n_manual, big.mark = ","))

  # Automated variance-based assignment for remaining temp1/temp2 pairs
  temperature <- assign_soil_veg_loggers(temperature)

  # Post-assignment audit across ALL assigned pairs (manual + automated)
  var_post <- temperature[
    logger %in% c("tempsoil", "tempabove") & !is.na(value),
    .(var_val = var(value, na.rm = TRUE), n_obs = sum(!is.na(value))),
    by = .(site, file, logger)
  ]

  var_post_wide <- NULL
  suspicious    <- NULL

  if (nrow(var_post) > 0L) {
    var_post_wide <- data.table::dcast(
      var_post, site + file ~ logger, value.var = "var_val"
    )

    if (all(c("tempsoil", "tempabove") %in% names(var_post_wide))) {
      var_post_wide[
        !is.na(tempsoil) & !is.na(tempabove),
        var_ratio_assigned := tempsoil / tempabove
      ]

      if (!is.null(var_pre_wide) && "stat_suggestion" %in% names(var_pre_wide)) {
        var_post_wide <- merge(
          var_post_wide,
          var_pre_wide[, .(site, file, var_ratio, stat_suggestion)],
          by = c("site", "file"), all.x = TRUE
        )
      }

      suspicious <- var_post_wide[
        !is.na(var_ratio_assigned) & var_ratio_assigned > 1.0, .(site, file)
      ]

      if (nrow(suspicious) > 0L) {
        message(
          "WARNING: ", nrow(suspicious),
          " file(s) have var(soil) > var(above) after assignment",
          " \u2014 see output/variance_assignment_audit.csv"
        )
        print(
          var_post_wide[suspicious, on = .(site, file),
                        .(site, file, tempsoil, tempabove,
                          var_ratio_assigned, stat_suggestion)]
        )
        temperature[
          suspicious, on = .(site, file),
          flag := data.table::fifelse(
            is.na(flag),
            "SuspiciousAssignment_SoilHigherVariance",
            paste(flag, "SuspiciousAssignment_SoilHigherVariance", sep = "|")
          )
        ]
      } else {
        message(
          "Post-assignment check passed: ",
          "soil variance < above-ground variance in all assigned pairs."
        )
        suspicious <- NULL
      }

      fwrite(var_post_wide, "output/variance_assignment_audit.csv")
      message(
        "Variance audit saved \u2192 output/variance_assignment_audit.csv"
      )
    }
  }

  # Warn about any still-unassigned ambiguous names
  remaining <- temperature[
    logger %in% c("temp1", "temp2", "thermistor 1", "thermistor 2",
                  "temperature", "temperature2")
  ]
  if (nrow(remaining) > 0L) {
    message(
      "WARNING: ", format(nrow(remaining), big.mark = ","),
      " rows still have unassigned logger names:"
    )
    print(remaining[, .N, by = .(site, logger, file)][order(site, logger)])
  } else {
    message("All ambiguous logger names have been assigned.")
  }

  list(
    data          = temperature,
    var_pre_wide  = var_pre_wide,
    var_post_wide = var_post_wide,
    suspicious    = suspicious,
    n_manual      = n_manual
  )
}


#' Step 7: Outlier detection and gap flagging
#'
#' @return list: data, flag_summary, n_flagged, pct_flagged
step_flag <- function(temperature) {
  temperature <- flag_outliers(temperature)
  temperature <- flag_gaps(temperature)

  n_flagged   <- temperature[!is.na(flag), .N]
  pct_flagged <- round(n_flagged / nrow(temperature) * 100, 2)
  flag_summary <- temperature[!is.na(flag), .N, by = flag][order(-N)]

  message(
    "Flagged rows: ", format(n_flagged, big.mark = ","),
    "  (", pct_flagged, "% of all rows)"
  )

  list(
    data         = temperature,
    flag_summary = flag_summary,
    n_flagged    = n_flagged,
    pct_flagged  = pct_flagged
  )
}


#' Step 8: Final rename, fill hourly time grid, and save
#'
#' @return list: data, n_final, date_min, date_max, loggers
step_finalise <- function(
    temperature,
    site_names  = SITE_NAMES,
    site_levels = SITE_LEVELS
) {
  temperature[logger == "temp30cm" & lubridate::year(date) < 2009L,
              value := NA_real_]
  temperature <- temperature[!is.na(value)]

  temperature[
    logger == "temp1" & is.na(logger_swap),
    logger_swap := paste0(
      "DefaultAssign: single-channel file, no pair for variance test; ",
      "temp1\u2192veg_temp"
    )
  ]
  temperature[
    logger == "temp2" & is.na(logger_swap),
    logger_swap := paste0(
      "DefaultAssign: single-channel file, no pair for variance test; ",
      "temp2\u2192soil_temp"
    )
  ]

  n_default <- temperature[
    grepl("DefaultAssign",
          data.table::fifelse(is.na(logger_swap), "", logger_swap)), .N
  ]
  if (n_default > 0L) {
    message(
      "NOTE: ", format(n_default, big.mark = ","),
      " rows given default assignment (single-channel files, see logger_swap)"
    )
  }

  temperature[, logger := data.table::fcase(
    logger == "tempabove", "veg_temp",
    logger == "tempsoil",  "soil_temp",
    logger == "temp200cm", "temp200cm",
    logger == "temp30cm",  "temp30cm",
    logger == "temp1",     "veg_temp",
    logger == "temp2",     "soil_temp",
    default = logger
  )]

  data.table::setkey(temperature, site, date)
  temperature <- temperature[, .SD[seq_len(.N) > 30L], by = site]

  temperature[, site := site_names[site]]
  temperature[, site := factor(site, levels = site_levels)]

  message("Building full hourly time grid...")
  full_grid <- data.table::CJ(
    logger = unique(temperature$logger),
    site   = site_levels,
    date   = seq(
      min(temperature$date, na.rm = TRUE),
      max(temperature$date, na.rm = TRUE),
      by = "hour"
    )
  )
  full_grid[, site := factor(site, levels = site_levels)]

  data.table::setkey(temperature, logger, site, date)
  data.table::setkey(full_grid,   logger, site, date)
  temperature_clean <- temperature[full_grid]

  fwrite(temperature_clean, "output/Temperature_clean.csv")
  message("Saved \u2192 output/Temperature_clean.csv")

  date_min <- min(temperature_clean$date, na.rm = TRUE)
  date_max <- max(temperature_clean$date, na.rm = TRUE)
  loggers  <- sort(unique(temperature_clean$logger[
    !is.na(temperature_clean$logger)
  ]))

  message("Final rows:  ", format(nrow(temperature_clean), big.mark = ","))
  message("Date range:  ", format(date_min, "%Y-%m-%d"),
          " \u2014 ", format(date_max, "%Y-%m-%d"))
  message("Loggers:     ", paste(loggers, collapse = ", "))

  list(
    data     = temperature_clean,
    n_final  = nrow(temperature_clean),
    date_min = date_min,
    date_max = date_max,
    loggers  = loggers
  )
}
