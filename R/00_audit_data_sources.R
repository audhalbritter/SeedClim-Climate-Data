############################################################
# DATA SOURCE AUDIT — ONE-TIME SCRIPT
############################################################
#
# Purpose:
#   Compare the three raw data folders to identify files
#   that are unique to each source.  Run this script once
#   when you receive new raw data or suspect the canonical
#   folder is incomplete.
#
# What it does:
#   1. Lists .txt files in all three source folders and
#      compares basenames.
#   2. Copies files that exist in BioFelles but NOT in the
#      canonical folder into data/supplemental_biofelles/.
#      Duplicate basenames are prefixed with their parent
#      folder name to avoid collisions.
#   3. Reports any files unique to BioEECRG (historically
#      these were all present in the canonical folder too).
#
# After running this script, the pipeline (02_climate_pipeline.R)
# reads from:
#   * data/ITAS_UTL_climate_data_2008-2025/  (canonical)
#   * data/supplemental_biofelles/           (unique additions)
#
# DO NOT run this script automatically as part of the
# regular pipeline — re-run it only when raw data changes.
############################################################

library(data.table)

#### PATHS ####
canonical_dir    <- "data/ITAS_UTL_climate_data_2008-2025"
biofelles_dir    <- "data/ClimatDataFromBioFelles"
bioeecrgn_dir    <- "data/ClimateDataFromBioEECRG"
supplemental_dir <- "data/supplemental_biofelles"

dir.create(supplemental_dir, showWarnings = FALSE, recursive = TRUE)

#### HELPER ####
get_file_info <- function(dir) {
  if (!dir.exists(dir)) {
    warning("Directory not found: ", dir)
    return(data.table(path = character(0), base = character(0)))
  }
  paths <- list.files(dir, pattern = "\\.txt$", recursive = TRUE,
                      full.names = TRUE)
  data.table(path = paths, base = basename(paths))
}

#### 1. INVENTORY ####
canonical_dt  <- get_file_info(canonical_dir)
biofelles_dt  <- get_file_info(biofelles_dir)
bioeecrgn_dt  <- get_file_info(bioeecrgn_dir)

message("=== DATA SOURCE AUDIT ===\n")
message("Canonical source  : ", nrow(canonical_dt), " .txt files")
message("BioFelles         : ", nrow(biofelles_dt), " .txt files")
message("BioEECRG          : ", nrow(bioeecrgn_dt), " .txt files")

#### 2. COMPARE — BIOFELLES ####
missing_biofelles <- biofelles_dt[!base %in% canonical_dt$base]

message("\nBioFelles files NOT in canonical: ", nrow(missing_biofelles))
if (nrow(missing_biofelles) > 0) {
  message(paste0("  ", missing_biofelles$base, collapse = "\n"))
}

#### 3. COMPARE — BIOEECRGN ####
missing_bioeecrgn <- bioeecrgn_dt[!base %in% canonical_dt$base]
message("\nBioEECRG files NOT in canonical : ", nrow(missing_bioeecrgn))
if (nrow(missing_bioeecrgn) > 0) {
  message(paste0("  ", missing_bioeecrgn$base, collapse = "\n"))
} else {
  message("  (all BioEECRG files already in canonical — nothing to copy)")
}

#### 4. COPY UNIQUE BIOFELLES FILES ####
if (nrow(missing_biofelles) > 0) {
  message("\nCopying unique BioFelles files to: ", supplemental_dir)

  # Detect duplicate basenames within the missing set
  dup_bases <- missing_biofelles$base[duplicated(missing_biofelles$base)]

  for (i in seq_len(nrow(missing_biofelles))) {
    src  <- missing_biofelles$path[i]
    base <- missing_biofelles$base[i]

    # For duplicate basenames, prefix with immediate parent folder name
    if (base %in% dup_bases) {
      parent <- basename(dirname(src))
      base   <- paste0(parent, "_", base)
    }

    dst <- file.path(supplemental_dir, base)
    if (!file.exists(dst)) {
      file.copy(src, dst, overwrite = FALSE)
      message("  Copied  : ", base)
    } else {
      message("  Exists  : ", base, " (skipped)")
    }
  }
}

#### 5. SUMMARY ####
final_n <- length(list.files(supplemental_dir, pattern = "\\.txt$"))
message("\n=== DONE ===")
message("Files in data/supplemental_biofelles/ : ", final_n)
message("\nNext step: run R/02_climate_pipeline.R")
message("It reads canonical + supplemental; deduplication handles any overlap.")
