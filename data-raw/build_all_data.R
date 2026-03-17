#!/usr/bin/env Rscript
#' Master Build Script for ExtendedPheWAS Package Data
#'
#' This script parses all source code files, builds mappings, and saves
#' the resulting data objects as .rda files in the data/ directory.
#'
#' Run from the package root directory:
#'   source("data-raw/build_all_data.R")

library(devtools)

# ============================================================================
# Configuration
# ============================================================================

# Source data directory
SOURCE_DIR <- file.path(
  dirname(getwd()),  # Parent of package directory
  "."                # Assumes source files are in parent directory
)

# Alternatively, set explicit path:
# SOURCE_DIR <- "/Users/Rob/Desktop/Howles/Recurrent Kidney stones/Recurrence Identification"

# Load all package functions
devtools::load_all(".")

cat("=== ExtendedPheWAS Data Build ===\n")
cat("Source directory:", SOURCE_DIR, "\n\n")

# ============================================================================
# Step 1: Parse Source Files
# ============================================================================

cat("--- Step 1: Parsing source files ---\n")

# ICD-10
cat("  Parsing ICD-10-CM codes...\n")
icd10_codes <- parse_icd10(
  filepath_codes = file.path(SOURCE_DIR, "icd10cm_codes_2024.txt"),
  filepath_order = file.path(SOURCE_DIR, "icd10cm_order_2024.txt")
)
cat("    Parsed", nrow(icd10_codes), "ICD-10 codes\n")

# ICD-9 Diagnostics
cat("  Parsing ICD-9 diagnostic codes...\n")
icd9_diagnostic_codes <- parse_icd9_diagnostics(
  filepath = file.path(SOURCE_DIR, "ICD 9 Diagnostics.rtf")
)
cat("    Parsed", nrow(icd9_diagnostic_codes), "ICD-9 diagnostic codes\n")

# ICD-9 Procedures
cat("  Parsing ICD-9 procedure codes...\n")
icd9_procedure_codes <- parse_icd9_procedures(
  filepath = file.path(SOURCE_DIR, "ICD9 Operative Codes.RTF")
)
cat("    Parsed", nrow(icd9_procedure_codes), "ICD-9 procedure codes\n")

# OPCS-4
cat("  Parsing OPCS-4 codes...\n")
opcs4_codes <- parse_opcs4(
  filepath = file.path(SOURCE_DIR, "OPCS 4.rtf")
)
cat("    Parsed", nrow(opcs4_codes), "OPCS-4 codes\n")

# OPCS-3
cat("  Parsing OPCS-3 codes...\n")
opcs3_codes <- parse_opcs3(
  filepath = file.path(SOURCE_DIR, "OPCS 3.rtf")
)
cat("    Parsed", nrow(opcs3_codes), "OPCS-3 codes\n")

# Read v3
cat("  Parsing Read v3 codes (this may take a moment)...\n")
read_v3_codes <- parse_read_v3(
  filepath = file.path(SOURCE_DIR, "read_code_v3.rtf"),
  resolve_crossrefs = TRUE,
  include_occupations = FALSE
)
cat("    Parsed", nrow(read_v3_codes), "Read v3 codes\n")

# Read v2 (optional - file may not be present)
read_v2_file <- file.path(SOURCE_DIR, "read_code_v2.rtf")
if (!file.exists(read_v2_file)) {
  # Also check for common alternative filenames
  alt_names <- c("Read_v2.rtf", "read_v2.txt", "Read_v2.txt",
                 "Unified_Read_v2.txt", "read_code_v2.txt")
  for (alt in alt_names) {
    candidate <- file.path(SOURCE_DIR, alt)
    if (file.exists(candidate)) {
      read_v2_file <- candidate
      break
    }
  }
  # Also check for NHS TRUD directory
  trud_dir <- file.path(SOURCE_DIR, "nhs_read_v2")
  if (!file.exists(read_v2_file) && dir.exists(trud_dir)) {
    read_v2_file <- trud_dir
  }
}

read_v2_codes <- NULL
if (file.exists(read_v2_file) || dir.exists(read_v2_file)) {
  cat("  Parsing Read v2 codes...\n")
  read_v2_codes <- parse_read_v2(filepath = read_v2_file)
  cat("    Parsed", nrow(read_v2_codes), "Read v2 codes\n")
} else {
  cat("  Read v2 source file not found - skipping\n")
}

# SNOMED-CT (optional - file/directory may not be present)
snomed_dir <- file.path(SOURCE_DIR, "SnomedCT_Release")
snomed_file <- file.path(SOURCE_DIR, "snomed_concepts.txt")
snomed_found <- FALSE
if (dir.exists(snomed_dir)) {
  snomed_found <- TRUE
  snomed_path <- snomed_dir
} else if (file.exists(snomed_file)) {
  snomed_found <- TRUE
  snomed_path <- snomed_file
} else {
  # Check alternative paths
  alt_snomed <- c("SnomedCT_InternationalRF2", "SNOMED_CT",
                  "sct2_Concept_Full", "snomed_rf2")
  for (alt in alt_snomed) {
    candidate <- file.path(SOURCE_DIR, alt)
    if (dir.exists(candidate) || file.exists(candidate)) {
      snomed_found <- TRUE
      snomed_path <- candidate
      break
    }
  }
}

snomed_codes <- NULL
snomed_icd_map <- NULL
if (snomed_found) {
  cat("  Parsing SNOMED-CT concepts...\n")
  snomed_codes <- parse_snomed(filepath = snomed_path)
  cat("    Parsed", nrow(snomed_codes), "SNOMED-CT concepts\n")

  # Try to get SNOMED->ICD mapping
  cat("  Loading SNOMED->ICD-10 cross-map...\n")
  snomed_icd_map <- tryCatch(
    parse_snomed(filepath = snomed_path, format = "mapping"),
    error = function(e) {
      cat("    SNOMED->ICD cross-map not found, will use NLP matching only\n")
      NULL
    }
  )
  if (!is.null(snomed_icd_map)) {
    cat("    Loaded", nrow(snomed_icd_map), "SNOMED->ICD mappings\n")
  }
} else {
  cat("  SNOMED-CT source files not found - skipping\n")
}

cat("\n")

# ============================================================================
# Step 2: Build Base Phecode Tables
# ============================================================================

cat("--- Step 2: Building base phecode tables ---\n")

# Check if PheWAS package is available for base maps
if (requireNamespace("PheWAS", quietly = TRUE)) {
  cat("  Using PheWAS package for base maps\n")
  base_phecode_map <- PheWAS::phecode_map
  base_pheinfo <- PheWAS::pheinfo
  base_rollup <- PheWAS::phecode_rollup_map
  base_exclude <- PheWAS::phecode_exclude
} else {
  cat("  PheWAS package not available - building base maps from ICD data\n")
  # Build minimal base maps from the ICD code lists
  # This is a simplified version - the full PheWAS mapping is much richer
  base_pheinfo <- build_base_pheinfo_from_icd(icd10_codes, icd9_diagnostic_codes)
  base_phecode_map <- build_base_phecode_map(icd10_codes, icd9_diagnostic_codes, base_pheinfo)
  base_rollup <- build_base_rollup(base_pheinfo)
  base_exclude <- data.frame(code = character(0), exclusion = character(0),
                              stringsAsFactors = FALSE)
}

cat("  Base pheinfo:", nrow(base_pheinfo), "phecodes\n")
cat("  Base phecode_map:", nrow(base_phecode_map), "mappings\n")

# ============================================================================
# Step 3: Build OPCS Mappings
# ============================================================================

cat("\n--- Step 3: Building OPCS-4 phecode mapping ---\n")

# Load chapter mapping
chapter_map <- utils::read.csv(
  system.file("extdata", "opcs_chapter_bodymap.csv", package = "ExtendedPheWAS"),
  stringsAsFactors = FALSE
)

# Build OPCS-4 mapping
opcs4_result <- build_opcs4_phecode_map(opcs4_codes, base_pheinfo, chapter_map)
cat("  OPCS-4 mapping complete:\n")
cat("    Tier 1 (condition match):", sum(opcs4_result$audit$tier == 1), "\n")
cat("    Tier 2 (anatomical match):", sum(opcs4_result$audit$tier == 2), "\n")
cat("    Tier 3 (new procedure phecode):", sum(opcs4_result$audit$tier == 3), "\n")
cat("    New procedure phecodes:", nrow(opcs4_result$new_phecodes), "\n")

# Build OPCS-3 mapping
cat("\n  Building OPCS-3 phecode mapping...\n")
opcs3_result <- build_opcs3_phecode_map(opcs3_codes, base_pheinfo, opcs4_result)
cat("  OPCS-3 mapping complete:", nrow(opcs3_result$map), "mappings\n")

# Save audit trail
nlp_match_audit <- opcs4_result$audit

# ============================================================================
# Step 4: Build Read v3 Mapping
# ============================================================================

cat("\n--- Step 4: Building Read code phecode mappings ---\n")

# Read v3
cat("  Building Read v3 phecode mapping...\n")
read_v3_result <- build_read_phecode_map(
  read_df = read_v3_codes,
  pheinfo = base_pheinfo,
  icd10_df = icd10_codes,
  icd9_df = icd9_diagnostic_codes,
  phecode_map = base_phecode_map,
  opcs4_phecode_map = opcs4_result,
  vocabulary_id = "READv3"
)
cat("  Read v3 mapping complete:", nrow(read_v3_result$map), "mappings\n")

# Read v2 (if source data was parsed)
read_v2_result <- NULL
if (!is.null(read_v2_codes)) {
  cat("  Building Read v2 phecode mapping...\n")
  read_v2_result <- build_read_phecode_map(
    read_df = read_v2_codes,
    pheinfo = base_pheinfo,
    icd10_df = icd10_codes,
    icd9_df = icd9_diagnostic_codes,
    phecode_map = base_phecode_map,
    opcs4_phecode_map = opcs4_result,
    vocabulary_id = "READv2"
  )
  cat("  Read v2 mapping complete:", nrow(read_v2_result$map), "mappings\n")
}

# ============================================================================
# Step 4b: Build SNOMED-CT Mapping
# ============================================================================

snomed_phecode_result <- NULL
if (!is.null(snomed_codes)) {
  cat("\n--- Step 4b: Building SNOMED-CT phecode mapping ---\n")
  snomed_phecode_result <- build_snomed_phecode_map(
    snomed_df = snomed_codes,
    snomed_icd_map = snomed_icd_map,
    phecode_map = base_phecode_map,
    pheinfo = base_pheinfo
  )
  cat("  SNOMED-CT mapping complete:", nrow(snomed_phecode_result$map), "mappings\n")
}

# ============================================================================
# Step 5: Build Extended Maps
# ============================================================================

cat("\n--- Step 5: Building extended mapping tables ---\n")

extended <- build_extended_maps(
  base_phecode_map = base_phecode_map,
  base_pheinfo = base_pheinfo,
  base_rollup = base_rollup,
  base_exclude = base_exclude,
  opcs4_result = opcs4_result,
  opcs3_result = opcs3_result,
  read_v3_result = read_v3_result,
  read_v2_result = read_v2_result,
  snomed_result = snomed_phecode_result
)

extended_phecode_map <- extended$extended_phecode_map
extended_pheinfo <- extended$extended_pheinfo
extended_rollup_map <- extended$extended_rollup_map
extended_phecode_exclude <- extended$extended_phecode_exclude

cat("  Extended phecode_map:", nrow(extended_phecode_map), "mappings\n")
cat("  Extended pheinfo:", nrow(extended_pheinfo), "phecodes\n")
cat("  Extended rollup_map:", nrow(extended_rollup_map), "entries\n")
cat("  Extended phecode_exclude:", nrow(extended_phecode_exclude), "exclusions\n")

# Build sex restriction table
extended_sex_restriction <- build_sex_restriction(extended_pheinfo)

# ============================================================================
# Step 6: Save Data Objects
# ============================================================================

cat("\n--- Step 6: Saving .rda files ---\n")

data_dir <- file.path(getwd(), "data")
if (!dir.exists(data_dir)) dir.create(data_dir)

save(extended_phecode_map, file = file.path(data_dir, "extended_phecode_map.rda"),
     compress = "xz")
save(extended_pheinfo, file = file.path(data_dir, "extended_pheinfo.rda"),
     compress = "xz")
save(extended_rollup_map, file = file.path(data_dir, "extended_rollup_map.rda"),
     compress = "xz")
save(extended_phecode_exclude, file = file.path(data_dir, "extended_phecode_exclude.rda"),
     compress = "xz")
save(extended_sex_restriction, file = file.path(data_dir, "extended_sex_restriction.rda"),
     compress = "xz")
save(opcs4_codes, file = file.path(data_dir, "opcs4_codes.rda"), compress = "xz")
save(opcs3_codes, file = file.path(data_dir, "opcs3_codes.rda"), compress = "xz")
save(read_v3_codes, file = file.path(data_dir, "read_v3_codes.rda"), compress = "xz")
if (!is.null(read_v2_codes)) {
  save(read_v2_codes, file = file.path(data_dir, "read_v2_codes.rda"), compress = "xz")
}
if (!is.null(snomed_codes)) {
  save(snomed_codes, file = file.path(data_dir, "snomed_codes.rda"), compress = "xz")
}
save(icd9_diagnostic_codes, file = file.path(data_dir, "icd9_diagnostic_codes.rda"),
     compress = "xz")
save(icd9_procedure_codes, file = file.path(data_dir, "icd9_procedure_codes.rda"),
     compress = "xz")
save(icd10_codes, file = file.path(data_dir, "icd10_codes.rda"), compress = "xz")
save(nlp_match_audit, file = file.path(data_dir, "nlp_match_audit.rda"),
     compress = "xz")

n_files <- 12 + (!is.null(read_v2_codes)) + (!is.null(snomed_codes))
cat("  Saved", n_files, ".rda files to", data_dir, "\n")

# ============================================================================
# Summary
# ============================================================================

cat("\n=== Build Complete ===\n")
cat("Vocabulary coverage:\n")
print(mappingSummary(extended_phecode_map))
cat("\nPhecode groups:", length(unique(extended_pheinfo$group)), "\n")
cat("Total phecodes:", nrow(extended_pheinfo), "\n")
cat("  Existing condition phecodes:", sum(as.numeric(extended_pheinfo$phecode) < 1000, na.rm = TRUE), "\n")
cat("  New procedure phecodes:", sum(as.numeric(extended_pheinfo$phecode) >= 1000, na.rm = TRUE), "\n")


# ============================================================================
# Helper: Build base maps when PheWAS package is not available
# ============================================================================

#' @keywords internal
build_base_pheinfo_from_icd <- function(icd10_df, icd9_df) {
  # Create a simplified pheinfo from ICD chapter structure
  # This maps ICD-10 chapters to phecode groups
  icd10_chapters <- data.frame(
    prefix = c("A", "B", "C", "D0", "D1", "D2", "D3", "D4",
               "D5", "D6", "D7", "D8", "E", "F", "G", "H0", "H1",
               "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9",
               "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R",
               "S", "T"),
    group = c("infectious diseases", "infectious diseases",
              "neoplasms", "neoplasms", "neoplasms", "neoplasms",
              "neoplasms", "neoplasms",
              "hematopoietic", "hematopoietic", "hematopoietic",
              "hematopoietic",
              "endocrine/metabolic", "mental disorders", "neurological",
              "sense organs", "sense organs",
              "sense organs", "sense organs", "sense organs",
              "sense organs", "sense organs", "sense organs",
              "sense organs", "sense organs",
              "circulatory system", "respiratory",
              "digestive", "dermatologic", "musculoskeletal",
              "genitourinary", "pregnancy complications",
              "congenital anomalies", "congenital anomalies",
              "symptoms",
              "injuries & poisonings", "injuries & poisonings"),
    groupnum = c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 5, 6,
                  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 9, 10, 11, 12,
                  13, 14, 15, 15, 16, 17, 17),
    stringsAsFactors = FALSE
  )

  # Generate pheinfo entries from ICD-10 codes
  pheinfo <- data.frame(
    phecode = character(0),
    description = character(0),
    groupnum = integer(0),
    group = character(0),
    color = character(0),
    stringsAsFactors = FALSE
  )

  # Use 3-character ICD-10 codes as base phecodes for simplified mapping
  icd10_3char <- unique(substr(icd10_df$code, 1, 3))

  for (code3 in icd10_3char) {
    # Find chapter
    matched_chapter <- NA
    for (j in seq_len(nrow(icd10_chapters))) {
      if (startsWith(code3, icd10_chapters$prefix[j])) {
        matched_chapter <- j
        break
      }
    }
    if (is.na(matched_chapter)) next

    desc_row <- icd10_df[substr(icd10_df$code, 1, 3) == code3, ]
    desc <- desc_row$description[1]

    pheinfo <- rbind(pheinfo, data.frame(
      phecode = code3,
      description = desc,
      groupnum = icd10_chapters$groupnum[matched_chapter],
      group = icd10_chapters$group[matched_chapter],
      color = grDevices::hsv(
        h = icd10_chapters$groupnum[matched_chapter] / 18,
        s = 0.6, v = 0.8),
      stringsAsFactors = FALSE
    ))
  }

  pheinfo <- pheinfo[!duplicated(pheinfo$phecode), ]
  return(pheinfo)
}

#' @keywords internal
build_base_phecode_map <- function(icd10_df, icd9_df, pheinfo) {
  # Create simplified ICD -> phecode mappings
  # Map full ICD-10 codes to their 3-character parent as phecode
  map_icd10 <- data.frame(
    vocabulary_id = "ICD10CM",
    code = icd10_df$code,
    phecode = substr(icd10_df$code, 1, 3),
    stringsAsFactors = FALSE
  )
  map_icd10 <- map_icd10[map_icd10$phecode %in% pheinfo$phecode, ]

  # Map ICD-9 codes similarly
  map_icd9 <- data.frame(
    vocabulary_id = "ICD9CM",
    code = icd9_df$code,
    phecode = substr(gsub("\\.", "", icd9_df$code), 1, 3),
    stringsAsFactors = FALSE
  )

  rbind(map_icd10, map_icd9)
}

#' @keywords internal
build_base_rollup <- function(pheinfo) {
  # Each phecode maps to itself
  data.frame(
    code = pheinfo$phecode,
    phecode_unrolled = pheinfo$phecode,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
build_sex_restriction <- function(pheinfo) {
  # Create sex restrictions for gender-specific phecodes
  male_keywords <- c("prostat", "testis", "testicl", "penis", "scrot",
                       "spermat", "male genital", "epididym")
  female_keywords <- c("uterus", "uteri", "ovary", "ovari", "vagin",
                         "cervix", "cervic", "fallopian", "endometri",
                         "menstrua", "female genital", "pregnancy",
                         "labour", "labor", "obstetric", "maternal")

  restrictions <- data.frame(phecode = character(0), sex = character(0),
                              stringsAsFactors = FALSE)

  for (i in seq_len(nrow(pheinfo))) {
    desc_lower <- tolower(pheinfo$description[i])
    group_lower <- tolower(pheinfo$group[i])

    is_male <- any(vapply(male_keywords, function(kw) grepl(kw, desc_lower),
                           logical(1)))
    is_female <- any(vapply(female_keywords, function(kw) grepl(kw, desc_lower),
                             logical(1))) ||
                  grepl("pregnancy", group_lower)

    if (is_male && !is_female) {
      restrictions <- rbind(restrictions,
                             data.frame(phecode = pheinfo$phecode[i], sex = "M",
                                        stringsAsFactors = FALSE))
    } else if (is_female && !is_male) {
      restrictions <- rbind(restrictions,
                             data.frame(phecode = pheinfo$phecode[i], sex = "F",
                                        stringsAsFactors = FALSE))
    }
  }

  return(restrictions)
}
