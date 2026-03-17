#' Build Read Code to Phecode Mapping
#'
#' Maps Read codes (v2 or v3) to phecodes via ICD description matching (for
#' diagnostic codes) or via OPCS mapping (for procedure codes).
#'
#' @param read_df Data frame from parse_read_v3() or parse_read_v2() with
#'   code, description, code_type
#' @param pheinfo Data frame of phecode info
#' @param icd10_df Data frame of ICD-10 codes (from parse_icd10)
#' @param icd9_df Data frame of ICD-9 codes (from parse_icd9_diagnostics)
#' @param phecode_map Existing ICD-to-phecode mapping
#' @param opcs4_phecode_map OPCS-4 phecode mapping (for Read procedure codes)
#' @param official_crossmap Optional data frame with Read-to-ICD official mapping
#'   (columns: read_code, icd10_code)
#' @param vocabulary_id The vocabulary ID to use in output ("READv3" or "READv2").
#'   Default "READv3".
#' @return List with map, audit components
#' @export
build_read_phecode_map <- function(read_df, pheinfo,
                                    icd10_df = NULL, icd9_df = NULL,
                                    phecode_map = NULL,
                                    opcs4_phecode_map = NULL,
                                    official_crossmap = NULL,
                                    vocabulary_id = "READv3") {

  # Split by code type
  diagnostic_reads <- read_df[read_df$code_type == "diagnostic", ]
  procedure_reads <- read_df[read_df$code_type == "procedure", ]

  # Include extended codes if present (Read v3 has them, v2 may not)
  if ("extended" %in% read_df$code_type) {
    extended_reads <- read_df[read_df$code_type == "extended", ]
  } else {
    extended_reads <- read_df[0, ]
  }

  # Combine diagnostic and extended codes for mapping
  diag_all <- rbind(
    diagnostic_reads[, c("code", "description")],
    extended_reads[, c("code", "description")]
  )

  # --- Map diagnostic Read codes ---
  if (!is.null(official_crossmap)) {
    diag_map <- map_read_via_official_crossmap(diag_all, official_crossmap, phecode_map)
  } else {
    diag_map <- map_read_via_description(diag_all, pheinfo, icd10_df, icd9_df, phecode_map)
  }

  # --- Map procedure Read codes via OPCS ---
  proc_map <- NULL
  if (nrow(procedure_reads) > 0 && !is.null(opcs4_phecode_map)) {
    proc_map <- map_read_procedures_via_opcs(procedure_reads, opcs4_phecode_map)
  }

  # Combine
  all_maps <- diag_map
  if (!is.null(proc_map)) {
    all_maps <- rbind(all_maps, proc_map)
  }

  # Create mapping table with the specified vocabulary ID
  map_df <- data.frame(
    vocabulary_id = vocabulary_id,
    code = all_maps$read_code,
    phecode = all_maps$phecode,
    stringsAsFactors = FALSE
  )
  map_df <- map_df[!is.na(map_df$phecode), ]
  map_df <- map_df[!duplicated(paste(map_df$code, map_df$phecode)), ]

  return(list(
    map = map_df,
    audit = all_maps
  ))
}


#' Map Read Codes via Description Matching to ICD/Phecode
#'
#' When no official crossmap is available, match Read descriptions to ICD/phecode
#' descriptions using keyword overlap.
#'
#' @param read_diag Data frame with code, description
#' @param pheinfo Phecode info data frame
#' @param icd10_df ICD-10 codes data frame
#' @param icd9_df ICD-9 codes data frame
#' @param phecode_map ICD-to-phecode mapping
#' @return Data frame with read_code, phecode, confidence, method
#' @keywords internal
map_read_via_description <- function(read_diag, pheinfo, icd10_df, icd9_df,
                                      phecode_map) {
  # Read code chapter mapping (mirrors ICD chapter structure)
  read_to_phecode_groups <- list(
    A = "infectious diseases",
    B = "neoplasms",
    C = "endocrine/metabolic",
    D = "hematopoietic",
    E = "mental disorders",
    F = "neurological",
    G = "circulatory system",
    H = "respiratory",
    J = "digestive",
    K = "genitourinary",
    L = "pregnancy complications",
    M = "dermatologic",
    N = "musculoskeletal",
    P = "congenital anomalies",
    Q = "perinatal",
    R = "symptoms",
    S = "injuries & poisonings",
    T = "injuries & poisonings",
    U = "injuries & poisonings",
    Z = "symptoms"
  )

  results <- lapply(seq_len(nrow(read_diag)), function(i) {
    read_code <- read_diag$code[i]
    read_desc <- read_diag$description[i]

    # Skip codes with empty or very short descriptions
    if (is.na(read_desc) || nchar(read_desc) < 3) {
      return(data.frame(
        read_code = read_code,
        read_description = read_desc,
        phecode = NA_character_,
        phecode_description = NA_character_,
        confidence = 0,
        method = "no_description",
        stringsAsFactors = FALSE
      ))
    }

    # Clean Read description - remove parenthetical alternatives
    clean_desc <- gsub("\\s*\\([^)]*\\)", "", read_desc)
    clean_desc <- gsub("\\s*\\&/or\\s*", " ", clean_desc)
    clean_desc <- gsub("\\s+NOS$", "", clean_desc)
    clean_desc <- trimws(clean_desc)
    clean_lower <- tolower(clean_desc)

    # Determine chapter for group filtering
    chapter <- substr(read_code, 1, 1)
    groups <- read_to_phecode_groups[[chapter]]

    # Filter pheinfo to relevant groups
    if (!is.null(groups)) {
      candidates <- pheinfo[tolower(pheinfo$group) %in% tolower(groups), ]
      if (nrow(candidates) < 5) candidates <- pheinfo
    } else {
      candidates <- pheinfo
    }

    # Score against phecode descriptions
    candidates$score <- vapply(tolower(candidates$description), function(pdesc) {
      score_substring_overlap(clean_lower, pdesc)
    }, numeric(1))

    best_idx <- which.max(candidates$score)
    best_score <- candidates$score[best_idx]

    if (best_score >= 0.4) {
      return(data.frame(
        read_code = read_code,
        read_description = read_desc,
        phecode = as.character(candidates$phecode[best_idx]),
        phecode_description = candidates$description[best_idx],
        confidence = best_score,
        method = "description_match",
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      read_code = read_code,
      read_description = read_desc,
      phecode = NA_character_,
      phecode_description = NA_character_,
      confidence = 0,
      method = "no_match",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}


#' Map Read Codes via Official NHS Cross-Map
#'
#' @param read_diag Data frame with code, description
#' @param crossmap Data frame with read_code, icd10_code
#' @param phecode_map ICD-to-phecode mapping
#' @return Data frame with read_code, phecode, confidence, method
#' @keywords internal
map_read_via_official_crossmap <- function(read_diag, crossmap, phecode_map) {
  # Merge Read -> ICD-10
  read_icd <- merge(read_diag, crossmap, by.x = "code", by.y = "read_code",
                     all.x = TRUE)

  # Merge ICD-10 -> phecode
  if (!is.null(phecode_map)) {
    icd10_phecodes <- phecode_map[phecode_map$vocabulary_id == "ICD10CM", ]
    read_phecode <- merge(read_icd, icd10_phecodes,
                           by.x = "icd10_code", by.y = "code", all.x = TRUE)
  } else {
    read_phecode <- read_icd
    read_phecode$phecode <- NA_character_
  }

  data.frame(
    read_code = read_phecode$code,
    read_description = read_phecode$description,
    phecode = read_phecode$phecode,
    phecode_description = NA_character_,
    confidence = ifelse(is.na(read_phecode$phecode), 0, 1.0),
    method = ifelse(is.na(read_phecode$phecode), "crossmap_no_phecode", "official_crossmap"),
    stringsAsFactors = FALSE
  )
}


#' Map Read Procedure Codes via OPCS-4 Description Matching
#'
#' Read codes starting with 7 correspond to surgical procedures and can be
#' matched to OPCS-4 codes by description similarity.
#'
#' @param proc_reads Data frame of Read procedure codes
#' @param opcs4_phecode_map OPCS-4 phecode mapping (list with audit component)
#' @return Data frame with read_code, phecode, confidence, method
#' @keywords internal
map_read_procedures_via_opcs <- function(proc_reads, opcs4_phecode_map) {
  opcs4_audit <- opcs4_phecode_map$audit

  results <- lapply(seq_len(nrow(proc_reads)), function(i) {
    read_code <- proc_reads$code[i]
    read_desc <- tolower(proc_reads$description[i])

    # Score against OPCS-4 descriptions
    scores <- vapply(tolower(opcs4_audit$opcs_description), function(opcs_desc) {
      score_substring_overlap(read_desc, opcs_desc)
    }, numeric(1))

    best_idx <- which.max(scores)
    best_score <- scores[best_idx]

    if (best_score >= 0.4) {
      return(data.frame(
        read_code = read_code,
        read_description = proc_reads$description[i],
        phecode = opcs4_audit$phecode[best_idx],
        phecode_description = opcs4_audit$phecode_description[best_idx],
        confidence = best_score * 0.85, # Discount for indirect matching
        method = "opcs4_description_match",
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      read_code = read_code,
      read_description = proc_reads$description[i],
      phecode = NA_character_,
      phecode_description = NA_character_,
      confidence = 0,
      method = "no_opcs_match",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
