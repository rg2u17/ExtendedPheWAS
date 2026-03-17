#' Add Phecode Information
#'
#' Adds phecode descriptions, group names, group numbers, and colors to a
#' data frame that contains a phecode column.
#'
#' @param d A data frame with a \code{phecode} column
#' @param pheinfo_table The phecode info table. Defaults to extended_pheinfo.
#' @return The input data frame with added columns: description, group,
#'   groupnum, color
#' @export
addPhecodeInfo <- function(d, pheinfo_table = NULL) {
  if (is.null(pheinfo_table)) {
    pheinfo_table <- get_package_data("extended_pheinfo")
  }

  if (!"phecode" %in% names(d)) {
    stop("Data frame must contain a 'phecode' column")
  }

  # Merge
  result <- merge(d, pheinfo_table, by = "phecode", all.x = TRUE, sort = FALSE)

  return(result)
}


#' Restrict Phecodes by Sex
#'
#' Applies sex-specific restrictions to a list of phecodes.
#'
#' @param phecodes Character vector of phecodes
#' @param sex Character. "M" for male, "F" for female.
#' @return Filtered character vector of phecodes appropriate for the given sex
#' @export
restrictPhecodesBySex <- function(phecodes, sex) {
  sex_restrict <- tryCatch(
    get_package_data("extended_sex_restriction"),
    error = function(e) NULL
  )

  if (is.null(sex_restrict)) return(phecodes)

  # Remove phecodes restricted to the other sex
  if (toupper(sex) == "M") {
    female_only <- sex_restrict$phecode[sex_restrict$sex == "F"]
    phecodes <- phecodes[!phecodes %in% female_only]
  } else if (toupper(sex) == "F") {
    male_only <- sex_restrict$phecode[sex_restrict$sex == "M"]
    phecodes <- phecodes[!phecodes %in% male_only]
  }

  return(phecodes)
}


#' Lookup Code Information
#'
#' Quick lookup of phecode mappings and descriptions for any code type.
#'
#' @param codes Character vector of medical codes
#' @param vocabulary_id The vocabulary type (e.g., "ICD10CM", "OPCS4")
#' @param vocabulary.map The mapping table. Defaults to extended_phecode_map.
#' @param pheinfo_table The phecode info table. Defaults to extended_pheinfo.
#' @return A data frame with code, vocabulary_id, phecode, description, group
#' @export
lookupCode <- function(codes, vocabulary_id,
                        vocabulary.map = NULL,
                        pheinfo_table = NULL) {
  if (is.null(vocabulary.map)) vocabulary.map <- get_package_data("extended_phecode_map")
  if (is.null(pheinfo_table)) pheinfo_table <- get_package_data("extended_pheinfo")

  vocab <- normalize_vocab_id(vocabulary_id)

  result <- vocabulary.map[vocabulary.map$vocabulary_id == vocab &
                            vocabulary.map$code %in% codes, ]

  if (nrow(result) > 0) {
    result <- merge(result, pheinfo_table[, c("phecode", "description", "group")],
                     by = "phecode", all.x = TRUE)
  }

  return(result)
}


#' Mapping Coverage Summary
#'
#' Reports the number of codes and phecodes by vocabulary type.
#'
#' @param vocabulary.map The mapping table. Defaults to extended_phecode_map.
#' @return A data frame summarizing mapping coverage
#' @export
mappingSummary <- function(vocabulary.map = NULL) {
  if (is.null(vocabulary.map)) vocabulary.map <- get_package_data("extended_phecode_map")

  summary_df <- aggregate(
    list(
      n_codes = vocabulary.map$code,
      n_phecodes = vocabulary.map$phecode
    ),
    by = list(vocabulary_id = vocabulary.map$vocabulary_id),
    FUN = function(x) length(unique(x))
  )

  # Add totals
  totals <- data.frame(
    vocabulary_id = "TOTAL",
    n_codes = length(unique(vocabulary.map$code)),
    n_phecodes = length(unique(vocabulary.map$phecode)),
    stringsAsFactors = FALSE
  )
  summary_df <- rbind(summary_df, totals)

  return(summary_df)
}


#' Export Mapping Audit Trail
#'
#' Writes the NLP match audit data to CSV files for external review.
#'
#' @param output_dir Directory to write CSV files
#' @param audit_data The audit data frame from build_opcs4_phecode_map()
#' @export
exportMappingAudit <- function(output_dir, audit_data = NULL) {
  if (is.null(audit_data)) {
    audit_data <- get_package_data("nlp_match_audit")
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Full audit
  utils::write.csv(audit_data,
                    file.path(output_dir, "opcs_phecode_mapping_audit.csv"),
                    row.names = FALSE)

  # Needs review subset
  review <- audit_data[audit_data$needs_review, ]
  utils::write.csv(review,
                    file.path(output_dir, "opcs_needs_review.csv"),
                    row.names = FALSE)

  # Tier summary
  tier_summary <- table(audit_data$tier)
  message("Mapping audit exported to: ", output_dir)
  message("Tier 1 (high confidence): ", tier_summary["1"])
  message("Tier 2 (medium confidence): ", tier_summary["2"])
  message("Tier 3 (new procedure phecode): ", tier_summary["3"])
  message("Items needing review: ", nrow(review))

  invisible(NULL)
}
