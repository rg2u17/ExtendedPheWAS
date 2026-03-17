#' Build SNOMED-CT to Phecode Mapping
#'
#' Maps SNOMED-CT concepts to phecodes via the SNOMED -> ICD-10 -> phecode
#' chain. Uses the official SNOMED-CT Extended Map reference set for the
#' SNOMED-to-ICD step, then the existing ICD-10-to-phecode mapping.
#'
#' For SNOMED procedure concepts that don't map via ICD-10, falls back to
#' NLP description matching against phecode descriptions (same approach
#' used for OPCS codes).
#'
#' @param snomed_df Data frame from parse_snomed() with concept_id, description,
#'   semantic_tag, hierarchy
#' @param snomed_icd_map Data frame from parse_snomed(format="mapping") with
#'   concept_id and icd10_code. If NULL, uses NLP matching only.
#' @param phecode_map The ICD-to-phecode mapping table (vocabulary_id, code, phecode)
#' @param pheinfo Phecode info table for NLP matching fallback
#' @return List with:
#'   \describe{
#'     \item{map}{Data frame with vocabulary_id="SNOMEDCT", code=concept_id, phecode}
#'     \item{audit}{Data frame with mapping details}
#'   }
#' @export
build_snomed_phecode_map <- function(snomed_df, snomed_icd_map = NULL,
                                      phecode_map = NULL, pheinfo = NULL) {

  if (is.null(phecode_map)) {
    phecode_map <- PheWAS::phecode_map
  }
  if (is.null(pheinfo)) {
    pheinfo <- PheWAS::pheinfo
  }

  # ICD-10 subset of phecode_map
  icd10_phecodes <- phecode_map[phecode_map$vocabulary_id == "ICD10CM", ]

  results <- list()

  # ---- Strategy 1: SNOMED -> ICD-10 -> Phecode (via official cross-map) ----
  if (!is.null(snomed_icd_map) && nrow(snomed_icd_map) > 0) {
    message("Mapping SNOMED concepts via ICD-10 cross-map (",
            length(unique(snomed_icd_map$concept_id)), " concepts with ICD mappings)...")

    # Join SNOMED-ICD map with ICD-phecode map
    # First, normalize ICD codes (remove dots for matching)
    snomed_icd_map$icd10_clean <- gsub("\\.", "", snomed_icd_map$icd10_code)
    icd10_phecodes$code_clean <- gsub("\\.", "", icd10_phecodes$code)

    via_icd <- merge(snomed_icd_map,
                      icd10_phecodes[, c("code", "code_clean", "phecode")],
                      by.x = "icd10_clean", by.y = "code_clean",
                      all.x = TRUE)

    # Also try with original code format
    via_icd2 <- merge(snomed_icd_map[is.na(via_icd$phecode[
      match(snomed_icd_map$concept_id, via_icd$concept_id)]), ],
      icd10_phecodes[, c("code", "phecode")],
      by.x = "icd10_code", by.y = "code",
      all.x = TRUE)

    # Combine both matching attempts
    mapped_via_icd <- via_icd[!is.na(via_icd$phecode), ]

    if (nrow(mapped_via_icd) > 0) {
      # Keep best mapping per concept (lowest map_group = most specific)
      mapped_via_icd <- mapped_via_icd[order(mapped_via_icd$concept_id,
                                              mapped_via_icd$map_group), ]
      mapped_via_icd <- mapped_via_icd[!duplicated(mapped_via_icd$concept_id), ]

      results$via_icd <- data.frame(
        concept_id = mapped_via_icd$concept_id,
        phecode = mapped_via_icd$phecode,
        icd10_code = mapped_via_icd$icd10_code,
        method = "snomed_icd10_phecode",
        confidence = 1.0,
        stringsAsFactors = FALSE
      )

      message("  Mapped via ICD-10: ", nrow(results$via_icd), " concepts")
    }
  }

  # ---- Strategy 2: NLP description matching (for unmapped concepts) ----
  already_mapped <- if (!is.null(results$via_icd)) results$via_icd$concept_id else character(0)
  unmapped <- snomed_df[!snomed_df$concept_id %in% already_mapped, ]

  # Focus on clinically relevant concepts (disorders, findings, procedures)
  clinical_unmapped <- unmapped[unmapped$hierarchy %in%
    c("disorder", "finding", "procedure", "situation"), ]

  if (nrow(clinical_unmapped) > 0) {
    message("NLP matching ", nrow(clinical_unmapped),
            " remaining clinical concepts...")

    nlp_results <- lapply(seq_len(nrow(clinical_unmapped)), function(i) {
      concept_id <- clinical_unmapped$concept_id[i]
      desc <- clinical_unmapped$description[i]
      hierarchy <- clinical_unmapped$hierarchy[i]

      if (is.na(desc) || nchar(desc) < 3) {
        return(data.frame(concept_id = concept_id, phecode = NA_character_,
                           icd10_code = NA_character_,
                           method = "no_description", confidence = 0,
                           stringsAsFactors = FALSE))
      }

      # For procedures, try OPCS-style matching
      if (hierarchy == "procedure") {
        match <- match_opcs_to_phecode(concept_id, desc, pheinfo, "")
        if (match$tier <= 2 && !is.na(match$phecode)) {
          return(data.frame(concept_id = concept_id, phecode = match$phecode,
                             icd10_code = NA_character_,
                             method = "nlp_procedure", confidence = match$confidence,
                             stringsAsFactors = FALSE))
        }
      }

      # For disorders/findings, match against phecode descriptions
      desc_lower <- tolower(desc)
      scores <- vapply(tolower(pheinfo$description), function(pdesc) {
        score_substring_overlap(desc_lower, pdesc)
      }, numeric(1))

      best_idx <- which.max(scores)
      best_score <- scores[best_idx]

      if (best_score >= 0.4) {
        return(data.frame(
          concept_id = concept_id,
          phecode = as.character(pheinfo$phecode[best_idx]),
          icd10_code = NA_character_,
          method = "nlp_description",
          confidence = best_score,
          stringsAsFactors = FALSE
        ))
      }

      data.frame(concept_id = concept_id, phecode = NA_character_,
                  icd10_code = NA_character_,
                  method = "no_match", confidence = 0,
                  stringsAsFactors = FALSE)
    })

    results$nlp <- do.call(rbind, nlp_results)
    results$nlp <- results$nlp[!is.na(results$nlp$phecode), ]
    message("  Mapped via NLP: ", nrow(results$nlp), " concepts")
  }

  # ---- Combine results ----
  audit <- do.call(rbind, results)
  if (is.null(audit) || nrow(audit) == 0) {
    warning("No SNOMED concepts could be mapped to phecodes")
    return(list(
      map = data.frame(vocabulary_id = character(0), code = character(0),
                        phecode = character(0), stringsAsFactors = FALSE),
      audit = data.frame(concept_id = character(0), phecode = character(0),
                          icd10_code = character(0), method = character(0),
                          confidence = numeric(0), stringsAsFactors = FALSE)
    ))
  }

  # Deduplicate (prefer ICD cross-map over NLP)
  audit <- audit[order(audit$concept_id, -audit$confidence), ]
  audit <- audit[!duplicated(audit$concept_id), ]

  # Build mapping table
  map_df <- data.frame(
    vocabulary_id = "SNOMEDCT",
    code = audit$concept_id,
    phecode = audit$phecode,
    stringsAsFactors = FALSE
  )

  message("Total SNOMED -> phecode mappings: ", nrow(map_df))
  message("  Via ICD-10 cross-map: ", sum(audit$method == "snomed_icd10_phecode"))
  message("  Via NLP matching: ", sum(audit$method %in% c("nlp_description", "nlp_procedure")))

  return(list(map = map_df, audit = audit))
}
