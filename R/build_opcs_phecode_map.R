#' Build OPCS-4 to Phecode Mapping
#'
#' Runs the NLP matcher across all OPCS-4 codes to produce a mapping table.
#' Condition-specific operations are mapped to existing phecodes; generic
#' operations receive new procedure phecodes in the 1000+ range.
#'
#' @param opcs4_df Data frame from parse_opcs4() with code, description, chapter
#' @param pheinfo Data frame of existing phecode info
#' @param chapter_map Data frame from opcs_chapter_bodymap.csv
#' @param selectable_only Logical. If TRUE, only map selectable (leaf) codes.
#' @return List with components:
#'   \describe{
#'     \item{map}{Data frame with vocabulary_id, code, phecode}
#'     \item{audit}{Data frame with match details for review}
#'     \item{new_phecodes}{Data frame of new procedure phecode definitions}
#'   }
#' @export
build_opcs4_phecode_map <- function(opcs4_df, pheinfo, chapter_map,
                                     selectable_only = TRUE) {
  if (selectable_only) {
    codes_to_map <- opcs4_df[opcs4_df$selectable == "Y", ]
  } else {
    codes_to_map <- opcs4_df
  }

  # Skip Y and Z subsidiary chapters
  codes_to_map <- codes_to_map[!codes_to_map$chapter %in% c("Y", "Z"), ]

  # Build chapter group lookup
  chapter_groups <- setNames(chapter_map$phecode_groups, chapter_map$opcs_chapter)

  # Run NLP matching on each code
  results <- lapply(seq_len(nrow(codes_to_map)), function(i) {
    code <- codes_to_map$code[i]
    desc <- codes_to_map$description[i]
    chapter <- codes_to_map$chapter[i]
    groups <- chapter_groups[chapter]

    if (is.na(groups)) groups <- ""

    match_result <- match_opcs_to_phecode(code, desc, pheinfo, groups)

    # If Tier 3, assign new procedure phecode
    if (match_result$tier == 3L) {
      match_result$phecode <- assign_procedure_phecode(code, chapter)
      match_result$phecode_description <- desc
    }

    data.frame(
      opcs_code = code,
      opcs_description = desc,
      chapter = chapter,
      phecode = match_result$phecode,
      phecode_description = match_result$phecode_description,
      confidence = match_result$confidence,
      tier = match_result$tier,
      match_method = match_result$match_method,
      needs_review = match_result$tier >= 2L,
      stringsAsFactors = FALSE
    )
  })

  audit <- do.call(rbind, results)

  # Propagate parent code mappings to child codes that were Tier 3
  # If parent A01 maps to a condition phecode, children A011-A019 should too
  # (unless the child has a more specific Tier 1 match)
  audit <- propagate_parent_mappings(audit, opcs4_df)

  # Create the mapping table
  map_df <- data.frame(
    vocabulary_id = "OPCS4",
    code = audit$opcs_code,
    phecode = audit$phecode,
    stringsAsFactors = FALSE
  )
  map_df <- map_df[!is.na(map_df$phecode), ]

  # Create new procedure phecode definitions
  new_phecodes <- audit[audit$tier == 3L & !is.na(audit$phecode), ]
  new_phecode_defs <- data.frame(
    phecode = unique(new_phecodes$phecode),
    description = NA_character_,
    stringsAsFactors = FALSE
  )
  # Use the OPCS description for the new phecode
  for (i in seq_len(nrow(new_phecode_defs))) {
    matching <- new_phecodes[new_phecodes$phecode == new_phecode_defs$phecode[i], ]
    # Use the shortest matching description (usually the parent code)
    new_phecode_defs$description[i] <- matching$opcs_description[
      which.min(nchar(matching$opcs_description))]
  }

  # Assign groups based on chapter
  new_phecode_defs$group <- vapply(new_phecode_defs$phecode, function(pc) {
    base <- as.integer(gsub("\\..*", "", pc))
    chapter_idx <- which(vapply(c(A=1000L,B=1100L,C=1200L,D=1300L,E=1400L,
      F=1500L,G=1600L,H=1700L,J=1800L,K=1900L,L=2000L,M=2100L,N=2200L,
      P=2300L,Q=2400L,R=2500L,S=2600L,T=2700L,U=2800L,V=2900L,W=3000L,
      X=3100L), function(b) base >= b && base < b + 100L, logical(1)))
    if (length(chapter_idx) == 0) return("Miscellaneous procedures")
    chapter_names <- c(A="Nervous system procedures",B="Cranial nerve procedures",
      C="Peripheral nerve procedures",D="Eye procedures",E="Ear procedures",
      F="Mouth/pharynx procedures",G="Upper digestive procedures",
      H="Lower digestive procedures",J="Abdominal organ procedures",
      K="Heart procedures",L="Arterial/venous procedures",
      M="Urinary procedures",N="Male genital procedures",
      P="Lower female genital procedures",Q="Upper female genital procedures",
      R="Female genital associated procedures",S="Skin procedures",
      T="Soft tissue procedures",U="Diagnostic imaging procedures",
      V="Upper limb bone procedures",W="Lower limb/other bone procedures",
      X="Miscellaneous procedures")
    chapter_names[chapter_idx[length(chapter_idx)]]
  }, character(1))

  return(list(
    map = map_df,
    audit = audit,
    new_phecodes = new_phecode_defs
  ))
}


#' Build OPCS-3 to Phecode Mapping
#'
#' Same approach as OPCS-4 but for the older OPCS-3 code set.
#'
#' @param opcs3_df Data frame from parse_opcs3()
#' @param pheinfo Data frame of existing phecode info
#' @param opcs4_map Existing OPCS-4 mapping (to chain OPCS-3 -> OPCS-4 where possible)
#' @return List with map, audit, new_phecodes components
#' @export
build_opcs3_phecode_map <- function(opcs3_df, pheinfo, opcs4_map = NULL) {
  codes_to_map <- opcs3_df[opcs3_df$selectable == "Y", ]

  # Try description matching to OPCS-4 first
  if (!is.null(opcs4_map)) {
    # For each OPCS-3 code, find OPCS-4 codes with similar descriptions
    results <- lapply(seq_len(nrow(codes_to_map)), function(i) {
      code <- codes_to_map$code[i]
      desc <- codes_to_map$description[i]

      # Check OPCS-4 audit for similar description
      opcs4_audit <- opcs4_map$audit
      scores <- vapply(opcs4_audit$opcs_description, function(d4) {
        score_substring_overlap(tolower(desc), tolower(d4))
      }, numeric(1))

      best_idx <- which.max(scores)
      if (scores[best_idx] >= 0.6) {
        # Use the OPCS-4 mapping
        return(data.frame(
          opcs_code = code,
          opcs_description = desc,
          chapter = NA_character_,
          phecode = opcs4_audit$phecode[best_idx],
          phecode_description = opcs4_audit$phecode_description[best_idx],
          confidence = scores[best_idx] * 0.9,
          tier = opcs4_audit$tier[best_idx],
          match_method = "opcs3_via_opcs4",
          needs_review = TRUE,
          stringsAsFactors = FALSE
        ))
      }

      # Fall back to direct NLP matching against pheinfo
      match_result <- match_opcs_to_phecode(code, desc, pheinfo, "")
      if (match_result$tier == 3L) {
        # Assign new procedure phecode in 3200+ range for OPCS-3
        num <- as.integer(gsub("\\D", "", code))
        if (!is.na(num)) {
          match_result$phecode <- sprintf("%d", 3200L + num %/% 10)
          if (num %% 10 > 0) {
            match_result$phecode <- sprintf("%d.%d", 3200L + num %/% 10, num %% 10)
          }
        }
        match_result$phecode_description <- desc
      }

      data.frame(
        opcs_code = code,
        opcs_description = desc,
        chapter = NA_character_,
        phecode = match_result$phecode,
        phecode_description = match_result$phecode_description,
        confidence = match_result$confidence,
        tier = match_result$tier,
        match_method = match_result$match_method,
        needs_review = match_result$tier >= 2L,
        stringsAsFactors = FALSE
      )
    })

    audit <- do.call(rbind, results)
  } else {
    # No OPCS-4 map available, do direct NLP matching
    results <- lapply(seq_len(nrow(codes_to_map)), function(i) {
      code <- codes_to_map$code[i]
      desc <- codes_to_map$description[i]

      match_result <- match_opcs_to_phecode(code, desc, pheinfo, "")
      if (match_result$tier == 3L) {
        num <- as.integer(gsub("\\D", "", code))
        if (!is.na(num)) {
          match_result$phecode <- sprintf("%d", 3200L + num %/% 10)
        }
        match_result$phecode_description <- desc
      }

      data.frame(
        opcs_code = code, opcs_description = desc, chapter = NA_character_,
        phecode = match_result$phecode,
        phecode_description = match_result$phecode_description,
        confidence = match_result$confidence,
        tier = match_result$tier,
        match_method = match_result$match_method,
        needs_review = match_result$tier >= 2L,
        stringsAsFactors = FALSE
      )
    })
    audit <- do.call(rbind, results)
  }

  map_df <- data.frame(
    vocabulary_id = "OPCS3",
    code = audit$opcs_code,
    phecode = audit$phecode,
    stringsAsFactors = FALSE
  )
  map_df <- map_df[!is.na(map_df$phecode), ]

  new_phecodes <- audit[audit$tier == 3L & !is.na(audit$phecode), ]
  new_phecode_defs <- data.frame(
    phecode = unique(new_phecodes$phecode),
    description = vapply(unique(new_phecodes$phecode), function(pc) {
      matching <- new_phecodes[new_phecodes$phecode == pc, ]
      matching$opcs_description[which.min(nchar(matching$opcs_description))]
    }, character(1)),
    group = "OPCS-3 procedures",
    stringsAsFactors = FALSE
  )

  return(list(map = map_df, audit = audit, new_phecodes = new_phecode_defs))
}


#' Propagate Parent Code Mappings to Children
#'
#' If a parent OPCS code (e.g., M09) maps to a condition phecode via Tier 1,
#' its child codes (M091-M099) should inherit that mapping unless they have
#' their own Tier 1 match.
#'
#' @param audit Audit data frame from matching
#' @param opcs4_df Full OPCS-4 data frame with parent_id relationships
#' @return Updated audit data frame
#' @keywords internal
propagate_parent_mappings <- function(audit, opcs4_df) {
  # Build parent-child lookup from opcs4_df
  parent_lookup <- setNames(opcs4_df$code,
                             as.character(opcs4_df$node_id))

  for (i in seq_len(nrow(audit))) {
    if (audit$tier[i] == 3L) {
      # This code got no condition match - check if parent had one
      code <- audit$opcs_code[i]
      # Find this code's parent_id in opcs4_df
      code_row <- opcs4_df[opcs4_df$code == code, ]
      if (nrow(code_row) == 0) next

      parent_id <- as.character(code_row$parent_id[1])
      parent_code <- parent_lookup[parent_id]

      if (!is.na(parent_code)) {
        parent_audit <- audit[audit$opcs_code == parent_code, ]
        if (nrow(parent_audit) > 0 && parent_audit$tier[1] <= 2L) {
          # Inherit parent's mapping
          audit$phecode[i] <- parent_audit$phecode[1]
          audit$phecode_description[i] <- parent_audit$phecode_description[1]
          audit$confidence[i] <- parent_audit$confidence[1] * 0.9
          audit$tier[i] <- parent_audit$tier[1]
          audit$match_method[i] <- paste0("inherited_from_", parent_code)
          audit$needs_review[i] <- TRUE
        }
      }
    }
  }

  return(audit)
}
