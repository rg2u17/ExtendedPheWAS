#' Parse Read Codes v3
#'
#' Parses the Read v3 RTF file, resolves cross-references, and returns a
#' clean data frame of clinical codes with their descriptions.
#'
#' @param filepath Path to the read_code_v3.rtf file
#' @param resolve_crossrefs Logical. If TRUE (default), resolve "See XXXX"
#'   cross-references to actual descriptions.
#' @param include_occupations Logical. If FALSE (default), exclude occupation
#'   and demographic codes (numeric prefix 0-6).
#' @return A data frame with columns: code, description, code_type
#'   (diagnostic, procedure, occupation, admin, extended)
#' @export
parse_read_v3 <- function(filepath, resolve_crossrefs = TRUE,
                          include_occupations = FALSE) {
  data_lines <- strip_rtf_simple(filepath)

  # Split on tabs - Read v3 has 2 columns: coding, meaning
  parsed <- strsplit(data_lines, "\t")
  valid <- vapply(parsed, length, integer(1)) >= 2L

  # Skip header if present
  first_valid <- parsed[valid][[1]]
  if (tolower(first_valid[1]) == "coding") {
    parsed <- parsed[valid][-1]
  } else {
    parsed <- parsed[valid]
  }

  df <- data.frame(
    code = vapply(parsed, `[`, character(1), 1),
    meaning = vapply(parsed, `[`, character(1), 2),
    stringsAsFactors = FALSE
  )

  df$code <- trimws(df$code)
  df$meaning <- trimws(df$meaning)

  # Remove redacted entries

  df <- df[!df$code %in% c("-1", "-2"), ]
  df <- df[nchar(df$code) > 0, ]

  # Classify codes
  df$is_crossref <- grepl("^\\.", df$code) | grepl("^See\\s+", df$meaning)
  df$is_thesaurus_root <- df$code == "....."

  # Separate cross-references from definitions
  crossrefs <- df[df$is_crossref & !df$is_thesaurus_root, ]
  definitions <- df[!df$is_crossref & !df$is_thesaurus_root, ]

  if (resolve_crossrefs && nrow(crossrefs) > 0) {
    definitions <- resolve_read_crossrefs(definitions, crossrefs)
  }

  # Classify code types
  definitions$code_type <- classify_read_code(definitions$code)

  # Rename meaning to description
  names(definitions)[names(definitions) == "meaning"] <- "description"

  # Filter based on options
  if (!include_occupations) {
    definitions <- definitions[!definitions$code_type %in% c("occupation", "demographic"), ]
  }

  # Clean up helper columns
  definitions$is_crossref <- NULL
  definitions$is_thesaurus_root <- NULL

  return(definitions)
}


#' Resolve Read Code Cross-References
#'
#' Follows "See XXXX" chains to find actual descriptions.
#'
#' @param definitions Data frame of codes with direct definitions
#' @param crossrefs Data frame of codes with "See" references
#' @param max_depth Maximum chain depth to follow (default 5)
#' @return Updated definitions data frame with resolved cross-references appended
#' @keywords internal
resolve_read_crossrefs <- function(definitions, crossrefs, max_depth = 5) {
  # Extract target code from cross-references
  # Pattern 1: dotted prefix codes (.0111 -> meaning is "See 0111.")
  # Pattern 2: inline redirects (A0221 meaning is "See F0073")

  crossrefs$target_code <- NA_character_

  # Handle "See XXXX" in meaning
  see_pattern <- "^See\\s+(.+)$"
  has_see <- grepl(see_pattern, crossrefs$meaning)
  crossrefs$target_code[has_see] <- gsub(see_pattern, "\\1", crossrefs$meaning[has_see])

  # Handle dotted-prefix codes where meaning has the target
  dotted <- grepl("^\\.", crossrefs$code) & !has_see
  if (any(dotted)) {
    # The target is usually the code without the leading dot
    crossrefs$target_code[dotted] <- gsub("^\\.", "", crossrefs$code[dotted])
  }

  # Clean target codes
  crossrefs$target_code <- trimws(crossrefs$target_code)
  crossrefs <- crossrefs[!is.na(crossrefs$target_code), ]

  if (nrow(crossrefs) == 0) return(definitions)

  # Build lookup from definitions
  lookup <- setNames(definitions$meaning, definitions$code)

  # Resolve chains iteratively
  resolved <- crossrefs
  for (i in seq_len(max_depth)) {
    # Find which targets resolve to a known definition
    has_definition <- resolved$target_code %in% names(lookup)

    if (all(has_definition)) break

    # For unresolved, check if target is itself a cross-reference
    unresolved <- !has_definition
    still_crossref <- resolved$target_code[unresolved] %in% crossrefs$code

    if (!any(still_crossref)) break

    # Follow the chain one more step
    next_targets <- crossrefs$target_code[
      match(resolved$target_code[unresolved][still_crossref],
            crossrefs$code)
    ]
    resolved$target_code[unresolved][still_crossref] <- next_targets
  }

  # Apply resolved descriptions
  resolved$meaning <- lookup[resolved$target_code]

  # Remove codes that couldn't be resolved
  resolved <- resolved[!is.na(resolved$meaning), ]

  # Clean the original code (remove leading dot for dotted codes)
  resolved$code <- gsub("^\\.", "", resolved$code)

  # Combine with definitions (only add codes not already present)
  new_codes <- resolved[!resolved$code %in% definitions$code,
                        c("code", "meaning")]
  result <- rbind(definitions[, c("code", "meaning", "is_crossref", "is_thesaurus_root")],
                  data.frame(code = new_codes$code, meaning = new_codes$meaning,
                             is_crossref = FALSE, is_thesaurus_root = FALSE,
                             stringsAsFactors = FALSE))

  return(result)
}


#' Classify Read Code Type
#'
#' @param codes Character vector of Read codes
#' @return Character vector of code types
#' @keywords internal
classify_read_code <- function(codes) {
  code_type <- rep("other", length(codes))

  # Diagnostic codes: A-Z (excluding 7 for procedures)
  code_type[grepl("^[A-Z]", codes)] <- "diagnostic"

  # Procedure codes: start with 7
  code_type[grepl("^7", codes)] <- "procedure"

  # Occupation/demographic codes: start with 0-6
  code_type[grepl("^[0-6]", codes)] <- "occupation"

  # Extended codes: Xa, XE, etc.
  code_type[grepl("^X[a-z]|^XE", codes)] <- "extended"

  # Administrative codes: start with 8, 9
  code_type[grepl("^[89]", codes)] <- "admin"

  return(code_type)
}
