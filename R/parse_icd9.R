#' Parse ICD-9 Diagnostic Codes
#'
#' Parses the ICD-9 Diagnostics RTF file into a data frame of diagnostic codes.
#'
#' @param filepath Path to the ICD 9 Diagnostics.rtf file
#' @return A data frame with columns: code, description
#' @export
parse_icd9_diagnostics <- function(filepath) {
  text_lines <- strip_rtf_complex(filepath)

  # ICD-9 diagnostic codes: 001-999, V01-V91, E800-E999
  # Pattern: code at start of line followed by description
  # Codes can be: 3-digit (001), 4-digit with dot (001.0), 5-digit with dot (001.00)
  # V-codes: V01, V01.0, etc.
  # E-codes: E800, E800.0, etc.
  icd9_pattern <- "^\\s*(\\d{3}|V\\d{2}|E\\d{3})(\\.\\d{1,2})?\\s+(.+)$"

  matches <- regmatches(text_lines, regexec(icd9_pattern, text_lines))
  has_match <- vapply(matches, length, integer(1)) > 0

  if (sum(has_match) == 0) {
    warning("No ICD-9 diagnostic codes found. Check file format.")
    return(data.frame(code = character(0), description = character(0),
                      stringsAsFactors = FALSE))
  }

  matched_lines <- matches[has_match]

  df <- data.frame(
    code = vapply(matched_lines, function(m) {
      paste0(m[2], m[3]) # base + decimal
    }, character(1)),
    description = vapply(matched_lines, function(m) m[4], character(1)),
    stringsAsFactors = FALSE
  )

  # Clean up NA decimals (3-digit codes without decimal)
  df$code <- gsub("NA$", "", df$code)
  df$code <- trimws(df$code)
  df$description <- trimws(df$description)

  # Remove duplicates (RTF parsing may produce some)
  df <- df[!duplicated(df$code), ]

  return(df)
}


#' Parse ICD-9 Procedure Codes
#'
#' Parses the ICD-9 Operative Codes RTF file into a data frame.
#'
#' @param filepath Path to the ICD9 Operative Codes.RTF file
#' @return A data frame with columns: code, description
#' @export
parse_icd9_procedures <- function(filepath) {
  text_lines <- strip_rtf_complex(filepath)

  # ICD-9 procedure codes: 00-99, with decimals 00.0, 00.01, etc.
  icd9_proc_pattern <- "^\\s*(\\d{2})(\\.\\d{1,2})?\\s+(.+)$"

  matches <- regmatches(text_lines, regexec(icd9_proc_pattern, text_lines))
  has_match <- vapply(matches, length, integer(1)) > 0

  if (sum(has_match) == 0) {
    warning("No ICD-9 procedure codes found. Check file format.")
    return(data.frame(code = character(0), description = character(0),
                      stringsAsFactors = FALSE))
  }

  matched_lines <- matches[has_match]

  df <- data.frame(
    code = vapply(matched_lines, function(m) {
      paste0(m[2], m[3])
    }, character(1)),
    description = vapply(matched_lines, function(m) m[4], character(1)),
    stringsAsFactors = FALSE
  )

  df$code <- gsub("NA$", "", df$code)
  df$code <- trimws(df$code)
  df$description <- trimws(df$description)
  df <- df[!duplicated(df$code), ]

  return(df)
}
