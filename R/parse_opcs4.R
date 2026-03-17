#' Parse OPCS-4 Codes
#'
#' Parses the OPCS-4 RTF file into a clean data frame of procedure codes.
#'
#' @param filepath Path to the OPCS 4.rtf file
#' @return A data frame with columns: code, description, node_id, parent_id,
#'   selectable, chapter
#' @export
parse_opcs4 <- function(filepath) {
  data_lines <- strip_rtf_simple(filepath)

  # Split on tabs
  parsed <- strsplit(data_lines, "\t")

  # Filter lines with expected column count (5 columns: coding, meaning, node_id, parent_id, selectable)
  # The header line also has 5 columns
  valid <- vapply(parsed, length, integer(1)) == 5L

  # Check if first valid line is the header
  first_valid <- parsed[valid][[1]]
  if (tolower(first_valid[1]) == "coding") {
    # Skip header
    parsed <- parsed[valid][-1]
  } else {
    parsed <- parsed[valid]
  }

  # Build data frame
  df <- data.frame(
    code = vapply(parsed, `[`, character(1), 1),
    meaning_raw = vapply(parsed, `[`, character(1), 2),
    node_id = as.integer(vapply(parsed, `[`, character(1), 3)),
    parent_id = as.integer(vapply(parsed, `[`, character(1), 4)),
    selectable = vapply(parsed, `[`, character(1), 5),
    stringsAsFactors = FALSE
  )

  # Extract clean description from meaning_raw

  # The meaning column contains redundant code prefix like "A01.1 Hemispherectomy"
  # Strip the leading code pattern to get just the description
  df$description <- gsub("^[A-Z][0-9]+\\.?[0-9]*\\s+", "", df$meaning_raw)
  # Handle parent codes where meaning is just "A01 Major excision of tissue of brain"
  df$description <- gsub("^[A-Z][0-9]+\\s+", "", df$description)

  # For codes where the description is still the full meaning (edge cases)
  empty_desc <- nchar(trimws(df$description)) == 0
  if (any(empty_desc)) {
    df$description[empty_desc] <- df$meaning_raw[empty_desc]
  }

  # Extract chapter (first letter of code)
  df$chapter <- substr(df$code, 1, 1)

  # Clean up
  df$selectable <- trimws(df$selectable)
  df$code <- trimws(df$code)
  df$description <- trimws(df$description)

  # Remove the raw meaning column
  df$meaning_raw <- NULL

  return(df)
}
