#' Parse OPCS-3 Codes
#'
#' Parses the OPCS-3 RTF file into a clean data frame of procedure codes.
#'
#' @param filepath Path to the OPCS 3.rtf file
#' @return A data frame with columns: code, description, node_id, parent_id, selectable
#' @export
parse_opcs3 <- function(filepath) {
  data_lines <- strip_rtf_simple(filepath)

  # Split on tabs
  parsed <- strsplit(data_lines, "\t")

  # Filter lines with 5 columns
  valid <- vapply(parsed, length, integer(1)) == 5L

  # Check for header
  first_valid <- parsed[valid][[1]]
  if (tolower(first_valid[1]) == "coding") {
    parsed <- parsed[valid][-1]
  } else {
    parsed <- parsed[valid]
  }

  df <- data.frame(
    code = vapply(parsed, `[`, character(1), 1),
    meaning_raw = vapply(parsed, `[`, character(1), 2),
    node_id = as.integer(vapply(parsed, `[`, character(1), 3)),
    parent_id = as.integer(vapply(parsed, `[`, character(1), 4)),
    selectable = vapply(parsed, `[`, character(1), 5),
    stringsAsFactors = FALSE
  )

  # Extract clean description
  # OPCS-3 meaning format: "001 Craniotomy, not elsewhere classified"
  # or "001.1 Craniotomy, not elsewhere classified : burr-hole"
  df$description <- gsub("^[0-9]+\\.?[0-9]*\\s+", "", df$meaning_raw)

  empty_desc <- nchar(trimws(df$description)) == 0
  if (any(empty_desc)) {
    df$description[empty_desc] <- df$meaning_raw[empty_desc]
  }

  df$code <- trimws(df$code)
  df$description <- trimws(df$description)
  df$selectable <- trimws(df$selectable)
  df$meaning_raw <- NULL

  return(df)
}
