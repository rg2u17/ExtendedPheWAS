#' Parse ICD-10-CM Codes
#'
#' Parses ICD-10-CM text files into a clean data frame.
#'
#' @param filepath_codes Path to icd10cm_codes_2024.txt (code + description)
#' @param filepath_order Path to icd10cm_order_2024.txt (with hierarchy). Optional.
#' @return A data frame with columns: code, description, and optionally
#'   hierarchy_level (0 = header, 1 = billable)
#' @export
parse_icd10 <- function(filepath_codes, filepath_order = NULL) {

  # Parse the codes file (simpler format: code<tab>description)
  lines <- readLines(filepath_codes, warn = FALSE, encoding = "UTF-8")

  # The format appears to be fixed-width or tab-separated
  # Based on observed data: code is first field, description follows
  # Try tab-separated first
  parsed <- strsplit(lines, "\t")
  col_counts <- vapply(parsed, length, integer(1))

  if (all(col_counts >= 2)) {
    # Tab-separated
    df <- data.frame(
      code = vapply(parsed, `[`, character(1), 1),
      description = vapply(parsed, `[`, character(1), 2),
      stringsAsFactors = FALSE
    )
  } else {
    # Fixed-width: first 7 chars are code, rest is description
    # Based on observed format: "A000    Cholera due to Vibrio..."
    df <- data.frame(
      code = trimws(substr(lines, 1, 7)),
      description = trimws(substr(lines, 8, nchar(lines))),
      stringsAsFactors = FALSE
    )
  }

  df$code <- trimws(df$code)
  df$description <- trimws(df$description)

  # Remove empty rows

  df <- df[nchar(df$code) > 0, ]

  # If order file provided, merge hierarchy info
  if (!is.null(filepath_order)) {
    order_lines <- readLines(filepath_order, warn = FALSE, encoding = "UTF-8")

    # Fixed-width format: 5-digit seq, space(s), code, space(s), hierarchy(0/1),
    # space(s), short description, space-padded, long description
    order_df <- data.frame(
      code = trimws(substr(order_lines, 7, 13)),
      hierarchy_level = as.integer(trimws(substr(order_lines, 15, 15))),
      stringsAsFactors = FALSE
    )
    order_df <- order_df[nchar(order_df$code) > 0, ]

    df <- merge(df, order_df, by = "code", all.x = TRUE)
  }

  return(df)
}
