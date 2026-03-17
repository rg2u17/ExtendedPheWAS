#' RTF Parsing Utilities
#'
#' Functions for converting RTF files to plain text for data extraction.

#' Strip simple RTF formatting (OPCS, Read code files)
#'
#' For RTF files where the data is tab-separated after a short RTF header.
#' Removes RTF control sequences and returns clean text lines.
#'
#' @param filepath Path to the RTF file
#' @return Character vector of clean data lines
#' @keywords internal
strip_rtf_simple <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE, encoding = "latin1")

  # Find the end of RTF header - look for the first line with tab-separated data
  # RTF headers contain font/color tables, paragraph formatting, etc.
  header_end <- 0
  for (i in seq_along(lines)) {
    # Skip lines that are RTF control sequences
    if (grepl("^\\{|^\\\\|^\\}", lines[i])) next
    # Skip empty lines
    if (nchar(trimws(lines[i])) == 0) next
    # First line with a tab is likely data
    if (grepl("\t", lines[i])) {
      header_end <- i - 1
      break
    }
  }

  # If no clear header boundary found, try to detect by RTF markers

  if (header_end == 0) {
    # Look for kerning0 or similar end-of-header markers
    kerning_lines <- grep("kerning0$|kerning0\\\\?$", lines)
    if (length(kerning_lines) > 0) {
      header_end <- max(kerning_lines)
    } else {
      # Fallback: skip first 10 lines
      header_end <- min(10, length(lines))
    }
  }

  data_lines <- lines[(header_end + 1):length(lines)]

  # Remove RTF line-ending backslash
  data_lines <- gsub("\\\\$", "", data_lines)

  # Remove any remaining RTF control sequences
  # Match \word or \word0 patterns but not tab characters
  data_lines <- gsub("\\\\[a-zA-Z]+[0-9]*\\s?", "", data_lines)

  # Remove curly braces
  data_lines <- gsub("[{}]", "", data_lines)

  # Remove trailing/leading whitespace
  data_lines <- trimws(data_lines)

  # Filter empty lines
  data_lines <- data_lines[nchar(data_lines) > 0]

  return(data_lines)
}


#' Strip complex RTF formatting (ICD-9 files)
#'
#' For RTF files with full paragraph formatting, font switches, and complex
#' structure. Extracts all text content and removes RTF markup.
#'
#' @param filepath Path to the RTF file
#' @return Character vector of clean text lines
#' @keywords internal
strip_rtf_complex <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE, encoding = "latin1")

  # Collapse all lines into one string, then process
  raw <- paste(lines, collapse = "\n")

  # Remove RTF header groups: {\fonttbl...}, {\colortbl...}, {\info...}
  # These are nested brace groups
  raw <- gsub("\\{\\\\fonttbl[^}]*\\}", "", raw)
  raw <- gsub("\\{\\\\colortbl[^}]*\\}", "", raw)
  raw <- gsub("\\{\\\\\\*\\\\expandedcolortbl[^}]*\\}", "", raw)
  raw <- gsub("\\{\\\\info[^}]*\\{[^}]*\\}[^}]*\\}", "", raw) # nested info

  # Remove paragraph formatting: \pard...\partightenfactor0
  raw <- gsub("\\\\pard[^\n\\\\]*\\\\partightenfactor0", "", raw)

  # Remove font/style switches: \f0\b\fs24, \cf0, etc.
  raw <- gsub("\\\\f[0-9]+", "", raw)
  raw <- gsub("\\\\b[0-9]?\\s?", "", raw)
  raw <- gsub("\\\\fs[0-9]+\\s?", "", raw)
  raw <- gsub("\\\\cf[0-9]+\\s?", "", raw)
  raw <- gsub("\\\\expnd[0-9]+", "", raw)
  raw <- gsub("\\\\expndtw[0-9]+", "", raw)
  raw <- gsub("\\\\kerning[0-9]+", "", raw)
  raw <- gsub("\\\\li[0-9]+", "", raw)
  raw <- gsub("\\\\fi-?[0-9]+", "", raw)
  raw <- gsub("\\\\ri[0-9]+", "", raw)
  raw <- gsub("\\\\sb[0-9]+", "", raw)
  raw <- gsub("\\\\sa[0-9]+", "", raw)
  raw <- gsub("\\\\tx[0-9]+", "", raw)
  raw <- gsub("\\\\qc\\s?", "", raw)

  # Remove other RTF commands
  raw <- gsub("\\\\paperw[0-9]+", "", raw)
  raw <- gsub("\\\\paperh[0-9]+", "", raw)
  raw <- gsub("\\\\margl[0-9]+", "", raw)
  raw <- gsub("\\\\margr[0-9]+", "", raw)
  raw <- gsub("\\\\vieww[0-9]+", "", raw)
  raw <- gsub("\\\\viewh[0-9]+", "", raw)
  raw <- gsub("\\\\viewkind[0-9]+", "", raw)
  raw <- gsub("\\\\deftab[0-9]+", "", raw)
  raw <- gsub("\\\\pardeftab[0-9]+", "", raw)
  raw <- gsub("\\\\cocoatextscaling[0-9]+", "", raw)
  raw <- gsub("\\\\cocoaplatform[0-9]+", "", raw)
  raw <- gsub("\\\\ansi\\s?", "", raw)
  raw <- gsub("\\\\ansicpg[0-9]+", "", raw)
  raw <- gsub("\\\\cocoartf[0-9]+", "", raw)

  # Remove remaining RTF commands (general pattern)
  raw <- gsub("\\\\[a-zA-Z]+-?[0-9]*\\s?", " ", raw)

  # Remove curly braces and remaining backslashes
  raw <- gsub("[{}]", "", raw)
  raw <- gsub("\\\\", "", raw)

  # Split back into lines
  result_lines <- unlist(strsplit(raw, "\n"))

  # Clean up whitespace

  result_lines <- trimws(result_lines)
  result_lines <- gsub("\\s+", " ", result_lines)

  # Remove empty lines
  result_lines <- result_lines[nchar(result_lines) > 0]

  return(result_lines)
}
