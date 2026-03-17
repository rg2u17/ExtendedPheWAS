#' Parse Read Codes Version 2 (5-byte Read Codes)
#'
#' Parses Read v2 code files into a clean data frame. Read v2 uses 5-character
#' alphanumeric codes (e.g., "H33..", "K190.") and was the standard UK primary
#' care coding system until superseded by SNOMED-CT.
#'
#' Supports multiple input formats:
#' \itemize{
#'   \item Tab-separated text files (coding, term columns)
#'   \item RTF files with tab-separated data (same as Read v3 format)
#'   \item CSV files with code and description columns
#'   \item NHS TRUD release files (Unified or Standard format)
#' }
#'
#' @param filepath Path to the Read v2 code file (.txt, .rtf, or .csv)
#' @param format File format: "auto" (default, detects from extension),
#'   "tsv", "csv", "rtf", or "trud". The "trud" format expects NHS TRUD
#'   release structure.
#' @param code_col Column name or index containing the Read v2 code.
#'   Default "code" or detected automatically.
#' @param desc_col Column name or index containing the description/term.
#'   Default "description" or detected automatically.
#' @param include_synonyms Logical. If TRUE, include synonym terms
#'   (multiple descriptions per code). Default FALSE (preferred terms only).
#' @return A data frame with columns: code, description, code_type, chapter
#' @export
#' @examples
#' \dontrun{
#' # Parse a tab-separated Read v2 file
#' read2 <- parse_read_v2("Readv2_codes.txt")
#'
#' # Parse NHS TRUD release
#' read2 <- parse_read_v2("nhs_readv2/", format = "trud")
#' }
parse_read_v2 <- function(filepath, format = "auto",
                           code_col = NULL, desc_col = NULL,
                           include_synonyms = FALSE) {

  # Detect format
  if (format == "auto") {
    if (dir.exists(filepath)) {
      format <- "trud"
    } else {
      ext <- tolower(tools::file_ext(filepath))
      format <- switch(ext,
                        rtf = "rtf",
                        csv = "csv",
                        txt = "tsv",
                        tsv = "tsv",
                        "tsv") # default
    }
  }

  df <- switch(format,
    rtf = parse_read_v2_rtf(filepath),
    csv = parse_read_v2_csv(filepath, code_col, desc_col),
    tsv = parse_read_v2_tsv(filepath, code_col, desc_col),
    trud = parse_read_v2_trud(filepath, include_synonyms),
    stop("Unknown format: ", format)
  )

  # Standardize column names
  if (!"code" %in% names(df)) {
    # Try to detect code column
    for (cn in names(df)) {
      vals <- df[[cn]]
      if (is.character(vals) && all(nchar(vals[1:min(10, length(vals))]) <= 7)) {
        if (any(grepl("^[A-Za-z0-9][A-Za-z0-9.]{1,6}$", vals[1:min(100, length(vals))]))) {
          names(df)[names(df) == cn] <- "code"
          break
        }
      }
    }
  }

  if (!"description" %in% names(df)) {
    desc_candidates <- c("term", "desc", "description", "meaning", "rubric",
                          "term_30", "term_60", "term_198")
    for (dc in desc_candidates) {
      if (dc %in% names(df)) {
        names(df)[names(df) == dc] <- "description"
        break
      }
    }
    # If still not found, use second column
    if (!"description" %in% names(df) && ncol(df) >= 2) {
      names(df)[2] <- "description"
    }
  }

  # Ensure required columns exist
  if (!all(c("code", "description") %in% names(df))) {
    stop("Could not identify code and description columns. ",
         "Please specify code_col and desc_col parameters.")
  }

  # Clean codes
  df$code <- trimws(as.character(df$code))
  df$description <- trimws(as.character(df$description))

  # Remove empty rows
  df <- df[nchar(df$code) > 0 & !is.na(df$code), ]

  # Remove redacted/special entries
  df <- df[!df$code %in% c("-1", "-2", ".....", ""), ]

  # Classify code types based on Read v2 chapter structure
  df$code_type <- classify_read_v2_code(df$code)
  df$chapter <- substr(df$code, 1, 1)

  # Keep only code, description, code_type, chapter
  df <- df[, c("code", "description", "code_type", "chapter")]
  df <- df[!duplicated(df$code), ]

  return(df)
}


#' Parse Read v2 from RTF file
#' @keywords internal
parse_read_v2_rtf <- function(filepath) {
  data_lines <- strip_rtf_simple(filepath)

  parsed <- strsplit(data_lines, "\t")
  valid <- vapply(parsed, length, integer(1)) >= 2L

  # Skip header
  first_valid <- parsed[valid][[1]]
  if (tolower(first_valid[1]) %in% c("coding", "code", "read_code")) {
    parsed <- parsed[valid][-1]
  } else {
    parsed <- parsed[valid]
  }

  data.frame(
    code = vapply(parsed, `[`, character(1), 1),
    description = vapply(parsed, `[`, character(1), 2),
    stringsAsFactors = FALSE
  )
}


#' Parse Read v2 from TSV file
#' @keywords internal
parse_read_v2_tsv <- function(filepath, code_col, desc_col) {
  df <- utils::read.delim(filepath, header = TRUE, stringsAsFactors = FALSE,
                           quote = "", comment.char = "", fill = TRUE,
                           encoding = "latin1")

  # Apply user-specified columns if provided
  if (!is.null(code_col)) {
    if (is.numeric(code_col)) {
      names(df)[code_col] <- "code"
    } else {
      names(df)[names(df) == code_col] <- "code"
    }
  }
  if (!is.null(desc_col)) {
    if (is.numeric(desc_col)) {
      names(df)[desc_col] <- "description"
    } else {
      names(df)[names(df) == desc_col] <- "description"
    }
  }

  return(df)
}


#' Parse Read v2 from CSV file
#' @keywords internal
parse_read_v2_csv <- function(filepath, code_col, desc_col) {
  df <- utils::read.csv(filepath, header = TRUE, stringsAsFactors = FALSE,
                         encoding = "latin1")

  if (!is.null(code_col)) {
    if (is.numeric(code_col)) names(df)[code_col] <- "code"
    else names(df)[names(df) == code_col] <- "code"
  }
  if (!is.null(desc_col)) {
    if (is.numeric(desc_col)) names(df)[desc_col] <- "description"
    else names(df)[names(df) == desc_col] <- "description"
  }

  return(df)
}


#' Parse Read v2 from NHS TRUD Release Directory
#'
#' NHS TRUD releases contain multiple files. The key files are:
#' - Unified/V2/Corev2.all: Complete code list
#' - Or individual chapter files
#'
#' @param dirpath Path to the TRUD release directory
#' @param include_synonyms Include synonym terms
#' @return Data frame with code and description
#' @keywords internal
parse_read_v2_trud <- function(dirpath, include_synonyms = FALSE) {
  if (!dir.exists(dirpath)) {
    stop("TRUD directory not found: ", dirpath)
  }

  # Look for the main code file in common TRUD layouts
  candidates <- c(
    file.path(dirpath, "Unified", "Corev2.all"),
    file.path(dirpath, "V2", "Corev2.all"),
    file.path(dirpath, "Corev2.all"),
    # Alternative file patterns
    list.files(dirpath, pattern = "Corev2", full.names = TRUE, recursive = TRUE),
    list.files(dirpath, pattern = "unifiedv2", full.names = TRUE,
               recursive = TRUE, ignore.case = TRUE)
  )

  main_file <- NULL
  for (f in candidates) {
    if (file.exists(f)) {
      main_file <- f
      break
    }
  }

  if (is.null(main_file)) {
    # Try to find any text/csv file with Read codes
    all_files <- list.files(dirpath, pattern = "\\.(txt|csv|tsv)$",
                             full.names = TRUE, recursive = TRUE)
    if (length(all_files) == 0) {
      stop("No Read v2 code files found in TRUD directory: ", dirpath)
    }
    main_file <- all_files[1]
    message("Using file: ", main_file)
  }

  # TRUD Corev2.all format: fixed-width or pipe-delimited
  lines <- readLines(main_file, warn = FALSE, encoding = "latin1")

  # Detect delimiter
  if (any(grepl("\\|", lines[1:min(5, length(lines))]))) {
    # Pipe-delimited
    parsed <- strsplit(lines, "\\|")
  } else if (any(grepl("\t", lines[1:min(5, length(lines))]))) {
    # Tab-delimited
    parsed <- strsplit(lines, "\t")
  } else {
    # Fixed-width: first 5 chars = code, then spaces, then term
    df <- data.frame(
      code = trimws(substr(lines, 1, 7)),
      description = trimws(substr(lines, 8, nchar(lines))),
      stringsAsFactors = FALSE
    )
    return(df[nchar(df$code) > 0, ])
  }

  valid <- vapply(parsed, length, integer(1)) >= 2L
  parsed <- parsed[valid]

  df <- data.frame(
    code = trimws(vapply(parsed, `[`, character(1), 1)),
    description = trimws(vapply(parsed, `[`, character(1), 2)),
    stringsAsFactors = FALSE
  )

  # If there are more columns, check for term type (preferred vs synonym)
  if (any(vapply(parsed, length, integer(1)) >= 3)) {
    df$term_type <- vapply(parsed, function(x) {
      if (length(x) >= 3) trimws(x[3]) else "P"
    }, character(1))

    if (!include_synonyms) {
      # Keep only preferred terms (P) or first occurrence
      df <- df[df$term_type %in% c("P", "p", "") | !duplicated(df$code), ]
      df$term_type <- NULL
    }
  }

  return(df)
}


#' Classify Read v2 Code Type
#'
#' Read v2 codes have a hierarchical 5-character structure where the first
#' character indicates the chapter/domain.
#'
#' @param codes Character vector of Read v2 codes
#' @return Character vector of code types
#' @keywords internal
classify_read_v2_code <- function(codes) {
  first_char <- substr(codes, 1, 1)
  code_type <- rep("other", length(codes))

  # Read v2 chapter mapping
  # 0: Occupation/social
  # 1: History/symptoms
  # 2: Examination/signs
  # 3: Diagnostic procedures
  # 4: Laboratory procedures
  # 5: Radiology/imaging
  # 6: Prevention procedures
  # 7: Operations/procedures
  # 8: Other therapeutic procedures
  # 9: Administration
  # A-U: Disease chapters (mirror ICD)
  # Z: Unspecified conditions

  code_type[first_char %in% LETTERS[1:21]] <- "diagnostic"  # A-U
  code_type[first_char == "Z"] <- "diagnostic"
  code_type[first_char == "7"] <- "procedure"
  code_type[first_char == "8"] <- "procedure"
  code_type[first_char %in% c("3", "4", "5")] <- "investigation"
  code_type[first_char %in% c("1", "2")] <- "symptom"
  code_type[first_char == "6"] <- "prevention"
  code_type[first_char == "9"] <- "admin"
  code_type[first_char == "0"] <- "occupation"

  return(code_type)
}
