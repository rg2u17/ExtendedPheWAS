#' Map Medical Codes to Phecodes
#'
#' Maps ICD-9, ICD-10, OPCS-3, OPCS-4, Read v2, Read v3, and SNOMED-CT codes
#' to phecodes using the extended phecode mapping tables.
#'
#' @param input A data frame with at least two columns: \code{vocabulary_id}
#'   (one of "ICD9CM", "ICD10CM", "OPCS4", "OPCS3", "READv2", "READv3",
#'   "SNOMEDCT") and \code{code}
#'   (the medical code as character). Additional columns (e.g., patient ID) are
#'   preserved.
#' @param vocabulary.map The vocabulary-to-phecode mapping table. Defaults to
#'   the package's \code{extended_phecode_map}.
#' @param rollup.map The phecode rollup map for hierarchical expansion. Defaults
#'   to \code{extended_rollup_map}. Set to NULL to disable rollup.
#' @param make.distinct Logical. Remove duplicate rows? Default TRUE.
#' @return A data frame with the original columns plus a \code{phecode} column,
#'   with \code{code} and \code{vocabulary_id} removed.
#' @export
#' @examples
#' \dontrun{
#' # Map a mix of ICD-10 and OPCS-4 codes
#' input <- data.frame(
#'   id = c(1, 1, 2, 2),
#'   vocabulary_id = c("ICD10CM", "OPCS4", "ICD10CM", "OPCS4"),
#'   code = c("N20.0", "M091", "K80.0", "J181"),
#'   stringsAsFactors = FALSE
#' )
#' mapped <- mapCodesToPhecodes(input)
#' }
mapCodesToPhecodes <- function(input,
                                vocabulary.map = NULL,
                                rollup.map = NULL,
                                make.distinct = TRUE) {

  # Load package data if defaults not explicitly provided
  if (is.null(vocabulary.map)) {
    vocabulary.map <- get_package_data("extended_phecode_map")
  }
  if (is.null(rollup.map)) {
    rollup.map <- get_package_data("extended_rollup_map")
  }

  # Validate input
  if (!all(c("vocabulary_id", "code") %in% names(input))) {
    stop("Input must have 'vocabulary_id' and 'code' columns")
  }

  if (!is.character(input$code) && !is.factor(input$code)) {
    stop("'code' column must be character or factor")
  }

  # Normalize vocabulary IDs
  input$vocabulary_id <- normalize_vocab_id(input$vocabulary_id)

  # Validate vocabulary IDs
  valid_vocabs <- c("ICD9CM", "ICD10CM", "OPCS4", "OPCS3", "READv2", "READv3",
                     "SNOMEDCT", "phecode")
  unknown <- setdiff(unique(input$vocabulary_id), valid_vocabs)
  if (length(unknown) > 0) {
    warning("Unknown vocabulary_id values: ", paste(unknown, collapse = ", "),
            ". These rows will not be mapped.")
  }

  # Perform mapping
  if (!is.null(vocabulary.map)) {
    # Join input with vocabulary map
    output <- merge(input, vocabulary.map,
                    by = c("vocabulary_id", "code"),
                    all.x = FALSE) # inner join - drops unmatched codes

    # Remove code and vocabulary_id, keep phecode
    output$code <- NULL
    output$vocabulary_id <- NULL

    # Rename phecode to code for rollup step
    names(output)[names(output) == "phecode"] <- "code"
  } else {
    # No vocabulary map - assume input codes are already phecodes
    output <- input[input$vocabulary_id == "phecode", ]
    output$vocabulary_id <- NULL
  }

  # Apply rollup
  if (!is.null(rollup.map) && nrow(output) > 0) {
    output <- merge(output, rollup.map, by = "code", all.x = TRUE)

    # For codes not in rollup map, keep the original code
    no_rollup <- is.na(output$phecode_unrolled)
    output$phecode_unrolled[no_rollup] <- output$code[no_rollup]

    output$code <- NULL
    names(output)[names(output) == "phecode_unrolled"] <- "phecode"
  } else {
    names(output)[names(output) == "code"] <- "phecode"
  }

  # Remove duplicates
  if (make.distinct && nrow(output) > 0) {
    output <- output[!duplicated(output), ]
  }

  return(output)
}


#' Normalize Vocabulary ID Strings
#'
#' Converts various representations of vocabulary IDs to the canonical form.
#'
#' @param x Character vector of vocabulary IDs
#' @return Character vector of normalized IDs
#' @keywords internal
normalize_vocab_id <- function(x) {
  x <- toupper(x)
  x[x %in% c("ICD9", "ICD-9", "ICD9CM", "ICD-9-CM", "ICD9-CM")] <- "ICD9CM"
  x[x %in% c("ICD10", "ICD-10", "ICD10CM", "ICD-10-CM", "ICD10-CM")] <- "ICD10CM"
  x[x %in% c("OPCS4", "OPCS-4", "OPCS 4", "OPCS-IV")] <- "OPCS4"
  x[x %in% c("OPCS3", "OPCS-3", "OPCS 3", "OPCS-III")] <- "OPCS3"
  x[x %in% c("READV2", "READ V2", "READ-V2", "READ2")] <- "READv2"
  x[x %in% c("READ", "READV3", "READ V3", "CTV3", "READ-V3", "READCODE")] <- "READv3"
  x[x %in% c("SNOMED", "SNOMEDCT", "SNOMED-CT", "SNOMED CT", "SCT")] <- "SNOMEDCT"
  x[x %in% c("PHECODE", "PHE")] <- "phecode"
  x
}


#' Get Package Data Object
#'
#' Loads a data object from the package's data directory.
#'
#' @param name Name of the data object
#' @return The data object
#' @keywords internal
get_package_data <- function(name) {
  # Try to get from package environment first
  env <- asNamespace("ExtendedPheWAS")
  if (exists(name, envir = env)) {
    return(get(name, envir = env))
  }

  # Try lazy-loaded data
  data_env <- new.env(parent = emptyenv())
  data(list = name, package = "ExtendedPheWAS", envir = data_env)
  if (exists(name, envir = data_env)) {
    return(get(name, envir = data_env))
  }

  stop("Data object '", name, "' not found in ExtendedPheWAS package")
}
