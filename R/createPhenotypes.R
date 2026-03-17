#' Create Phenotype Table from Medical Codes
#'
#' Takes a data frame of individual-level medical codes and creates a binary
#' phenotype matrix suitable for PheWAS analysis. Each row is an individual,
#' each column is a phecode, and values indicate case (TRUE/1), control (FALSE/0),
#' or excluded (NA).
#'
#' @param id.vocab.code.index A data frame with columns: id (patient identifier),
#'   vocabulary_id, code, and optionally index (date/index for time-based filtering).
#'   Can also be a data frame with just id and code if vocabulary_id and index
#'   are not needed.
#' @param min.code.count Minimum number of distinct code occurrences to be
#'   considered a case. Default 2.
#' @param add.phecode.exclusions Logical. Apply phecode exclusion criteria?
#'   Default TRUE.
#' @param translate Logical. Translate codes to phecodes? Default TRUE.
#'   Set to FALSE if input already contains phecodes.
#' @param id.sex Optional data frame with columns: id, sex. Used for
#'   sex-specific phecode restrictions.
#' @param full.population.ids Vector of all individual IDs in the population.
#'   Used to set controls (0) for individuals not appearing in the code data.
#' @param vocabulary.map Vocabulary-to-phecode mapping table.
#' @param rollup.map Phecode rollup map.
#' @param exclusion.map Phecode exclusion map.
#' @return A data frame in wide format: one row per individual, one column per
#'   phecode. Values are NA (excluded), TRUE/1 (case), or FALSE/0 (control).
#' @export
createPhenotypes <- function(id.vocab.code.index,
                              min.code.count = 2,
                              add.phecode.exclusions = TRUE,
                              translate = TRUE,
                              id.sex = NULL,
                              full.population.ids = NULL,
                              vocabulary.map = NULL,
                              rollup.map = NULL,
                              exclusion.map = NULL) {

  # Load defaults
  if (is.null(vocabulary.map)) vocabulary.map <- get_package_data("extended_phecode_map")
  if (is.null(rollup.map)) rollup.map <- get_package_data("extended_rollup_map")
  if (is.null(exclusion.map)) exclusion.map <- get_package_data("extended_phecode_exclude")

  # Standardize input column names
  input <- id.vocab.code.index
  col_names <- names(input)

  # Detect column structure
  if (length(col_names) >= 3 && all(c("vocabulary_id", "code") %in% col_names)) {
    id_col <- col_names[!col_names %in% c("vocabulary_id", "code", "index")][1]
  } else if (length(col_names) >= 2) {
    id_col <- col_names[1]
    if (!"vocabulary_id" %in% col_names) {
      input$vocabulary_id <- "phecode"
      translate <- FALSE
    }
    if (!"code" %in% col_names) {
      names(input)[2] <- "code"
    }
  } else {
    stop("Input must have at least 2 columns (id and code)")
  }

  # Set population IDs
  if (is.null(full.population.ids)) {
    full.population.ids <- unique(input[[id_col]])
  }

  # Step 1: Translate codes to phecodes
  if (translate) {
    mapped <- mapCodesToPhecodes(
      input[, c(id_col, "vocabulary_id", "code")],
      vocabulary.map = vocabulary.map,
      rollup.map = rollup.map,
      make.distinct = FALSE
    )
  } else {
    mapped <- input[, c(id_col, "code")]
    names(mapped)[2] <- "phecode"
  }

  if (nrow(mapped) == 0) {
    warning("No codes could be mapped to phecodes")
    return(data.frame())
  }

  # Step 2: Count code occurrences per individual per phecode
  count_df <- aggregate(
    list(count = rep(1, nrow(mapped))),
    by = list(id = mapped[[id_col]], phecode = mapped$phecode),
    FUN = sum
  )

  # Step 3: Apply minimum count threshold
  count_df$is_case <- count_df$count >= min.code.count

  # Step 4: Pivot to wide format
  all_phecodes <- sort(unique(count_df$phecode))

  # Create base matrix with all population IDs
  result <- data.frame(id = full.population.ids, stringsAsFactors = FALSE)
  names(result)[1] <- id_col

  for (pc in all_phecodes) {
    pc_cases <- count_df[count_df$phecode == pc & count_df$is_case, "id"]
    result[[pc]] <- result[[id_col]] %in% pc_cases
  }

  # Step 5: Apply exclusions
  if (add.phecode.exclusions && !is.null(exclusion.map) && nrow(exclusion.map) > 0) {
    for (i in seq_len(nrow(exclusion.map))) {
      target_pc <- as.character(exclusion.map$code[i])
      excl_pc <- as.character(exclusion.map$exclusion[i])

      if (target_pc %in% names(result) && excl_pc %in% names(result)) {
        # Set excluded individuals to NA for the target phecode
        has_exclusion <- result[[excl_pc]] == TRUE
        has_exclusion[is.na(has_exclusion)] <- FALSE
        result[[target_pc]][has_exclusion] <- NA
      }
    }
  }

  # Step 6: Apply sex restrictions if provided
  if (!is.null(id.sex)) {
    sex_restrict <- tryCatch(
      get_package_data("extended_sex_restriction"),
      error = function(e) NULL
    )
    if (!is.null(sex_restrict)) {
      result <- apply_sex_restrictions(result, id.sex, sex_restrict, id_col)
    }
  }

  return(result)
}


#' Apply Sex Restrictions to Phenotype Table
#'
#' @param phenotypes Wide-format phenotype data frame
#' @param id.sex Data frame with id and sex columns
#' @param sex_restrict Sex restriction table
#' @param id_col Name of the ID column
#' @return Modified phenotype data frame
#' @keywords internal
apply_sex_restrictions <- function(phenotypes, id.sex, sex_restrict, id_col) {
  # Merge sex info
  phenotypes <- merge(phenotypes, id.sex, by.x = id_col, by.y = names(id.sex)[1],
                       all.x = TRUE)
  sex_col <- names(id.sex)[2]

  for (i in seq_len(nrow(sex_restrict))) {
    pc <- as.character(sex_restrict$phecode[i])
    allowed_sex <- sex_restrict$sex[i]

    if (pc %in% names(phenotypes)) {
      wrong_sex <- !is.na(phenotypes[[sex_col]]) &
                   phenotypes[[sex_col]] != allowed_sex
      phenotypes[[pc]][wrong_sex] <- NA
    }
  }

  phenotypes[[sex_col]] <- NULL
  return(phenotypes)
}
