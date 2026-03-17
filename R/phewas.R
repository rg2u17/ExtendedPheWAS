#' Perform PheWAS Analysis
#'
#' Tests the association between a genetic variant (or other exposure) and
#' all phecodes in a phenotype table.
#'
#' @param phenotypes A wide-format phenotype data frame (from createPhenotypes).
#' @param genotypes A data frame or vector of the exposure variable.
#' @param data A merged data frame containing both phenotypes and genotypes.
#'   If provided, phenotypes and genotypes are column name(s) in data.
#' @param covariates Character vector of covariate column names.
#' @param outcomes Character vector of phecode column names to test. If NULL,
#'   all phecode columns are tested.
#' @param cores Number of parallel cores (default 1).
#' @param significance.threshold P-value threshold for significance (default
#'   uses Bonferroni correction).
#' @param min.cases Minimum number of cases required to test a phecode (default 20).
#' @param additive.genotypes Logical. Use additive genetic model? Default TRUE.
#' @return A data frame with columns: phecode, OR, p, n_cases, n_controls,
#'   description, group, and adjustment info.
#' @export
phewas <- function(phenotypes = NULL,
                    genotypes = NULL,
                    data = NULL,
                    covariates = NULL,
                    outcomes = NULL,
                    cores = 1,
                    significance.threshold = NULL,
                    min.cases = 20,
                    additive.genotypes = TRUE) {

  # If data is provided, extract phenotypes and genotypes
  if (!is.null(data)) {
    if (is.character(genotypes) && length(genotypes) == 1) {
      geno_col <- genotypes
      genotypes <- data[[geno_col]]
    }

    if (is.null(outcomes)) {
      # Find phecode columns (numeric-looking column names)
      all_cols <- names(data)
      outcomes <- all_cols[grepl("^\\d+(\\.\\d+)?$", all_cols)]
    }

    phenotypes <- data[, outcomes, drop = FALSE]

    if (!is.null(covariates)) {
      covar_data <- data[, covariates, drop = FALSE]
    } else {
      covar_data <- NULL
    }
  } else {
    covar_data <- NULL
  }

  if (is.null(phenotypes) || is.null(genotypes)) {
    stop("Must provide phenotypes and genotypes (directly or via 'data')")
  }

  # Set significance threshold
  n_tests <- ncol(phenotypes)
  if (is.null(significance.threshold)) {
    significance.threshold <- 0.05 / n_tests
  }

  # Run association tests for each phecode
  results <- lapply(names(phenotypes), function(pc) {
    outcome <- phenotypes[[pc]]

    # Count cases and controls (exclude NA)
    n_cases <- sum(outcome == TRUE, na.rm = TRUE)
    n_controls <- sum(outcome == FALSE, na.rm = TRUE)

    # Skip if too few cases
    if (n_cases < min.cases) {
      return(data.frame(
        phecode = pc, snp = if(exists("geno_col")) geno_col else "exposure",
        OR = NA_real_, p = NA_real_, beta = NA_real_, SE = NA_real_,
        n_cases = n_cases, n_controls = n_controls,
        n_total = n_cases + n_controls,
        convergence = FALSE,
        stringsAsFactors = FALSE
      ))
    }

    # Build model
    tryCatch({
      if (!is.null(covar_data)) {
        model_data <- cbind(
          data.frame(outcome = as.numeric(outcome), genotype = genotypes),
          covar_data
        )
        model_data <- model_data[complete.cases(model_data), ]
        formula_str <- paste("outcome ~ genotype +",
                              paste(covariates, collapse = " + "))
      } else {
        model_data <- data.frame(
          outcome = as.numeric(outcome),
          genotype = genotypes
        )
        model_data <- model_data[complete.cases(model_data), ]
        formula_str <- "outcome ~ genotype"
      }

      fit <- stats::glm(stats::as.formula(formula_str),
                         data = model_data,
                         family = stats::binomial())

      coefs <- summary(fit)$coefficients
      geno_row <- which(rownames(coefs) == "genotype")

      if (length(geno_row) == 0) {
        return(data.frame(
          phecode = pc, snp = if(exists("geno_col")) geno_col else "exposure",
          OR = NA_real_, p = NA_real_, beta = NA_real_, SE = NA_real_,
          n_cases = n_cases, n_controls = n_controls,
          n_total = nrow(model_data),
          convergence = fit$converged,
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        phecode = pc,
        snp = if(exists("geno_col")) geno_col else "exposure",
        OR = exp(coefs[geno_row, "Estimate"]),
        p = coefs[geno_row, "Pr(>|z|)"],
        beta = coefs[geno_row, "Estimate"],
        SE = coefs[geno_row, "Std. Error"],
        n_cases = n_cases,
        n_controls = n_controls,
        n_total = nrow(model_data),
        convergence = fit$converged,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        phecode = pc, snp = if(exists("geno_col")) geno_col else "exposure",
        OR = NA_real_, p = NA_real_, beta = NA_real_, SE = NA_real_,
        n_cases = n_cases, n_controls = n_controls,
        n_total = n_cases + n_controls,
        convergence = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })

  results_df <- do.call(rbind, results)

  # Add phecode info
  results_df <- addPhecodeInfo(results_df)

  # Add Bonferroni significance flag
  results_df$bonferroni_significant <- !is.na(results_df$p) &
    results_df$p < significance.threshold

  # Sort by p-value
  results_df <- results_df[order(results_df$p), ]

  return(results_df)
}
