#' @import PheWAS
#' @importFrom stats aggregate complete.cases setNames
#' @importFrom utils data
#' @importFrom rlang .data
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Package startup message suppressed by default
  # Data is lazy-loaded from data/ directory
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ExtendedPheWAS v", utils::packageVersion(pkgname), "\n",
    "Multi-vocabulary PheWAS: ICD-9, ICD-10, OPCS-3, OPCS-4, ",
    "Read v2, Read v3, SNOMED-CT\n",
    "Use mappingSummary() to see mapping coverage statistics."
  )
}
