#' Parse SNOMED-CT Release Files
#'
#' Parses SNOMED-CT data from RF2 (Release Format 2) files or simplified
#' flat files. Supports the International Edition, US Edition, and UK Edition.
#'
#' SNOMED-CT data can be obtained from:
#' \itemize{
#'   \item UK: NHS TRUD (https://isd.digital.nhs.uk/trud/)
#'   \item US: NLM UMLS (https://www.nlm.nih.gov/healthit/snomedct/)
#'   \item International: SNOMED International MLDS
#' }
#'
#' @param filepath Path to either:
#'   \itemize{
#'     \item An RF2 release directory (containing Snapshot/ or Full/ subdirs)
#'     \item A single concepts+descriptions file (tab or comma separated)
#'     \item A SNOMED-to-ICD mapping file
#'   }
#' @param format File format: "auto" (default), "rf2" (RF2 release directory),
#'   "flat" (single file with concept_id and description columns),
#'   "mapping" (SNOMED-to-ICD cross-map file)
#' @param active_only Logical. If TRUE (default), include only active concepts.
#' @param preferred_only Logical. If TRUE (default), use only preferred terms
#'   (not synonyms).
#' @param language_code Language refset code for preferred terms.
#'   Default "900000000000509007" (US English) or "999001261000000100" (UK).
#' @return A data frame with columns: concept_id, description, semantic_tag,
#'   hierarchy, active
#' @export
#' @examples
#' \dontrun{
#' # Parse RF2 release directory
#' snomed <- parse_snomed("/path/to/SnomedCT_Release/")
#'
#' # Parse a flat concepts file
#' snomed <- parse_snomed("snomed_concepts.txt", format = "flat")
#' }
parse_snomed <- function(filepath, format = "auto",
                          active_only = TRUE,
                          preferred_only = TRUE,
                          language_code = NULL) {

  # Detect format
  if (format == "auto") {
    if (dir.exists(filepath)) {
      format <- "rf2"
    } else {
      # Check file content
      first_lines <- readLines(filepath, n = 3, warn = FALSE)
      if (any(grepl("conceptId|id\t", first_lines[1], ignore.case = TRUE))) {
        if (any(grepl("mapTarget|mapGroup", first_lines[1], ignore.case = TRUE))) {
          format <- "mapping"
        } else {
          format <- "flat"
        }
      } else {
        format <- "flat"
      }
    }
  }

  df <- switch(format,
    rf2 = parse_snomed_rf2(filepath, active_only, preferred_only, language_code),
    flat = parse_snomed_flat(filepath),
    mapping = parse_snomed_mapping(filepath, active_only),
    stop("Unknown format: ", format)
  )

  return(df)
}


#' Parse SNOMED-CT RF2 Release Directory
#'
#' RF2 releases contain:
#' - Terminology/sct2_Concept_*.txt (concept IDs and status)
#' - Terminology/sct2_Description_*.txt (human-readable terms)
#' - Refset/Map/der2_iisssccRefset_ExtendedMap*.txt (ICD-10 mapping)
#'
#' @keywords internal
parse_snomed_rf2 <- function(dirpath, active_only, preferred_only,
                               language_code) {
  if (!dir.exists(dirpath)) {
    stop("RF2 directory not found: ", dirpath)
  }

  # Find Snapshot directory (preferred) or Full
  snapshot_dir <- NULL
  for (subdir in c("Snapshot", "Full", "Delta")) {
    candidate <- file.path(dirpath, subdir)
    if (dir.exists(candidate)) {
      snapshot_dir <- candidate
      break
    }
    # Also check one level deeper (some releases have extra nesting)
    deeper <- list.dirs(dirpath, recursive = TRUE)
    snap_match <- deeper[grepl(paste0("/", subdir, "$"), deeper)]
    if (length(snap_match) > 0) {
      snapshot_dir <- snap_match[1]
      break
    }
  }

  if (is.null(snapshot_dir)) {
    # Try to find files directly
    snapshot_dir <- dirpath
  }

  # Find concept file
  concept_files <- list.files(snapshot_dir, pattern = "sct2_Concept",
                               recursive = TRUE, full.names = TRUE)
  if (length(concept_files) == 0) {
    stop("No sct2_Concept file found in: ", snapshot_dir)
  }

  # Find description file
  desc_files <- list.files(snapshot_dir, pattern = "sct2_Description",
                             recursive = TRUE, full.names = TRUE)
  if (length(desc_files) == 0) {
    stop("No sct2_Description file found in: ", snapshot_dir)
  }

  # Parse concepts
  concepts <- utils::read.delim(concept_files[1], header = TRUE,
                                 stringsAsFactors = FALSE, quote = "",
                                 colClasses = "character")

  if (active_only && "active" %in% names(concepts)) {
    concepts <- concepts[concepts$active == "1", ]
  }

  # Parse descriptions
  descriptions <- utils::read.delim(desc_files[1], header = TRUE,
                                     stringsAsFactors = FALSE, quote = "",
                                     colClasses = "character")

  if (active_only && "active" %in% names(descriptions)) {
    descriptions <- descriptions[descriptions$active == "1", ]
  }

  # Filter to preferred terms using typeId
  # 900000000000003001 = Fully Specified Name (FSN)
  # 900000000000013009 = Synonym
  if (preferred_only) {
    # Use Fully Specified Names for the canonical description
    fsn <- descriptions[descriptions$typeId == "900000000000003001", ]
    if (nrow(fsn) > 0) {
      descriptions <- fsn
    }
  }

  # Keep one description per concept (prefer FSN)
  descriptions <- descriptions[!duplicated(descriptions$conceptId), ]

  # Merge concepts with descriptions
  result <- merge(concepts[, c("id", "active", "definitionStatusId")],
                   descriptions[, c("conceptId", "term")],
                   by.x = "id", by.y = "conceptId", all.x = TRUE)

  # Extract semantic tag from FSN (text in parentheses at end)
  result$semantic_tag <- gsub(".*\\(([^)]+)\\)$", "\\1", result$term)
  result$semantic_tag[!grepl("\\(", result$term)] <- NA_character_

  # Clean description (remove semantic tag for cleaner text)
  result$description <- gsub("\\s*\\([^)]+\\)$", "", result$term)

  # Classify hierarchy from semantic tag
  result$hierarchy <- classify_snomed_hierarchy(result$semantic_tag)

  # Rename columns
  names(result)[names(result) == "id"] <- "concept_id"

  result <- result[, c("concept_id", "description", "semantic_tag",
                         "hierarchy", "active")]

  return(result)
}


#' Parse SNOMED-CT from a Flat File
#'
#' For simplified exports or custom SNOMED code lists.
#'
#' @keywords internal
parse_snomed_flat <- function(filepath) {
  # Try tab-separated first, then comma
  df <- tryCatch(
    utils::read.delim(filepath, header = TRUE, stringsAsFactors = FALSE,
                       quote = "", colClasses = "character"),
    error = function(e) {
      utils::read.csv(filepath, header = TRUE, stringsAsFactors = FALSE,
                       colClasses = "character")
    }
  )

  # Standardize column names
  name_map <- c(
    conceptId = "concept_id", concept_id = "concept_id",
    ConceptId = "concept_id", CONCEPT_ID = "concept_id",
    sctid = "concept_id", SCTID = "concept_id",
    id = "concept_id",
    term = "description", Term = "description",
    description = "description", Description = "description",
    FSN = "description", fsn = "description",
    preferredTerm = "description"
  )

  for (old_name in names(name_map)) {
    if (old_name %in% names(df)) {
      names(df)[names(df) == old_name] <- name_map[old_name]
    }
  }

  if (!"concept_id" %in% names(df)) {
    # Use first column as concept_id
    names(df)[1] <- "concept_id"
  }
  if (!"description" %in% names(df)) {
    # Use second column as description
    if (ncol(df) >= 2) names(df)[2] <- "description"
  }

  df$concept_id <- trimws(as.character(df$concept_id))
  df$description <- trimws(as.character(df$description))

  # Add classification
  if (!"semantic_tag" %in% names(df)) df$semantic_tag <- NA_character_
  if (!"hierarchy" %in% names(df)) df$hierarchy <- "unknown"
  if (!"active" %in% names(df)) df$active <- "1"

  df <- df[, intersect(c("concept_id", "description", "semantic_tag",
                           "hierarchy", "active"), names(df))]

  return(df)
}


#' Parse SNOMED-CT to ICD-10 Mapping File
#'
#' Parses the Extended Map reference set file that provides SNOMED-CT to
#' ICD-10 mappings. This is the key file for the SNOMED -> phecode chain.
#'
#' File pattern: der2_iisssccRefset_ExtendedMap*.txt
#'
#' @param filepath Path to the mapping file
#' @param active_only Include only active mappings
#' @return Data frame with: concept_id (SNOMED), icd10_code, map_group,
#'   map_priority, map_rule, map_advice
#' @keywords internal
parse_snomed_mapping <- function(filepath, active_only = TRUE) {
  df <- utils::read.delim(filepath, header = TRUE, stringsAsFactors = FALSE,
                           quote = "", colClasses = "character")

  # RF2 Extended Map columns:
  # id, effectiveTime, active, moduleId, refsetId, referencedComponentId,
  # mapGroup, mapPriority, mapRule, mapAdvice, mapTarget, correlationId,
  # mapCategoryId

  # Standardize column names
  name_map <- c(
    referencedComponentId = "concept_id",
    mapTarget = "icd10_code",
    mapGroup = "map_group",
    mapPriority = "map_priority",
    mapRule = "map_rule",
    mapAdvice = "map_advice"
  )

  for (old in names(name_map)) {
    if (old %in% names(df)) {
      names(df)[names(df) == old] <- name_map[old]
    }
  }

  if (active_only && "active" %in% names(df)) {
    df <- df[df$active == "1", ]
  }

  # Keep relevant columns
  keep_cols <- intersect(c("concept_id", "icd10_code", "map_group",
                            "map_priority", "map_rule", "map_advice"),
                          names(df))
  df <- df[, keep_cols]

  # Remove empty ICD-10 mappings
  if ("icd10_code" %in% names(df)) {
    df <- df[!is.na(df$icd10_code) & nchar(trimws(df$icd10_code)) > 0, ]
    df$icd10_code <- trimws(df$icd10_code)
  }

  # Keep highest priority mapping per concept per group
  if (all(c("map_group", "map_priority") %in% names(df))) {
    df <- df[order(df$concept_id, df$map_group, df$map_priority), ]
    df <- df[!duplicated(paste(df$concept_id, df$map_group)), ]
  }

  return(df)
}


#' Classify SNOMED Semantic Tag into Hierarchy
#'
#' SNOMED FSNs end with semantic tags like (disorder), (procedure),
#' (finding), (body structure), etc.
#'
#' @param tags Character vector of semantic tags
#' @return Character vector of hierarchy classifications
#' @keywords internal
classify_snomed_hierarchy <- function(tags) {
  hierarchy <- rep("other", length(tags))
  tags_lower <- tolower(tags)

  hierarchy[tags_lower %in% c("disorder", "disease")] <- "disorder"
  hierarchy[tags_lower %in% c("finding", "clinical finding")] <- "finding"
  hierarchy[tags_lower %in% c("procedure", "operation", "intervention",
                                "regime/therapy")] <- "procedure"
  hierarchy[tags_lower %in% c("observable entity", "evaluation procedure")] <- "observable"
  hierarchy[tags_lower %in% c("body structure", "morphologic abnormality",
                                "cell structure")] <- "body_structure"
  hierarchy[tags_lower %in% c("substance", "product",
                                "medicinal product")] <- "substance"
  hierarchy[tags_lower %in% c("organism")] <- "organism"
  hierarchy[tags_lower %in% c("qualifier value", "attribute",
                                "namespace concept")] <- "qualifier"
  hierarchy[tags_lower %in% c("situation", "context-dependent category")] <- "situation"
  hierarchy[tags_lower %in% c("event", "environment",
                                "geographic location")] <- "event"
  hierarchy[tags_lower %in% c("specimen", "physical object",
                                "record artifact")] <- "artifact"
  hierarchy[tags_lower %in% c("social context", "person",
                                "occupation")] <- "social"
  hierarchy[is.na(tags)] <- "unknown"

  return(hierarchy)
}
