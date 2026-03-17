#' Build All Extended Mapping Tables
#'
#' Combines ICD-9, ICD-10, OPCS-3, OPCS-4, Read v2, Read v3, and SNOMED-CT
#' mappings into unified extended mapping objects for the package.
#'
#' @param base_phecode_map Base ICD phecode_map (from PheWAS or built-in)
#' @param base_pheinfo Base pheinfo table
#' @param base_rollup Base rollup map
#' @param base_exclude Base exclusion map
#' @param opcs4_result Result from build_opcs4_phecode_map()
#' @param opcs3_result Result from build_opcs3_phecode_map()
#' @param read_v3_result Result from build_read_phecode_map() for Read v3
#' @param read_v2_result Result from build_read_phecode_map() for Read v2
#' @param snomed_result Result from build_snomed_phecode_map()
#' @param read_result Deprecated. Use read_v3_result instead.
#' @return List with extended_phecode_map, extended_pheinfo, extended_rollup_map,
#'   extended_phecode_exclude
#' @export
build_extended_maps <- function(base_phecode_map, base_pheinfo,
                                 base_rollup, base_exclude,
                                 opcs4_result, opcs3_result = NULL,
                                 read_v3_result = NULL,
                                 read_v2_result = NULL,
                                 snomed_result = NULL,
                                 read_result = NULL) {

  # Support old argument name for backwards compatibility
  if (!is.null(read_result) && is.null(read_v3_result)) {
    read_v3_result <- read_result
  }

  # --- Extended phecode map ---
  maps_to_combine <- list(base_phecode_map)

  if (!is.null(opcs4_result)) {
    maps_to_combine <- c(maps_to_combine, list(opcs4_result$map))
  }
  if (!is.null(opcs3_result)) {
    maps_to_combine <- c(maps_to_combine, list(opcs3_result$map))
  }
  if (!is.null(read_v3_result)) {
    maps_to_combine <- c(maps_to_combine, list(read_v3_result$map))
  }
  if (!is.null(read_v2_result)) {
    maps_to_combine <- c(maps_to_combine, list(read_v2_result$map))
  }
  if (!is.null(snomed_result)) {
    maps_to_combine <- c(maps_to_combine, list(snomed_result$map))
  }

  extended_phecode_map <- do.call(rbind, maps_to_combine)
  extended_phecode_map <- extended_phecode_map[!duplicated(
    paste(extended_phecode_map$vocabulary_id, extended_phecode_map$code,
          extended_phecode_map$phecode)), ]

  # --- Extended pheinfo ---
  # Collect new procedure phecode definitions
  new_phecodes <- data.frame(
    phecode = character(0), description = character(0), group = character(0),
    stringsAsFactors = FALSE
  )

  if (!is.null(opcs4_result$new_phecodes)) {
    new_phecodes <- rbind(new_phecodes, opcs4_result$new_phecodes)
  }
  if (!is.null(opcs3_result) && !is.null(opcs3_result$new_phecodes)) {
    new_phecodes <- rbind(new_phecodes, opcs3_result$new_phecodes)
  }

  # Remove duplicates
  new_phecodes <- new_phecodes[!duplicated(new_phecodes$phecode), ]

  # Assign group numbers
  existing_max_groupnum <- max(base_pheinfo$groupnum, na.rm = TRUE)
  procedure_groups <- unique(new_phecodes$group)
  procedure_groups <- procedure_groups[!is.na(procedure_groups)]

  group_nums <- setNames(
    seq(existing_max_groupnum + 1, length.out = length(procedure_groups)),
    procedure_groups
  )

  # Generate distinct colors for new groups
  new_colors <- generate_procedure_colors(length(procedure_groups))
  group_colors <- setNames(new_colors, procedure_groups)

  # Build new pheinfo rows
  new_pheinfo_rows <- data.frame(
    phecode = new_phecodes$phecode,
    description = new_phecodes$description,
    groupnum = group_nums[new_phecodes$group],
    group = new_phecodes$group,
    color = group_colors[new_phecodes$group],
    stringsAsFactors = FALSE
  )

  extended_pheinfo <- rbind(base_pheinfo, new_pheinfo_rows)
  extended_pheinfo <- extended_pheinfo[!duplicated(extended_pheinfo$phecode), ]

  # --- Extended rollup map ---
  # New procedure phecodes follow OPCS hierarchy
  # Parent: 1001 (A01), Child: 1001.1 (A011) rolls up to 1001
  new_rollups <- build_procedure_rollup(new_phecodes$phecode)

  extended_rollup_map <- rbind(base_rollup, new_rollups)
  extended_rollup_map <- extended_rollup_map[!duplicated(
    paste(extended_rollup_map$code, extended_rollup_map$phecode_unrolled)), ]

  # --- Extended exclusion map ---
  # New procedure phecodes: exclude other procedures on the same organ system
  new_exclusions <- build_procedure_exclusions(new_phecodes, group_nums)
  extended_phecode_exclude <- rbind(base_exclude, new_exclusions)

  return(list(
    extended_phecode_map = extended_phecode_map,
    extended_pheinfo = extended_pheinfo,
    extended_rollup_map = extended_rollup_map,
    extended_phecode_exclude = extended_phecode_exclude
  ))
}


#' Generate Distinct Colors for Procedure Groups
#'
#' @param n Number of colors needed
#' @return Character vector of hex color codes
#' @keywords internal
generate_procedure_colors <- function(n) {
  if (n == 0) return(character(0))

  # Use HSV color space with distinct hues
  hues <- seq(0, 1, length.out = n + 1)[1:n]
  # Shift to avoid overlap with existing PheWAS colors
  hues <- (hues + 0.05) %% 1
  colors <- grDevices::hsv(h = hues, s = 0.6, v = 0.8)
  return(colors)
}


#' Build Procedure Phecode Rollup Map
#'
#' Creates hierarchical rollup entries for new procedure phecodes.
#' e.g., 1001.1 rolls up to 1001, which rolls up to itself.
#'
#' @param phecodes Character vector of new procedure phecodes
#' @return Data frame with code, phecode_unrolled columns
#' @keywords internal
build_procedure_rollup <- function(phecodes) {
  rollups <- data.frame(code = character(0), phecode_unrolled = character(0),
                         stringsAsFactors = FALSE)

  for (pc in phecodes) {
    # Each phecode maps to itself
    rollups <- rbind(rollups, data.frame(code = pc, phecode_unrolled = pc,
                                          stringsAsFactors = FALSE))

    # If it has a decimal, also roll up to parent
    if (grepl("\\.", pc)) {
      parent <- gsub("\\..*", "", pc)
      rollups <- rbind(rollups, data.frame(code = pc, phecode_unrolled = parent,
                                            stringsAsFactors = FALSE))
    }
  }

  rollups <- rollups[!duplicated(paste(rollups$code, rollups$phecode_unrolled)), ]
  return(rollups)
}


#' Build Procedure Phecode Exclusions
#'
#' @param new_phecodes Data frame of new procedure phecodes
#' @param group_nums Named vector of group numbers
#' @return Data frame with code, exclusion columns
#' @keywords internal
build_procedure_exclusions <- function(new_phecodes, group_nums) {
  # For procedure phecodes, exclude other procedures in the same group
  # This prevents, e.g., a "kidney transplant" phecode from being compared

  # against "kidney stone surgery" as a control
  exclusions <- data.frame(code = character(0), exclusion = character(0),
                            stringsAsFactors = FALSE)

  for (grp in unique(new_phecodes$group)) {
    grp_codes <- new_phecodes$phecode[new_phecodes$group == grp]
    if (length(grp_codes) <= 1) next

    # Each code excludes all others in the same group
    for (pc in grp_codes) {
      others <- grp_codes[grp_codes != pc]
      exclusions <- rbind(exclusions,
        data.frame(code = pc, exclusion = others, stringsAsFactors = FALSE))
    }
  }

  return(exclusions)
}
