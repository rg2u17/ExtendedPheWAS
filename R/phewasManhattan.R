#' PheWAS Manhattan Plot
#'
#' Creates a Manhattan plot of PheWAS results, with phecodes grouped by
#' phenotype category. Handles both existing phecodes (001-999) and new
#' procedure phecodes (1000+).
#'
#' @param d A data frame from \code{phewas()} with columns: phecode, p, and
#'   optionally description, group, groupnum, color.
#' @param title Plot title. Default "PheWAS Manhattan Plot".
#' @param annotate.phenotype.description Logical. Label significant points
#'   with phecode descriptions? Default TRUE.
#' @param significance.threshold Numeric vector of significance thresholds to
#'   draw as horizontal lines. Default uses Bonferroni correction.
#' @param size Point size. Default 2.
#' @param annotate.size Text size for annotations. Default 3.
#' @param y.axis.interval Interval for y-axis breaks. Default 5.
#' @param max.y Maximum y-axis value. Default NULL (auto).
#' @return A ggplot2 object
#' @export
phewasManhattan <- function(d,
                             title = "PheWAS Manhattan Plot",
                             annotate.phenotype.description = TRUE,
                             significance.threshold = NULL,
                             size = 2,
                             annotate.size = 3,
                             y.axis.interval = 5,
                             max.y = NULL) {

  # Ensure phecode info is present
  if (!"group" %in% names(d) || !"color" %in% names(d)) {
    d <- addPhecodeInfo(d)
  }

  # Remove rows with no p-value
  d <- d[!is.na(d$p) & d$p > 0, ]

  if (nrow(d) == 0) {
    warning("No valid p-values to plot")
    return(ggplot2::ggplot())
  }

  # Calculate -log10(p)
  d$logp <- -log10(d$p)

  # Set significance threshold
  if (is.null(significance.threshold)) {
    significance.threshold <- 0.05 / nrow(d)
  }

  # Sort by group and phecode for x-axis ordering
  d$phecode_num <- as.numeric(d$phecode)
  d <- d[order(d$groupnum, d$phecode_num, na.last = TRUE), ]
  d$x_pos <- seq_len(nrow(d))

  # Handle missing groups
  d$group[is.na(d$group)] <- "Unknown"
  d$color[is.na(d$color)] <- "#999999"

  # Create color mapping
  group_colors <- unique(d[, c("group", "color")])
  color_map <- setNames(group_colors$color, group_colors$group)

  # Max y value
  if (is.null(max.y)) {
    max.y <- max(d$logp, na.rm = TRUE) * 1.1
  }
  d$logp[d$logp > max.y] <- max.y

  # Build plot
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x_pos, y = .data$logp,
                                         color = .data$group)) +
    ggplot2::geom_point(size = size, alpha = 0.7) +
    ggplot2::scale_color_manual(values = color_map, name = "Phenotype Group") +
    ggplot2::geom_hline(yintercept = -log10(significance.threshold),
                         linetype = "dashed", color = "red", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = -log10(0.05),
                         linetype = "dotted", color = "blue", linewidth = 0.3) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, ceiling(max.y), by = y.axis.interval),
      limits = c(0, max.y)
    ) +
    ggplot2::labs(
      title = title,
      x = "Phenotype",
      y = expression(-log[10](p))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 7),
      legend.key.size = ggplot2::unit(0.5, "lines")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 4, override.aes = list(size = 3)))

  # Add group separator lines
  group_boundaries <- which(diff(as.numeric(factor(d$group))) != 0) + 0.5
  if (length(group_boundaries) > 0) {
    p <- p + ggplot2::geom_vline(xintercept = group_boundaries,
                                  color = "grey80", linewidth = 0.2)
  }

  # Annotate significant points
  if (annotate.phenotype.description) {
    sig_points <- d[d$p < significance.threshold, ]
    if (nrow(sig_points) > 0) {
      # Limit annotations to top 20 to avoid overcrowding
      sig_points <- sig_points[order(sig_points$p), ]
      if (nrow(sig_points) > 20) {
        sig_points <- sig_points[1:20, ]
      }

      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_text_repel(
          data = sig_points,
          ggplot2::aes(label = .data$description),
          size = annotate.size,
          max.overlaps = 15,
          segment.size = 0.2,
          show.legend = FALSE
        )
      } else {
        p <- p + ggplot2::geom_text(
          data = sig_points,
          ggplot2::aes(label = .data$description),
          size = annotate.size,
          angle = 45, hjust = 0, vjust = 0,
          show.legend = FALSE
        )
      }
    }
  }

  return(p)
}


#' Phenotype Plot
#'
#' Alternative visualization showing effect sizes (OR) for each phecode.
#'
#' @param d Data frame from phewas() results
#' @param title Plot title
#' @param significance.threshold P-value threshold for highlighting
#' @return A ggplot2 object
#' @export
phenotypePlot <- function(d, title = "PheWAS Effect Sizes",
                           significance.threshold = NULL) {

  if (!"group" %in% names(d)) {
    d <- addPhecodeInfo(d)
  }

  d <- d[!is.na(d$OR) & !is.na(d$p), ]
  if (nrow(d) == 0) return(ggplot2::ggplot())

  if (is.null(significance.threshold)) {
    significance.threshold <- 0.05 / nrow(d)
  }

  d$significant <- d$p < significance.threshold
  d$logOR <- log(d$OR)
  d$phecode_num <- as.numeric(d$phecode)
  d <- d[order(d$groupnum, d$phecode_num), ]
  d$x_pos <- seq_len(nrow(d))

  group_colors <- unique(d[, c("group", "color")])
  color_map <- setNames(group_colors$color, group_colors$group)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x_pos, y = .data$logOR,
                                         color = .data$group)) +
    ggplot2::geom_point(ggplot2::aes(size = .data$significant), alpha = 0.7) +
    ggplot2::scale_size_manual(values = c("FALSE" = 1, "TRUE" = 3),
                                guide = "none") +
    ggplot2::scale_color_manual(values = color_map) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::labs(title = title, x = "Phenotype", y = "log(OR)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  return(p)
}
