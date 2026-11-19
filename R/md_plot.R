#' Plot Aggregate Betas with Confidence Intervals
#'
#' Horizontal dot-and-whisker plot of AE-MNL beta coefficients with CI.
#'
#' @param agg_df Data frame from [md_aggregate()].
#' @param title Character plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#'
#' prep <- md_prep(md_survey, md_design,
#'                 id_col = "id", version_col = "version",
#'                 best_prefix = "QMAXDIFF_", best_suffix = "b",
#'                 worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'                 n_blocks = 8, n_items = 8, items_per_block = 4,
#'                 item_labels = md_items$item_label)
#' agg <- md_aggregate(prep$long, id_col = "pid",
#'                     item_col = "message", response_col = "y")
#' md_plot_beta(agg, title = "Example Beta Plot")
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar coord_flip labs
#'   theme_minimal ggtitle theme element_text
md_plot_beta <- function(agg_df, title = "MaxDiff Beta Estimates") {

  # Sort by beta
  agg_df <- dplyr::arrange(agg_df, .data[["b"]])
  agg_df$message <- factor(agg_df$message, levels = agg_df$message)

  ggplot2::ggplot(agg_df, ggplot2::aes(x = message, y = b)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lb, ymax = ub),
      width = 0.2
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Message", y = "Beta (ln B/W)") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10))
}


#' Plot Anchored 100-Point Scores
#'
#' Dot plot of anchored 100-point scores with a reference line at 100 (anchor).
#' Items above 100 are more preferred than anchor; below are less preferred.
#'
#' @param anchor_df Data frame from [md_anchor_100()].
#' @param title Character plot title.
#' @param anchor_label Character label of the anchor item (default "ANCHOR").
#' @param sample_label Character label for the sample (default "Full Sample").
#'
#' @return A ggplot2 object.
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#'
#' prep_anc <- md_prep(md_survey, md_design,
#'                     id_col = "id", version_col = "version",
#'                     best_prefix = "QMAXDIFF_", best_suffix = "b",
#'                     worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'                     n_blocks = 8, n_items = 8, items_per_block = 4,
#'                     item_labels = md_items$item_label,
#'                     anchor_prefix = "QMAXDIFF_",
#'                     anchor_suffix = "_anchor")
#' agg_anc <- md_aggregate(prep_anc$long, id_col = "pid",
#'                         item_col = "message", response_col = "y")
#' a100 <- md_anchor_100(agg_anc)
#' md_plot_anchor(a100, sample_label = "Simulated Data")
#'
#' @export
md_plot_anchor <- function(anchor_df, title = NULL,
                           anchor_label = "ANCHOR",
                           sample_label = "Full Sample") {

  if (is.null(title)) {
    title <- paste0("Anchored MaxDiff Score (", sample_label, ")")
  }

  # Sort by thresh100
  anchor_df <- dplyr::arrange(anchor_df, .data[["thresh100"]])
  anchor_df$message <- factor(anchor_df$message, levels = anchor_df$message)

  # Bold the anchor label
  face_vec <- ifelse(
    levels(anchor_df$message) == anchor_label, "bold", "plain"
  )

  ggplot2::ggplot(
    anchor_df,
    ggplot2::aes(x = message, y = thresh100, label = thresh100)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(color = factor(cut)),
      size = 3
    ) +
    ggplot2::geom_text(size = 3, nudge_y = 2) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed") +
    ggplot2::labs(
      x = "Message",
      y = "(NOT Important)     Anchor Score     (Important)"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = face_vec),
      legend.position = "none"
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_color_manual(
      values = c("1" = "firebrick", "2" = "gray50", "3" = "steelblue")
    )
}


#' Plot TURF Cumulative Reach
#'
#' Line plot showing cumulative reach as items are added to the portfolio.
#'
#' @param turf_df Data frame from [md_turf_matrix()].
#' @param id_col Character name of respondent ID column (default "pid").
#' @param weight_col Character name of weight column, or NULL.
#' @param item_cols Character vector of TURF item column names. If NULL,
#'   auto-detected.
#' @param title Character plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#'
#' prep <- md_prep(md_survey, md_design,
#'                 id_col = "id", version_col = "version",
#'                 best_prefix = "QMAXDIFF_", best_suffix = "b",
#'                 worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'                 n_blocks = 8, n_items = 8, items_per_block = 4,
#'                 item_labels = md_items$item_label)
#' eb_wide <- md_ebayes(prep$long, id_col = "pid", block_col = "block",
#'                      item_col = "message", response_col = "y", wide = TRUE)
#' turf <- md_turf_matrix(eb_wide, id_col = "pid")
#' md_plot_turf(turf, id_col = "pid", title = "Example TURF Plot")
#'
#' @export
md_plot_turf <- function(turf_df,
                         id_col = "pid", weight_col = NULL,
                         item_cols = NULL,
                         title = "TURF Cumulative Reach") {

  if (is.null(item_cols)) {
    item_cols <- grep("_turf$", names(turf_df), value = TRUE)
  }

  turf_only <- turf_df[, item_cols, drop = FALSE]

  # Compute reach per item
  if (!is.null(weight_col) && weight_col %in% names(turf_df)) {
    weight <- turf_df[[weight_col]]
    reach <- apply(turf_only, 2, .weighted_pct, weight = weight)
  } else {
    reach <- colSums(turf_only, na.rm = TRUE) / nrow(turf_only)
  }

  reach <- sort(reach, decreasing = TRUE)
  cum_reach <- cumsum(reach) * 100
  # Cap at 100%
  cum_reach <- pmin(cum_reach, 100)

  plot_df <- data.frame(
    n_items = seq_along(cum_reach),
    reach   = cum_reach,
    label   = gsub("_turf$", "", names(cum_reach))
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = n_items, y = reach)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(color = "steelblue", size = 2) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      size = 2.5, hjust = -0.1, vjust = -0.5, angle = 15
    ) +
    ggplot2::labs(
      x = "Number of Items in Portfolio",
      y = "Cumulative Reach (%)"
    ) +
    ggplot2::ylim(0, 105) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(title)
}
