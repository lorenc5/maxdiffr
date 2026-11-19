#' Build TURF Binary Matrix
#'
#' Creates a binary matrix where 1 indicates an item was the top-scoring item
#' for a given respondent (from empirical Bayes wide-format output).
#'
#' @param ebayes_wide Wide-format data frame from [md_ebayes()] with
#'   `wide = TRUE`. One row per respondent, one column per item.
#' @param id_col Character name of respondent ID column (default "pid").
#' @param weight_col Character name of weight column, or NULL.
#' @param item_cols Character vector of item column names to include. If NULL,
#'   all columns except `id_col` and `weight_col` are used.
#'
#' @return Data frame with `id_col`, optional weight column, and binary (0/1)
#'   columns for each item (suffixed "_turf").
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
#' head(turf)
#'
#' @export
md_turf_matrix <- function(ebayes_wide,
                           id_col = "pid", weight_col = NULL,
                           item_cols = NULL) {

  exclude <- c(id_col, weight_col)
  if (is.null(item_cols)) {
    item_cols <- setdiff(names(ebayes_wide), exclude)
  }

  # Build TURF binary: 1 if item is respondent's top-scored item
  turf_df <- data.frame(ebayes_wide[[id_col]])
  names(turf_df) <- id_col

  if (!is.null(weight_col) && weight_col %in% names(ebayes_wide)) {
    turf_df$WEIGHT <- ebayes_wide[[weight_col]]
  }

  item_mat <- as.matrix(ebayes_wide[, item_cols, drop = FALSE])

  # For each respondent, find the max score and mark that item as 1
  max_scores <- apply(item_mat, 1, max, na.rm = TRUE)

  for (ic in item_cols) {
    turf_df[[paste0(ic, "_turf")]] <- as.integer(ebayes_wide[[ic]] == max_scores)
  }

  turf_df
}


#' Portfolio Optimization (TURF)
#'
#' Computes reach for portfolios of a given combination size. Generalized to
#' handle any combo size (replaces hardcoded if/else for sizes 2-6 in
#' `comb_portfolio`). Fixes the `+ +` bug in the original.
#'
#' @param turf_df Data frame from [md_turf_matrix()].
#' @param combo Integer combination size (default 1).
#' @param id_col Character name of respondent ID column (default "pid").
#' @param weight_col Character name of weight column, or NULL for unweighted.
#' @param item_cols Character vector of TURF item column names. If NULL, auto-
#'   detected as columns ending in "_turf".
#' @param top_n Integer; return only the top N portfolios (default 10).
#'
#' @return Data frame with columns: `Reach (%)`, `Freq.`, `Portfolio`. Sorted
#'   by reach descending.
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
#'
#' # Single items
#' md_portfolio(turf, combo = 1, id_col = "pid")
#'
#' # Pairs
#' md_portfolio(turf, combo = 2, id_col = "pid", top_n = 5)
#'
#' @export
md_portfolio <- function(turf_df, combo = 1,
                         id_col = "pid", weight_col = NULL,
                         item_cols = NULL, top_n = 10) {

  if (is.null(item_cols)) {
    item_cols <- grep("_turf$", names(turf_df), value = TRUE)
  }

  turf_only <- turf_df[, item_cols, drop = FALSE]

  # Compute reach per item (weighted or unweighted)
  if (!is.null(weight_col) && weight_col %in% names(turf_df)) {
    weight <- turf_df[[weight_col]]
    reach <- apply(turf_only, 2, .weighted_pct, weight = weight)
  } else {
    reach <- colSums(turf_only, na.rm = TRUE) / nrow(turf_only)
  }
  reach <- sort(reach, decreasing = TRUE)
  freq <- round(reach * nrow(turf_df), 0)

  if (combo == 1) {
    out <- data.frame(
      `Reach (%)` = round(reach * 100, 2),
      `Freq.`     = freq,
      Portfolio    = names(reach),
      check.names = FALSE
    )
  } else {
    # Generic combination approach — union-based reach
    combos <- utils::combn(names(reach), combo, simplify = FALSE)

    results <- lapply(combos, function(items) {
      # Union reach: respondent is reached if ANY item in portfolio is 1
      reached <- as.integer(rowSums(turf_only[, items, drop = FALSE]) > 0)
      if (!is.null(weight_col) && weight_col %in% names(turf_df)) {
        total_reach <- .weighted_pct(reached, weight)
      } else {
        total_reach <- sum(reached) / nrow(turf_only)
      }
      total_freq <- sum(reached)
      portfolio  <- paste(items, collapse = " + ")
      data.frame(
        `Reach (%)` = round(total_reach * 100, 2),
        `Freq.`     = total_freq,
        Portfolio    = portfolio,
        check.names = FALSE
      )
    })

    out <- dplyr::bind_rows(results)
  }

  # Sort by reach descending
  out <- dplyr::arrange(out, dplyr::desc(.data[["Reach (%)"]]))
  rownames(out) <- NULL

  # Trim _turf suffix from portfolio labels
  out$Portfolio <- gsub("_turf", "", out$Portfolio)

  # Return top_n
  if (nrow(out) > top_n) {
    out <- out[seq_len(top_n), ]
  }
  rownames(out) <- seq_len(nrow(out))

  out
}
