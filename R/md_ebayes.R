#' Individual-Level Empirical Bayes Scoring
#'
#' Computes individual-level item scores using empirical Bayes shrinkage.
#' Consolidates `e_bayescoring_w()` and `e_bayescoring2()` from the original
#' codebase. Set `skip_checks = TRUE` for anchored data (where not every
#' respondent has both a best and worst for every item).
#'
#' @param long_df Long-format data frame from [md_prep()].
#' @param id_col Character name of respondent ID column (default "pid").
#' @param block_col Character name of block column (default "block").
#' @param item_col Character name of item/message column (default "message").
#' @param response_col Character name of response column (default "y").
#' @param E Continuity correction for extreme probabilities (default 0.01).
#' @param alpha Shrinkage parameter (default 0.1).
#' @param wide Logical; if TRUE, return wide format with one column per item
#'   (default FALSE).
#' @param weight_df Optional data frame with two columns: ID and "WEIGHT".
#'   If NULL, unweighted.
#' @param skip_checks Logical; if TRUE, skip the requirement that every
#'   respondent has both best and worst choices. Use TRUE for anchored data
#'   (default FALSE).
#'
#' @return Data frame with columns: id, item, `b_ebayes`. If `wide = TRUE`,
#'   pivoted so each item is a column.
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#'
#' prep <- md_prep(md_survey, md_design,
#'   id_col = "id", version_col = "version",
#'   best_prefix = "QMAXDIFF_", best_suffix = "b",
#'   worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label
#' )
#'
#' # Long format
#' eb <- md_ebayes(prep$long,
#'   id_col = "pid", block_col = "block",
#'   item_col = "message", response_col = "y"
#' )
#' head(eb)
#'
#' # Wide format
#' eb_wide <- md_ebayes(prep$long,
#'   id_col = "pid", block_col = "block",
#'   item_col = "message", response_col = "y", wide = TRUE
#' )
#' head(eb_wide)
#'
#' @export
#' @importFrom dplyr group_by summarise mutate ungroup left_join n select
#'   all_of case_when
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym .data
md_ebayes <- function(long_df,
                      id_col = "pid", block_col = "block",
                      item_col = "message", response_col = "y",
                      E = 0.01, alpha = 0.1, wide = FALSE,
                      weight_df = NULL, skip_checks = FALSE) {
  if (!is.null(weight_df)) {
    # Weighted path
    colnames(weight_df) <- c(id_col, "WEIGHT")
    data_w <- dplyr::left_join(long_df, weight_df, by = id_col)
    split_group <- split(data_w, data_w[[item_col]])

    agg_dat_w <- dplyr::bind_rows(lapply(names(split_group), function(nm) {
      sg <- split_group[[nm]]
      wt <- .wtd_table(x = sg[[response_col]], weights = sg$WEIGHT)
      worst_count <- if ("-1" %in% names(wt)) wt[["-1"]] else 0
      none_count <- if ("0" %in% names(wt)) wt[["0"]] else 0
      best_count <- if ("1" %in% names(wt)) wt[["1"]] else 0
      data.frame(
        item = nm, worsts = worst_count, none = none_count,
        bests = best_count
      )
    }))
    names(agg_dat_w)[names(agg_dat_w) == "item"] <- item_col
    agg_dat_w$all <- agg_dat_w$worsts + agg_dat_w$none + agg_dat_w$bests
    agg_dat <- agg_dat_w
    agg_dat$p_j <- (agg_dat$all - agg_dat$worsts + agg_dat$bests) /
      (2 * agg_dat$all)
    agg_dat <- agg_dat[, c(item_col, "p_j")]

    # Individual weighted counts
    ind_dat <- data_w |>
      dplyr::group_by(.data[[id_col]], .data[[item_col]]) |>
      dplyr::summarise(
        bests = sum(.data[[response_col]] == 1),
        worsts = sum(.data[[response_col]] == -1),
        all = dplyr::n(),
        WEIGHT = .data[["WEIGHT"]][1],
        .groups = "drop"
      )

    # Weight the counts
    ind_dat$bests <- ind_dat$bests * ind_dat$WEIGHT
    ind_dat$worsts <- ind_dat$worsts * ind_dat$WEIGHT
    ind_dat$all <- ind_dat$all * ind_dat$WEIGHT

    ind_dat$p_ij <- (ind_dat$all - ind_dat$worsts + ind_dat$bests) /
      (2 * ind_dat$all)
    ind_dat$p_ij <- dplyr::case_when(
      ind_dat$p_ij == 0 ~ E,
      ind_dat$p_ij == 1 ~ (1 - E),
      TRUE ~ ind_dat$p_ij
    )

    out <- dplyr::left_join(ind_dat, agg_dat, by = item_col)
    out$p_ij <- ((1 / (1 + alpha)) * out$p_ij) +
      ((alpha / (1 + alpha)) * out$p_j)
    out$b_ebayes <- log(out$p_ij / (1 - out$p_ij))
    out <- dplyr::select(out, dplyr::all_of(c(id_col, item_col, "b_ebayes")))
  } else {
    # Unweighted path
    # Aggregate p_j
    agg_dat <- long_df |>
      dplyr::group_by(.data[[item_col]]) |>
      dplyr::summarise(
        bests = sum(.data[[response_col]] == 1),
        worsts = sum(.data[[response_col]] == -1),
        all = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(p_j = (all - worsts + bests) / (2 * all)) |>
      dplyr::select(dplyr::all_of(item_col), "p_j")

    # Individual p_ij
    ind_dat <- long_df |>
      dplyr::group_by(.data[[id_col]], .data[[item_col]]) |>
      dplyr::summarise(
        bests = sum(.data[[response_col]] == 1),
        worsts = sum(.data[[response_col]] == -1),
        all = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        p_ij = (all - worsts + bests) / (2 * all),
        p_ij = dplyr::case_when(
          p_ij == 0 ~ E,
          p_ij == 1 ~ (1 - E),
          TRUE ~ p_ij
        )
      )

    out <- dplyr::left_join(ind_dat, agg_dat, by = item_col)
    out$p_ij <- ((1 / (1 + alpha)) * out$p_ij) +
      ((alpha / (1 + alpha)) * out$p_j)
    out$b_ebayes <- log(out$p_ij / (1 - out$p_ij))
    out <- dplyr::select(out, dplyr::all_of(c(id_col, item_col, "b_ebayes")))
  }

  if (wide) {
    out <- tidyr::pivot_wider(
      out,
      names_from = dplyr::all_of(item_col),
      values_from = "b_ebayes"
    )
  }

  as.data.frame(out)
}
