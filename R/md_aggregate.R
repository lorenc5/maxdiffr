#' Aggregate Best-Worst Analysis
#'
#' Computes aggregate best/worst counts, normalized BW scores, and AE-MNL
#' estimates (beta, SE, choice probability, CI). Supports optional weighting.
#'
#' @param long_df Long-format data frame from [md_prep()], with columns
#'   for respondent ID, item label, and response (1/0/-1).
#' @param id_col Character name of respondent ID column (default "pid").
#' @param item_col Character name of item/message column (default "message").
#' @param response_col Character name of response column (default "y").
#' @param weight_df Optional data frame with two columns: ID and weight.
#'   Column names must match `id_col` and "WEIGHT". If NULL, unweighted.
#' @param ci_level Confidence level for AE-MNL intervals (default 0.95).
#'
#' @return Data frame with one row per item, columns: `message`, `all`, `bests`,
#'   `worsts`, `nbw`, `b`, `se`, `choice_p`, `lb`, `ub`. Sorted by `b`.
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
#' agg <- md_aggregate(prep$long,
#'   id_col = "pid", item_col = "message",
#'   response_col = "y"
#' )
#' agg
#'
#' @export
#' @importFrom dplyr group_by summarise n arrange bind_rows left_join
md_aggregate <- function(long_df,
                         id_col = "pid", item_col = "message",
                         response_col = "y",
                         weight_df = NULL, ci_level = 0.95) {
  if (is.null(weight_df)) {
    # Unweighted
    bw_dat_a <- long_df |>
      dplyr::group_by(.data[[item_col]]) |>
      dplyr::summarise(
        all = dplyr::n(),
        bests = sum(.data[[response_col]] == 1),
        worsts = sum(.data[[response_col]] == -1),
        .groups = "drop"
      )
  } else {
    # Weighted
    colnames(weight_df) <- c(id_col, "WEIGHT")
    merged <- dplyr::left_join(long_df, weight_df, by = id_col)
    split_group <- split(merged, merged[[item_col]])

    bw_dat_a <- dplyr::bind_rows(lapply(names(split_group), function(nm) {
      sg <- split_group[[nm]]
      wt <- .wtd_table(x = sg[[response_col]], weights = sg$WEIGHT)
      worst_count <- if ("-1" %in% names(wt)) wt[["-1"]] else 0
      none_count <- if ("0" %in% names(wt)) wt[["0"]] else 0
      best_count <- if ("1" %in% names(wt)) wt[["1"]] else 0
      data.frame(
        message = nm,
        all     = round(worst_count + none_count + best_count, 0),
        bests   = round(best_count, 0),
        worsts  = round(worst_count, 0)
      )
    }))
  }

  # Rename for consistency
  names(bw_dat_a)[names(bw_dat_a) == item_col] <- "message"

  # Normalized BW
  bw_dat_a$nbw <- (bw_dat_a$bests - bw_dat_a$worsts) / bw_dat_a$all

  # AE-MNL
  res <- ae_mnl(bw_dat_a, "all", "bests", "worsts", ci_level = ci_level)
  agg_res <- data.frame(bw_dat_a, res)

  # Sort by beta
  agg_res <- dplyr::arrange(agg_res, .data[["b"]])
  rownames(agg_res) <- NULL

  agg_res
}
