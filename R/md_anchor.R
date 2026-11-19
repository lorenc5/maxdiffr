#' Anchored 100-Point Scale (Aggregate)
#'
#' Converts aggregate AE-MNL choice probabilities to a 100-point scale where
#' the anchor item = 100. Items above 100 are preferred to the anchor; below
#' 100 are less preferred.
#'
#' @param agg_df Data frame from [md_aggregate()], must contain `choice_p` and
#'   `message` columns.
#' @param anchor_label Character label of the anchor item (default "ANCHOR").
#' @param round_dec Number of decimal places (default 0).
#'
#' @return The input data frame with added columns: `thresh100` (anchored score)
#'   and `cut` (1 = below anchor, 2 = at anchor, 3 = above anchor).
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#'
#' # Anchored prep + aggregate + 100-point scale
#' prep_anc <- md_prep(md_survey, md_design,
#'   id_col = "id", version_col = "version",
#'   best_prefix = "QMAXDIFF_", best_suffix = "b",
#'   worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label,
#'   anchor_prefix = "QMAXDIFF_",
#'   anchor_suffix = "_anchor"
#' )
#' agg_anc <- md_aggregate(prep_anc$long,
#'   id_col = "pid",
#'   item_col = "message", response_col = "y"
#' )
#' a100 <- md_anchor_100(agg_anc)
#' a100 |> dplyr::select(message, thresh100, cut)
#'
#' @export
md_anchor_100 <- function(agg_df, anchor_label = "ANCHOR", round_dec = 0) {
  c100 <- agg_df$choice_p * 100
  anchor_value <- c100[agg_df$message == anchor_label]

  if (length(anchor_value) == 0) {
    stop("Anchor label '", anchor_label, "' not found in agg_df$message")
  }

  c100_t <- ((c100 - anchor_value) / anchor_value) * 100
  agg_df$thresh100 <- round(c100_t + 100, round_dec)

  agg_df$cut <- dplyr::case_when(
    agg_df$thresh100 < 100 ~ 1L,
    agg_df$thresh100 == 100 ~ 2L,
    agg_df$thresh100 > 100 ~ 3L
  )

  agg_df
}


#' Anchored 100-Point Scale (Individual)
#'
#' Converts individual empirical Bayes scores to a 100-point scale where the
#' anchor = 100 for each respondent. Fixes the `round_dec` undefined bug in
#' the original `ind_anchor_100()`.
#'
#' @param ebayes_df Long-format data frame from [md_ebayes()] (wide = FALSE),
#'   with columns for respondent ID, item label, and `b_ebayes`.
#' @param id_col Character name of respondent ID column (default "pid").
#' @param item_col Character name of item/message column (default "message").
#' @param score_col Character name of ebayes score column (default "b_ebayes").
#' @param anchor_label Character label of the anchor item (default "ANCHOR").
#' @param round_dec Number of decimal places (default 0).
#'
#' @return Wide-format data frame with one row per respondent and one column
#'   per item (suffixed "_anchor100"), plus the ID column.
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#'
#' # Anchored prep + individual ebayes + individual anchor 100
#' prep_anc <- md_prep(md_survey, md_design,
#'   id_col = "id", version_col = "version",
#'   best_prefix = "QMAXDIFF_", best_suffix = "b",
#'   worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label,
#'   anchor_prefix = "QMAXDIFF_",
#'   anchor_suffix = "_anchor"
#' )
#' eb_anc <- md_ebayes(prep_anc$long,
#'   id_col = "pid", block_col = "block",
#'   item_col = "message", response_col = "y",
#'   skip_checks = TRUE
#' )
#' ind_a100 <- md_ind_anchor_100(eb_anc, id_col = "pid", item_col = "message")
#' head(ind_a100)
#'
#' @export
md_ind_anchor_100 <- function(ebayes_df,
                              id_col = "pid", item_col = "message",
                              score_col = "b_ebayes",
                              anchor_label = "ANCHOR", round_dec = 0) {
  e_res_split <- split(ebayes_df, ebayes_df[[id_col]])

  out_list <- lapply(e_res_split, function(x) {
    # Exponentiate scores
    x$choice_p <- exp(x[[score_col]])

    anchor_value <- x$choice_p[x[[item_col]] == anchor_label]
    if (length(anchor_value) == 0 || anchor_value == 0) {
      x$thresh100 <- NA_real_
    } else {
      c100_t <- ((x$choice_p - anchor_value) / anchor_value) * 100
      x$thresh100 <- round(c100_t + 100, round_dec)
    }
    x
  })

  out <- dplyr::bind_rows(out_list)

  # Pivot to wide
  out <- dplyr::select(out, dplyr::all_of(c(id_col, item_col, "thresh100")))
  out <- tidyr::pivot_wider(
    out,
    names_from = dplyr::all_of(item_col),
    values_from = "thresh100"
  )
  out <- as.data.frame(out)

  # Rename columns with _anchor100 suffix (except id)
  item_names <- setdiff(names(out), id_col)
  names(out)[names(out) %in% item_names] <- paste0(item_names, "_anchor100")

  out
}
