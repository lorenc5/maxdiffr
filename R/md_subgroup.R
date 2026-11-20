#' Create a Reusable Configuration for md_prep
#'
#' Packages all [md_prep()] parameters into a named list so that
#' [md_subgroup()] can call `md_prep()` for each subgroup without
#' requiring 20+ arguments.
#'
#' @param design_df Data frame of the design file (see [md_prep()]).
#' @param id_col Character name of respondent ID column.
#' @param version_col Character name of design version column.
#' @param best_prefix Character prefix for best-choice columns.
#' @param best_suffix Character suffix for best-choice columns.
#' @param worst_prefix Character prefix for worst-choice columns.
#' @param worst_suffix Character suffix for worst-choice columns.
#' @param n_blocks Number of blocks/screens.
#' @param n_items Number of items.
#' @param items_per_block Number of items per block.
#' @param item_labels Character vector of item labels (length `n_items`).
#' @param anchor_prefix Character prefix for anchor columns, or NULL for
#'   standard (non-anchored) mode.
#' @param anchor_suffix Character suffix for anchor columns.
#' @param weight_col Character name of weight column, or NULL.
#'
#' @return A list of class `"md_prep_config"`.
#'
#' @examples
#' data(md_design)
#' data(md_items)
#'
#' config <- md_prep_config(
#'   design_df = md_design,
#'   id_col = "id", version_col = "version",
#'   best_prefix = "QMAXDIFF_", best_suffix = "b",
#'   worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label
#' )
#' class(config)
#'
#' @export
md_prep_config <- function(design_df,
                           id_col = "id",
                           version_col = "version",
                           best_prefix = "QMAXDIFF_",
                           best_suffix = "b",
                           worst_prefix = "QMAXDIFF_",
                           worst_suffix = "w",
                           n_blocks = 12,
                           n_items = 12,
                           items_per_block = 4,
                           item_labels = NULL,
                           anchor_prefix = NULL,
                           anchor_suffix = "_anchor",
                           weight_col = NULL) {
  config <- list(
    design_df      = design_df,
    id_col         = id_col,
    version_col    = version_col,
    best_prefix    = best_prefix,
    best_suffix    = best_suffix,
    worst_prefix   = worst_prefix,
    worst_suffix   = worst_suffix,
    n_blocks       = n_blocks,
    n_items        = n_items,
    items_per_block = items_per_block,
    item_labels    = item_labels,
    anchor_prefix  = anchor_prefix,
    anchor_suffix  = anchor_suffix,
    weight_col     = weight_col
  )
  class(config) <- "md_prep_config"
  config
}


#' Run the MaxDiff pipeline for one subgroup
#'
#' @param survey_df Filtered survey data frame.
#' @param config An `md_prep_config` object.
#' @param turf Logical; run TURF analysis?
#' @param turf_combo Combination size for [md_portfolio()].
#' @param turf_top_n Number of top portfolios to return.
#' @param ci_level Confidence level for [md_aggregate()].
#' @param ebayes_alpha Shrinkage parameter for [md_ebayes()].
#'
#' @return List with `$aggregate`, `$anchor` (or NULL), `$turf` (or NULL).
#' @keywords internal
.run_one_subgroup <- function(survey_df, config, turf, turf_combo, turf_top_n,
                              ci_level, ebayes_alpha) {
  anchored <- !is.null(config$anchor_prefix)


  # 1. Prep -----------------------------------------------------------
  prep <- md_prep(
    survey_df       = survey_df,
    design_df       = config$design_df,
    id_col          = config$id_col,
    version_col     = config$version_col,
    best_prefix     = config$best_prefix,
    best_suffix     = config$best_suffix,
    worst_prefix    = config$worst_prefix,
    worst_suffix    = config$worst_suffix,
    n_blocks        = config$n_blocks,
    n_items         = config$n_items,
    items_per_block = config$items_per_block,
    item_labels     = config$item_labels,
    anchor_prefix   = config$anchor_prefix,
    anchor_suffix   = config$anchor_suffix,
    weight_col      = config$weight_col
  )

  # 2. Aggregate -------------------------------------------------------
  agg <- md_aggregate(
    prep$long,
    id_col       = "pid",
    item_col     = "message",
    response_col = "y",
    weight_col   = config$weight_col,
    ci_level     = ci_level
  )

  # 3. Anchor (if applicable) ------------------------------------------
  anchor <- NULL
  if (anchored) {
    anchor <- md_anchor_100(agg, anchor_label = "ANCHOR")
  }

  # 4. TURF (if requested) ---------------------------------------------
  turf_result <- NULL
  if (turf) {
    eb_wide <- md_ebayes(
      prep$long,
      id_col       = "pid",
      block_col    = "block",
      item_col     = "message",
      response_col = "y",
      alpha        = ebayes_alpha,
      wide         = TRUE,
      weight_col   = config$weight_col,
      skip_checks  = anchored
    )

    # md_ebayes wide output does not include the weight column;
    # join it back so md_turf_matrix can use it
    if (!is.null(config$weight_col) &&
        config$weight_col %in% names(survey_df)) {
      wgt_df <- stats::setNames(
        data.frame(survey_df[[config$id_col]],
                   survey_df[[config$weight_col]]),
        c("pid", config$weight_col)
      )
      wgt_df <- wgt_df[!duplicated(wgt_df$pid), ]
      eb_wide <- dplyr::left_join(eb_wide, wgt_df, by = "pid")
    }

    turf_mat <- md_turf_matrix(
      eb_wide,
      id_col     = "pid",
      weight_col = config$weight_col
    )

    # md_turf_matrix renames the weight column to "WEIGHT"
    portfolio_wt <- if (!is.null(config$weight_col)) "WEIGHT" else NULL

    turf_result <- md_portfolio(
      turf_mat,
      combo      = turf_combo,
      id_col     = "pid",
      weight_col = portfolio_wt,
      top_n      = turf_top_n
    )
  }

  list(aggregate = agg, anchor = anchor, turf = turf_result)
}


#' Subgroup Comparison for MaxDiff
#'
#' Runs the full MaxDiff pipeline (prep, aggregate, optionally anchor and TURF)
#' for each subgroup and the full sample, returning side-by-side results.
#'
#' @param survey_df Data frame of survey responses.
#' @param prep_config An [md_prep_config()] object.
#' @param group_col Character name of the column in `survey_df` that defines
#'   subgroups (e.g., `"QPARTYID"`).
#' @param groups Named list mapping labels to values, e.g.
#'   `list(Democrat = 1:2, Republican = 6:7)`.
#' @param include_full Logical; include the full (unfiltered) sample as a
#'   subgroup? Default TRUE.
#' @param full_label Character label for the full sample (default
#'   `"Full Sample"`).
#' @param turf Logical; run TURF analysis? Default FALSE.
#' @param turf_combo Combination size for [md_portfolio()] (default 1).
#' @param turf_top_n Number of top portfolios to return (default 10).
#' @param ci_level Confidence level for [md_aggregate()] (default 0.95).
#' @param ebayes_alpha Shrinkage parameter for [md_ebayes()] (default 0.1).
#'
#' @return An S3 object of class `"md_subgroup"` with elements:
#'   \item{aggregate}{Long data frame with columns from [md_aggregate()] plus
#'     `subgroup`.}
#'   \item{anchor}{Long data frame with anchored scores plus `subgroup`,
#'     or NULL if not anchored.}
#'   \item{turf}{Long data frame with TURF portfolios plus `subgroup`,
#'     or NULL if `turf = FALSE`.}
#'   \item{n_per_group}{Named integer vector of sample sizes.}
#'   \item{wide}{List of pre-pivoted wide tables: `$aggregate` and optionally
#'     `$anchor`, where rows are items and columns are
#'     `{metric}_{subgroup}`.}
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#'
#' config <- md_prep_config(
#'   design_df = md_design,
#'   id_col = "id", version_col = "version",
#'   best_prefix = "QMAXDIFF_", best_suffix = "b",
#'   worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label
#' )
#'
#' results <- md_subgroup(
#'   md_survey, config,
#'   group_col = "QPARTYID",
#'   groups = list(Democrat = 1:2, Republican = 6:7)
#' )
#' results
#' results$wide$aggregate
#'
#' @export
#' @importFrom dplyr bind_rows left_join
#' @importFrom tidyr pivot_wider
md_subgroup <- function(survey_df, prep_config, group_col, groups,
                        include_full = TRUE, full_label = "Full Sample",
                        turf = FALSE, turf_combo = 1, turf_top_n = 10,
                        ci_level = 0.95, ebayes_alpha = 0.1) {

  if (!inherits(prep_config, "md_prep_config")) {
    stop("`prep_config` must be created by md_prep_config()")
  }
  if (!(group_col %in% names(survey_df))) {
    stop("Column '", group_col, "' not found in survey_df")
  }

  anchored <- !is.null(prep_config$anchor_prefix)

  # Build ordered list of subgroup data frames --------------------------
  subgroup_dfs <- list()
  if (include_full) {
    subgroup_dfs[[full_label]] <- survey_df
  }

  for (nm in names(groups)) {
    sub_df <- survey_df[survey_df[[group_col]] %in% groups[[nm]], ]
    n_sub  <- nrow(sub_df)
    if (n_sub == 0) {
      warning("Subgroup '", nm, "' has 0 respondents; skipping.")
      next
    }
    if (n_sub < 30) {
      warning("Subgroup '", nm, "' has only ", n_sub,
              " respondents; estimates may be unstable.")
    }
    subgroup_dfs[[nm]] <- sub_df
  }

  # Run pipeline for each subgroup -------------------------------------
  n_per_group <- integer(0)
  agg_list  <- list()
  anc_list  <- list()
  turf_list <- list()

  for (nm in names(subgroup_dfs)) {
    sub_df <- subgroup_dfs[[nm]]
    n_per_group[nm] <- nrow(sub_df)

    res <- .run_one_subgroup(
      survey_df    = sub_df,
      config       = prep_config,
      turf         = turf,
      turf_combo   = turf_combo,
      turf_top_n   = turf_top_n,
      ci_level     = ci_level,
      ebayes_alpha = ebayes_alpha
    )

    res$aggregate$subgroup <- nm
    agg_list[[nm]] <- res$aggregate

    if (!is.null(res$anchor)) {
      res$anchor$subgroup <- nm
      anc_list[[nm]] <- res$anchor
    }

    if (!is.null(res$turf)) {
      res$turf$subgroup <- nm
      turf_list[[nm]] <- res$turf
    }
  }

  # Combine into long data frames --------------------------------------
  agg_long  <- dplyr::bind_rows(agg_list)
  anc_long  <- if (length(anc_list) > 0)  dplyr::bind_rows(anc_list)  else NULL
  turf_long <- if (length(turf_list) > 0)  dplyr::bind_rows(turf_list) else NULL

  # Build wide tables --------------------------------------------------
  wide <- list()

  agg_for_wide <- agg_long[, c("message", "subgroup", "b", "choice_p", "nbw")]
  wide$aggregate <- as.data.frame(tidyr::pivot_wider(
    agg_for_wide,
    id_cols     = "message",
    names_from  = "subgroup",
    values_from = c("b", "choice_p", "nbw"),
    names_glue  = "{.value}_{subgroup}"
  ))

  if (!is.null(anc_long)) {
    anc_for_wide <- anc_long[, c("message", "subgroup", "thresh100", "cut")]
    wide$anchor <- as.data.frame(tidyr::pivot_wider(
      anc_for_wide,
      id_cols     = "message",
      names_from  = "subgroup",
      values_from = c("thresh100", "cut"),
      names_glue  = "{.value}_{subgroup}"
    ))
  }

  structure(
    list(
      aggregate   = agg_long,
      anchor      = anc_long,
      turf        = turf_long,
      n_per_group = n_per_group,
      wide        = wide
    ),
    class = "md_subgroup"
  )
}


#' Print method for md_subgroup objects
#'
#' @param x An `md_subgroup` object.
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.md_subgroup <- function(x, ...) {
  cat("MaxDiff Subgroup Comparison\n")
  cat("---------------------------\n")
  cat("Groups:", paste(names(x$n_per_group), collapse = ", "), "\n")
  cat("Sample sizes:\n")
  for (nm in names(x$n_per_group)) {
    cat("  ", nm, ": n = ", x$n_per_group[nm], "\n", sep = "")
  }
  cat("\nAnalyses:\n")
  cat("  Aggregate: yes\n")
  cat("  Anchored:  ", if (!is.null(x$anchor)) "yes" else "no", "\n", sep = "")
  cat("  TURF:      ", if (!is.null(x$turf)) "yes" else "no", "\n", sep = "")
  invisible(x)
}
