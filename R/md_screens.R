#' Build a Screen-Level Table of MaxDiff Responses
#'
#' Reshapes survey responses into one row per respondent x screen, with the
#' items shown, the best and worst picks, and (optionally) the anchor answer.
#' This is the input for the model-based anchoring functions
#' [md_anchor_logit()], [md_hb()] and [md_anchor_patterns()].
#'
#' @inheritParams md_prep
#' @param anchor_codes Named numeric vector giving the survey codes for the
#'   anchor answers `all`, `some` and `none` (default `c(all = 1, some = 2,
#'   none = 3)`).
#'
#' @return A data frame with class `md_screens` and columns `pid`, `block`,
#'   `i1` ... `ik` (item numbers shown), `best`, `worst`, `anchor` (`"all"`,
#'   `"some"`, `"none"` or `NA`) and `valid` (`TRUE` when both picks are
#'   among the items shown and differ). Item labels are stored in
#'   `attr(x, "item_labels")`.
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#' scr <- md_screens(md_survey, md_design,
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label,
#'   anchor_prefix = "QMAXDIFF_", anchor_suffix = "_anchor"
#' )
#' head(scr)
#' table(scr$anchor)
#'
#' @export
md_screens <- function(survey_df, design_df,
                       id_col = "id", version_col = "version",
                       best_prefix = "QMAXDIFF_", best_suffix = "b",
                       worst_prefix = "QMAXDIFF_", worst_suffix = "w",
                       n_blocks = 12, n_items = 12, items_per_block = 4,
                       item_labels = NULL,
                       anchor_prefix = NULL, anchor_suffix = "_anchor",
                       anchor_codes = c(all = 1, some = 2, none = 3)) {
  if (is.null(item_labels)) item_labels <- paste0("Item_", seq_len(n_items))
  stopifnot(length(item_labels) == n_items)
  item_cols <- paste0("Item", seq_len(items_per_block))
  miss <- setdiff(c("version", "Block", item_cols), names(design_df))
  if (length(miss)) stop("design_df is missing columns: ", paste(miss, collapse = ", "))

  des <- design_df[!duplicated(design_df[, c("version", "Block")]), ]
  des_key <- paste(.md_num(des$version), .md_num(des$Block))
  ver <- .md_num(survey_df[[version_col]])
  pid <- survey_df[[id_col]]

  out <- lapply(seq_len(n_blocks), function(j) {
    row <- match(paste(ver, j), des_key)
    if (anyNA(row)) {
      stop(
        "No design row for block ", j, " and version(s): ",
        paste(unique(ver[is.na(row)]), collapse = ", ")
      )
    }
    shown <- as.matrix(des[row, item_cols])
    dimnames(shown) <- list(NULL, paste0("i", seq_len(items_per_block)))
    b <- .md_num(survey_df[[paste0(best_prefix, j, best_suffix)]])
    w <- .md_num(survey_df[[paste0(worst_prefix, j, worst_suffix)]])
    a <- rep(NA_character_, length(pid))
    if (!is.null(anchor_prefix)) {
      a_col <- paste0(anchor_prefix, j, anchor_suffix)
      if (!(a_col %in% names(survey_df))) stop("Anchor column ", a_col, " not found")
      a_raw <- .md_num(survey_df[[a_col]])
      a <- names(anchor_codes)[match(a_raw, anchor_codes)]
    }
    data.frame(pid = pid, block = j, shown, best = b, worst = w, anchor = a)
  })
  out <- do.call(rbind, out)
  shown <- as.matrix(out[, paste0("i", seq_len(items_per_block))])
  out$valid <- !is.na(out$best) & !is.na(out$worst) & out$best != out$worst &
    rowSums(shown == out$best) == 1 & rowSums(shown == out$worst) == 1
  out <- out[order(match(out$pid, pid), out$block), ]
  rownames(out) <- NULL
  attr(out, "item_labels") <- item_labels
  class(out) <- c("md_screens", "data.frame")
  out
}

# Numeric conversion that tolerates haven_labelled, factor and character input
.md_num <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(unclass(x)))
}

# Valid, anchored screens only (warns when screens are dropped)
.md_anchored_screens <- function(screens) {
  if (!inherits(screens, "md_screens")) stop("`screens` must come from md_screens()")
  if (all(is.na(screens$anchor))) {
    stop("No anchor answers found; call md_screens() with `anchor_prefix`")
  }
  keep <- screens$valid & !is.na(screens$anchor)
  if (any(!keep)) {
    warning(sum(!keep), " screen(s) dropped (invalid best/worst or missing anchor)")
  }
  labs <- attr(screens, "item_labels")
  screens <- screens[keep, ]
  attr(screens, "item_labels") <- labs
  screens
}
