#' Validate MaxDiff Survey Responses Against Design
#'
#' Checks that each respondent's best/worst selections are consistent with the
#' items shown in their design version. Validates that best != worst, both are
#' in the shown item set, and values are within the expected range.
#'
#' @param survey_df Data frame of survey responses.
#' @param design_df Data frame of the design (one row per respondent-block with
#'   columns for version, Block, Item1, Item2, ...).
#' @param id_col Character name of respondent ID column in `survey_df`.
#' @param version_col Character name of design version column in `survey_df`.
#' @param best_prefix Character prefix for best-choice columns (e.g., "QMAXDIFF_").
#' @param best_suffix Character suffix for best-choice columns (e.g., "b").
#' @param worst_prefix Character prefix for worst-choice columns.
#' @param worst_suffix Character suffix for worst-choice columns (e.g., "w").
#' @param n_blocks Number of blocks/screens (default 12).
#' @param n_items Number of items (default 12).
#' @param items_per_block Number of items per screen (default 4).
#' @param drop_invalid Logical; if TRUE, remove invalid respondents from returned
#'   data frame (default FALSE).
#' @param verbose Logical; if TRUE, print validation summary (default TRUE).
#'
#' @return A list with:
#'   \item{valid}{Logical; TRUE if all responses passed validation.}
#'   \item{issues}{Data frame of issues found (respondent, block, issue type).}
#'   \item{survey_df}{The (possibly filtered) survey data frame.}
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' val <- md_validate(md_survey, md_design,
#'   id_col = "id", version_col = "version",
#'   best_prefix = "QMAXDIFF_", best_suffix = "b",
#'   worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'   n_blocks = 8, n_items = 8, items_per_block = 4
#' )
#' val$valid
#'
#' @export
md_validate <- function(survey_df, design_df,
                        id_col = "id", version_col = "version",
                        best_prefix = "QMAXDIFF_", best_suffix = "b",
                        worst_prefix = "QMAXDIFF_", worst_suffix = "w",
                        n_blocks = 12, n_items = 12, items_per_block = 4,
                        drop_invalid = FALSE, verbose = TRUE) {
  issue_list <- list()
  item_cols <- paste0("Item", seq_len(items_per_block))

  for (r in seq_len(nrow(survey_df))) {
    rid <- survey_df[[id_col]][r]
    ver <- as.numeric(survey_df[[version_col]][r])

    # Get this version's design (one respondent's worth = n_blocks rows)
    des_ver <- design_df[design_df$version == ver, ]
    des_ver <- des_ver[!duplicated(des_ver$Block), ]

    for (j in seq_len(n_blocks)) {
      best_col_name <- paste0(best_prefix, j, best_suffix)
      worst_col_name <- paste0(worst_prefix, j, worst_suffix)

      if (!(best_col_name %in% names(survey_df)) ||
        !(worst_col_name %in% names(survey_df))) {
        issue_list <- c(issue_list, list(data.frame(
          id = as.character(rid), block = j, issue = "missing column"
        )))
        next
      }

      best_val <- as.numeric(survey_df[[best_col_name]][r])
      worst_val <- as.numeric(survey_df[[worst_col_name]][r])

      # Check NA
      if (is.na(best_val) || is.na(worst_val)) {
        issue_list <- c(issue_list, list(data.frame(
          id = as.character(rid), block = j, issue = "NA response"
        )))
        next
      }

      # Check range
      if (best_val < 1 || best_val > n_items ||
        worst_val < 1 || worst_val > n_items) {
        issue_list <- c(issue_list, list(data.frame(
          id = as.character(rid), block = j,
          issue = paste0("out of range: best=", best_val, " worst=", worst_val)
        )))
        next
      }

      # Check best != worst
      if (best_val == worst_val) {
        issue_list <- c(issue_list, list(data.frame(
          id = as.character(rid), block = j, issue = "best == worst"
        )))
        next
      }

      # Check both are in the shown item set for this block
      block_row <- des_ver[des_ver$Block == j, ]
      if (nrow(block_row) == 0) {
        issue_list <- c(issue_list, list(data.frame(
          id = as.character(rid), block = j,
          issue = "block not found in design"
        )))
        next
      }

      shown_items <- as.numeric(block_row[1, item_cols])

      if (!(best_val %in% shown_items)) {
        issue_list <- c(issue_list, list(data.frame(
          id = as.character(rid), block = j,
          issue = paste0(
            "best=", best_val, " not in shown items: ",
            paste(shown_items, collapse = ",")
          )
        )))
      }

      if (!(worst_val %in% shown_items)) {
        issue_list <- c(issue_list, list(data.frame(
          id = as.character(rid), block = j,
          issue = paste0(
            "worst=", worst_val, " not in shown items: ",
            paste(shown_items, collapse = ",")
          )
        )))
      }
    }
  }

  issues <- if (length(issue_list) > 0) {
    dplyr::bind_rows(issue_list)
  } else {
    data.frame(id = character(), block = integer(), issue = character())
  }

  valid <- nrow(issues) == 0

  if (verbose) {
    cat("--- MaxDiff Validation ---\n")
    cat("Respondents:", nrow(survey_df), "\n")
    cat("Issues found:", nrow(issues), "\n")
    if (!valid) {
      cat("Invalid respondents:", length(unique(issues$id)), "\n")
      cat("Issue types:\n")
      print(table(issues$issue))
    } else {
      cat("All responses valid.\n")
    }
  }

  if (drop_invalid && !valid) {
    bad_ids <- unique(issues$id)
    survey_df <- survey_df[!(as.character(survey_df[[id_col]]) %in% bad_ids), ]
    if (verbose) cat("Dropped", length(bad_ids), "respondents.\n")
  }

  list(valid = valid, issues = issues, survey_df = survey_df)
}
