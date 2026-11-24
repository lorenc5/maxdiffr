#' Prepare MaxDiff Survey Data for Analysis
#'
#' Converts raw survey responses + design into long-format Best-Worst data
#' suitable for aggregate and individual-level analysis. Handles both standard
#' and anchored MaxDiff in a single function (set `anchor_prefix` to enable
#' anchored mode).
#'
#' Replaces the duplicated 12-branch if/else block preparation code with a
#' single dynamic loop using `paste0(prefix, j, suffix)`.
#'
#' @param survey_df Data frame of survey responses.
#' @param design_df Data frame of the design file. Must have columns: `version`,
#'   `Block`, `Item1`, `Item2`, ... One row per respondent-block (or one set of
#'   blocks per version — duplicates by pid are handled).
#' @param id_col Character name of respondent ID column.
#' @param version_col Character name of design version column.
#' @param best_prefix Character prefix for best-choice columns.
#' @param best_suffix Character suffix for best-choice columns.
#' @param worst_prefix Character prefix for worst-choice columns.
#' @param worst_suffix Character suffix for worst-choice columns.
#' @param n_blocks Number of blocks/screens (default 12).
#' @param n_items Number of items (default 12).
#' @param items_per_block Number of items per block (default 4).
#' @param item_labels Character vector of item labels (length `n_items`).
#' @param anchor_prefix Character prefix for anchor columns, or NULL for
#'   standard (non-anchored) prep. When set, adds an ANCHOR pseudo-item.
#' @param anchor_suffix Character suffix for anchor columns (default "_anchor").
#' @param anchor_coding How ALL and NONE screens are coded when anchored.
#'   `"drop_choices"` (default, original behavior): the screen's best and worst
#'   picks are set to 0 and only the ANCHOR pseudo-item carries information
#'   (-1 for ALL, +1 for NONE). `"keep_choices"`: ALL keeps the best pick (+1)
#'   with ANCHOR as worst (-1); NONE keeps the worst pick (-1) with ANCHOR as
#'   best (+1). `"keep_choices"` retains the preference information on those
#'   screens and gives more conservative anchored scores. For model-based
#'   anchoring see [md_anchor_logit()] and [md_hb()].
#' @param weight_col Character name of weight column in `survey_df`, or NULL.
#'
#' @return A list with:
#'   \item{wide}{Wide-format data frame (pid + block*item columns for y and t).}
#'   \item{long}{Long-format data frame with columns: pid, block, t (item label), y (1/0/-1), message.}
#'
#' @examples
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#'
#' # Standard (non-anchored) prep
#' prep <- md_prep(md_survey, md_design,
#'   id_col = "id", version_col = "version",
#'   best_prefix = "QMAXDIFF_", best_suffix = "b",
#'   worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label
#' )
#' head(prep$long)
#'
#' # Anchored prep
#' prep_anc <- md_prep(md_survey, md_design,
#'   id_col = "id", version_col = "version",
#'   best_prefix = "QMAXDIFF_", best_suffix = "b",
#'   worst_prefix = "QMAXDIFF_", worst_suffix = "w",
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label,
#'   anchor_prefix = "QMAXDIFF_",
#'   anchor_suffix = "_anchor"
#' )
#' table(prep_anc$long$message) # includes ANCHOR
#'
#' @export
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom dplyr mutate case_when select left_join all_of any_of bind_rows
md_prep <- function(survey_df, design_df,
                    id_col = "id", version_col = "version",
                    best_prefix = "QMAXDIFF_", best_suffix = "b",
                    worst_prefix = "QMAXDIFF_", worst_suffix = "w",
                    n_blocks = 12, n_items = 12, items_per_block = 4,
                    item_labels = NULL,
                    anchor_prefix = NULL, anchor_suffix = "_anchor",
                    anchor_coding = c("drop_choices", "keep_choices"),
                    weight_col = NULL) {
  anchor_coding <- match.arg(anchor_coding)
  if (is.null(item_labels)) {
    item_labels <- paste0("Item_", seq_len(n_items))
  }

  anchored <- !is.null(anchor_prefix)
  n_cols <- if (anchored) n_items + 1 else n_items

  # Get unique design per version (one set of blocks)
  item_col_names <- paste0("Item", seq_len(items_per_block))
  des_by_ver <- list()
  for (v in unique(design_df$version)) {
    dv <- design_df[design_df$version == v, ]
    dv <- dv[!duplicated(dv$Block), ]
    des_by_ver[[as.character(v)]] <- dv
  }

  n_resp <- nrow(survey_df)

  # -----------------------------------------------------------
  # Step 1: Build the response matrix (bw_hold) for each block
  # -----------------------------------------------------------
  bw_hold <- list()

  for (j in seq_len(n_blocks)) {
    # Create blank matrix: n respondents x n_cols (items + maybe anchor)
    bw_dat <- data.frame(matrix(NA, nrow = n_resp, ncol = n_cols))

    col_names <- paste0("q", j, "_i", seq_len(n_items), "_y")
    if (anchored) {
      col_names <- c(col_names, paste0("q", j, "_ia_y"))
    }
    colnames(bw_dat) <- col_names

    # Fill in 0s for the items shown in this block per respondent's version
    for (i in seq_len(n_resp)) {
      ver <- as.character(as.numeric(survey_df[[version_col]][i]))
      des_ver <- des_by_ver[[ver]]
      block_row <- des_ver[des_ver$Block == j, ]
      shown <- as.numeric(block_row[1, item_col_names])
      bw_dat[i, shown] <- 0
    }

    # Fill in best (1) and worst (-1) using dynamic column names
    best_col_name <- paste0(best_prefix, j, best_suffix)
    worst_col_name <- paste0(worst_prefix, j, worst_suffix)

    for (i in seq_len(n_resp)) {
      b_val <- as.numeric(survey_df[[best_col_name]][i])
      w_val <- as.numeric(survey_df[[worst_col_name]][i])
      if (!is.na(b_val) && b_val >= 1 && b_val <= n_items) {
        bw_dat[i, b_val] <- 1
      }
      if (!is.na(w_val) && w_val >= 1 && w_val <= n_items) {
        bw_dat[i, w_val] <- -1
      }
    }

    bw_hold[[j]] <- bw_dat
  }

  # Combine all blocks
  bw_dat_all <- do.call(cbind, bw_hold)

  # -----------------------------------------------------------
  # Step 2: Create label columns for each block
  # -----------------------------------------------------------
  label_list <- list()

  for (j in seq_len(n_blocks)) {
    lbl <- data.frame(matrix(NA, nrow = n_resp, ncol = n_cols))
    for (m in seq_len(n_items)) {
      lbl[, m] <- item_labels[m]
    }
    if (anchored) {
      lbl[, n_items + 1] <- "ANCHOR"
    }
    col_names_t <- paste0("q", j, "_i", seq_len(n_items), "_t")
    if (anchored) {
      col_names_t <- c(col_names_t, paste0("q", j, "_ia_t"))
    }
    colnames(lbl) <- col_names_t
    label_list[[j]] <- lbl
  }

  labels_all <- do.call(cbind, label_list)

  # -----------------------------------------------------------
  # Step 3: Handle anchor recoding (if anchored)
  # -----------------------------------------------------------
  if (anchored) {
    for (k in seq_len(n_blocks)) {
      anc_col <- paste0(anchor_prefix, k, anchor_suffix)
      if (!(anc_col %in% names(survey_df))) {
        warning(paste("Anchor column", anc_col, "not found; skipping block", k))
        next
      }

      y_vals <- as.numeric(survey_df[[anc_col]])
      anchor_y_col <- paste0("q", k, "_ia_y")
      item_y_cols <- paste0("q", k, "_i", seq_len(n_items), "_y")

      for (i in seq_len(n_resp)) {
        anc_resp <- y_vals[i]
        if (is.na(anc_resp)) next

        if (anc_resp == 1 || anc_resp == 3) {
          # ALL or NONE: anchor = -1 (ALL) or +1 (NONE).
          # drop_choices: zero out the screen's best and worst picks.
          # keep_choices: ALL keeps best, NONE keeps worst.
          keep_val <- if (anchor_coding == "keep_choices") {
            if (anc_resp == 1) 1 else -1
          } else {
            NA
          }
          for (col_name in item_y_cols) {
            v <- bw_dat_all[i, col_name]
            if (!is.na(v) && !(!is.na(keep_val) && v == keep_val)) {
              bw_dat_all[i, col_name] <- 0
            }
          }
          if (anc_resp == 1) {
            bw_dat_all[i, anchor_y_col] <- -1
          } else {
            bw_dat_all[i, anchor_y_col] <- 1
          }
        } else {
          # SOME: keep best/worst as-is, anchor = 0
          bw_dat_all[i, anchor_y_col] <- 0
        }
      }
    }
  }

  # -----------------------------------------------------------
  # Step 4: Combine into wide format
  # -----------------------------------------------------------
  pid <- survey_df[[id_col]]
  wide_df <- data.frame(pid = pid, bw_dat_all, labels_all)

  # -----------------------------------------------------------
  # Step 5: Pivot to long format
  # -----------------------------------------------------------
  long_df <- wide_df
  val_cols <- setdiff(names(long_df), "pid")

  long_df <- tidyr::pivot_longer(
    long_df,
    cols = dplyr::all_of(val_cols),
    names_to = "temp",
    values_to = "value",
    values_transform = list(value = as.character)
  )

  # Separate: q{block}_i{item}_{y or t}
  long_df <- tidyr::separate(
    long_df, "temp",
    into = c("block", "item", "t_or_y"),
    sep = "_",
    extra = "merge"
  )

  # Clean up t_or_y: "y" or "t"
  long_df$t_or_y <- sub("^.*_", "", long_df$t_or_y)

  long_df <- tidyr::pivot_wider(
    long_df,
    names_from = "t_or_y",
    values_from = "value"
  )

  # Convert y to numeric and recode
  long_df$y <- as.numeric(long_df$y)
  long_df <- dplyr::mutate(
    long_df,
    y = dplyr::case_when(
      y == 1 ~ 1,
      y == -1 ~ -1,
      y == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

  # Remove item number column, keep label
  long_df <- dplyr::select(long_df, -dplyr::any_of("item"))
  long_df$message <- long_df$t

  # Drop NAs (items not shown)
  long_df <- long_df[!is.na(long_df$y), ]

  # Add weight if present — keeps the original column name so downstream

  # functions can reference it via weight_col = "weight" (or whatever it is)
  if (!is.null(weight_col) && weight_col %in% names(survey_df)) {
    wgt_df <- stats::setNames(
      data.frame(survey_df[[id_col]], survey_df[[weight_col]]),
      c("pid", weight_col)
    )
    wgt_df <- wgt_df[!duplicated(wgt_df$pid), ]
    long_df <- dplyr::left_join(long_df, wgt_df, by = "pid")
  }

  list(wide = wide_df, long = long_df)
}
