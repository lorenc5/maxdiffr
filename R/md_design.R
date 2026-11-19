#' Generate MaxDiff IBD Design
#'
#' Wraps [ibd::ibd()] to generate incomplete block designs for MaxDiff surveys.
#' Produces one design per version with per-version randomization of item order
#' within blocks.
#'
#' @param n_items Number of items (default 12).
#' @param n_blocks Number of blocks/screens per respondent (default 12).
#' @param items_per_block Number of items shown per screen (default 4).
#' @param n_respondents Total number of respondents (default 1000).
#' @param n_versions Number of design versions (default 4).
#' @param seeds Integer vector of seeds, one per version. If NULL, uses
#'   sequential seeds from 12345.
#' @param item_labels Character vector of item labels (length `n_items`). If
#'   NULL, items are labeled "Item_1" through "Item_n".
#'
#' @return A data frame with columns: `card`, `version`, `pid`, `Block`,
#'   `Item1` through `Item{items_per_block}`, and optionally label columns.
#'
#' @examples
#' # Generate a small design: 6 items, 6 blocks, 3 per block, 2 versions
#' des <- md_make_design(
#'   n_items = 6, n_blocks = 6, items_per_block = 3,
#'   n_respondents = 100, n_versions = 2,
#'   seeds = c(111, 222),
#'   item_labels = paste("MSG", LETTERS[1:6])
#' )
#' head(des)
#' table(des$version)
#'
#' @export
#' @importFrom ibd ibd
md_make_design <- function(n_items = 12, n_blocks = 12, items_per_block = 4,
                           n_respondents = 1000, n_versions = 4,
                           seeds = NULL, item_labels = NULL) {
  if (is.null(seeds)) {
    seeds <- 12345 + seq_len(n_versions)
  }
  stopifnot(length(seeds) == n_versions)

  if (is.null(item_labels)) {
    item_labels <- paste0("Item_", seq_len(n_items))
  }
  stopifnot(length(item_labels) == n_items)

  v_per <- n_respondents %/% n_versions
  pid_start <- c(0, v_per * seq_len(n_versions - 1))

  all_versions <- list()

  for (ver in seq_len(n_versions)) {
    set.seed(seeds[ver])
    ib <- ibd::ibd(n_items, n_blocks, items_per_block, pbar = FALSE)
    ib <- ib$design

    # Randomize item order within each block
    ib_mat <- matrix(NA, ncol = items_per_block, nrow = nrow(ib))
    for (i in seq_len(nrow(ib))) {
      ib_mat[i, ] <- sample(ib[i, ], items_per_block)
    }

    ib_mat <- cbind(Block = seq_len(n_blocks), ib_mat)
    item_cols <- paste0("Item", seq_len(items_per_block))
    colnames(ib_mat) <- c("Block", item_cols)

    # Replicate for each respondent in this version
    ver_list <- lapply(seq_len(v_per), function(i) {
      data.frame(pid = i + pid_start[ver], ib_mat, check.names = FALSE)
    })

    ver_df <- dplyr::bind_rows(ver_list)
    ver_df$version <- ver
    all_versions[[ver]] <- ver_df
  }

  des_mat <- dplyr::bind_rows(all_versions)
  des_mat <- data.frame(
    card = seq_len(nrow(des_mat)),
    version = des_mat$version,
    des_mat[, setdiff(names(des_mat), "version")],
    check.names = FALSE
  )

  # Add label columns
  for (ic in item_cols) {
    des_mat[[paste0(ic, "_label")]] <- item_labels[des_mat[[ic]]]
  }

  des_mat
}
