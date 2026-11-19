##############################################
# Generate anonymous toy datasets for maxdiffr
# Based on real MaxDiff survey structure with
# jittered/shuffled responses for anonymity
##############################################

library(haven)
library(ibd)
set.seed(42)

# ---- 1. Generate a fresh design (not the real one) ---- #

n_items <- 8
n_blocks <- 8
items_per_block <- 4
n_versions <- 2
n_respondents <- 150
v_per <- n_respondents / n_versions

item_labels <- c(
  "LOWER COST", "MORE JOBS", "CLEAN ENERGY",
  "BETTER SCHOOLS", "SAFE STREETS", "TAX RELIEF",
  "HEALTH ACCESS", "LOCAL CONTROL"
)

# Generate IBD per version
all_versions <- list()
seeds <- c(99901, 99902)

for (ver in 1:n_versions) {
  set.seed(seeds[ver])
  ib <- ibd::ibd(n_items, n_blocks, items_per_block, pbar = FALSE)
  ib_des <- ib$design

  ib_mat <- matrix(NA, ncol = items_per_block, nrow = nrow(ib_des))
  for (i in 1:nrow(ib_des)) {
    ib_mat[i, ] <- sample(ib_des[i, ], items_per_block)
  }
  ib_mat <- cbind(Block = 1:n_blocks, ib_mat)
  colnames(ib_mat) <- c("Block", paste0("Item", 1:items_per_block))

  pid_offset <- (ver - 1) * v_per
  ver_list <- list()
  for (i in 1:v_per) {
    ver_list[[i]] <- data.frame(pid = i + pid_offset, ib_mat, check.names = FALSE)
  }
  ver_df <- do.call(rbind, ver_list)
  ver_df <- data.frame(version = ver, ver_df, check.names = FALSE)
  all_versions[[ver]] <- ver_df
}

md_design <- do.call(rbind, all_versions)
md_design <- data.frame(card = 1:nrow(md_design), md_design, check.names = FALSE)

# ---- 2. Simulate survey responses ---- #

# Item "true scores" (latent attractiveness) — narrow range so anchored
# scores stay realistic (top performer ~150-200, bottom ~30-70).
# Large per-trial noise ensures choices are noisy, keeping betas moderate.
true_scores <- c(0.18, 0.10, 0.28, 0.04, -0.04, -0.10, 0.22, 0.00)

survey_rows <- list()

for (resp in 1:n_respondents) {
  ver <- ifelse(resp <= v_per, 1, 2)
  des_ver <- md_design[md_design$version == ver, ]
  des_ver <- des_ver[!duplicated(des_ver$Block), ]

  row <- list(id = resp, version = ver)

  # Individual noise (per-respondent variation in item preference)
  ind_noise <- rnorm(n_items, 0, 0.2)

  for (j in 1:n_blocks) {
    block_row <- des_ver[des_ver$Block == j, ]
    shown <- as.numeric(block_row[1, paste0("Item", 1:items_per_block)])

    # Score each shown item with noise (high trial noise = realistic noisy choices)
    scores <- true_scores[shown] + ind_noise[shown] + rnorm(items_per_block, 0, 0.7)

    # Best = highest score, Worst = lowest score
    best_item <- shown[which.max(scores)]
    worst_item <- shown[which.min(scores)]

    # Make sure best != worst (shouldn't happen with continuous scores, but safety)
    if (best_item == worst_item) {
      scores[which.min(scores)] <- scores[which.min(scores)] - 1
      worst_item <- shown[which.min(scores)]
    }

    row[[paste0("QMAXDIFF_", j, "b")]] <- best_item
    row[[paste0("QMAXDIFF_", j, "w")]] <- worst_item

    # Anchor: based on mean score of shown items
    # Thresholds centered around expected mean (~0.085) for balanced ALL/NONE
    mean_score <- mean(true_scores[shown] + ind_noise[shown])
    anchor_noise <- rnorm(1, 0, 0.2)
    combined <- mean_score + anchor_noise
    if (combined > 0.22) {
      anchor_resp <- 1 # ALL important
    } else if (combined < -0.05) {
      anchor_resp <- 3 # NONE important
    } else {
      anchor_resp <- 2 # SOME
    }
    row[[paste0("QMAXDIFF_", j, "_anchor")]] <- anchor_resp
  }

  # Add a party ID variable (1-7 scale)
  row[["QPARTYID"]] <- sample(1:7, 1, prob = c(0.2, 0.15, 0.1, 0.1, 0.1, 0.15, 0.2))

  survey_rows[[resp]] <- as.data.frame(row, stringsAsFactors = FALSE)
}

md_survey <- do.call(rbind, survey_rows)

# ---- 3. Save as package data ---- #

usethis::use_data(md_survey, overwrite = TRUE)
usethis::use_data(md_design, overwrite = TRUE)

# Also save item_labels as a character vector
md_items <- data.frame(
  item_num = 1:n_items,
  item_label = item_labels,
  stringsAsFactors = FALSE
)
usethis::use_data(md_items, overwrite = TRUE)

cat("Done. Created: md_survey, md_design, md_items\n")
