scr <- md_screens(md_survey, md_design,
  n_blocks = 8, n_items = 8, items_per_block = 4,
  item_labels = md_items$item_label,
  anchor_prefix = "QMAXDIFF_", anchor_suffix = "_anchor"
)

test_that("md_screens returns one row per respondent x screen", {
  expect_s3_class(scr, "md_screens")
  expect_equal(nrow(scr), nrow(md_survey) * 8)
  expect_true(all(scr$valid))
  expect_setequal(unique(scr$anchor), c("all", "some", "none"))
  expect_equal(attr(scr, "item_labels"), md_items$item_label)
})

test_that("implied_pairs coding produces the expected tasks", {
  tk <- md_anchor_tasks(scr, "implied_pairs")
  n_s <- nrow(scr)
  expect_equal(length(unique(tk$choice$task)), 2 * n_s)
  expect_true(all(tapply(tk$choice$chosen, tk$choice$task, sum) == 1))
  n_pairs <- 4 * sum(scr$anchor %in% c("all", "none")) + 2 * sum(scr$anchor == "some")
  expect_equal(nrow(tk$pairs), n_pairs)
  expect_equal(sum(tk$pairs$win), 4 * sum(scr$anchor == "all") + sum(scr$anchor == "some"))
})

test_that("dual_response coding adds the threshold as an alternative", {
  tk <- md_anchor_tasks(scr, "dual_response")
  thr <- length(md_items$item_label) + 1
  expect_equal(nrow(tk$pairs), 0)
  expect_true(all(table(tk$choice$task) == 5))
  best_thr <- tk$choice$alt == thr & tk$choice$chosen == 1 & tk$choice$sign == 1
  worst_thr <- tk$choice$alt == thr & tk$choice$chosen == 1 & tk$choice$sign == -1
  expect_equal(sum(best_thr), sum(scr$anchor == "none"))
  expect_equal(sum(worst_thr), sum(scr$anchor == "all"))
})

test_that("md_anchor_logit returns one row per message with ordered intervals", {
  fit <- md_anchor_logit(scr)
  expect_equal(nrow(fit), 8)
  expect_true(all(fit$lo < fit$score100 & fit$score100 < fit$hi))
  expect_equal(fit$score100, 100 * exp(fit$b))
  sub <- md_anchor_logit(scr, ids = md_survey$id[md_survey$QPARTYID <= 2])
  expect_equal(nrow(sub), 8)
})

test_that("md_prep anchor_coding = 'keep_choices' keeps picks on ALL/NONE screens", {
  args <- list(md_survey, md_design,
    n_blocks = 8, n_items = 8, items_per_block = 4,
    item_labels = md_items$item_label,
    anchor_prefix = "QMAXDIFF_", anchor_suffix = "_anchor"
  )
  drop <- do.call(md_prep, args)$long
  keep <- do.call(md_prep, c(args, anchor_coding = "keep_choices"))$long
  items <- drop$message != "ANCHOR"
  n_all <- sum(scr$anchor == "all")
  n_none <- sum(scr$anchor == "none")
  expect_equal(sum(keep$y[items] == 1) - sum(drop$y[items] == 1), n_all)
  expect_equal(sum(keep$y[items] == -1) - sum(drop$y[items] == -1), n_none)
  expect_equal(keep$y[!items], drop$y[!items])
})

test_that("md_anchor_patterns summarizes anchor answers", {
  p <- md_anchor_patterns(scr)
  expect_equal(sum(p$patterns), 100)
  expect_equal(nrow(p$respondents), nrow(md_survey))
  expect_equal(nrow(p$exposure), 8)
  expect_true(all(abs(rowSums(p$exposure[, c("moved", "not_moved", "undetermined")]) - 100) < 1e-8))
})

test_that("md_hb_data indexes the utility matrix correctly", {
  sd <- md_hb_data(scr)
  d <- sd$data
  expect_equal(d$N, nrow(md_survey))
  expect_equal(d$K, 4)
  expect_true(all(d$idxA <= d$N * (d$J + 1)))
  expect_true(all(d$idxB <= d$N * d$J))
  expect_true(all(d$chA %in% 1:4))
})

test_that("HB summaries and TURF work on a fitted object", {
  # Fake posterior: 50 draws, all respondents, utilities from a fixed matrix
  set.seed(1)
  N <- 20
  J <- 3
  beta <- array(rnorm(50 * N * J), c(50, N, J))
  hb <- structure(list(
    beta = beta, pid = seq_len(N), item_labels = c("A", "B", "C"),
    coding = "implied_pairs", diag = list(max_rhat = 1, min_neff = 100, divergent = 0)
  ), class = "md_hb")
  s <- md_hb_summary(hb, groups = list(all = 1:N, half = 1:10))
  expect_equal(nrow(s), 6)
  expect_equal(s$pct_above[1], 100 * mean(beta[, , 1] > 0))
  tf <- md_hb_turf(hb, combo = 1:3)
  expect_equal(tf$reach[tf$size == 3], attr(tf, "ceiling"))
  expect_true(all(diff(tf$reach[tf$rank == 1]) >= 0))
  sc <- md_hb_scores(hb)
  expect_equal(nrow(sc), N * J)
})

test_that("md_hb fits the Stan model", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not(identical(Sys.getenv("MAXDIFFR_TEST_HB"), "true"), "set MAXDIFFR_TEST_HB=true")
  fit <- md_hb(scr, chains = 1, iter = 200, refresh = 0)
  expect_s3_class(fit, "md_hb")
  expect_equal(dim(fit$beta)[2:3], c(nrow(md_survey), 8))
})
