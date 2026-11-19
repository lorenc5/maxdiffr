#' Analytical Estimation Multinomial Logit (AE-MNL)
#'
#' Computes Best-Worst beta coefficients, standard errors, choice probabilities,
#' and confidence intervals from aggregate best/worst counts. Replaces the
#' `bwsTools::ae_mnl` dependency.
#'
#' Zero counts are guarded with a 0.5 continuity correction.
#'
#' @param agg_df Data frame with aggregate counts per item.
#' @param all_col Character name of column with total appearance count.
#' @param best_col Character name of column with best-choice count.
#' @param worst_col Character name of column with worst-choice count.
#' @param ci_level Confidence level for intervals (default 0.95).
#'
#' @return Data frame with columns: `b` (beta), `se`, `choice_p`, `lb`, `ub`.
#'
#' @examples
#' # Simple example with 3 items
#' df <- data.frame(
#'   item = c("A", "B", "C"),
#'   all = c(100, 100, 100),
#'   bests = c(60, 30, 10),
#'   worsts = c(10, 30, 60)
#' )
#' ae_mnl(df, "all", "bests", "worsts")
#'
#' @export
ae_mnl <- function(agg_df, all_col = "all", best_col = "bests",
                   worst_col = "worsts", ci_level = 0.95) {

  best_count <- agg_df[[best_col]]
  worst_count <- agg_df[[worst_col]]

  # Continuity correction for zero counts
  best_count <- ifelse(best_count == 0, 0.5, best_count)
  worst_count <- ifelse(worst_count == 0, 0.5, worst_count)

  # Beta = ln(B/W)
  beta <- log(best_count / worst_count)

  # SE = sqrt(1/B + 1/W)
  se <- sqrt(1 / best_count + 1 / worst_count)

  # Choice probability via softmax
  choice_p <- .softmax(beta)

  # Confidence interval
  z_val <- stats::qnorm(1 - (1 - ci_level) / 2)
  lb <- beta - z_val * se
  ub <- beta + z_val * se

  data.frame(b = beta, se = se, choice_p = choice_p, lb = lb, ub = ub)
}
