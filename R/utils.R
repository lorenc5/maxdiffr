#' @importFrom stats setNames qnorm
#' @importFrom rlang sym .data !!
#' @importFrom dplyr .data desc
NULL

# Suppress R CMD check NOTEs for NSE / ggplot2 aes() variables
utils::globalVariables(c(
  "b", "lb", "ub", "bests", "worsts", "thresh100",
  "n_items", "label", "message", "reach", "cut"
))

#' Weighted percent for a binary (0/1) column
#'
#' Returns the proportion of 1s, handling edge cases where all values are 0 or 1.
#'
#' @param k Numeric vector of 0s and 1s.
#' @param weight Numeric weight vector.
#' @return Scalar proportion of 1s (weighted).
#' @keywords internal
.weighted_pct <- function(k, weight) {
  total <- sum(weight)
  if (total == 0) {
    return(0)
  }
  sum(weight[k == 1]) / total
}

#' Weighted frequency table
#'
#' Replacement for `questionr::wtd.table()` that sums weights by factor level.
#'
#' @param x Factor or character vector.
#' @param weights Numeric weight vector, same length as `x`.
#' @return Named numeric vector of weighted counts.
#' @keywords internal
.wtd_table <- function(x, weights) {
  tapply(weights, x, sum)
}

#' Softmax transform
#'
#' @param x Numeric vector.
#' @return Numeric vector of softmax probabilities.
#' @keywords internal
.softmax <- function(x) {
  ex <- exp(x - max(x))
  ex / sum(ex)
}
