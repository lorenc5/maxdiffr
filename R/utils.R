#' @importFrom questionr wtd.table
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
#' Uses [questionr::wtd.table()] to compute weighted frequencies, then normalizes.
#'
#' @param k Numeric vector of 0s and 1s.
#' @param weight Numeric weight vector.
#' @return Scalar proportion of 1s (weighted).
#' @keywords internal
.weighted_pct <- function(k, weight) {
  wt <- questionr::wtd.table(k, weights = weight)
  if (length(wt) == 0) {
    return(0)
  }
  total <- sum(wt)
  if (total == 0) {
    return(0)
  }
  # Return proportion for the "1" category
  if ("1" %in% names(wt)) {
    return(as.numeric(wt[["1"]]) / total)
  }
  0
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
