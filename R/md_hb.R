#' Hierarchical Bayes Anchored MaxDiff
#'
#' Fits a hierarchical Bayes (HB) multinomial logit to anchored MaxDiff data,
#' giving every respondent their own utility for each message relative to their
#' own threshold (fixed at 0). Respondent utilities are drawn from a
#' multivariate normal population with mean `mu`, scales `tau` and an LKJ(2)
#' correlation prior (non-centered parameterization).
#'
#' The model is written in Stan (`inst/stan/hb_mnl_anchor.stan`). `rstan`
#' translates it to C++ and compiles it the first time `md_hb()` is called in a
#' session (about a minute); the compiled model is reused afterwards. Sampling
#' runs as compiled C++ (NUTS), and R only prepares the data and collects the
#' draws. Requires the `rstan` package.
#'
#' @param screens Output of [md_screens()] built with `anchor_prefix`.
#' @param coding `"implied_pairs"` (default) or `"dual_response"`; see
#'   [md_anchor_tasks()].
#' @param chains,iter,warmup,seed Passed to `rstan::sampling()`. `iter`
#'   includes warmup.
#' @param cores Number of cores for parallel chains (default
#'   `getOption("mc.cores", 1)`).
#' @param keep_fit If `TRUE`, keep the full `stanfit` object in `$fit`.
#' @param ... Further arguments to `rstan::sampling()` (e.g. `control`).
#'
#' @return An object of class `md_hb`: a list with `beta` (array of draws x
#'   respondents x messages), `mu` (draws x messages), `pid`, `item_labels`,
#'   `coding`, `diag` (`max_rhat`, `min_neff`, `divergent`) and optionally
#'   `fit`.
#'
#' @seealso [md_hb_summary()], [md_hb_scores()], [md_hb_turf()]
#'
#' @examples
#' \dontrun{
#' data(md_survey)
#' data(md_design)
#' data(md_items)
#' scr <- md_screens(md_survey, md_design,
#'   n_blocks = 8, n_items = 8, items_per_block = 4,
#'   item_labels = md_items$item_label,
#'   anchor_prefix = "QMAXDIFF_", anchor_suffix = "_anchor"
#' )
#' hb <- md_hb(scr, chains = 4, iter = 1000, cores = 4)
#' hb
#' md_hb_summary(hb)
#' }
#'
#' @export
md_hb <- function(screens, coding = c("implied_pairs", "dual_response"),
                  chains = 4, iter = 1500, warmup = floor(iter / 2),
                  seed = 1234, cores = getOption("mc.cores", 1L),
                  keep_fit = FALSE, ...) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("md_hb() requires the 'rstan' package: install.packages(\"rstan\")")
  }
  coding <- match.arg(coding)
  sd <- md_hb_data(screens, coding)
  mod <- .md_stan_model()
  fit <- rstan::sampling(mod,
    data = sd$data, chains = chains, iter = iter, warmup = warmup,
    seed = seed, cores = cores, pars = c("mu", "tau", "beta"), ...
  )
  summ <- rstan::summary(fit)$summary
  out <- list(
    beta = rstan::extract(fit, "beta")$beta,
    mu = rstan::extract(fit, "mu")$mu,
    pid = sd$pid, item_labels = sd$item_labels, coding = coding,
    summary = rstan::summary(fit, pars = c("mu", "tau"))$summary,
    diag = list(
      max_rhat = max(summ[, "Rhat"], na.rm = TRUE),
      min_neff = min(summ[, "n_eff"], na.rm = TRUE),
      divergent = sum(rstan::get_divergent_iterations(fit))
    ),
    fit = if (keep_fit) fit else NULL
  )
  class(out) <- "md_hb"
  out
}

#' Stan Data for the HB Anchored Model
#'
#' Builds the data list passed to the Stan model used by [md_hb()]. Exposed
#' for users who want to fit the model themselves (e.g. with `cmdstanr`).
#'
#' @inheritParams md_hb
#' @return A list with `data` (the Stan data list), `pid` (respondent ids in
#'   model order) and `item_labels`.
#' @export
md_hb_data <- function(screens, coding = c("implied_pairs", "dual_response")) {
  coding <- match.arg(coding)
  tk <- md_anchor_tasks(screens, coding)
  pid <- unique(tk$choice$pid)
  N <- length(pid)
  J <- tk$n_items
  # Flat index into the column-major [N x (J + 1)] utility matrix
  flat <- function(p, item) (item - 1) * N + match(p, pid)
  ch <- tk$choice
  K <- unique(table(ch$task))
  if (length(K) != 1) stop("All choice tasks must have the same number of alternatives")
  ch <- ch[order(ch$task), ]
  idxA <- matrix(flat(ch$pid, ch$alt), ncol = K, byrow = TRUE)
  chA <- as.integer(t(matrix(ch$chosen, nrow = K)) %*% seq_len(K))
  sA <- ch$sign[seq(1, nrow(ch), by = K)]
  pr <- tk$pairs
  list(
    data = list(
      N = N, J = J, TA = nrow(idxA), K = K, idxA = idxA, chA = chA, sA = sA,
      TB = nrow(pr), idxB = as.array(as.integer(flat(pr$pid, pr$item))),
      yB = as.array(as.integer(pr$win))
    ),
    pid = pid, item_labels = tk$item_labels
  )
}

.md_cache <- new.env(parent = emptyenv())

.md_stan_model <- function() {
  if (is.null(.md_cache$hb_model)) {
    f <- system.file("stan", "hb_mnl_anchor.stan", package = "maxdiffr")
    if (!nzchar(f)) stop("Stan model file not found in maxdiffr installation")
    message("Compiling the Stan model (once per session; about a minute)...")
    .md_cache$hb_model <- rstan::stan_model(file = f, model_name = "hb_mnl_anchor")
  }
  .md_cache$hb_model
}

#' @export
print.md_hb <- function(x, ...) {
  d <- dim(x$beta)
  cat("Hierarchical Bayes anchored MaxDiff (", x$coding, " coding)\n", sep = "")
  cat("  respondents:", d[2], " messages:", d[3], " draws:", d[1], "\n")
  cat(
    "  max R-hat:", round(x$diag$max_rhat, 3),
    " min n_eff:", round(x$diag$min_neff),
    " divergent:", x$diag$divergent, "\n"
  )
  invisible(x)
}


#' Summarize an HB Anchored Fit, Overall and by Subgroup
#'
#' For each group and message, reports the share of respondents whose
#' utility for the message is above their own threshold (`pct_above`, the
#' share of voters/respondents the message moves), with a posterior interval,
#' and the threshold score of the average respondent (`typical` = 100 x
#' exp(mean utility); 100 = threshold).
#'
#' @param hb An [md_hb()] fit.
#' @param groups Optional named list of respondent id vectors. `NULL` gives
#'   the full sample only.
#' @param compare If `TRUE` (default), each group is compared with all other
#'   respondents within every posterior draw; `sig` is 1 / -1 when the
#'   interval of the difference in `pct_above` lies above / below zero.
#' @param level Interval level (default 0.95).
#'
#' @return A data frame with `group`, `n`, `message`, `pct_above`, `pa_lo`,
#'   `pa_hi`, `typical`, `typ_lo`, `typ_hi` and (when `compare`) `sig`.
#' @export
md_hb_summary <- function(hb, groups = NULL, compare = TRUE, level = 0.95) {
  if (is.null(groups)) groups <- list(overall = hb$pid)
  if (is.null(names(groups))) names(groups) <- paste0("group", seq_along(groups))
  q <- c((1 - level) / 2, 1 - (1 - level) / 2)
  above <- hb$beta > 0
  pa_draws <- function(ii) {
    sapply(seq_len(dim(above)[3]), function(j) rowMeans(above[, ii, j, drop = FALSE]))
  }
  do.call(rbind, lapply(names(groups), function(g) {
    ii <- which(hb$pid %in% groups[[g]])
    if (!length(ii)) stop("Group '", g, "' has no respondents in the fit")
    pa <- pa_draws(ii)
    typ <- apply(hb$beta[, ii, , drop = FALSE], c(1, 3), mean)
    out <- data.frame(
      group = g, n = length(ii), message = hb$item_labels,
      pct_above = 100 * colMeans(pa),
      pa_lo = 100 * apply(pa, 2, stats::quantile, q[1]),
      pa_hi = 100 * apply(pa, 2, stats::quantile, q[2]),
      typical = 100 * exp(colMeans(typ)),
      typ_lo = 100 * exp(apply(typ, 2, stats::quantile, q[1])),
      typ_hi = 100 * exp(apply(typ, 2, stats::quantile, q[2]))
    )
    if (compare) {
      rest <- setdiff(seq_along(hb$pid), ii)
      out$sig <- 0L
      if (length(rest)) {
        d <- pa - pa_draws(rest)
        lo <- apply(d, 2, stats::quantile, q[1])
        hi <- apply(d, 2, stats::quantile, q[2])
        out$sig <- ifelse(lo > 0, 1L, ifelse(hi < 0, -1L, 0L))
      }
    }
    out
  }))
}


#' Respondent-Level Scores from an HB Anchored Fit
#'
#' @param hb An [md_hb()] fit.
#'
#' @return A long data frame with one row per respondent x message: `pid`,
#'   `message`, `utility` (posterior mean, relative to the respondent's
#'   threshold), `score100` (= 100 x exp(utility)) and `p_above` (posterior
#'   probability that the message is above the respondent's threshold).
#' @export
md_hb_scores <- function(hb) {
  u <- apply(hb$beta, c(2, 3), mean)
  p <- apply(hb$beta > 0, c(2, 3), mean)
  data.frame(
    pid = rep(hb$pid, times = length(hb$item_labels)),
    message = rep(hb$item_labels, each = length(hb$pid)),
    utility = as.vector(u), score100 = 100 * exp(as.vector(u)),
    p_above = as.vector(p)
  )
}


#' Anchored TURF from an HB Fit
#'
#' Reach of message sets where a respondent is reached if at least one
#' message in the set is above their own threshold. Reach is the expected
#' share over the posterior: for each respondent, 1 minus the product of the
#' probabilities that each message in the set fails to clear the threshold,
#' averaged across respondents.
#'
#' @param hb An [md_hb()] fit.
#' @param combo Set sizes to evaluate (default 1:3).
#' @param ids Optional respondent ids (e.g. a subgroup).
#' @param top_n Number of best sets to return per size (default 3).
#'
#' @return A data frame with `size`, `rank`, `portfolio` (messages joined by
#'   " + ") and `reach` (%). The reach of all messages combined (the
#'   ceiling) is in `attr(x, "ceiling")`.
#' @export
md_hb_turf <- function(hb, combo = 1:3, ids = NULL, top_n = 3) {
  P <- apply(hb$beta > 0, c(2, 3), mean)
  if (!is.null(ids)) P <- P[hb$pid %in% ids, , drop = FALSE]
  J <- ncol(P)
  reach <- function(set) 100 * mean(1 - exp(rowSums(log1p(-P[, set, drop = FALSE]))))
  out <- do.call(rbind, lapply(combo, function(k) {
    cb <- utils::combn(J, k)
    r <- apply(cb, 2, reach)
    o <- order(-r)[seq_len(min(top_n, length(r)))]
    data.frame(
      size = k, rank = seq_along(o),
      portfolio = apply(cb[, o, drop = FALSE], 2, function(s) {
        paste(hb$item_labels[s], collapse = " + ")
      }),
      reach = r[o]
    )
  }))
  attr(out, "ceiling") <- reach(seq_len(J))
  out
}
