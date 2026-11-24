#' Code Anchored MaxDiff Screens as Choice Tasks
#'
#' Turns screen-level responses into the choice tasks used by
#' [md_anchor_logit()] and [md_hb()]. The anchor (threshold) is an extra
#' alternative numbered `n_items + 1` whose utility is fixed at zero, so every
#' message utility is measured relative to the respondent's threshold.
#'
#' Two codings are available:
#' \describe{
#'   \item{`"implied_pairs"`}{Best and worst among the items shown, plus the
#'     message-vs-threshold comparisons the anchor answer implies. ALL: each
#'     item shown beats the threshold. NONE: the threshold beats each item.
#'     SOME: the best item beats the threshold and the threshold beats the
#'     worst item.}
#'   \item{`"dual_response"`}{The threshold is added as an extra alternative in
#'     both the best and the worst task (the coding commonly used for dual-
#'     response anchored MaxDiff). ALL: best = original best, worst =
#'     threshold. NONE: best = threshold, worst = original worst. SOME:
#'     original best and worst.}
#' }
#'
#' @param screens Output of [md_screens()] built with `anchor_prefix`.
#' @param coding `"implied_pairs"` (default) or `"dual_response"`.
#'
#' @return A list with
#'   \item{choice}{Long data frame, one row per alternative per task: `task`,
#'     `pid`, `alt` (item number; `n_items + 1` = threshold), `chosen` (0/1)
#'     and `sign` (+1 best task, -1 worst task).}
#'   \item{pairs}{Data frame of binary message-vs-threshold comparisons:
#'     `pid`, `item`, `win` (1 = the message beats the threshold). Empty for
#'     `"dual_response"`.}
#'   \item{n_items, item_labels, coding}{Metadata.}
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
#' tk <- md_anchor_tasks(scr)
#' head(tk$choice)
#' table(tk$pairs$win)
#'
#' @export
md_anchor_tasks <- function(screens, coding = c("implied_pairs", "dual_response")) {
  coding <- match.arg(coding)
  labs <- attr(screens, "item_labels")
  s <- .md_anchored_screens(screens)
  n_items <- length(labs)
  thr <- n_items + 1
  shown <- as.matrix(s[, grep("^i[0-9]+$", names(s))])
  k <- ncol(shown)
  n_s <- nrow(s)

  if (coding == "implied_pairs") {
    alts <- shown
    best <- s$best
    worst <- s$worst
  } else {
    alts <- cbind(shown, thr)
    best <- ifelse(s$anchor == "none", thr, s$best)
    worst <- ifelse(s$anchor == "all", thr, s$worst)
  }
  ka <- ncol(alts)

  # Tasks 1..n_s are best tasks, n_s + 1..2 n_s are worst tasks
  task <- rep(seq_len(2 * n_s), each = ka)
  alt <- as.vector(t(rbind(alts, alts)))
  pick <- rep(c(best, worst), each = ka)
  choice <- data.frame(
    task = task,
    pid = rep(rep(s$pid, 2), each = ka),
    alt = alt,
    chosen = as.integer(alt == pick),
    sign = rep(c(1, -1), each = n_s * ka)
  )

  pairs <- data.frame(pid = s$pid[0], item = integer(0), win = integer(0))
  if (coding == "implied_pairs") {
    is_all <- s$anchor == "all"
    is_none <- s$anchor == "none"
    is_some <- s$anchor == "some"
    pairs <- rbind(
      data.frame(
        pid = rep(s$pid[is_all], each = k),
        item = as.vector(t(shown[is_all, , drop = FALSE])), win = 1L
      ),
      data.frame(
        pid = rep(s$pid[is_none], each = k),
        item = as.vector(t(shown[is_none, , drop = FALSE])), win = 0L
      ),
      data.frame(pid = s$pid[is_some], item = s$best[is_some], win = 1L),
      data.frame(pid = s$pid[is_some], item = s$worst[is_some], win = 0L)
    )
  }
  list(
    choice = choice, pairs = pairs, n_items = n_items,
    item_labels = labs, coding = coding
  )
}


#' Anchored MaxDiff via Conditional Logit
#'
#' Fits an aggregate (pooled) conditional logit to anchored MaxDiff data
#' coded by [md_anchor_tasks()], with the threshold fixed at zero. Standard
#' errors are clustered by respondent, so they account for each respondent
#' answering many screens.
#'
#' @param screens Output of [md_screens()] built with `anchor_prefix`.
#' @param coding `"implied_pairs"` (default) or `"dual_response"`; see
#'   [md_anchor_tasks()].
#' @param ids Optional vector of respondent ids to restrict the fit to (for
#'   example a subgroup).
#' @param level Confidence level for intervals (default 0.95).
#'
#' @return A data frame with one row per message: `message`, `b` (utility
#'   relative to the threshold), `se` (cluster-robust), `score100` (=
#'   100 x exp(b); 100 = as likely as not to beat the threshold), `lo` and
#'   `hi` (interval for `score100`), sorted by `b`.
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
#' md_anchor_logit(scr)
#' md_anchor_logit(scr, coding = "dual_response")
#'
#' @importFrom survival clogit strata cluster coxph Surv
#' @export
md_anchor_logit <- function(screens, coding = c("implied_pairs", "dual_response"),
                            ids = NULL, level = 0.95) {
  coding <- match.arg(coding)
  if (!is.null(ids)) {
    labs <- attr(screens, "item_labels")
    screens <- screens[screens$pid %in% ids, ]
    attr(screens, "item_labels") <- labs
  }
  tk <- md_anchor_tasks(screens, coding)
  J <- tk$n_items

  ch <- tk$choice
  st <- ch$task
  X <- matrix(0, nrow(ch), J)
  is_item <- ch$alt <= J
  X[cbind(which(is_item), ch$alt[is_item])] <- ch$sign[is_item]
  y <- ch$chosen
  cl <- ch$pid

  if (nrow(tk$pairs)) {
    pr <- tk$pairs
    np <- nrow(pr)
    Xp <- matrix(0, 2 * np, J)
    Xp[cbind(seq(1, 2 * np, 2), pr$item)] <- 1 # message row; threshold row = 0
    X <- rbind(X, Xp)
    st <- c(st, max(st) + rep(seq_len(np), each = 2))
    y <- c(y, as.vector(rbind(pr$win, 1L - pr$win)))
    cl <- c(cl, rep(pr$pid, each = 2))
  }
  colnames(X) <- paste0("x", seq_len(J))
  d <- data.frame(y = y, st = st, cl = cl, X)
  f <- stats::as.formula(paste(
    "y ~", paste(colnames(X), collapse = " + "),
    "+ strata(st) + cluster(cl)"
  ))
  m <- clogit(f, data = d, method = "efron")
  b <- stats::coef(m)
  se <- sqrt(diag(stats::vcov(m)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  out <- data.frame(
    message = tk$item_labels, b = unname(b), se = unname(se),
    score100 = 100 * exp(unname(b)),
    lo = 100 * exp(unname(b - z * se)), hi = 100 * exp(unname(b + z * se))
  )
  out <- out[order(-out$b), ]
  rownames(out) <- NULL
  out
}


#' Diagnose Who Moves on the Anchor Question
#'
#' Summarizes how respondents answered the anchor question, which helps judge
#' how robust anchored results are. A large share of respondents who answer
#' NONE (or ALL) on every screen means the threshold is driven by fixed
#' dispositions rather than by the messages.
#'
#' @param screens Output of [md_screens()] built with `anchor_prefix`.
#'
#' @return A list with
#'   \item{screens}{Share of screens answered ALL, SOME and NONE.}
#'   \item{respondents}{One row per respondent: counts of ALL / SOME / NONE
#'     screens and a `pattern` (`"none_all_screens"`, `"all_all_screens"`,
#'     `"some_all_screens"` or `"varied"`).}
#'   \item{patterns}{Share of respondents in each pattern.}
#'   \item{exposure}{Per message, the % of times it was shown that it was
#'     clearly moved (ALL screen, or picked best on a SOME screen), clearly
#'     not moved (NONE screen, or picked worst on a SOME screen) or
#'     undetermined.}
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
#' md_anchor_patterns(scr)$patterns
#'
#' @export
md_anchor_patterns <- function(screens) {
  labs <- attr(screens, "item_labels")
  s <- .md_anchored_screens(screens)
  lev <- c("all", "some", "none")
  a <- factor(s$anchor, lev)
  resp <- as.data.frame.matrix(table(s$pid, a))
  names(resp) <- paste0("n_", lev)
  resp <- data.frame(pid = rownames(resp), resp, row.names = NULL)
  n_tot <- resp$n_all + resp$n_some + resp$n_none
  resp$pattern <- ifelse(resp$n_none == n_tot, "none_all_screens",
    ifelse(resp$n_all == n_tot, "all_all_screens",
      ifelse(resp$n_some == n_tot, "some_all_screens", "varied")
    )
  )
  pat_lev <- c("none_all_screens", "all_all_screens", "some_all_screens", "varied")
  shown <- as.matrix(s[, grep("^i[0-9]+$", names(s))])
  exposure <- do.call(rbind, lapply(seq_along(labs), function(q) {
    x <- s[rowSums(shown == q) > 0, ]
    data.frame(
      message = labs[q], n_shown = nrow(x),
      moved = 100 * mean(x$anchor == "all" | (x$anchor == "some" & x$best == q)),
      not_moved = 100 * mean(x$anchor == "none" | (x$anchor == "some" & x$worst == q)),
      undetermined = 100 * mean(x$anchor == "some" & x$best != q & x$worst != q)
    )
  }))
  list(
    screens = 100 * prop.table(table(a)),
    respondents = resp,
    patterns = 100 * prop.table(table(factor(resp$pattern, pat_lev))),
    exposure = exposure
  )
}
