# maxdiffr 0.2.0

## New features

* Model-based anchoring. Anchored results can be sensitive to how ALL and
  NONE screens are coded, so several alternatives to the counts-based
  approach are now available:
  * `md_prep()` gains `anchor_coding`. `"keep_choices"` keeps the best pick
    on ALL screens and the worst pick on NONE screens. The default,
    `"drop_choices"`, is unchanged.
  * `md_screens()` builds a respondent x screen table (items shown, best,
    worst, anchor answer).
  * `md_anchor_tasks()` codes screens as `"implied_pairs"` or
    `"dual_response"` choice tasks, with the threshold fixed at zero.
  * `md_anchor_logit()` fits a conditional logit with respondent-clustered
    standard errors, overall or for a subgroup.
  * `md_anchor_patterns()` shows who moves on the anchor question (answer
    patterns and per-message exposure).
* Hierarchical Bayes anchored MaxDiff, written in Stan and run through rstan
  (in Suggests; compiled once per session):
  * `md_hb()` fits the model, and `md_hb_data()` returns its Stan data list.
  * `md_hb_summary()` reports the % of respondents above their own
    threshold, plus the typical score, by subgroup.
  * `md_hb_scores()` returns respondent-level utilities and the probability
    each message is above the respondent's threshold.
  * `md_hb_turf()` runs anchored TURF: a respondent is reached when at least
    one message in the set clears their threshold.

## Other changes

* `survival` is now imported.
* Added a testthat suite. The Stan fit test is opt-in via
  `MAXDIFFR_TEST_HB=true`.

# maxdiffr 0.1.0

* Initial release.
