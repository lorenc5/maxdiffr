# maxdiffr

A complete pipeline for **MaxDiff (Best-Worst Scaling)** survey analysis in R.

## Installation

```r
# Install from GitHub
devtools::install_github("lorenc5/maxdiffr")
```

## Overview

`maxdiffr` provides a complete pipeline for MaxDiff (Best-Worst Scaling) survey analysis. The package includes simulated data so you can follow along immediately.

The pipeline has six stages:

1. **Design** --- Generate an IBD design for the survey
2. **Validate** --- Check that survey responses match the design
3. **Prep** --- Convert raw survey data to long-format BW data
4. **Analyze** --- Aggregate and individual-level scoring
5. **Anchor** --- Convert to absolute 100-point scale (optional)
6. **TURF** --- Portfolio optimization

## Load the Package and Data

```r
library(maxdiffr)
library(dplyr)

data(md_survey)   # 150 simulated respondents
data(md_design)   # IBD design: 8 items, 8 blocks, 4 items/block, 2 versions
data(md_items)    # Item labels
```

The simulated survey has 150 respondents across 2 design versions. Each respondent sees 8 screens with 4 items each, and picks a best and worst on each screen. An anchor question follows each screen.

## Step 1: Design Generation

If you need to create a new design from scratch, use `md_make_design()`. This wraps `ibd::ibd()` to produce a balanced incomplete block design with per-version randomization.

```r
new_des <- md_make_design(
  n_items = 8, n_blocks = 8, items_per_block = 4,
  n_respondents = 200, n_versions = 2,
  seeds = c(111, 222),
  item_labels = md_items$item_label
)
head(new_des)
```

The design file tells the survey platform which items to show on each screen. Each item appears the same number of times across blocks (balanced), so every item gets equal exposure.

## Step 2: Validate Responses

Before analysis, check that every respondent's best/worst picks are consistent with the items shown on their screen.

```r
val <- md_validate(
  md_survey, md_design,
  id_col = "id", version_col = "version",
  best_prefix = "QMAXDIFF_", best_suffix = "b",
  worst_prefix = "QMAXDIFF_", worst_suffix = "w",
  n_blocks = 8, n_items = 8, items_per_block = 4
)
```

`md_validate()` checks that:

- Best and worst are not NA
- Both are in the item range (1--8)
- Best != worst
- Both are in the set of items shown on that screen (per the design)

If `drop_invalid = TRUE`, it removes respondents with any issues.

## Step 3: Prep Data

`md_prep()` converts the raw survey columns (`QMAXDIFF_1b`, `QMAXDIFF_1w`, etc.) into a long-format data frame suitable for analysis. This is where the package saves the most work --- it replaces what was previously a manual, block-by-block recoding step.

```r
prep <- md_prep(
  md_survey, md_design,
  id_col = "id", version_col = "version",
  best_prefix = "QMAXDIFF_", best_suffix = "b",
  worst_prefix = "QMAXDIFF_", worst_suffix = "w",
  n_blocks = 8, n_items = 8, items_per_block = 4,
  item_labels = md_items$item_label
)

head(prep$long)
```

Each row in the long data represents one item shown to one respondent in one block. The `y` column encodes:

- `1` = chosen as best
- `-1` = chosen as worst
- `0` = shown but not chosen

Items not shown in a given block are excluded (not coded as 0 --- they simply don't appear).

## Step 4a: Aggregate Analysis

`md_aggregate()` computes best/worst counts per item and fits the **Analytical Estimation Multinomial Logit (AE-MNL)** model. This is a closed-form estimator for MaxDiff data that avoids iterative MLE.

### The AE-MNL Model

The beta coefficient for each item *i* is the log-ratio of best to worst counts:

**beta_i = ln(B_i / W_i)**

where *B_i* and *W_i* are the number of times item *i* was chosen as best and worst, respectively. A continuity correction of 0.5 is applied when either count is zero.

The standard error is:

**SE_i = sqrt(1/B_i + 1/W_i)**

Betas are converted to choice probabilities via the **softmax** function, which gives the probability that item *i* would be chosen as best if all items were shown simultaneously:

**P_i = exp(beta_i) / sum(exp(beta_k)) for all k**

These probabilities sum to 1 across all items. A normalized best-worst score is also computed:

**NBW_i = (B_i - W_i) / N_i**

where *N_i* is the total number of times item *i* was shown.

```r
agg <- md_aggregate(
  prep$long,
  id_col = "pid", item_col = "message", response_col = "y"
)
agg |> select(message, bests, worsts, nbw, b, choice_p)
```

**Interpreting the output:**

- `nbw` --- normalized best-worst score; ranges from -1 to +1
- `b` --- AE-MNL beta; log-odds scale, centered at 0
- `choice_p` --- softmax probability; sums to 1 across all items

```r
md_plot_beta(agg, title = "Aggregate MaxDiff Results")
```

## Step 4b: Individual-Level Scoring

`md_ebayes()` computes individual-level scores using empirical Bayes shrinkage. Each respondent gets a score per item, pulled toward the aggregate to stabilize estimates from sparse per-person data.

```r
eb_long <- md_ebayes(
  prep$long,
  id_col = "pid", block_col = "block",
  item_col = "message", response_col = "y"
)
head(eb_long)

# Wide format: one row per respondent, one column per item
eb_wide <- md_ebayes(
  prep$long,
  id_col = "pid", block_col = "block",
  item_col = "message", response_col = "y",
  wide = TRUE
)
eb_wide |> select(1:5) |> head()
```

The `alpha` parameter controls shrinkage strength. Higher alpha pulls individual scores more toward the aggregate (default 0.1).

## Step 5: Anchored MaxDiff

Standard MaxDiff produces **relative** scores --- you know item A beats item B, but not whether respondents actually like either one. Anchoring adds an absolute reference point.

After each screen, respondents answer: "Are ALL, SOME, or NONE of these important?" This gets coded as an ANCHOR pseudo-item in the data.

### Anchored Prep

Pass the `anchor_prefix` and `anchor_suffix` arguments to `md_prep()`:

```r
prep_anc <- md_prep(
  md_survey, md_design,
  id_col = "id", version_col = "version",
  best_prefix = "QMAXDIFF_", best_suffix = "b",
  worst_prefix = "QMAXDIFF_", worst_suffix = "w",
  n_blocks = 8, n_items = 8, items_per_block = 4,
  item_labels = md_items$item_label,
  anchor_prefix = "QMAXDIFF_",
  anchor_suffix = "_anchor"
)

# ANCHOR now appears as an item
table(prep_anc$long$message)
```

### Anchored Aggregate + 100-Point Scale

```r
agg_anc <- md_aggregate(
  prep_anc$long,
  id_col = "pid", item_col = "message", response_col = "y"
)

a100 <- md_anchor_100(agg_anc, anchor_label = "ANCHOR")
a100 |> arrange(desc(thresh100)) |> select(message, thresh100, cut)
```

The ANCHOR item is always 100. Items above 100 are considered genuinely important by respondents; items below 100 are not.

```r
md_plot_anchor(a100, sample_label = "Simulated Data")
```

### Individual Anchored Scores

```r
eb_anc <- md_ebayes(
  prep_anc$long,
  id_col = "pid", block_col = "block",
  item_col = "message", response_col = "y",
  skip_checks = TRUE  # required for anchored data
)

ind_a100 <- md_ind_anchor_100(eb_anc, id_col = "pid", item_col = "message")
ind_a100 |> select(1:5) |> head()
```

## Step 6: TURF Analysis

TURF (Total Unduplicated Reach and Frequency) answers: "If I can only use N messages, which combination reaches the most people?"

```r
# Build binary matrix: 1 = this item is the respondent's top-scoring item
turf <- md_turf_matrix(eb_wide, id_col = "pid")

# Single item reach
md_portfolio(turf, combo = 1, id_col = "pid")

# Best pairs
md_portfolio(turf, combo = 2, id_col = "pid", top_n = 5)

# Best trios
md_portfolio(turf, combo = 3, id_col = "pid", top_n = 5)
```

```r
md_plot_turf(turf, id_col = "pid", title = "Cumulative Reach")
```

## Subgroup Analysis

To analyze subgroups, filter the survey before calling `md_prep()`:

```r
# Democrats (QPARTYID 1-2)
dem_survey <- md_survey |> filter(QPARTYID %in% 1:2)

dem_prep <- md_prep(
  dem_survey, md_design,
  id_col = "id", version_col = "version",
  best_prefix = "QMAXDIFF_", best_suffix = "b",
  worst_prefix = "QMAXDIFF_", worst_suffix = "w",
  n_blocks = 8, n_items = 8, items_per_block = 4,
  item_labels = md_items$item_label
)

dem_agg <- md_aggregate(dem_prep$long, id_col = "pid",
                        item_col = "message", response_col = "y")
dem_agg |> arrange(desc(b)) |> select(message, b, choice_p)
```

## Weighted Analysis

If you have survey weights, pass them to `md_aggregate()` and `md_ebayes()`:

```r
# Example (not run --- simulated data has no weights)
wgt_df <- data.frame(pid = survey$id, WEIGHT = survey$weight_var)

agg_w <- md_aggregate(prep$long, id_col = "pid", item_col = "message",
                      response_col = "y", weight_df = wgt_df)

eb_w <- md_ebayes(prep$long, id_col = "pid", block_col = "block",
                  item_col = "message", response_col = "y",
                  weight_df = wgt_df, wide = TRUE)
```

## Function Reference

| Step | Function | What it does |
|------|----------|-------------|
| Design | `md_make_design()` | Generate IBD survey design |
| Validate | `md_validate()` | Check responses against design |
| Prep | `md_prep()` | Raw survey to long-format BW data |
| Aggregate | `md_aggregate()` | Item-level betas + choice probabilities |
| Individual | `md_ebayes()` | Per-respondent empirical Bayes scores |
| Anchor | `md_anchor_100()` | Absolute 100-point scale (aggregate) |
| Anchor | `md_ind_anchor_100()` | Absolute 100-point scale (individual) |
| TURF | `md_turf_matrix()` | Binary reach matrix |
| TURF | `md_portfolio()` | Portfolio optimization |
| Plot | `md_plot_beta()` | Beta dot-and-whisker |
| Plot | `md_plot_anchor()` | Anchored score dot plot |
| Plot | `md_plot_turf()` | Cumulative reach curve |

## License

MIT
