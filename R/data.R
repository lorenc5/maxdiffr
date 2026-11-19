#' Simulated MaxDiff Survey Data
#'
#' A simulated survey dataset with 150 respondents and 8 items across 8 blocks
#' with 4 items per block. Responses were generated from latent utility scores
#' with added noise. Includes best/worst choices, anchor responses, and party ID.
#'
#' @format A data frame with 150 rows and 27 columns:
#' \describe{
#'   \item{id}{Respondent ID (1-150).}
#'   \item{version}{Design version (1-2).}
#'   \item{QMAXDIFF_1b}{Best-choice item number for block 1.}
#'   \item{QMAXDIFF_1w}{Worst-choice item number for block 1.}
#'   \item{QMAXDIFF_1_anchor}{Anchor response for block 1 (1 = ALL, 2 = SOME, 3 = NONE).}
#'   \item{QMAXDIFF_2b}{Best-choice item number for block 2.}
#'   \item{QMAXDIFF_2w}{Worst-choice item number for block 2.}
#'   \item{QMAXDIFF_2_anchor}{Anchor response for block 2.}
#'   \item{QMAXDIFF_3b}{Best-choice item number for block 3.}
#'   \item{QMAXDIFF_3w}{Worst-choice item number for block 3.}
#'   \item{QMAXDIFF_3_anchor}{Anchor response for block 3.}
#'   \item{QMAXDIFF_4b}{Best-choice item number for block 4.}
#'   \item{QMAXDIFF_4w}{Worst-choice item number for block 4.}
#'   \item{QMAXDIFF_4_anchor}{Anchor response for block 4.}
#'   \item{QMAXDIFF_5b}{Best-choice item number for block 5.}
#'   \item{QMAXDIFF_5w}{Worst-choice item number for block 5.}
#'   \item{QMAXDIFF_5_anchor}{Anchor response for block 5.}
#'   \item{QMAXDIFF_6b}{Best-choice item number for block 6.}
#'   \item{QMAXDIFF_6w}{Worst-choice item number for block 6.}
#'   \item{QMAXDIFF_6_anchor}{Anchor response for block 6.}
#'   \item{QMAXDIFF_7b}{Best-choice item number for block 7.}
#'   \item{QMAXDIFF_7w}{Worst-choice item number for block 7.}
#'   \item{QMAXDIFF_7_anchor}{Anchor response for block 7.}
#'   \item{QMAXDIFF_8b}{Best-choice item number for block 8.}
#'   \item{QMAXDIFF_8w}{Worst-choice item number for block 8.}
#'   \item{QMAXDIFF_8_anchor}{Anchor response for block 8.}
#'   \item{QPARTYID}{Party identification (1-7 scale; 1-2 = Dem, 3-5 = Ind, 6-7 = Rep).}
#' }
#'
#' @examples
#' data(md_survey)
#' head(md_survey)
#' table(md_survey$version)
"md_survey"


#' Simulated MaxDiff Design File
#'
#' An incomplete block design (IBD) for 8 items, 8 blocks, 4 items per block,
#' across 2 versions with 75 respondents per version. Generated via
#' [ibd::ibd()].
#'
#' @format A data frame with 1200 rows and 8 columns:
#' \describe{
#'   \item{card}{Unique row ID.}
#'   \item{version}{Design version (1-2).}
#'   \item{pid}{Respondent ID within the design.}
#'   \item{Block}{Block/screen number (1-8).}
#'   \item{Item1, Item2, Item3, Item4}{Item numbers shown in positions 1-4.}
#' }
#'
#' @examples
#' data(md_design)
#' head(md_design)
"md_design"


#' Simulated MaxDiff Item Labels
#'
#' A lookup table mapping item numbers (1-8) to short text labels for the
#' simulated dataset.
#'
#' @format A data frame with 8 rows and 2 columns:
#' \describe{
#'   \item{item_num}{Item number (1-8).}
#'   \item{item_label}{Short label (e.g., "LOWER COST", "MORE JOBS").}
#' }
#'
#' @examples
#' data(md_items)
#' md_items
"md_items"
