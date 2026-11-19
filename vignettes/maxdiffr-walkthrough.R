## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----load---------------------------------------------------------------------
library(maxdiffr)
library(ggplot2)

data(md_survey)   # 150 simulated respondents
data(md_design)   # IBD design: 8 items, 8 blocks, 4 items/block, 2 versions
data(md_items)    # Item labels

## ----peek---------------------------------------------------------------------
md_items
head(md_survey[, 1:6])

