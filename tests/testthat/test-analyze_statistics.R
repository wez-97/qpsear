# tests/testthat/test-analyze_statistics.R

library(testthat)
library(dplyr)
library(rstatix)
library(readr)

test_that("analyze_statistics works for ttest and anova", {
  # 예시 데이터 생성 (long-format, 18 rows)
  dct_long_sel <- tibble::tibble(
    sample = rep(c("A", "B", "C"), each = 6),
    target = rep(c("FABP4", "FABP4", "FABP4", "LEPTIN", "LEPTIN", "LEPTIN"), times = 3),
    delta_ct = seq(3, 4.7, length.out = 18)
  )
  stat_dir <- tempdir()
  short_labels <- c("A", "B")
  
  # t-test test
  stat_ttest <- analyze_statistics(
    dct_long_sel = dct_long_sel %>% filter(sample %in% c("A", "B")),
    stat_method = "ttest",
    short_labels = short_labels,
    stat_dir = stat_dir
  )
  expect_true("p" %in% colnames(stat_ttest) | "p.adj" %in% colnames(stat_ttest))
  expect_true(file.exists(file.path(stat_dir, "ttest_matrix.csv")))
  
  # anova test
  stat_anova <- analyze_statistics(
    dct_long_sel = dct_long_sel,
    stat_method = "anova",
    short_labels = c("A", "B", "C"),
    stat_dir = stat_dir
  )
  expect_true("p.adj" %in% colnames(stat_anova))
  expect_true(file.exists(file.path(stat_dir, "anova_tukey.csv")))
})

