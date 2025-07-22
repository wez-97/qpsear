# tests/testthat/test-check_outliers.R

library(testthat)
library(quickpcr)
library(dplyr)

test_that("check_outliers flags and plots outliers correctly", {
  # 더미 데이터 생성: 세 개 중 하나가 크게 벗어난 outlier
  ct_data <- tibble::tibble(
    sample     = rep("S1", 3),
    target     = rep("T1", 3),
    `Cell Name` = rep("C1", 3),
    Treatment  = rep("Tr", 3),
    Day        = rep("D1", 3),
    experiment = c("a", "b", "c"),
    ct         = c(20, 20.5, 30)  # 세 번째 값이 outlier
  )
  
  meta_cols <- c("Cell Name", "Treatment", "Day", "experiment")
  outdir    <- tempdir()
  
  # return_plot=TRUE 로 호출
  res <- check_outliers(
    ct_data     = ct_data,
    meta_cols   = meta_cols,
    outlier_dir = outdir,
    return_plot = TRUE
  )
  
  # 반환된 리스트 구조 확인
  expect_true(is.list(res))
  expect_named(res, c("ct_data_outlier", "outlier_plot"))
  
  # ct_data_outlier: is_outlier 컬럼이 있고, 하나 이상 TRUE 여야 함
  df_flag <- res$ct_data_outlier
  expect_true("is_outlier" %in% names(df_flag))
  expect_true(any(df_flag$is_outlier, na.rm = TRUE))
  
  # 파일이 생성되었는지 확인
  expect_true(file.exists(file.path(outdir, "ct_outlier_flag.csv")))
  expect_true(file.exists(file.path(outdir, "outlier_plot.pdf")))
})
