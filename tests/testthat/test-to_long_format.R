# tests/testthat/test-to_long_format.R

library(testthat)
library(dplyr)
library(tidyr)

test_that("to_long_format converts wide to long correctly", {
  # 1. 예시 wide-format 데이터 생성
  wide_clean <- tibble::tibble(
    sample      = c("A", "B"),
    `Cell Name` = c("cell1", "cell2"),
    Treatment   = c("trt1", "trt2"),
    Day         = c("D0", "D1"),
    experiment  = c("a", "b"),
    rel_exp_FABP4  = c(1.1, 1.2),
    rel_exp_LEPTIN = c(0.7, 0.6),
    delta_ct_FABP4  = c(3.0, 3.5),
    delta_ct_LEPTIN = c(7.5, 7.1)
  )
  meta_cols_full <- c("sample", "Cell Name", "Treatment", "Day", "experiment")
  
  # 2. 변환 함수 실행
  long_list <- to_long_format(wide_clean, meta_cols_full)
  rel_long  <- long_list$rel_long
  dct_long  <- long_list$dct_long
  
  # 3. rel_long 테스트
  expect_equal(
    rel_long$target,
    rep(c("FABP4", "LEPTIN"), times = 2)
  )
  expect_equal(
    rel_long$rel_exp,
    c(1.1, 0.7, 1.2, 0.6)
  )
  expect_true(all(meta_cols_full %in% colnames(rel_long)))
  
  # 4. dct_long 테스트
  expect_equal(
    dct_long$target,
    rep(c("FABP4", "LEPTIN"), times = 2)
  )
  expect_equal(
    dct_long$delta_ct,
    c(3.0, 7.5, 3.5, 7.1)
  )
  expect_true(all(meta_cols_full %in% colnames(dct_long)))
})
