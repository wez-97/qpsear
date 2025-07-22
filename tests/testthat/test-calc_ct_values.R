# tests/testthat/test-calc_ct_values.R

library(testthat)
library(dplyr)

test_that("calc_ct_values calculates correct ΔCt, ΔΔCt, rel_exp", {
  # 가짜 데이터 생성
  wide <- tibble::tibble(
    sample = c("REF", "A", "B"),
    GAPDH = c(20, 21, 20.5),
    FABP4 = c(24, 26, 25)
  )
  housekeeping <- "GAPDH"
  ref_sample <- "REF"
  target_genes <- c("FABP4")
  
  # 함수 실행
  res <- calc_ct_values(wide, housekeeping, ref_sample, target_genes)
  
  # ΔCt = target - housekeeping
  expect_equal(res$delta_ct_FABP4, res$FABP4 - res$GAPDH)
  # ΔΔCt = ΔCt - ref_mean
  ref_mean <- mean(res$delta_ct_FABP4[res$sample == ref_sample])
  expect_equal(res$delta_dct_FABP4, res$delta_ct_FABP4 - ref_mean)
  # rel_exp = 2^(-ΔΔCt)
  expect_equal(res$rel_exp_FABP4, 2^(-res$delta_dct_FABP4))
})
