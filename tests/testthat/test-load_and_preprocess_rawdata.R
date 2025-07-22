library(testthat)
library(readxl)
library(writexl)
library(quickpcr)  # 패키지 이름에 맞게 조정
library(dplyr)

test_that("load_and_preprocess_rawdata works as expected", {
  # 1) 임시 xlsx 파일에 rawdata 시트 생성
  tmp <- tempfile(fileext = ".xlsx")
  rawdf <- tibble::tibble(
    `Sample Name` = c("A_X_D1", "A_X_D1", "B_Y_D2"),
    `Target Name` = c("T1", "T1", "T2"),
    CT            = c("1.1", "Undetermined", "3.3"),
    `Cell Name`   = c("A", "A", "B"),
    Treatment     = c("X", "X", "Y"),
    Day           = c("D1", "D1", "D2"),
    experiment    = c("a", "b", "a")
  )
  writexl::write_xlsx(list(rawdata = rawdf), path = tmp)
  
  # 2) 함수 호출
  out <- load_and_preprocess_rawdata(files = tmp,
                                     meta_cols = c("Cell Name", "Treatment", "Day", "experiment"))
  
  # 3) 컬럼명 변경 확인
  expect_true(all(c("sample", "target", "ct",
                    "Cell Name", "Treatment", "Day", "experiment")
                  %in% names(out)))
  
  # 4) Undetermined → 40, 나머는 numeric 변환 확인
  expect_type(out$ct, "double")
  expect_equal(out$ct, c(1.1, 40, 3.3))
  
  # 5) 정렬 순서: sample, target, experiment 기준 오름차순
  expect_equal(out$experiment, c("a", "b", "a"))
  expect_equal(out$sample, c("A_X_D1", "A_X_D1", "B_Y_D2"))
})
