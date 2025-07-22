# tests/testthat/test-create_rawdata_sheet.R

library(testthat)
library(readxl)
library(writexl)
library(quickpcr)

test_that("create_rawdata_sheet adds rawdata sheet correctly", {
  # 더미 데이터 생성
  df <- tibble::tibble(
    `Sample Name` = c("A_X_D1", "A_X_D1", "B_Y_D2"),
    `Target Name` = c("T1", "T1", "T2"),
    CT            = c(1.23, 2.34, 3.45)
  )

  # 임시 .xlsx 파일 생성
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Results = df), path = tmp)

  # 함수 실행 (skip=0 -> 바로 헤더 읽기)
  create_rawdata_sheet(
    file_path      = tmp,
    sheet_name     = "Results",
    skip           = 0,
    new_sheet_name = "rawdata",
    output_path    = tmp
  )

  # 결과 시트 읽기
  out <- readxl::read_excel(tmp, sheet = "rawdata", skip = 0)

  # 컬럼이 모두 존재하는지
  expect_true(all(
    c("Sample Name", "Target Name", "CT",
      "Cell Name", "Treatment", "Day", "experiment")
    %in% names(out)
  ))

  # experiment 값 확인
  exp1 <- out$experiment[out$`Sample Name` == "A_X_D1" & out$`Target Name` == "T1"]
  expect_equal(exp1, c("a", "b"))

  # 분리된 Day 확인
  expect_equal(out$Day, c("D1", "D1", "D2"))
})
