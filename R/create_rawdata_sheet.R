#' 엑셀에 rawdata 시트 생성하기 (.xlsx 전용)
#'
#' 지정한 .xlsx 파일의 특정 시트를 읽어와서
#' Sample Name, Target Name, CT, Cell Name, Treatment, Day, experiment 컬럼으로 구성된
#' rawdata 시트를 추가·저장합니다.
#'
#' @param file_path 원본 .xlsx 파일 경로
#' @param sheet_name 원본 데이터 시트 이름, 기본값 "Results"
#' @param skip 헤더 위치 전까지 건너뛸 행 수, 기본값 43
#' @param new_sheet_name 추가할 시트 이름, 기본값 "rawdata"
#' @param sep_pattern 샘플명 분리 패턴, 기본값 "_"
#' @param output_path 결과 파일 저장 경로(.xlsx), 기본값 원본 파일 덮어쓰기
#' @return NULL. 함수 실행 후 메시지를 출력합니다.
#' @importFrom readxl read_excel excel_sheets
#' @importFrom writexl write_xlsx
#' @importFrom dplyr group_by mutate ungroup select
#' @importFrom tidyr separate
#' @export
create_rawdata_sheet <- function(
    file_path,
    sheet_name     = "Results",
    skip           = 43,
    new_sheet_name = "rawdata",
    sep_pattern    = "_",
    output_path    = file_path
) {
  # 입력 파일 확장자 검사
  if (tolower(tools::file_ext(file_path)) != "xlsx") {
    stop("file_path는 .xlsx 파일이어야 합니다.")
  }
  
  # 1. 원본 시트 읽기
  orig <- readxl::read_excel(file_path, sheet = sheet_name, skip = skip)
  
  # 2. 관심 열만 선택 (CT 컬럼만)
  df <- orig %>%
    select(
      sample_name = `Sample Name`,
      target_name = `Target Name`,
      ct          = `CT`
    )
  
  # 3. 그룹별 실험 반복 문자 부여
  df <- df %>%
    group_by(sample_name, target_name) %>%
    mutate(experiment = letters[row_number()]) %>%
    ungroup()
  
  # 4. Sample Name 분리
  df <- df %>%
    separate(
      col    = sample_name,
      into   = c("Cell Name", "Treatment", "Day"),
      sep    = sep_pattern,
      remove = FALSE
    )
  
  # 5. 최종 열 순서 지정
  rawdata <- df %>%
    select(
      `Sample Name` = sample_name,
      `Target Name` = target_name,
      CT             = ct,
      `Cell Name`,
      Treatment,
      Day,
      experiment
    )
  
  # 6. 모든 시트 읽어 리스트로 저장
  sheets    <- readxl::excel_sheets(file_path)
  orig_list <- setNames(
    lapply(sheets, function(sh) readxl::read_excel(file_path, sheet = sh)),
    sheets
  )
  
  # 7. rawdata 추가 후 쓰기
  orig_list[[new_sheet_name]] <- rawdata
  writexl::write_xlsx(orig_list, path = output_path)
  
  message("'", new_sheet_name, "' 시트가 생성되어 '", output_path, "'에 저장되었습니다.")
}
