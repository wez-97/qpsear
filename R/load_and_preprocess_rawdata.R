#' Load and preprocess qPCR raw data
#'
#' @param files Character vector of file paths (Excel).
#' @param meta_cols Character vector, names of metadata columns.
#' @return A data.frame of tidy qPCR data.
#' @importFrom readxl read_excel
#' @importFrom purrr map_dfr
#' @importFrom dplyr rename mutate select arrange
#' @export
load_and_preprocess_rawdata <- function(files, meta_cols) {
  raw <- purrr::map_dfr(files, function(fp) {
    dat <- readxl::read_excel(fp, sheet = "rawdata")
    req_cols <- unique(c("Sample Name", "Target Name", "CT", meta_cols))
    stopifnot(all(req_cols %in% colnames(dat)))
    dat
  })

  # CT 컬럼을 문자로 가져온 뒤, 숫자로 바꿀 때 경고 억제
  raw <- raw %>%
    dplyr::rename(
      sample = `Sample Name`,
      target = `Target Name`,
      ct_str = CT
    ) %>%
    dplyr::mutate(
      numeric_ct = suppressWarnings(as.numeric(ct_str)),
      ct = ifelse(ct_str == "Undetermined", 40, numeric_ct),
      experiment = as.character(experiment)
    ) %>%
    dplyr::select(-ct_str, -numeric_ct) %>%
    dplyr::arrange(sample, target, experiment)

  raw
}
