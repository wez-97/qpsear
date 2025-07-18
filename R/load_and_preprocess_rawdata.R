#' Load and preprocess qPCR raw data
#'
#' @param files Character vector of file paths (Excel).
#' @param meta_cols Character vector, names of metadata columns.
#' @return A data.frame of tidy qPCR data.
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% rename mutate arrange
#' @export
load_and_preprocess_rawdata <- function(files, meta_cols) {
  raw <- purrr::map_dfr(files, function(fp) {
    dat <- readxl::read_excel(fp, sheet = "rawdata")
    req_cols <- unique(c("Sample Name", "Target Name", "CT", meta_cols))
    stopifnot(all(req_cols %in% colnames(dat)))
    dat
  })
  ct_data <- raw %>%
    dplyr::rename(sample = `Sample Name`, target = `Target Name`, ct = CT) %>%
    dplyr::mutate(
      ct = ifelse(ct == "Undetermined", 40, as.numeric(ct)),
      experiment = as.character(experiment)
    ) %>%
    dplyr::arrange(sample, target, experiment)
  ct_data
}
