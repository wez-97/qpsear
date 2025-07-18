#' Check and plot qPCR outliers
#'
#' @param ct_data Data.frame from load_and_preprocess_rawdata
#' @param meta_cols Character vector of metadata columns
#' @param outlier_dir Output directory for result csv/plots
#' @param return_plot If TRUE, return ggplot object
#' @return Data.frame with outlier flag or list (data + plot)
#' @export
check_outliers <- function(ct_data, meta_cols, outlier_dir, return_plot = FALSE) {
  ct_data_outlier <- ct_data %>%
    dplyr::group_by(sample, target) %>%
    dplyr::mutate(
      dev1 = abs(ct[1] - mean(c(ct[2], ct[3]))),
      dev2 = abs(ct[2] - mean(c(ct[1], ct[3]))),
      dev3 = abs(ct[3] - mean(c(ct[2], ct[1]))),
      max_dev = max(c(dev1, dev2, dev3)),
      is_outlier = dplyr::case_when(
        dev1 == max_dev & dev1 > 1 ~ experiment == experiment[1],
        dev2 == max_dev & dev2 > 1 ~ experiment == experiment[2],
        dev3 == max_dev & dev3 > 1 ~ experiment == experiment[3],
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::ungroup()
  readr::write_csv(ct_data_outlier, file.path(outlier_dir, "ct_outlier_flag.csv"))
  
  n_outlier <- sum(ct_data_outlier$is_outlier, na.rm = TRUE)
  n_total <- nrow(ct_data_outlier)
  if (n_outlier > 0) {
    cat(sprintf("[INFO] %d outlier(s) detected (%.2f%% of total). Please check the outlier plot for details.\n", n_outlier, 100 * n_outlier / n_total))
  } else {
    cat("[INFO] No outliers detected. Great job!\n")
  }
  
  p_out <- NULL
  if (any(ct_data_outlier$is_outlier)) {
    outlier_groups <- ct_data_outlier %>%
      dplyr::group_by(sample, target) %>%
      dplyr::filter(any(is_outlier)) %>%
      dplyr::mutate(outlier_status = ifelse(is_outlier, "FAIL", "PASS")) %>%
      dplyr::ungroup()
    p_out <- ggplot2::ggplot(outlier_groups, ggplot2::aes(x = experiment, y = ct, color = outlier_status)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~ paste(sample, target, sep = "_"), scales = "free_y") +
      ggplot2::labs(title = "Groups containing Ct outlier", x = "Technical replicates", y = "Ct (cycles)", color = "QC status") +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 15, margin = ggplot2::margin(b = 10)))
    ggplot2::ggsave(file.path(outlier_dir, "outlier_plot.pdf"), p_out, width = 10, height = 5)
  }
  if (return_plot) {
    return(list(ct_data_outlier = ct_data_outlier, outlier_plot = p_out))
  } else {
    return(ct_data_outlier)
  }
}
