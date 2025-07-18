#' Pipeline for multi-group qPCR comparison and plotting
#'
#' @param files List of file paths to qPCR Excel files
#' @param meta_cols Metadata columns to include
#' @param housekeeping Housekeeping gene
#' @param ref_sample Reference sample
#' @param comparison_list List (per comparison) of list(name, selected_samples, short_labels, stat_method)
#' @param target_groups List of character vectors (target genes per comparison)
#' @param stat_dir Directory for stat csv files
#' @param plot_dir Directory for output plots
#' @param outlier_dir Directory for outlier results
#' @param ... (additional params)
multi_comparison <- function(
    files,
    meta_cols,
    housekeeping,
    ref_sample,
    comparison_list,
    target_groups,
    stat_dir = "statistics",
    plot_dir = "plots",
    outlier_dir = "outlier",
    annotation_y_expand = 1.2,
    plot_height = 5,
    annotation_base = 0.08,
    annotation_step = 0.08
) {
  # 1. Load and preprocess
  ct_data <- load_and_preprocess_rawdata(files, meta_cols)
  ct_data_outlier <- check_outliers(ct_data, meta_cols, outlier_dir)
  wide <- remove_outliers_and_wide(ct_data_outlier, meta_cols)
  
  purrr::walk(comparison_list, function(comp) {
    purrr::walk(target_groups, function(tg) {
      wide_clean <- calc_ct_values(wide, housekeeping, ref_sample, tg)
      meta_cols_full <- get_meta_cols_full(wide_clean, meta_cols)
      long_list <- to_long_format(wide_clean, meta_cols_full)
      prep <- prep_plot_data(long_list$rel_long, long_list$dct_long, comp$selected_samples, comp$short_labels)
      stat_matrix <- analyze_statistics(
        prep$dct_long_sel, comp$stat_method, comp$short_labels, stat_dir
      )
      file_prefix <- paste0(comp$name, "_", paste(comp$short_labels, collapse = "_vs_"))
      # Plots: all and by-target
      multi_comparison_plot_expression(
        rel_long_sel = prep$rel_long_sel,
        stat_matrix = stat_matrix,
        plot_dir = plot_dir,
        targets = tg,
        short_labels = comp$short_labels,
        file_prefix = file_prefix,
        plot_type = c("all", "by_target"),
        plot_height = plot_height
      )
    })
  })
  cat("[INFO] All plots successfully saved\n")
}
