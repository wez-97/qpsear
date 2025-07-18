#' Perform group-wise t-test or ANOVA+TukeyHSD for qPCR
#'
#' @param dct_long_sel Long-format data (delta_ct)
#' @param stat_method 'ttest' or 'anova'
#' @param short_labels Sample names for comparison
#' @param stat_dir Output directory for csv results
#' @return Data.frame (statistical results)
analyze_statistics <- function(dct_long_sel, stat_method, short_labels, stat_dir) {
  if (stat_method == "ttest") {
    comparisons_dmf <- combn(short_labels, 2, simplify = FALSE)
    stat_matrix <- purrr::map_dfr(comparisons_dmf, function(pair) {
      dct_long_sel %>%
        filter(sample %in% pair) %>%
        group_by(target) %>%
        pairwise_t_test(delta_ct ~ sample, ref.group = pair[1], p.adjust.method = "none") %>%
        mutate(group1 = pair[1], group2 = pair[2])
    })
    write_csv(stat_matrix, file.path(stat_dir, "ttest_matrix.csv"))
    cat(sprintf("[INFO] t-test analysis complete: %d comparison results saved (file: %s)\n",
                nrow(stat_matrix), file.path(stat_dir, "ttest_matrix.csv")))
  } else if (stat_method == "anova") {
    stat_matrix <- dct_long_sel %>%
      group_by(target) %>%
      do({
        aov_fit <- aov(delta_ct ~ sample, data = .)
        tukey <- TukeyHSD(aov_fit)
        tukey_df <- as.data.frame(tukey$sample)
        tukey_df$comparison <- rownames(tukey_df)
        tukey_df <- tukey_df %>%
          tidyr::separate(comparison, into = c("group1", "group2"), sep = "-") %>%
          dplyr::select(group1, group2, diff, lwr, upr, `p adj`)
        names(tukey_df)[names(tukey_df) == "p adj"] <- "p.adj"
        tukey_df
      }) %>% ungroup()
    write_csv(stat_matrix, file.path(stat_dir, "anova_tukey.csv"))
    cat(sprintf("[INFO] ANOVA + TukeyHSD analysis complete: %d comparison results saved (file: %s)\n",
                nrow(stat_matrix), file.path(stat_dir, "anova_tukey.csv")))
  } else {
    stop("Only 'ttest' or 'anova' are supported for stat_method")
  }
  stat_matrix
}
