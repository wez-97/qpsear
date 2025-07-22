#' Perform group-wise t-test or ANOVA+TukeyHSD for qPCR
#'
#' @param dct_long_sel Long-format data (delta_ct)
#' @param stat_method 'ttest' or 'anova'
#' @param sample_names Character vector: exact sample names (as in sample column) to compare
#' @param stat_dir Output directory for csv results
#' @return Data.frame (statistical results)
#' @importFrom rstatix pairwise_t_test
#' @importFrom readr write_csv
#' @importFrom dplyr group_by ungroup mutate select filter
#' @importFrom tidyr separate
#' @export
analyze_statistics <- function(dct_long_sel, stat_method, sample_names, stat_dir) {
  # 내부에서 사용할 p-value → significance 변환 함수
  get_p_signif <- function(p) {
    cut(
      p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", "ns"),
      right = TRUE
    )
  }

  if (stat_method == "ttest") {
    comparisons_dmf <- combn(sample_names, 2, simplify = FALSE)
    stat_matrix <- purrr::map_dfr(comparisons_dmf, function(pair) {
      dct_long_sel %>%
        dplyr::filter(sample %in% pair) %>%
        dplyr::group_by(target) %>%
        rstatix::pairwise_t_test(delta_ct ~ sample, ref.group = pair[1], p.adjust.method = "none") %>%
        dplyr::mutate(
          group1 = pair[1],
          group2 = pair[2],
          p.adj.signif = get_p_signif(p.adj)
        )
    })
    readr::write_csv(stat_matrix, file.path(stat_dir, "ttest_matrix.csv"))
    cat(sprintf("[INFO] t-test analysis complete: %d comparison results saved (file: %s)\n",
                nrow(stat_matrix), file.path(stat_dir, "ttest_matrix.csv")))
  } else if (stat_method == "anova") {
    stat_matrix <- dct_long_sel %>%
      dplyr::group_by(target) %>%
      do({
        aov_fit <- aov(delta_ct ~ sample, data = .)
        tukey <- TukeyHSD(aov_fit)
        tukey_df <- as.data.frame(tukey$sample)
        tukey_df$comparison <- rownames(tukey_df)
        tukey_df <- tukey_df %>%
          tidyr::separate(
            comparison,
            into = c("group1", "group2"),
            sep = "-", extra = "merge"
          ) %>%
          dplyr::select(group1, group2, diff, lwr, upr, `p adj`)
        names(tukey_df)[names(tukey_df) == "p adj"] <- "p.adj"
        tukey_df <- tukey_df %>%
          dplyr::mutate(
            p.adj.signif = get_p_signif(p.adj)
          )
        tukey_df
      }) %>% dplyr::ungroup()
    readr::write_csv(stat_matrix, file.path(stat_dir, "anova_tukey.csv"))
    cat(sprintf("[INFO] ANOVA + TukeyHSD analysis complete: %d comparison results saved (file: %s)\n",
                nrow(stat_matrix), file.path(stat_dir, "anova_tukey.csv")))
  } else {
    stop("Only 'ttest' or 'anova' are supported for stat_method")
  }
  stat_matrix
}
