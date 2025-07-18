#' Plot qPCR results (multi-comparison: all genes or by gene per cell type)
#'
#' @param rel_long_sel Long-format expression data
#' @param stat_matrix Statistics output from analyze_statistics()
#' @param plot_dir Output directory for plots
#' @param targets Character vector of target gene(s) for this comparison
#' @param short_labels Short sample names for labeling
#' @param plot_type 'all', 'by_target', or both
#' @param file_prefix Filename prefix (e.g., comparison type)
#' @param ... (etc)
multi_comparison_plot_expression <- function(
    rel_long_sel, stat_matrix, plot_dir, targets, short_labels,
    plot_type = c("all", "by_target"),
    y_axis_max = NULL, annotation_y_expand = 1.35,
    annotation_base = 0.15, annotation_step = 0.13, plot_height = 5,
    file_prefix = NULL
) {
  rel_long_sel <- rel_long_sel %>%
    mutate(target = str_remove_all(target, "\\s*#2"))
  
  # 1. Combined plot (all target genes)
  if ("all" %in% plot_type) {
    rel_long_sel_all <- rel_long_sel %>% filter(target %in% targets)
    for (cell_name in unique(rel_long_sel_all$`Cell Name`)) {
      rel_sub <- rel_long_sel_all %>% filter(`Cell Name` == cell_name)
      p_single <- ggplot(rel_sub, aes(x = sample, y = log2_rel_exp, fill = target)) +
        geom_boxplot(alpha = 0.5, outlier.shape = NA, position = position_dodge(width = 0.8)) +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.8)) +
        geom_jitter(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
        labs(title = paste(cell_name), y = "log2(Relative Expression)", x = "") +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
      fname <- paste0(file_prefix, "_", gsub("[^A-Za-z0-9]", "", cell_name), ".tiff")
      ggsave(filename = file.path(plot_dir, fname), plot = p_single, width = 5, height = plot_height, dpi = 300, units = "in", device = "tiff")
    }
  }
  # 2. By target (per gene per cell type)
  if ("by_target" %in% plot_type) {
    for (cell_name in unique(rel_long_sel$`Cell Name`)) {
      for (tg in targets) {
        rel_sub <- rel_long_sel %>% filter(`Cell Name` == cell_name, target == tg)
        if (nrow(rel_sub) == 0) next
        p_single <- ggplot(rel_sub, aes(x = sample, y = log2_rel_exp, fill = sample)) +
          geom_boxplot(alpha = 0.5, outlier.shape = NA) +
          stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
          geom_jitter(alpha = 0.5, width = 0.2) +
          labs(title = paste(cell_name, tg), y = "log2(Relative Expression)", x = "") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
        fname <- paste0(
          file_prefix, "_",
          paste(short_labels, collapse = "_vs_"), "_",
          gsub("[^A-Za-z0-9]", "", cell_name), "_",
          tg, ".tiff"
        )
        ggsave(filename = file.path(plot_dir, fname), plot = p_single,
               width = 5, height = plot_height, dpi = 300, units = "in", device = "tiff")
      }
    }
  }
}
