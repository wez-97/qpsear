#' Plot qPCR results (multi-comparison: all genes or by gene per cell type)
#'
#' @param rel_long_sel Long-format expression data
#' @param stat_matrix Statistics output from analyze_statistics()
#' @param plot_dir Output directory for plots
#' @param targets Character vector of target gene(s) for this comparison
#' @param show_legend Logical. 범례 표시 여부 (default: TRUE)
#' @param x_labels_map Named character vector, x축 sample명을 다른 이름으로 표시하고 싶을 때 사용. 예) c("No.4_None_D0"="Ctrl", ...)
#' @param x_angle x축 레이블 각도 (default: 45)
#' @param x_hjust x축 레이블 hjust (default: 1)
#' @param x_vjust x축 레이블 vjust (default: 1)
#' @param plot_type 'all', 'by_target', or both
#' @param file_prefix Filename prefix (e.g., comparison type)
#' @param y_axis_max Numeric maximum value for the y-axis, or NULL to compute automatically
#' @param annotation_y_expand Numeric expansion factor for annotation y-positions (default: 1.35)
#' @param annotation_base Numeric base offset for annotation placement (default: 0.15)
#' @param annotation_step Numeric incremental step size for successive annotations (default: 0.13)
#' @param plot_height Numeric plot height in inches (default: 5)
#' @return Invisibly returns ggplot objects
#' @export
multi_comparison_plot_expression <- function(
    rel_long_sel, stat_matrix, plot_dir, targets,
    show_legend = TRUE,
    x_labels_map = NULL,
    x_angle = 45, x_hjust = 1, x_vjust = 1,
    plot_type = c("all", "by_target"),
    y_axis_max = NULL, annotation_y_expand = 1.35,
    annotation_base = 0.15, annotation_step = 0.13, plot_height = 5,
    file_prefix = NULL
) {
  require(ggplot2)
  require(dplyr)
  require(stringr)

  # 1. plot_dir 없으면 생성
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
    message(sprintf("[INFO] Created output directory: %s", plot_dir))
  } else {
    message(sprintf("[INFO] Output directory exists: %s", plot_dir))
  }

  rel_long_sel <- rel_long_sel %>%
    mutate(target = str_remove_all(target, "\\s*#2"))

  # x축 레이블 변환
  if (!is.null(x_labels_map)) {
    rel_long_sel <- rel_long_sel %>%
      mutate(sample = ifelse(sample %in% names(x_labels_map), x_labels_map[sample], sample))
  }

  # x축 label 테마 (각도 등)
  axis_text_theme <- theme(
    axis.text.x = element_text(angle = x_angle, hjust = x_hjust, vjust = x_vjust)
  )
  # 범례 표시 여부
  legend_theme <- if (show_legend) theme() else theme(legend.position = "none")

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
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
        axis_text_theme + legend_theme
      fname <- paste0(file_prefix, "_", gsub("[^A-Za-z0-9]", "", cell_name), ".tiff")
      fpath <- file.path(plot_dir, fname)
      ggsave(filename = fpath, plot = p_single, width = 5, height = plot_height, dpi = 300, units = "in", device = "tiff")
      if (file.exists(fpath)) {
        message(sprintf("[OK] Plot saved: %s", fpath))
      } else {
        warning(sprintf("[FAIL] Plot NOT saved: %s", fpath))
      }
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
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
          axis_text_theme + legend_theme
        fname <- paste0(
          file_prefix, "_",
          gsub("[^A-Za-z0-9]", "", cell_name), "_",
          tg, ".tiff"
        )
        fpath <- file.path(plot_dir, fname)
        ggsave(filename = fpath, plot = p_single,
               width = 5, height = plot_height, dpi = 300, units = "in", device = "tiff")
        if (file.exists(fpath)) {
          message(sprintf("[OK] Plot saved: %s", fpath))
        } else {
          warning(sprintf("[FAIL] Plot NOT saved: %s", fpath))
        }
      }
    }
  }
}
