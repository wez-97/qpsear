## zz-globals.R: Resolve global binding warnings

# List all variables and functions used non-standardly across package functions
utils::globalVariables(c(
  # dplyr/tidyr verbs and operators
  "filter", "group_by", "ungroup", "do",
  "select", "all_of", "starts_with",

  # pipe operator
  "%>%",

  # column names used in data frames
  "CT", "ct", "Cell Name", "Sample Name", "Target Name",
  "experiment", "target", "rel_exp", "delta_ct", "log2_rel_exp",
  "outlier_status", "dev1", "dev2", "dev3", "is_outlier",

  # functions imported or called via ::
  "remove_outliers_and_wide", "get_meta_cols_full", "prep_plot_data",
  "pairwise_t_test", "sym", "pull", "write_csv",
  "str_remove_all",

  # ggplot2 aesthetics and layers
  "aes", "ggplot", "geom_boxplot", "geom_jitter",
  "stat_summary", "position_dodge", "position_jitterdodge",
  "labs", "theme_classic", "theme", "element_text",
  "ggsave",

  # annotation and plotting parameters
  "annotation_y_expand", "annotation_base", "annotation_step",
  "y_axis_max", "plot_height",

  # statistical summary variables
  "comparison", "group1", "group2", "lwr", "upr", "p adj",

  # operators and miscellaneous globals
  ".", ":="
))
