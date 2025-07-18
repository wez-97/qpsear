#' Convert wide-format qPCR data to long-format for plotting/statistics
#'
#' @param wide_clean Output of calc_ct_values()
#' @param meta_cols_full Vector of meta-data column names to keep
#' @return List with $rel_long and $dct_long (both long-format tibbles)
to_long_format <- function(wide_clean, meta_cols_full) {
  rel_long <- wide_clean %>%
    select(all_of(meta_cols_full), starts_with("rel_exp_")) %>%
    pivot_longer(
      cols = starts_with("rel_exp_"),
      names_to = "target",
      names_prefix = "rel_exp_",
      values_to = "rel_exp"
    ) %>%
    filter(!is.na(rel_exp))
  
  dct_long <- wide_clean %>%
    select(all_of(meta_cols_full), starts_with("delta_ct_")) %>%
    pivot_longer(
      cols = starts_with("delta_ct_"),
      names_to = "target",
      names_prefix = "delta_ct_",
      values_to = "delta_ct"
    ) %>%
    filter(!is.na(delta_ct))
  
  list(rel_long = rel_long, dct_long = dct_long)
}
