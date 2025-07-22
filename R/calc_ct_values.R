#' Calculate ΔCt, ΔΔCt, and relative expression values
#'
#' @param wide Wide-format data.frame after outlier removal
#' @param housekeeping Name of housekeeping gene (string)
#' @param ref_sample Reference sample name (string)
#' @param target_genes Vector of target gene names
#' @return Data.frame with added columns for delta_ct, delta_dct, rel_exp for each gene
#' @export
calc_ct_values <- function(wide, housekeeping, ref_sample, target_genes) {
  wide_clean <- wide %>% filter(!is.na(!!sym(housekeeping)))
  for (gene in target_genes) {
    delta_ct <- paste0("delta_ct_", gene)
    delta_dct <- paste0("delta_dct_", gene)
    rel_exp <- paste0("rel_exp_", gene)
    wide_clean <- wide_clean %>%
      mutate(!!delta_ct := ifelse(is.na(!!sym(gene)), NA, !!sym(gene) - !!sym(housekeeping)))
    ref_vals <- wide_clean %>% filter(sample == ref_sample) %>% pull(!!sym(delta_ct))
    ref_mean <- mean(ref_vals, na.rm = TRUE)
    wide_clean <- wide_clean %>%
      mutate(
        !!delta_dct := !!sym(delta_ct) - ref_mean,
        !!rel_exp := 2^(-!!sym(delta_dct))
      )
  }
  wide_clean
}
