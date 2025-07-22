test_that("multi_comparison_plot_expression works and saves plots", {
  # 예시 데이터 생성
  rel_long_sel <- tibble::tibble(
    sample    = rep(c("A", "B", "C"), each = 6),
    target    = rep(c("FABP4", "LEPTIN"), each = 3, times = 3),
    `Cell Name` = rep("No.1", 18),
    log2_rel_exp = rnorm(18, mean = 1, sd = 0.3)
  )
  stat_matrix <- tibble::tibble(
    group1 = "A", group2 = "B", p.adj = 0.04, p.adj.signif = "*"
  )
  plot_dir <- tempdir()
  expect_silent(
    multi_comparison_plot_expression(
      rel_long_sel = rel_long_sel,
      stat_matrix  = stat_matrix,
      plot_dir     = plot_dir,
      targets      = c("FABP4", "LEPTIN"),
      short_labels = c("A", "B"),
      plot_type    = c("all", "by_target"),
      file_prefix  = "test"
    )
  )
  # 실제 파일 생성 여부 테스트 (예: test_No1.tiff 등)
  expect_true(length(list.files(plot_dir, pattern = "test.*\\.tiff$")) > 0)
})
