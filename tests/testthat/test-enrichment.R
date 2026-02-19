test_that("visualize PTM-SEA result works", {
  gct_file <- test_path("testdata", "ccRCC-combined.gct")

  # basic default call
  expect_true(ggplot2::is_ggplot(visualize_PTMSEA(gct_file, "Tumor_vs_NAT_diff")))

  # explicit selected concepts (existing test)
  expect_true(
    ggplot2::is_ggplot(
      visualize_PTMSEA(
        gct_file,
        "Tumor_vs_NAT_diff",
        selected_concepts = c(
          "KINASE-PSP_PKCA/PRKCA", "KINASE-PSP_PKCB/PRKCB",
          "KINASE-PSP_PKCG/PRKCG", "KINASE-PSP_PKCB_iso2/PRKCB",
          "KINASE-PSP_PKCD/PRKCD", "KINASE-PSP_PKCI/PRKCI",
          "KINASE-PSP_PKCT/PRKCQ", "KINASE-PSP_PKCH/PRKCH",
          "KINASE-PSP_PKCE/PRKCE", "KINASE-PSP_PKCZ/PRKCZ"
        )
      )
    )
  )

  # direction filtering: Up and Down should both return a ggplot (if matching rows exist)
  expect_true(ggplot2::is_ggplot(visualize_PTMSEA(gct_file, "Tumor_vs_NAT_diff", direction = "Up")))
  expect_true(ggplot2::is_ggplot(visualize_PTMSEA(gct_file, "Tumor_vs_NAT_diff", direction = "Down")))

  # test selected_collections, smaller num_concepts, and relaxed cutoffs so plotting occurs
  expect_true(
    ggplot2::is_ggplot(
      visualize_PTMSEA(
        gct_file,
        "Tumor_vs_NAT_diff",
        selected_collections = c("KINASE"),
        num_concepts = 3,
        fdr_pvalue_cutoff = 0.2,
        score_cutoff = 0
      )
    )
  )
})
