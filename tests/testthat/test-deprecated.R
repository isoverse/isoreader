context("Deprecated functions")

test_that("test that deprecated functions give the appropriate error", {
  
  expect_warning(iso_export_to_excel(), "deprecated")
  expect_warning(iso_export_to_feather(), "deprecated")
  
})
