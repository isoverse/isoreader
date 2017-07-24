context("Export functions")

test_that("test that export to rda works properly", {
  expect_error(export_to_rda(42), "can only export iso files")
  expect_error(export_to_rda(isoreader:::make_cf_data_structure()), "no filename provided")
  expect_error(export_to_rda(isoreader:::make_cf_data_structure(), "test", folder = "DOESNOTEXIST"), "provided folder .* does not exist")
})