context("Deprecated functions")

test_that("test that deprecated functions give the appropriate error", {
  expect_error(isoread(), "Deprecated")
  expect_warning(tryCatch(iso_read_rda(make_cf_data_structure()), error = function(e) {}), "deprecated")
  expect_warning(tryCatch(iso_reread_archive(), error = function(e) {}), "deprecated")
})