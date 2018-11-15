context("Deprecated functions")

test_that("test that deprecated functions give the appropriate error", {
  expect_error(isoread(), "Deprecated")
  expect_message(tryCatch(iso_read_rda(make_cf_data_structure()), error = function(e) {}, warning = function(w){}), "deprecated")
  expect_message(tryCatch(iso_reread_archive(), error = function(e) {}), "deprecated")
})