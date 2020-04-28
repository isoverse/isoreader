context("Deprecated functions")

test_that("test that deprecated functions give the appropriate error", {
  expect_error(isoread(), "Deprecated")
  
  # reading rda files
  expect_message(tryCatch(iso_read_rda(make_cf_data_structure("NA")), error = function(e) {}, warning = function(w){}), "deprecated")
  
  # re-read rda and rds collections directly (deprecated)
  expect_error(iso_reread_storage(), "deprecated")
  expect_error(iso_reread_archive(), "deprecated")
  
})


test_that("test that ratio functions throw deprecation error", {
  expect_error(iso_calculate_ratios(), "moved to the isoprocessor")
})


test_that("test that plottings functions throw deprecation error", {
  expect_error(iso_convert_time(), "moved to the isoprocessor")
  expect_error(iso_convert_signals(), "moved to the isoprocessor")
})


test_that("test that plottings functions throw deprecation error", {
  expect_error(iso_plot_raw_data(), "moved to the isoprocessor")
  expect_error(iso_plot_dual_inlet_data(), "moved to the isoprocessor")
  expect_error(iso_plot_continuous_flow_data(), "moved to the isoprocessor")
})


test_that("test that cache files with errors is deprecated", {
  expect_warning(tryCatch(iso_read_continuous_flow("NA", cache_files_with_errors = FALSE), error = function(e) {}, warning = function(w){ warning(w) }), "deprecated")
  expect_warning(tryCatch(iso_read_dual_inlet("NA", cache_files_with_errors = FALSE), error = function(e) {}, warning = function(w){ warning(w) }), "deprecated")
  expect_warning(tryCatch(iso_read_scan("NA", cache_files_with_errors = FALSE), error = function(e) {}, warning = function(w){ warning(w) }), "deprecated")
})