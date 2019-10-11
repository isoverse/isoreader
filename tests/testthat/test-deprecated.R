context("Deprecated functions")

test_that("test that deprecated functions give the appropriate error", {
  expect_error(isoread(), "Deprecated")
  expect_message(tryCatch(iso_read_rda(make_cf_data_structure("NA")), error = function(e) {}, warning = function(w){}), "deprecated")
  expect_message(tryCatch(iso_reread_archive(), error = function(e) {}), "deprecated")
})


context("Ratio calculations")

test_that("test that ratio functions throw deprecation error", {
  expect_error(iso_calculate_ratios(), "moved to the isoprocessor")
})


context("Unit scaling")

test_that("test that plottings functions throw deprecation error", {
  expect_error(iso_convert_time(), "moved to the isoprocessor")
  expect_error(iso_convert_signals(), "moved to the isoprocessor")
})


context("Plotting functions")

test_that("test that plottings functions throw deprecation error", {
  expect_error(iso_plot_raw_data(), "moved to the isoprocessor")
  expect_error(iso_plot_dual_inlet_data(), "moved to the isoprocessor")
  expect_error(iso_plot_continuous_flow_data(), "moved to the isoprocessor")
})
