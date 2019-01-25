context("Plotting functions")

test_that("test that plottings functions throw deprecation error", {
  expect_error(iso_plot_raw_data(), "moved to the isoprocessor")
  expect_error(iso_plot_dual_inlet_data(), "moved to the isoprocessor")
  expect_error(iso_plot_continuous_flow_data(), "moved to the isoprocessor")
})
