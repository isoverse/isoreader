context("Plotting functions")

test_that("test that raw data plot throws appropriate errors", {
  expect_error(iso_plot_raw_data(), "moved to the isoprocessor")
  expect_error(iso_plot_dual_inlet_data(), "moved to the isoprocessor")
  expect_error(iso_plot_continuous_flow_data(), "moved to the isoprocessor")
})
