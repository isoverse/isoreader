context("Unit scaling")

test_that("test that plottings functions throw deprecation error", {
  expect_error(iso_convert_time(), "moved to the isoprocessor")
  expect_error(iso_convert_signals(), "moved to the isoprocessor")
})
