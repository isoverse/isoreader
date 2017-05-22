context("Deprecated functions")

test_that("test that deprecated functions give the appropriate error", {
  expect_error(isoread(), "Deprecated")
})