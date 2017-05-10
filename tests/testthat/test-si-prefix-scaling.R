context("SI prefixes")

test_that("common SI prefixes are supported", {
  expect_error(get_si_prefix_scaling("lm", "m"), "unrecognized unit")
})
