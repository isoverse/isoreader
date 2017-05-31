context("Unit scaling")

test_that("common SI prefixes are supported", {
  # parameters missing
  expect_error(isoreader:::get_si_prefix_scaling(unit = "km"), "no unit suffix specified")
  expect_error(isoreader:::get_si_prefix_scaling(suffix = "m"), "no unit supplied")
  
  # prefix mismatch
  expect_error(isoreader:::get_si_prefix_scaling("lm", "m"), "unrecognized unit")
  
  # suffix mismatch
  expect_error(isoreader:::get_si_prefix_scaling("km", "V"), "unrecognized unit") 
  
  # specific values
  expect_equal(isoreader:::get_si_prefix_scaling("m", "m"), 1)
  expect_equal(isoreader:::get_si_prefix_scaling(c("mm", "km"), "m"), c(0.001, 1000))
  expect_equal(isoreader:::get_si_prefix_scaling(c("mm", "km"), "m"), c(0.001, 1000))
  expect_equal(
    isoreader:::get_si_prefix_scaling(
      c("fm", "pm", "nm", "\U00B5m", "mm", "m", "km", "Mm", "Gm", "Tm"), "m"),
    10^(3*c(-5:4)))
  
  # suffix independence
  expect_equal(
    isoreader:::get_si_prefix_scaling("fA", "A"), 
    isoreader:::get_si_prefix_scaling("fV", "V"))
  
  # parameter order
  expect_equal(
    isoreader:::get_si_prefix_scaling(unit = "fA", suffix = "A"), 
    isoreader:::get_si_prefix_scaling(suffix = "A", unit = "fA"))
})
