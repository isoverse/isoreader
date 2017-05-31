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

test_that("test that time scaling works", {
  time <- 1:60
  
  # direct numbers
  expect_error(isoreader:::scale_time(time, to = "s"), "requires specifying from unit")
  expect_equal(isoreader:::scale_time(time, from = "min", to = "s"), time*60)
  expect_equal(isoreader:::scale_time(time, from = "min", to = "hour"), time/60)
  expect_equal(isoreader:::scale_time(time, from = "hours", to = "seconds"), time*60*60)
  expect_equal(isoreader:::scale_time(time, from = "second", to = "days"), time/60/60/24)
  
  # with duration object
  expect_warning(isoreader:::scale_time(lubridate::duration(time, "hours"), from = "s", to = "minutes"), "ignored")
  expect_equal(isoreader:::scale_time(lubridate::duration(time, "hours"), to = "minutes"), time*60)
})
