context("Unit scaling")

# Basic unit scaling ==== 

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

test_that("test that proper units can be found", {
  expect_error(isoreader:::get_unit_scaling(), "missing parameters")
  expect_error(isoreader:::get_unit_scaling("mV"), "missing parameters")
  expect_error(isoreader:::get_unit_scaling(c("mV", "V"), "V"), "can only find scaling for one unit")
  expect_error(isoreader:::get_unit_scaling("mV", "A"), "encountered invalid unit")
  expect_error(isoreader:::get_unit_scaling("blaV", "V"), "Encountered unrecognized unit")
  expect_equal(isoreader:::get_unit_scaling("mV", c("A", "V")),
               list(base_unit = "V", si_scaling = 1e-3))
  expect_equal(isoreader:::get_unit_scaling("nA", c("A", "V")),
               list(base_unit = "A", si_scaling = 1e-9))  
})

# TIme conversions ==== 

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

test_that("test that time conversion works for isofiles", {
  expect_error(convert_time(42), "can only convert time in continuous flow isofiles")
  cf <- isoreader:::make_cf_data_structure()
  expect_error(convert_time(cf), "no time unit to convert to specified")
  expect_warning(convert_time(cf, to = "min"), "read without extracting the raw data")
  
  # test data
  cf$read_options$raw_data <- TRUE
  cf$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2)
  expect_message(result <- convert_time(cf, to = "min", quiet = FALSE), "converting time")
  expect_true(is_isofile(result))
  expect_silent(convert_time(cf, to = "min", quiet = TRUE))
  expect_equal(convert_time(cf, to = "min")$raw_data$time.min, cf$raw_data$time.s/60)
  expect_equal(convert_time(cf, to = "s")$raw_data$time.s, cf$raw_data$time.s)
  
  # multiple files
  isofiles <- c(modifyList(cf, list(file_info = list(file_id = "a"))),
                modifyList(cf, list(file_info = list(file_id = "b"))))
  isofiles$b$raw_data$time.min <- isofiles$b$raw_data$time.s/60
  isofiles$b$raw_data$time.min <- NULL
  expect_true(is_isofile_list(isofiles_in_hrs <- convert_time(isofiles, to = "hours")))
  expect_equal(isofiles_in_hrs$a$raw_data$time.hours, cf$raw_data$time.s/3600)
  expect_equal(isofiles_in_hrs$b$raw_data$time.hours, cf$raw_data$time.s/3600)
  
})

# Voltage/current conversion ==== 
