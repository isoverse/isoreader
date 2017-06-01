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

# Voltage/current conversion ==== 

context("Voltage/Current conversion")

test_that("test that function parameter checks work", {
  # data frame supplied
  expect_error(convert_voltages_to_currents(), "data has to be supplied as a data frame")
  expect_error(convert_voltages_to_currents(5), "data has to be supplied as a data frame")
  
  # resistance values supplied
  expect_error(convert_voltages_to_currents(data_frame()), "resistance values have to be a named numeric vector")
  expect_error(convert_voltages_to_currents(data_frame(), c()), "resistance values have to be a named numeric vector")
  expect_error(convert_voltages_to_currents(data_frame(), c(1)), "resistance values have to be a named numeric vector")
  expect_error(convert_voltages_to_currents(data_frame(), c(R2 = "text")), "resistance values have to be a named numeric vector")
  
  # voltage columns exist
  expect_error(convert_voltages_to_currents(data_frame(), c(R2 = 1)), "no voltage columns found")
  expect_warning(convert_voltages_to_currents(data_frame(v45 = 1), c(R2 = 1)), "not all provided R values have matching voltage columns")
})

test_that("test that voltage to current conversion works", {
  
  test_data <- 
    data_frame(
      v44.mV = runif(10, min = 100, max = 10000),
      v45.mV = runif(10, min = 500, max = 5000)
    )
  
  # unit specific conversions
  expect_message(
    conv_data <- test_data %>% turn_info_messages_on() %>% 
      convert_voltages_to_currents(R = c(R44 = 0.3, R45 = 1e3), V_units = "mV", I_units = "pA", R_units = "GOhm"),
    "converted 2 voltage columns .* with resistances 0\\.3, 1000 GOhm"
  )
  
  expect_equal(conv_data %>% names, c("v44.mV", "v45.mV", "i44.pA", "i45.pA"))
  expect_equal(conv_data$i44.pA, test_data$v44.mV / 0.3)
  expect_equal(conv_data$i45.pA, test_data$v45.mV / 1e3)
  
  expect_silent(
    conv_data <- test_data %>% turn_info_messages_off() %>% 
      convert_voltages_to_currents(R = c(R44 = 0.3, R45 = 1e3), V_units = "mV", I_units = "nA", R_units = "GOhm")
  )
  
  expect_equal(conv_data %>% names, c("v44.mV", "v45.mV", "i44.nA", "i45.nA"))
  expect_equal(conv_data$i44.nA, test_data$v44.mV / 0.3 * 1/1000)
  expect_equal(conv_data$i45.nA, test_data$v45.mV / 1e3 * 1/1000)
  
  # test scaling equivalency (need rounding to account for small errors from machine calculations)
  expect_equivalent( # vary resistors
    convert_voltages_to_currents(
      test_data, R = c(R44 = 0.3, R45 = 1e3), V_units = "mV", I_units = "pA", R_units = "GOhm") %>% 
      mutate_all(signif),
    convert_voltages_to_currents(
      test_data, R = c(R44 = 300, R45 = 1e6), V_units = "mV", I_units = "pA", R_units = "MOhm") %>% 
      mutate_all(signif))
  
  expect_equivalent( # vary input and output units with resistors identical (column names change!)
    convert_voltages_to_currents(
      test_data, R = c(R44 = 0.3, R45 = 1e3), V_units = "V", I_units = "nA", R_units = "GOhm") %>% 
      { .$i44.nA } %>% signif(),
    convert_voltages_to_currents(
      test_data, R = c(R44 = 0.3, R45 = 1e3), V_units = "mV", I_units = "pA", R_units = "GOhm") %>% 
      { .$i44.pA } %>% signif())
  
  # testing unit defaults
  expect_silent(
    conv_data <- test_data %>% turn_info_messages_off() %>% 
      convert_voltages_to_currents(R = c(R44 = 0.3, R45 = 1e3))
  )
  
  # defaults should be mV, nA, GOhm
  expect_equivalent( # vary resistors
    convert_voltages_to_currents(
      test_data, R = c(R44 = 0.3, R45 = 1e3), V_units = "mV", I_units = "nA", R_units = "GOhm") %>% 
      mutate_all(signif),
    convert_voltages_to_currents(
      test_data, R = c(R44 = 0.3, R45 = 1e3)) %>% 
      mutate_all(signif))
  
  
})
