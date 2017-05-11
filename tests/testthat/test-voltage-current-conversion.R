context("Voltage/Current conversion")

test_that("test function parameter checks", {
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

