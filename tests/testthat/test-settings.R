context("Settings and default values")

test_that("default values can be set and retrieved", {
  expect_error(isoreader:::setting("don't know"), "setting .* does not exist")
  expect_true(isoreader:::set_setting("quiet", TRUE))
  expect_true(isoreader:::setting("quiet"))
  expect_false(isoreader:::set_setting("quiet", FALSE))
  expect_false(isoreader:::setting("quiet"))
})


test_that("info messages can be turned on and off", {
  expect_message(turn_info_messages_on(), "messages turned on")
  expect_false(isoreader:::setting("quiet"))
  expect_silent(turn_info_messages_off())
  expect_true(isoreader:::setting("quiet"))
})

test_that("info message functions can be part of a pipeline", {
  library(dplyr)
  df <- data_frame(a = 1:5)
  expect_equal(df %>% turn_info_messages_on(), df)
  expect_equal(df %>% turn_info_messages_off(), df)
})

test_that("test that debug mode can be activated", {
  expect_message(isoreader:::turn_debug_on(), "debug mode turned on")
  expect_equal(isoreader:::setting("debug"), TRUE)
  expect_message(isoreader:::turn_debug_off(), "debug mode turned off")
  expect_equal(isoreader:::setting("debug"), FALSE)
})