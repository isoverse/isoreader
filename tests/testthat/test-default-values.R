context("Default values")

test_that("default values can be set and retrieved", {
  expect_error(isoreader:::default("don't know"), "default .* does not exist")
  expect_true(isoreader:::set_default("quiet", TRUE))
  expect_true(isoreader:::default("quiet"))
  expect_false(isoreader:::set_default("quiet", FALSE))
  expect_false(isoreader:::default("quiet"))
})


test_that("info messages can be turned on and off", {
  expect_message(turn_info_messages_on(), "messages turned on")
  expect_false(isoreader:::default("quiet"))
  expect_silent(turn_info_messages_off())
  expect_true(isoreader:::default("quiet"))
})

test_that("info message functions can be part of a pipeline", {
  library(dplyr)
  df <- data_frame(a = 1:5)
  expect_equal(df %>% turn_info_messages_on(), df)
  expect_equal(df %>% turn_info_messages_off(), df)
})