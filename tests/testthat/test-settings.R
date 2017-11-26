context("Settings and default values")

test_that("default values can be set and retrieved", {
  expect_error(isoreader:::default("don't know"), "setting .* does not exist")
  expect_true(isoreader:::set_default("quiet", TRUE))
  expect_true(isoreader:::default("quiet"))
  expect_true(isoreader:::default(quiet))
  expect_false(isoreader:::set_default("quiet", FALSE))
  expect_false(isoreader:::default("quiet"))
  expect_false(isoreader:::default(quiet))
})

test_that("info messages can be turned on and off", {
  expect_message(iso_turn_info_messages_on(), "messages turned on")
  expect_false(isoreader:::default(quiet))
  expect_silent(iso_turn_info_messages_off())
  expect_true(isoreader:::default(quiet))
})

test_that("info messages can be switched for just one function", {
  
  quiet_test <- function(quiet) {
    on_exit_quiet <- isoreader:::update_quiet(quiet)
    on.exit(on_exit_quiet())
    return(default(quiet))
  }
  
  expect_message(iso_turn_info_messages_on(), "messages turned on")
  expect_equal(quiet_test(TRUE), TRUE)
  expect_equal(isoreader:::default(quiet), FALSE)
  expect_equal(quiet_test(FALSE), FALSE)
  expect_equal(isoreader:::default(quiet), FALSE)
  
})

test_that("info message functions can be part of a pipeline", {
  df <- data_frame(a = 1:5)
  expect_equal(df %>% iso_turn_info_messages_on(), df)
  expect_equal(df %>% iso_turn_info_messages_off(), df)
})

test_that("test that caching can be turned on/off", {
  iso_turn_info_messages_on()
  expect_message(iso_turn_reader_caching_off(), "caching turned off")
  expect_equal(isoreader:::default(cache), FALSE)
  expect_equal(isoreader:::default("cache"), FALSE)
  expect_message(iso_turn_reader_caching_on(), "caching turned on")
  expect_equal(isoreader:::default(cache), TRUE)
  expect_equal(isoreader:::default("cache"), TRUE)
})

test_that("test that debug mode can be activated", {
  expect_message(isoreader:::iso_turn_debug_on(), "debug mode turned on")
  expect_equal(isoreader:::default(debug), TRUE)
  expect_equal(isoreader:::default(catch_errors), TRUE)
  expect_message(isoreader:::iso_turn_debug_on(catch_errors = FALSE), "debug mode turned on")
  expect_equal(isoreader:::default(debug), TRUE)
  expect_equal(isoreader:::default(catch_errors), FALSE)
  expect_message(isoreader:::iso_turn_debug_off(), "debug mode turned off")
  expect_equal(isoreader:::default(debug), FALSE)
  expect_equal(isoreader:::default(catch_errors), TRUE)
})

test_that("setting default read_parameters", {
  expect_error(iso_set_default_reader_parameters(read_raw_data = "NAN"), "must be TRUE or FALSE")
  
  expect_silent(iso_set_default_reader_parameters(read_file_info = FALSE, quiet=TRUE))
  expect_message(iso_set_default_reader_parameters(read_method_info = FALSE, quiet=FALSE))
  expect_false(default(read_file_info))
  expect_false(default(read_method_info))
  df <- data_frame(a = 1:5)
  expect_equal(iso_set_default_reader_parameters(df, read_file_info = TRUE, read_method_info = TRUE, quiet=TRUE), df)
  expect_true(default(read_file_info))
  expect_true(default(read_method_info))
  
})

## this seems to break covr::package_coverage() because of the reload and is therefore commented out for now
# test_that("default values restored on package load", {
#   expect_true({iso_turn_info_messages_off(); isoreader:::default(quiet)})
#   expect_true({suppressMessages(isoreader:::iso_turn_debug_on()); isoreader:::default(debug)})
#   
#   # reloading package should reset settings
#   detach("package:isoreader", unload=TRUE) # this is the problem for code coverage
#   library(isoreader)
#   expect_false(isoreader:::default(quiet))
#   expect_false(isoreader:::default(debug))
# })
