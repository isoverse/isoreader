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

test_that("info messages can be switched for just one function", {
  
  quiet_test <- function(quiet) {
    on_exit_quiet <- isoreader:::update_quiet(quiet)
    on.exit(on_exit_quiet())
    return(setting("quiet"))
  }
  
  expect_message(turn_info_messages_on(), "messages turned on")
  expect_equal(quiet_test(TRUE), TRUE)
  expect_equal(isoreader:::setting("quiet"), FALSE)
  expect_equal(quiet_test(FALSE), FALSE)
  expect_equal(isoreader:::setting("quiet"), FALSE)
  
})

test_that("info message functions can be part of a pipeline", {
  df <- data_frame(a = 1:5)
  expect_equal(df %>% turn_info_messages_on(), df)
  expect_equal(df %>% turn_info_messages_off(), df)
})

test_that("test that debug mode can be activated", {
  expect_message(isoreader:::turn_debug_on(), "debug mode turned on")
  expect_equal(isoreader:::setting("debug"), TRUE)
  expect_equal(isoreader:::setting("catch_errors"), TRUE)
  expect_message(isoreader:::turn_debug_on(catch_errors = FALSE), "debug mode turned on")
  expect_equal(isoreader:::setting("debug"), TRUE)
  expect_equal(isoreader:::setting("catch_errors"), FALSE)
  expect_message(isoreader:::turn_debug_off(), "debug mode turned off")
  expect_equal(isoreader:::setting("debug"), FALSE)
  expect_equal(isoreader:::setting("catch_errors"), TRUE)
})

## this seems to break covr::package_coverage() because of the reload and is therefore commented out for now
# test_that("default values restored on package load", {
#   expect_true({turn_info_messages_off(); isoreader:::setting("quiet")})
#   expect_true({suppressMessages(isoreader:::turn_debug_on()); isoreader:::setting("debug")})
#   
#   # reloading package should reset settings
#   detach("package:isoreader", unload=TRUE) # this is the problem for code coverage
#   library(isoreader)
#   expect_false(isoreader:::setting("quiet"))
#   expect_false(isoreader:::setting("debug"))
# })
