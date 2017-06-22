context("Utility functions")

test_that("test that retrieving file paths works correctly", {
  
  expect_error(isoreader:::retrieve_file_paths())
  expect_error(isoreader:::retrieve_file_paths("DOESNOTEXIST"), "not exist")
  expect_error(isoreader:::retrieve_file_paths(c("DOESNOTEXIST", "NOTEITHER")), "not exist")
  expect_error(isoreader:::retrieve_file_paths(system.file("extdata", package = "isoreader")), "no extensions")
  expect_error(isoreader:::retrieve_file_paths(system.file("extdata", package = "isoreader") %>% list.files(full.names = TRUE), "did"), 
               "do not have one of the supported extensions")
  
  # check expected result
  expect_identical(
    direct_list <- system.file("extdata", package = "isoreader") %>% list.files(full.names = T, pattern = "\\.(dxf|did|cf)$"),
    system.file("extdata", package = "isoreader") %>% isoreader:::retrieve_file_paths(c("did", "dxf", "cf"))
  )
  expect_identical(
    system.file("extdata", package = "isoreader") %>% list.files(full.names = T, pattern = "\\.(dxf|did|cf)$") %>% {.[c(2,1,3:length(.))]},
    c(direct_list[2], system.file("extdata", package = "isoreader")) %>% isoreader:::retrieve_file_paths(c("did", "dxf", "cf"))
  )
  
  # test duplicated file names error
  # @FIXME: not sure how to test this without actually artifically duplicating an included data file
})

test_that("test that column name checks work correctly", {
  expect_error(isoreader:::col_check("x", data_frame(y=1:5)), "not in data")
  expect_error(isoreader:::col_check(c("x", "y"), data_frame(x=1:5)), "not in data")
  expect_silent(isoreader:::col_check("x", data_frame(x=1:5)))
  expect_silent(isoreader:::col_check(c("x", "y"), data_frame(x=1:5, y="test")))
})


test_that("test that support file types are listed", {
  expect_output(show_supported_file_types(), "supported file types")
})

test_that("test that error catching works correctly", {
  expect_equal( {suppressMessages(isoreader:::turn_debug_on(catch_errors = TRUE)); isoreader:::setting("debug")}, TRUE)
  expect_warning(y <- isoreader:::exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal( {suppressMessages(isoreader:::turn_debug_on(catch_errors = FALSE)); isoreader:::setting("debug")}, TRUE)
  expect_error(isoreader:::exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal( {suppressMessages(isoreader:::turn_debug_off()); isoreader:::setting("debug")}, FALSE)
  expect_warning(y <- isoreader:::exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal(problems(y) %>% select(type, details), data_frame(type = "error", details = "problem"))
})

