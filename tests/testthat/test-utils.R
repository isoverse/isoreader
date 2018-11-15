context("Utility functions")

test_that("retrieving example files works correctly", {
  
  expect_true(is.data.frame(iso_get_reader_examples()))
  expect_error(iso_get_reader_example("DNE"), "does not exist")
  expect_equal(system.file(package = "isoreader", "extdata", "linearity_example.dxf"), path <- iso_get_reader_example("linearity_example.dxf"))
  expect_true(file.exists(path))
  
})

test_that("test that file extension helpers work correctly", {
  
  # get file extension
  expect_equal(isoreader:::get_file_ext("test.csv"), ".csv")
  expect_equal(isoreader:::get_file_ext("test.x.csv"), ".csv")
  
  # match file extension
  expect_equal(isoreader:::match_file_ext("test.csv", c(".txt", ".csv")), ".csv")
  expect_equal(isoreader:::match_file_ext("test.CSV", c(".txt", ".csv")), ".csv")
  expect_equal(isoreader:::match_file_ext("test.csv", c(".txt", ".CSV")), ".CSV")
  expect_equal(isoreader:::match_file_ext("test.CSV", c(".txt", ".CSV")), ".CSV")
  expect_equal(isoreader:::match_file_ext("test.csv", c(".txt", "csv", ".csv")), ".csv")
  expect_equal(isoreader:::match_file_ext("test.dne", c(".txt", ".csv")), NA_character_)
  
  # match supported filed types
  expect_error(isoreader:::match_to_supported_file_types())
  expect_error(isoreader:::match_to_supported_file_types(data_frame(), data_frame()))
  expect_error(
    isoreader:::match_to_supported_file_types(data_frame(filepath = "test.txt"), data_frame(extension = ".csv")),
    "unexpected file extension"
  )
  expect_equal(
    isoreader:::match_to_supported_file_types(
      data_frame(filepath = c("test.csv", "test.dxf")), 
      data_frame(extension = c(".dxf", "t.dxf", ".csv"))),
    data_frame(filepath = c("test.csv", "test.dxf"), extension = c(".csv", "t.dxf"))
  )
})

test_that("test that retrieving file paths works correctly", {
  
  expect_error(isoreader:::expand_file_paths())
  expect_error(isoreader:::expand_file_paths("DOESNOTEXIST"), "not exist")
  expect_error(isoreader:::expand_file_paths(c("DOESNOTEXIST", "NOTEITHER")), "not exist")
  expect_error(isoreader:::expand_file_paths(system.file("extdata", package = "isoreader")), "no extensions")
  expect_error(isoreader:::expand_file_paths(system.file("extdata", package = "isoreader") %>% list.files(full.names = TRUE), "did"), 
               "do not have one of the supported extensions")
  
  # check expected result
  expect_identical(
    direct_list <- system.file("extdata", package = "isoreader") %>% list.files(full.names = T, pattern = "\\.(dxf|did|cf)$"),
    system.file("extdata", package = "isoreader") %>% isoreader:::expand_file_paths(c("did", "dxf", "cf"))
  )
  expect_identical(
    system.file("extdata", package = "isoreader") %>% list.files(full.names = T, pattern = "\\.(dxf|did|cf)$") %>% {.[c(2,1,3:length(.))]},
    c(direct_list[2], system.file("extdata", package = "isoreader")) %>% isoreader:::expand_file_paths(c("did", "dxf", "cf"))
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


test_that("test that get support file types are listed", {
  expect_true(is.data.frame(iso_get_supported_file_types()))
})

test_that("logging works correctly", {
  iso_turn_info_messages_on()
  expect_message(log_message("test"), "Info: test")
  expect_message(log_warning("test"), "Warning: test")
  
  iso_turn_info_messages_off()
  expect_silent(log_message("test"))
  expect_message(log_warning("test"), "Warning: test")
  
  iso_turn_info_messages_on()
})

test_that("test that error catching works correctly", {
  expect_equal( {suppressMessages(isoreader:::iso_turn_debug_on(catch_errors = TRUE)); isoreader:::default(debug)}, TRUE)
  expect_message(y <- isoreader:::exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal( {suppressMessages(isoreader:::iso_turn_debug_on(catch_errors = FALSE)); isoreader:::default(debug)}, TRUE)
  expect_error(isoreader:::exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal( {suppressMessages(isoreader:::iso_turn_debug_off()); isoreader:::default(debug)}, FALSE)
  expect_message(y <- isoreader:::exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal(problems(y) %>% select(type, details), data_frame(type = "error", details = "problem"))
})


test_that("test that info concatenation works", {
  expect_equal(isoreader:::get_info_message_concat(1:2), "'1', '2'")
  expect_equal(isoreader:::get_info_message_concat(1:2, quotes = FALSE), "1, 2")
  expect_equal(isoreader:::get_info_message_concat(prefix = "hello ", 1:2), "hello '1', '2'")
  expect_equal(isoreader:::get_info_message_concat(1:2, suffix = " world"), "'1', '2' world")
  expect_equal(isoreader:::get_info_message_concat(c()), "")
  expect_equal(isoreader:::get_info_message_concat(NULL), "")
  expect_equal(isoreader:::get_info_message_concat(quo(NULL)), "")
  expect_equal(isoreader:::get_info_message_concat(quo(xyz)), "'xyz'")
})
