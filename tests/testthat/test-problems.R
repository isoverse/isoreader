context("Problems")

test_that("Test that problem registration and reporting works properly", {
  
  x <- letters
  expect_equal(n_problems(x), 0L)
  
  # initialize problems attribute
  expect_equal({
    y <- initialize_problems_attribute(x)
    as.character(y)
  }, x)
  expect_equal(n_problems(y), 0L)
  expect_equal(problems(y) %>% names(), c("type", "func", "details"))
  
  # test auto initialize of get_problems
  expect_equal(get_problems(x), get_problems(y))
  expect_equal(problems(y), get_problems(y))
  
  # add a problem
  expect_equal({
    y <- register_problem(x, func = "testing", details = "problem")
    as.character(y)
  }, x)
  
  expect_equal(n_problems(y), 1)
  expect_equal(problems(y) %>% select(func, details), tibble(func = "testing", details = "problem"))
  
  # add another problem
  expect_equal({
    z <- register_problem(y, details = "problem2", code = 5)
    as.character(z)
  }, x)
  expect_equal(n_problems(z), 2)
  expect_equal(problems(z) %>% select(details, code), tibble(details = c("problem", "problem2"), code = c(NA, 5)))
  
  # stop for problems
  expect_error(stop_for_problems(z), "2 parsing failures")
  
})

test_that("Test that problems set for iso_file lists get propagated to all files", {
  # propagate problems for iso_files
  expect_is(iso_file <- make_iso_file_data_structure("NA"), "iso_file")
  iso_file1 <- iso_file %>% { .$file_info$file_id <- "A"; . }
  iso_file2 <- iso_file %>% { .$file_info$file_id <- "B"; . }
  expect_is(iso_files <- c(iso_file1, iso_file2), "iso_file_list")
  expect_equal(problems(iso_files) %>% nrow(), 0L)
  expect_error(iso_has_problems(), "provide iso_files")
  expect_false(iso_has_problems(iso_files))
  
  expect_is(iso_files_w_probs <- register_problem(iso_files, type = "test"), "iso_file_list")
  expect_true(iso_has_problems(iso_files_w_probs))
  expect_equal(problems(iso_files_w_probs) %>% select(file_id, type),
               tibble(file_id = c("A", "B"), type = c("test")))
  expect_equal(problems(iso_files_w_probs[[1]]) %>% select(type), tibble(type = "test"))
  expect_equal(problems(iso_files_w_probs[[2]]) %>% select(type), tibble(type = "test"))
})

test_that("Test that warning and error registration works properly", {
  x <- letters
  
  # add a warning
  expect_message(y <- register_warning(x, details = "problem", warn = TRUE), "problem")
  expect_silent(y <- register_warning(x, details = "problem", warn = FALSE))
  expect_equal(as.character(y), x)
  expect_equal(n_problems(y), 1)
  expect_equal(problems(y) %>% select(type, details), tibble(type = "warning", details = "problem"))
  
  # add an error
  expect_message(y <- register_error(x, details = "problem", warn = TRUE), "caught error - problem")
  expect_silent(y <- register_error(x, details = "problem", warn = FALSE))
  expect_equal(as.character(y), x)
  expect_equal(n_problems(y), 1)
  expect_equal(problems(y) %>% select(type, details), tibble(type = "error", details = "problem"))
})
  
test_that("Combing problems works properly", {
  x <- register_problem(letters, type = "problem_x")
  y <- register_problem(letters, type = "problem_y")
  z <- letters
  
  expect_equal(combined_problems(x, y), bind_rows(problems(x), problems(y)))
  expect_equal(combined_problems(x, y, z), combined_problems(x, y))  
})

test_that("Test that removing files with errors works properly", {
  
  # iso_filter_files_with_problems
  iso_file <- make_iso_file_data_structure("NA")
  expect_message(iso_warn <- register_warning(iso_file, "test warning"))
  expect_message(iso_err <- register_error(iso_file, "test error"))
  iso_file$file_info$file_id <- "A"
  iso_warn$file_info$file_id <- "B"
  iso_err$file_info$file_id <- "C"
  iso_files <- c(iso_file, iso_err, iso_warn)
  expect_error(iso_filter_files_with_problems(42), "provide a list of iso_files")
  expect_equal(iso_filter_files_with_problems(iso_files, remove_files_with_errors = FALSE, remove_files_with_warnings = FALSE), iso_files)
  expect_message(iso_filter_files_with_problems(iso_files, quiet = FALSE), "removing")
  expect_silent(iso_filter_files_with_problems(iso_files, quiet = TRUE))
  expect_equal(iso_filter_files_with_problems(iso_files) %>% # default parameters
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A", "B"))
  expect_equal(iso_filter_files_with_problems(iso_files, remove_files_with_errors = FALSE, remove_files_with_warnings = TRUE) %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A", "C"))
  expect_equal(iso_filter_files_with_problems(iso_files, remove_files_with_warnings = TRUE) %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A"))
  
  # deprecated iso_omit_files_with_problems
  expect_warning(iso_omit_files_with_problems(iso_files), "renamed.*will be removed")
  
})

test_that("actual problem file works", {
  expect_warning(
    err <- iso_read_continuous_flow(system.file("errdata", "cf_without_data.dxf", package = "isoreader")),
    "encountered 1 problem\\.")
  expect_warning( # warnings cannot be quieted with quiet
    err <- iso_read_continuous_flow(system.file("errdata", "cf_without_data.dxf", package = "isoreader"), quiet = TRUE),
    "encountered 1 problem\\.")
  expect_warning(warn_problems(err), "encountered 1 problem\\.")
  expect_equal(
    iso_get_problems(err),
    tibble(
      file_id = "cf_without_data.dxf",
      type = "error",
      func = "extract_dxf_raw_voltage_data",
      details = "cannot identify measured masses - block 'CEvalDataIntTransferPart' not found after position 1 (pos 65327)"
    ) 
  )
})
