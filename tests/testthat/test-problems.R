context("Problems")

test_that("Test that problem registration and reporting works properly", {
  
  x <- letters
  expect_equal(isoreader:::n_problems(x), 0L)
  
  # initialize problems attribute
  expect_equal({
    y <- isoreader:::initialize_problems_attribute(x)
    as.character(y)
  }, x)
  expect_equal(isoreader:::n_problems(y), 0L)
  expect_equal(problems(y) %>% names(), c("type", "func", "details"))
  
  # test auto initialize of get_problems
  expect_equal(isoreader:::get_problems(x), isoreader:::get_problems(y))
  expect_equal(problems(y), isoreader:::get_problems(y))
  
  # add a problem
  expect_equal({
    y <- isoreader:::register_problem(x, func = "testing", details = "problem")
    as.character(y)
  }, x)
  
  expect_equal(isoreader:::n_problems(y), 1)
  expect_equal(problems(y) %>% select(func, details), data_frame(func = "testing", details = "problem"))
  
  # add another problem
  expect_equal({
    z <- isoreader:::register_problem(y, details = "problem2", code = 5)
    as.character(z)
  }, x)
  expect_equal(isoreader:::n_problems(z), 2)
  expect_equal(problems(z) %>% select(details, code), data_frame(details = c("problem", "problem2"), code = c(NA, 5)))
  
  # stop for problems
  expect_error(stop_for_problems(z), "2 parsing failures")
  
})

test_that("Test that problems set for iso_file lists get propagated to all files", {
  # propagate problems for iso_files
  expect_is(iso_file <- isoreader:::make_iso_file_data_structure(), "iso_file")
  iso_file1 <- iso_file %>% { .$file_info$file_id <- "A"; . }
  iso_file2 <- iso_file %>% { .$file_info$file_id <- "B"; . }
  expect_is(iso_files <- c(iso_file1, iso_file2), "iso_file_list")
  expect_equal(problems(iso_files) %>% nrow(), 0L)
  expect_error(iso_has_problems(), "provide iso_files")
  expect_false(iso_has_problems(iso_files))
  
  expect_is(iso_files_w_probs <- register_problem(iso_files, type = "test"), "iso_file_list")
  expect_true(iso_has_problems(iso_files_w_probs))
  expect_equal(problems(iso_files_w_probs) %>% select(file_id, type),
               data_frame(file_id = c("A", "B"), type = c("test")))
  expect_equal(problems(iso_files_w_probs[[1]]) %>% select(type), data_frame(type = "test"))
  expect_equal(problems(iso_files_w_probs[[2]]) %>% select(type), data_frame(type = "test"))
})

test_that("Test that warning and error registration works properly", {
  x <- letters
  
  # add a warning
  expect_warning(y <- isoreader:::register_warning(x, details = "problem", warn = TRUE), "problem")
  expect_silent(y <- isoreader:::register_warning(x, details = "problem", warn = FALSE))
  expect_equal(as.character(y), x)
  expect_equal(isoreader:::n_problems(y), 1)
  expect_equal(problems(y) %>% select(type, details), data_frame(type = "warning", details = "problem"))
  
  # add an error
  expect_warning(y <- isoreader:::register_error(x, details = "problem", warn = TRUE), "problem")
  expect_silent(y <- isoreader:::register_error(x, details = "problem", warn = FALSE))
  expect_equal(as.character(y), x)
  expect_equal(isoreader:::n_problems(y), 1)
  expect_equal(problems(y) %>% select(type, details), data_frame(type = "error", details = "problem"))
})
  
test_that("Combing problems works properly", {
  x <- isoreader:::register_problem(letters, type = "problem_x")
  y <- isoreader:::register_problem(letters, type = "problem_y")
  z <- letters
  
  expect_equal(isoreader:::combined_problems(x, y), bind_rows(problems(x), problems(y)))
  expect_equal(isoreader:::combined_problems(x, y, z), isoreader:::combined_problems(x, y))  
})

test_that("removing files with errors works properly", {
  
  iso_file <- isoreader:::make_iso_file_data_structure()
  expect_warning(iso_warn <- isoreader:::register_warning(iso_file, "test warning"))
  expect_warning(iso_err <- isoreader:::register_error(iso_file, "test error"))
  iso_file$file_info$file_id <- "A"
  iso_warn$file_info$file_id <- "B"
  iso_err$file_info$file_id <- "C"
  iso_files <- c(iso_file, iso_err, iso_warn)
  expect_error(iso_omit_files_with_problems(42), "provide a list of iso_files")
  expect_error(iso_omit_files_with_problems(iso_files, "all"), "unknown problem type specified")
  expect_message(iso_omit_files_with_problems(iso_files, "warning", quiet = FALSE), "removing")
  expect_silent(iso_omit_files_with_problems(iso_files, "warning", quiet = TRUE))
  expect_equal(iso_omit_files_with_problems(iso_files, "warning") %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A", "C"))
  expect_equal(iso_omit_files_with_problems(iso_files, "error") %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A", "B"))
  expect_equal(iso_omit_files_with_problems(iso_files, "both") %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A"))
})
