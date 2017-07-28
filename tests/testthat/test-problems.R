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
    y <- isoreader:::register_problem(x, details = "problem")
    as.character(y)
  }, x)
  
  expect_equal(isoreader:::n_problems(y), 1)
  expect_equal(problems(y) %>% select(func, details), 
               data_frame(func = "compare(object, expected, ...)", details = "problem"))
  
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

test_that("Test that problems set for isofile lists get propagated to all files", {
  # propagate problems for isofiles
  expect_is(isofile <- isoreader:::make_isofile_data_structure(), "isofile")
  isofile1 <- isofile %>% { .$file_info$file_id <- "A"; . }
  isofile2 <- isofile %>% { .$file_info$file_id <- "B"; . }
  expect_is(isofiles <- c(isofile1, isofile2), "isofile_list")
  expect_equal(problems(isofiles) %>% nrow(), 0L)
  expect_is(isofiles_w_probs <- register_problem(isofiles, type = "test"), "isofile_list")
  expect_equal(problems(isofiles_w_probs) %>% select(file_id, type),
               data_frame(file_id = c("A", "B"), type = c("test")))
  expect_equal(problems(isofiles_w_probs[[1]]) %>% select(type), data_frame(type = "test"))
  expect_equal(problems(isofiles_w_probs[[2]]) %>% select(type), data_frame(type = "test"))
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
  
  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(iso_warn <- isoreader:::register_warning(isofile, "test warning"))
  expect_warning(iso_err <- isoreader:::register_error(isofile, "test error"))
  isofile$file_info$file_id <- "A"
  iso_warn$file_info$file_id <- "B"
  iso_err$file_info$file_id <- "C"
  isofiles <- c(isofile, iso_err, iso_warn)
  expect_error(omit_files_with_problems(42), "provide a list of isofiles")
  expect_error(omit_files_with_problems(isofiles, "all"), "unknown problem type specified")
  expect_message(omit_files_with_problems(isofiles, "warning", quiet = FALSE), "removing")
  expect_silent(omit_files_with_problems(isofiles, "warning", quiet = TRUE))
  expect_equal(omit_files_with_problems(isofiles, "warning") %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A", "C"))
  expect_equal(omit_files_with_problems(isofiles, "error") %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A", "B"))
  expect_equal(omit_files_with_problems(isofiles, "both") %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A"))
})
