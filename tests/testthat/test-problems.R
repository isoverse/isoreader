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
  expect_equal(problems(y) %>% select(func, details), data_frame(func = "testing", details = "problem"))
  
  # add another problem
  expect_equal({
    z <- register_problem(y, details = "problem2", code = 5)
    as.character(z)
  }, x)
  expect_equal(n_problems(z), 2)
  expect_equal(problems(z) %>% select(details, code), data_frame(details = c("problem", "problem2"), code = c(NA, 5)))
  
  # stop for problems
  expect_error(stop_for_problems(z), "2 parsing failures")
  
})

test_that("Test that problems set for iso_file lists get propagated to all files", {
  # propagate problems for iso_files
  expect_is(iso_file <- make_iso_file_data_structure(), "iso_file")
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
  expect_message(y <- register_warning(x, details = "problem", warn = TRUE), "problem")
  expect_silent(y <- register_warning(x, details = "problem", warn = FALSE))
  expect_equal(as.character(y), x)
  expect_equal(n_problems(y), 1)
  expect_equal(problems(y) %>% select(type, details), data_frame(type = "warning", details = "problem"))
  
  # add an error
  expect_message(y <- register_error(x, details = "problem", warn = TRUE), "caught error - problem")
  expect_silent(y <- register_error(x, details = "problem", warn = FALSE))
  expect_equal(as.character(y), x)
  expect_equal(n_problems(y), 1)
  expect_equal(problems(y) %>% select(type, details), data_frame(type = "error", details = "problem"))
})
  
test_that("Combing problems works properly", {
  x <- register_problem(letters, type = "problem_x")
  y <- register_problem(letters, type = "problem_y")
  z <- letters
  
  expect_equal(combined_problems(x, y), bind_rows(problems(x), problems(y)))
  expect_equal(combined_problems(x, y, z), combined_problems(x, y))  
})
