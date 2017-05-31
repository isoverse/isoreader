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

