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
  
  # add a problem
  expect_equal({
    y <- isoreader:::register_problem(x, details = "problem")
    as.character(y)
  }, x)
  
  expect_equal(isoreader:::n_problems(y), 1)
  expect_equal(problems(y) %>% select(details), data_frame(details = "problem"))
  
  # add another problem
  expect_equal({
    z <- isoreader:::register_problem(y, details = "problem2", code = 5)
    as.character(z)
  }, x)
  expect_equal(isoreader:::n_problems(z), 2)
  expect_equal(problems(z) %>% select(details, code), data_frame(details = c("problem", "problem2"), code = c(NA, 5)))
  
  # register a file problem
  expect_equal({
    x_file <- isoreader:::register_problem(x, "unspecified error", "message")
    as.character(x_file)
  }, x)
  expect_equal(isoreader:::n_problems(x_file), 1)
  expect_equal(problems(x_file), 
               data_frame(type = "unspecified error",
                          func = "compare(object, expected, ...)", 
                          details = "message"))
  
  # stop for problems
  expect_error(stop_for_problems(z), "2 parsing failures")
  
})