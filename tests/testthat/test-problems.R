context("Problems")

test_that("Test that problem registration and reporting works properly", {
  
  x <- letters
  expect_equal(isoreader:::n_problems(x), 0)
  
  # add a problem
  expect_equal({
    y <- isoreader:::register_problem(x, text = "problem")
    as.character(y)
  }, x)
  
  expect_equal(isoreader:::n_problems(y), 1)
  expect_equal(problems(y), data_frame(text = "problem"))
  
  # add another problem
  expect_equal({
    z <- isoreader:::register_problem(y, text = "problem2", code = 5)
    as.character(z)
  }, x)
  expect_equal(isoreader:::n_problems(z), 2)
  expect_equal(problems(z), data_frame(text = c("problem", "problem2"), code = c(NA, 5)))
  
  # register a file problem
  expect_equal({
    x_file <- isoreader:::register_file_problem(x, "testfile.did", "unspecified error", "message")
    as.character(x_file)
  }, x)
  expect_equal(isoreader:::n_problems(x_file), 1)
  expect_equal(problems(x_file), 
               data_frame(file = "testfile.did", type = "unspecified error",
                          func = "compare(object, expected, ...)", message = "message"))
  
  # stop for problems
  expect_error(stop_for_problems(z), "2 parsing failures")
  
})