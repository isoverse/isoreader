context("Data cleanup functions")

test_that("text extraction work", {
  
  # errors
  expect_error(extract_substring(), "no string supplied")
  expect_error(extract_substring("test"), "no extraction pattern supplied")
  expect_equal(extract_substring(c(), "\\w"), c())
  expect_error(extract_substring("test", "(\\w)\\w", capture_bracket = 2), "only 1 groups captured")
  
  # test bracket n
  expect_equal(extract_substring(c("abc123def#", "ABC456DEF"), "(\\d+)(\\w+)(#)?", capture_bracket = 0), c("123def#", "456DEF"))
  expect_equal(extract_substring(c("abc123def#", "ABC456DEF"), "(\\d+)(\\w+)(#)?", capture_bracket = 1), c("123", "456"))
  expect_equal(extract_substring(c("abc123def#", "ABC456DEF"), "(\\d+)(\\w+)(#)?", capture_bracket = 2), c("def", "DEF"))
  expect_equal(extract_substring(c("abc123def#", "ABC456DEF"), "(\\d+)(\\w+)(#)?", capture_bracket = 3), c("#", NA))
  
  # test match n
  expect_equal(extract_substring(c("abc123def345a", "51 41 "), "(\\d+)."), c("123d", "51 "))
  expect_equal(extract_substring(c("abc123def345b", "51 41 "), "(\\d+).", capture_bracket = 1), c("123", "51"))
  expect_equal(extract_substring(c("abc123def345z", "51 41 "), "(\\d+).", capture_n = 2), c("345z", "41 "))
  expect_equal(extract_substring(c("abc123def345z", "51 41 "), "(\\d+).", capture_bracket = 1, capture_n = 2), c("345", "41"))
  
  # test word
  expect_equal(extract_word(c("sample-1_number 16.2", "sample-2_number 7b")), c("sample", "sample"))
  expect_equal(extract_word(c("sample-1_number 16.2", "sample-2_number 7b"), include_dash = TRUE), c("sample-1", "sample-2"))
  expect_equal(extract_word(c("sample-1_number 16.2", "sample-2_number 7b"), include_dash = TRUE, include_numbers = FALSE), c("sample-", "sample-"))
  expect_equal(extract_word(c("sample-1_number 16.2", "sample-2_number 7b"), include_dash = TRUE, include_underscore = TRUE), c("sample-1_number", "sample-2_number"))
  expect_equal(extract_word(c("sample-1_number 16.2", "sample-2_number 7b"), include_dash = TRUE, capture_n = 2), c("number", "number"))
  expect_equal(extract_word(c("sample-1_number 16.2", "sample-2_number 7b"), include_dash = TRUE, include_space = TRUE, capture_n = 2), c("number 16", "number 7b"))
  expect_equal(extract_word(c("sample-1_number 16.2", "sample-2_number 7b"), include_dash = TRUE, include_space = TRUE, include_colon = TRUE, capture_n = 2), c("number 16.2", "number 7b"))
  
})


test_that("text to numeric conversions work", {
  
  # taken care of by the parse_number function in readr so testing here is not extensive
  expect_equal(parse_number(extract_word(c("abd23.2 a16.2", "hello why7"), capture_n = 2, include_colon = TRUE)), c(16.2,7))
  
})