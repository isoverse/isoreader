# file info ====

context("Source File Information")

test_that("test iso_get_source_file_structure()", {
  
  expect_error(iso_get_source_file_structure(), "`iso_file` has to be an iso file object")
  expect_error(iso_get_source_file_structure(42), "`iso_file` has to be an iso file object")
  
  
})