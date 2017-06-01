context("Data aggregation")


test_that("test that aggregation functions refuse to work with non isofiles", {
  
  expect_error(isoreader:::check_isofiles(1), "encountered non-isofile data type")
  expect_error(get_raw_data(1), "encountered non-isofile data type")
  expect_error(get_file_info(1), "encountered non-isofile data type")
  
})