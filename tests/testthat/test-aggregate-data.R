context("Data aggregation")


test_that("test that aggregation functions refuse to work with non isofiles", {
  
  expect_error(aggregate_raw_data(1), "encountered incompatible data type")
  expect_error(aggregate_file_info(1), "encountered incompatible data type")
  
})