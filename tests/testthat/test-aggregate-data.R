context("File info")

test_that("test that file information can be recovered from isofiles", {
  
  expect_true(is_isofile(isofile <- isoreader:::make_isofile_data_structure()))
  
  expect_error(get_isofile_id(), "no isofile provided")
  expect_error(get_isofile_id(42), "can only retrieve file information from an isofile object")
  expect_equal(get_isofile_id(isofile), NA_character_)
  
  expect_error(get_isofile_path(), "no isofile provided")
  expect_error(get_isofile_path(42), "can only retrieve file information from an isofile object")
  expect_equal(get_isofile_path(isofile), NA_character_)
  
  expect_error(get_isofile_subpath(), "no isofile provided")
  expect_error(get_isofile_subpath(42), "can only retrieve file information from an isofile object")
  expect_equal(get_isofile_subpath(isofile), NA_character_)
  
  expect_error(get_isofile_datetime(), "no isofile provided")
  expect_error(get_isofile_datetime(42), "can only retrieve file information from an isofile object")
  expect_equal(get_isofile_datetime(isofile), NA_real_)
  
  isofile$file_info$file_id <- "id"
  isofile$file_info$file_path <- "path"
  isofile$file_info$file_subpath <- "subpath"
  isofile$file_info$file_datetime <- as.POSIXct("2010-11-12 13:14:15", tz = "UTC")
  
  expect_equal(get_isofile_id(isofile), isofile$file_info$file_id)
  expect_equal(get_isofile_path(isofile), isofile$file_info$file_path)
  expect_equal(get_isofile_subpath(isofile), isofile$file_info$file_subpath)
  expect_equal(get_isofile_datetime(isofile), isofile$file_info$file_datetime)})

context("Data aggregation")

test_that("test that aggregation functions refuse to work with non isofiles", {
  
  expect_error(aggregate_raw_data(1), "encountered incompatible data type")
  expect_error(aggregate_file_info(1), "encountered incompatible data type")
  
})