# file info ====

context("File info")

test_that("test that file information can be recovered from isofiles", {
  
  expect_true(is_isofile(isofile <- isoreader:::make_isofile_data_structure()))
  
  expect_error(get_file_id(), "no isofile provided")
  expect_error(get_file_id(42), "can only retrieve file information from an isofile object")
  expect_equal(get_file_id(isofile), NA_character_)
  
  expect_error(get_file_path(), "no isofile provided")
  expect_error(get_file_path(42), "can only retrieve file information from an isofile object")
  expect_equal(get_file_path(isofile), NA_character_)
  
  expect_error(get_file_subpath(), "no isofile provided")
  expect_error(get_file_subpath(42), "can only retrieve file information from an isofile object")
  expect_equal(get_file_subpath(isofile), NA_character_)
  
  expect_error(get_file_datetime(), "no isofile provided")
  expect_error(get_file_datetime(42), "can only retrieve file information from an isofile object")
  expect_equal(get_file_datetime(isofile), NA_real_)
  
  isofile$file_info$file_id <- "id"
  isofile$file_info$file_path <- "path"
  isofile$file_info$file_subpath <- "subpath"
  isofile$file_info$file_datetime <- as.POSIXct("2010-11-12 13:14:15", tz = "UTC")
  
  expect_equal(get_file_id(isofile), isofile$file_info$file_id)
  expect_equal(get_file_path(isofile), isofile$file_info$file_path)
  expect_equal(get_file_subpath(isofile), isofile$file_info$file_subpath)
  expect_equal(get_file_datetime(isofile), isofile$file_info$file_datetime)
  
})

# data aggregation ====

context("Data aggregation")

## check read options ====

test_that("test that read option checks work properly", {
  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(isoreader:::check_read_options(isofile, "raw_data"), "read without extracting the raw data")
  isofile <- modifyList(isofile, list(read_options = list(raw_data = TRUE)))
  expect_silent(isoreader:::check_read_options(isofile, "raw_data"))
  expect_warning(isoreader:::check_read_options(isofile, "file_info"), "read without extracting the file info")
})

## check aggregate functions' errors ====

test_that("test that aggregation functions refuse to work with non isofiles", {
  expect_error(aggregate_raw_data(1), "encountered incompatible data type")
  expect_error(aggregate_file_info(1), "encountered incompatible data type")
  expect_error(aggregate_standards_info(1), "encountered incompatible data type")
  expect_error(aggregate_vendor_data_table(1), "encountered incompatible data type")
})

## check raw data aggregation

test_that("test that aggregeting raw data works", {
  
  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(aggregate_raw_data(isofile), "read without extracting the raw data")
  
  # test data
  isofile$read_options$raw_data <- TRUE
  isofile1 <- modifyList(isofile, list(file_info = list(file_id = "a")))
  isofile2 <- modifyList(isofile, list(file_info = list(file_id = "b")))
  isofile1$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), `r46/44` = v46.mV/v44.mV)
  isofile2$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), v45.mV = runif(10))
  
  expect_message(aggregate_raw_data(c(isofile1, isofile2), quiet = FALSE), "aggregating")
  expect_silent(aggregate_raw_data(c(isofile1, isofile2), quiet = TRUE))
  expect_equal(aggregate_raw_data(c(isofile1, isofile2)), 
               data <- bind_rows(mutate(isofile1$raw_data, file_id="a"), 
                                 mutate(isofile2$raw_data, file_id = "b")))
  
  expect_equal(aggregate_raw_data(c(isofile1, isofile2), gather = TRUE), 
               data %>% gather(column, value, starts_with("v"), starts_with("r")) %>% 
                 left_join(data_frame(
                   column = c("v44.mV", "v45.mV", "v46.mV", "r46/44"),
                   category = c("mass", "mass", "mass", "ratio"),
                   dataset = c("44", "45", "46", "46/44"),
                   units = c("mV", "mV", "mV", NA_character_)
                  ), by = "column") %>% select(-column) %>% filter(!is.na(value)))
})

## check aggregating file info works

test_that("test that aggregating file info works", {

  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(aggregate_file_info(isofile), "read without extracting the file info")
  
  # test data
  isofile$read_options$file_info <- TRUE
  isofile1 <- modifyList(isofile, list(
    file_info = list(file_id = "a", test_info = "x", multi_value = 1:2, only_a = TRUE)))
  isofile2 <- modifyList(isofile, list(
    file_info = list(file_id = "b", test_info = "y", multi_value = 1:3)))
  
  expect_message(aggregate_file_info(c(isofile1, isofile2), quiet = FALSE), "aggregating")
  expect_silent(aggregate_file_info(c(isofile1, isofile2), quiet = TRUE))
  expect_equal(aggregate_file_info(c(isofile1, isofile2)), 
               {
                  # check for multi value collapse functionality
                  isofile1$file_info$multi_value <- str_c(isofile1$file_info$multi_value, collapse = "; ")
                  isofile2$file_info$multi_value <- str_c(isofile2$file_info$multi_value, collapse = "; ")
                  bind_rows(as_data_frame(isofile1$file_info), as_data_frame(isofile2$file_info))
               })
  
})

## CONTINUE HERE: vendor data table and methods info