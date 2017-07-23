# file info ====

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
  expect_equal(get_isofile_datetime(isofile), isofile$file_info$file_datetime)
  
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
  
  cf <- isoreader:::make_cf_data_structure()
  expect_warning(aggregate_raw_data(cf), "read without extracting the raw data")
  
  # test data
  cf$read_options$raw_data <- TRUE
  cf1 <- modifyList(cf, list(file_info = list(file_id = "a")))
  cf2 <- modifyList(cf, list(file_info = list(file_id = "b")))
  cf1$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), `r46/44` = v46.mV/v44.mV)
  cf2$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), v45.mV = runif(10))
  
  # tests
  expect_equal(aggregate_raw_data(c(cf1, cf2)), 
               data <- bind_rows(mutate(cf1$raw_data, file_id="a"), mutate(cf2$raw_data, file_id = "b")))
  
  expect_equal(aggregate_raw_data(c(cf1, cf2), gather = TRUE), 
               data %>% gather(column, value, starts_with("v"), starts_with("r")) %>% 
                 left_join(data_frame(
                   column = c("v44.mV", "v45.mV", "v46.mV", "r46/44"),
                   category = c("mass", "mass", "mass", "ratio"),
                   dataset = c("44", "45", "46", "46/44"),
                   units = c("mV", "mV", "mV", NA_character_)
                  ), by = "column") %>% select(-column) %>% filter(!is.na(value)))
})


