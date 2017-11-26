# file info ====

context("File info")

test_that("test that file information can be recovered from isofiles", {
  
  expect_true(iso_is_file(isofile <- isoreader:::make_isofile_data_structure()))
  
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
  expect_equal(get_file_datetime(isofile), NA)
  
  isofile$file_info$file_id <- "id"
  isofile$file_info$file_path <- "path"
  isofile$file_info$file_subpath <- "subpath"
  isofile$file_info$file_datetime <- as.POSIXct("2010-11-12 13:14:15", tz = "UTC")
  
  expect_equal(get_file_id(isofile), isofile$file_info$file_id)
  expect_equal(get_file_path(isofile), isofile$file_info$file_path)
  expect_equal(get_file_subpath(isofile), isofile$file_info$file_subpath)
  expect_equal(get_file_datetime(isofile), isofile$file_info$file_datetime)
  
})

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
  expect_error(iso_aggregate_raw_data(1), "encountered incompatible data type")
  expect_error(iso_aggregate_file_info(1), "encountered incompatible data type")
  expect_error(iso_aggregate_standards_info(1), "encountered incompatible data type")
  expect_error(iso_aggregate_vendor_data_table(1), "encountered incompatible data type")
})

## check aggregating file info works

test_that("test that aggregating file info works", {

  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(iso_aggregate_file_info(isofile), "read without extracting the file info")
  
  # test data
  isofile$read_options$file_info <- TRUE
  isofile1 <- modifyList(isofile, list(
    file_info = list(file_id = "a", test_info = "x", multi_value = 1:2, only_a = TRUE)))
  isofile2 <- modifyList(isofile, list(
    file_info = list(file_id = "b", test_info = "y", multi_value = 1:3)))
  
  expect_message(iso_aggregate_file_info(c(isofile1, isofile2), quiet = FALSE), "aggregating")
  expect_silent(agg <- iso_aggregate_file_info(c(isofile1, isofile2), quiet = TRUE))
  expect_equal(names(agg), unique(names(isofile1$file_info), names(isofile2$file_info)))
  expect_equal(iso_aggregate_file_info(c(isofile1, isofile2)), 
               {
                  # check for multi value collapse functionality
                  isofile1$file_info$multi_value <- str_c(isofile1$file_info$multi_value, collapse = "; ")
                  isofile2$file_info$multi_value <- str_c(isofile2$file_info$multi_value, collapse = "; ")
                  bind_rows(as_data_frame(isofile1$file_info), as_data_frame(isofile2$file_info))
               })
  
  # check selecte functionality
  expect_equal(names(iso_aggregate_file_info(isofile1, select = c("file_datetime", "only_a"))), c("file_id", "file_datetime", "only_a"))
  expect_warning(agg <- iso_aggregate_file_info(isofile2, select = c("file_datetime", "only_a")), "file info entries do not exist")
  expect_equal(names(agg), c("file_id", "file_datetime"))
})


## check raw data aggregation

test_that("test that aggregeting raw data works", {
  
  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(iso_aggregate_raw_data(isofile), "read without extracting the raw data")
  
  # test data
  isofile$read_options$raw_data <- TRUE
  isofile$read_options$file_info <- TRUE
  isofile1 <- modifyList(isofile, list(file_info = list(file_id = "a")))
  isofile2 <- modifyList(isofile, list(file_info = list(file_id = "b")))
  isofile1$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), `r46/44` = v46.mV/v44.mV)
  isofile2$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), v45.mV = runif(10))
  
  expect_message(iso_aggregate_raw_data(c(isofile1, isofile2), quiet = FALSE), "aggregating")
  expect_silent(iso_aggregate_raw_data(c(isofile1, isofile2), quiet = TRUE))
  expect_equal(iso_aggregate_raw_data(c(isofile1, isofile2)), 
               data <- bind_rows(mutate(isofile1$raw_data, file_id="a"), 
                                 mutate(isofile2$raw_data, file_id = "b")))
  
  expect_equal(iso_aggregate_raw_data(c(isofile1, isofile2), gather = TRUE), 
               data %>% gather(column, value, starts_with("v"), starts_with("r")) %>% 
                 left_join(data_frame(
                   column = c("v44.mV", "v45.mV", "v46.mV", "r46/44"),
                   category = c("mass", "mass", "mass", "ratio"),
                   data = c("44", "45", "46", "46/44"),
                   units = c("mV", "mV", "mV", NA_character_)
                 ), by = "column") %>% select(-column) %>% filter(!is.na(value)))
  
  # include file info
  isofile1 <- modifyList(isofile1, list(file_info = list(test_info = "x")))
  isofile2 <- modifyList(isofile2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_aggregate_raw_data(c(isofile1, isofile2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_aggregate_raw_data(
      c(isoreader:::make_isofile_data_structure(), isofile1, isofile2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
})


## check standards aggreation ====

test_that("test that aggregating of methods standards works", {
  
  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(iso_aggregate_standards_info(isofile), "read without extracting the method info")
  
  # test data
  isofile$read_options$method_info <- TRUE
  isofile$read_options$file_info <- TRUE
  isofile1 <- modifyList(isofile, list(file_info = list(file_id = "a"), 
                                       method_info = list(standards = data_frame(standard = "test a"))))
  isofile2 <- modifyList(isofile, list(file_info = list(file_id = "b"),
                                       method_info = list(standards = data_frame(standard = "test a"))))
  
  expect_message(iso_aggregate_standards_info(c(isofile1, isofile2), quiet = FALSE), "aggregating")
  expect_silent(iso_aggregate_standards_info(c(isofile1, isofile2), quiet = TRUE))
  expect_equal(iso_aggregate_standards_info(c(isofile1, isofile2)), 
               data <- bind_rows(mutate(isofile1$method_info$standards, file_id="a"), 
                                 mutate(isofile2$method_info$standards, file_id="b")))
  # include file info
  isofile1 <- modifyList(isofile1, list(file_info = list(test_info = "x")))
  isofile2 <- modifyList(isofile2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_aggregate_standards_info(c(isofile1, isofile2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_aggregate_standards_info(
      c(isoreader:::make_isofile_data_structure(), isofile1, isofile2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})


## check resistors aggreation ====

test_that("test that aggregating of resistors works", {
  
  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(iso_aggregate_resistors_info(isofile), "read without extracting the method info")
  
  # test data
  isofile$read_options$method_info <- TRUE
  isofile$read_options$file_info <- TRUE
  isofile1 <- modifyList(isofile, list(file_info = list(file_id = "a"), 
                                       method_info = list(resistors = data_frame(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11)))))
  isofile2 <- modifyList(isofile, list(file_info = list(file_id = "b"),
                                       method_info = list(resistors = data_frame(cup = 1:3, R.Ohm = c(3e9, 1e11, 1e12)))))
  
  expect_message(iso_aggregate_resistors_info(c(isofile1, isofile2), quiet = FALSE), "aggregating")
  expect_silent(iso_aggregate_resistors_info(c(isofile1, isofile2), quiet = TRUE))
  expect_equal(iso_aggregate_resistors_info(c(isofile1, isofile2)), 
               data <- bind_rows(mutate(isofile1$method_info$resistors, file_id="a"), 
                                 mutate(isofile2$method_info$resistors, file_id="b")))
  # include file info
  isofile1 <- modifyList(isofile1, list(file_info = list(test_info = "x")))
  isofile2 <- modifyList(isofile2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_aggregate_resistors_info(c(isofile1, isofile2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_aggregate_resistors_info(
      c(isoreader:::make_isofile_data_structure(), isofile1, isofile2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})


## check vendor data table aggreation ====

test_that("test that aggregating of vendor data table works", {
  
  isofile <- isoreader:::make_isofile_data_structure()
  expect_warning(iso_aggregate_vendor_data_table(isofile), "read without extracting the vendor data table")
  
  # test data
  isofile$read_options$vendor_data_table <- TRUE
  isofile$read_options$file_info <- TRUE
  isofile1 <- modifyList(isofile, list(file_info = list(file_id = "a")))
  isofile1$vendor_data_table <- data_frame(column1 = "col1 a", column2 = "col2 a", col_a_only = "test a")
  isofile2 <- modifyList(isofile, list(file_info = list(file_id = "b")))
  isofile2$vendor_data_table <- data_frame(column1 = "col1 b", column2 = "col2 b")
  
  # unit information
  expect_warning(iso_aggregate_vendor_data_table(isofile1, with_units = TRUE), "do not have unit information")
  expect_message(iso_aggregate_vendor_data_table(isofile1, with_units = FALSE, quiet = FALSE), "aggregating")
  expect_silent(iso_aggregate_vendor_data_table(isofile1, with_units = FALSE, quiet = TRUE))
  
  attr(isofile1$vendor_data_table, "units") <- attr(isofile2$vendor_data_table, "units") <- 
    data_frame(column = c("column1", "column2", "col_a_only"), units = c("[1]", "[2]", ""))
  
  # aggregated with and without units
  expect_message(agg <- iso_aggregate_vendor_data_table(c(isofile1, isofile2), with_units = TRUE, quiet = FALSE), "aggregating")
  expect_equal(agg, 
               bind_rows(mutate(isofile1$vendor_data_table, file_id="a"),
                              mutate(isofile2$vendor_data_table, file_id="b")) %>% 
                 rename(`column1 [1]` = column1, `column2 [2]` = column2))
  expect_equal(iso_aggregate_vendor_data_table(c(isofile1, isofile2), with_units = FALSE), 
               bind_rows(mutate(isofile1$vendor_data_table, file_id="a"),
                         mutate(isofile2$vendor_data_table, file_id="b")))
  
  # include file info
  isofile1 <- modifyList(isofile1, list(file_info = list(test_info = "x")))
  isofile2 <- modifyList(isofile2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_aggregate_vendor_data_table(c(isofile1, isofile2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_aggregate_vendor_data_table(
      c(isoreader:::make_isofile_data_structure(), isofile1, isofile2),
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})