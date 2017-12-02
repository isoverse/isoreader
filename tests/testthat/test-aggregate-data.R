# file info ====

context("File info")

test_that("test that file information can be recovered from iso_files", {
  
  expect_true(iso_is_file(iso_file <- isoreader:::make_iso_file_data_structure()))
  
  expect_error(get_file_id(), "no iso_file provided")
  expect_error(get_file_id(42), "can only retrieve file information from an iso_file object")
  expect_equal(get_file_id(iso_file), NA_character_)
  
  expect_error(get_file_path(), "no iso_file provided")
  expect_error(get_file_path(42), "can only retrieve file information from an iso_file object")
  expect_equal(get_file_path(iso_file), NA_character_)
  
  expect_error(get_file_subpath(), "no iso_file provided")
  expect_error(get_file_subpath(42), "can only retrieve file information from an iso_file object")
  expect_equal(get_file_subpath(iso_file), NA_character_)
  
  expect_error(get_file_datetime(), "no iso_file provided")
  expect_error(get_file_datetime(42), "can only retrieve file information from an iso_file object")
  expect_equal(get_file_datetime(iso_file), NA)
  
  iso_file$file_info$file_id <- "id"
  iso_file$file_info$file_path <- "path"
  iso_file$file_info$file_subpath <- "subpath"
  iso_file$file_info$file_datetime <- parse_datetime("2010-11-12 13:14:15")
  
  expect_equal(get_file_id(iso_file), iso_file$file_info$file_id)
  expect_equal(get_file_path(iso_file), iso_file$file_info$file_path)
  expect_equal(get_file_subpath(iso_file), iso_file$file_info$file_subpath)
  expect_equal(get_file_datetime(iso_file), iso_file$file_info$file_datetime)
  
})

context("Data aggregation")

## check read options ====

test_that("test that read option checks work properly", {
  iso_file <- isoreader:::make_iso_file_data_structure()
  expect_warning(isoreader:::check_read_options(iso_file, "raw_data"), "read without extracting the raw data")
  iso_file <- modifyList(iso_file, list(read_options = list(raw_data = TRUE)))
  expect_silent(isoreader:::check_read_options(iso_file, "raw_data"))
  expect_warning(isoreader:::check_read_options(iso_file, "file_info"), "read without extracting the file info")
})

## check aggregate functions' errors ====

test_that("test that aggregation functions refuse to work with non iso_files", {
  expect_error(iso_get_data_summary(1), "encountered incompatible data type")
  expect_error(iso_get_raw_data(1), "encountered incompatible data type")
  expect_error(iso_get_file_info(1), "encountered incompatible data type")
  expect_error(iso_get_standards_info(1), "encountered incompatible data type")
  expect_error(iso_get_vendor_data_table(1), "encountered incompatible data type")
})

## check data summary ====

test_that("test that data summary is accessible", {
  
  iso_file <- isoreader:::make_iso_file_data_structure()
  expect_true(is.data.frame(iso_get_data_summary(iso_file)))
  
  # test data
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a")))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b")))
  expect_equal(nrow(iso_get_data_summary(c(iso_file1, iso_file2))), 2)
})

## check aggregating file info works

test_that("test that aggregating file info works", {

  iso_file <- isoreader:::make_iso_file_data_structure()
  expect_warning(iso_get_file_info(iso_file), "read without extracting the file info")
  
  # test data
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(
    file_info = list(file_id = "a", test_info = "x", multi_value = 1:2, only_a = TRUE)))
  iso_file2 <- modifyList(iso_file, list(
    file_info = list(file_id = "b", test_info = "y", multi_value = 1:3)))
  
  expect_message(iso_get_file_info(c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(agg <- iso_get_file_info(c(iso_file1, iso_file2), quiet = TRUE))
  expect_equal(names(agg), unique(names(iso_file1$file_info), names(iso_file2$file_info)))
  expect_equal(iso_get_file_info(c(iso_file1, iso_file2)), 
               {
                  # check for multi value collapse functionality
                  iso_file1$file_info$multi_value <- str_c(iso_file1$file_info$multi_value, collapse = "; ")
                  iso_file2$file_info$multi_value <- str_c(iso_file2$file_info$multi_value, collapse = "; ")
                  bind_rows(as_data_frame(iso_file1$file_info), as_data_frame(iso_file2$file_info))
               })
  
  # check selecte functionality
  expect_equal(names(iso_get_file_info(iso_file1, select = c("file_datetime", "only_a"))), c("file_id", "file_datetime", "only_a"))
  expect_warning(agg <- iso_get_file_info(iso_file2, select = c("file_datetime", "only_a")), "refers to unknown column")
  expect_equal(names(agg), c("file_id", "file_datetime"))
})


## check raw data aggregation

test_that("test that aggregeting raw data works", {
  
  iso_file <- isoreader:::make_iso_file_data_structure()
  expect_warning(iso_get_raw_data(iso_file), "read without extracting the raw data")
  
  # test data
  iso_file$read_options$raw_data <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a")))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b")))
  iso_file1$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), `r46/44` = v46.mV/v44.mV)
  iso_file2$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), v45.mV = runif(10))
  
  expect_message(iso_get_raw_data(c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(iso_get_raw_data(c(iso_file1, iso_file2), quiet = TRUE))
  expect_equal(iso_get_raw_data(c(iso_file1, iso_file2)), 
               data <- bind_rows(mutate(iso_file1$raw_data, file_id="a"), 
                                 mutate(iso_file2$raw_data, file_id = "b")))
  
  expect_equal(iso_get_raw_data(c(iso_file1, iso_file2), gather = TRUE), 
               data %>% gather(column, value, starts_with("v"), starts_with("r")) %>% 
                 left_join(data_frame(
                   column = c("v44.mV", "v45.mV", "v46.mV", "r46/44"),
                   category = c("mass", "mass", "mass", "ratio"),
                   data = c("44", "45", "46", "46/44"),
                   units = c("mV", "mV", "mV", NA_character_)
                 ), by = "column") %>% select(-column) %>% filter(!is.na(value)))
  
  # include file info
  iso_file1 <- modifyList(iso_file1, list(file_info = list(test_info = "x")))
  iso_file2 <- modifyList(iso_file2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_get_raw_data(c(iso_file1, iso_file2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_get_raw_data(
      c(isoreader:::make_iso_file_data_structure(), iso_file1, iso_file2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
})


## check standards aggreation ====

test_that("test that aggregating of methods standards works", {
  
  iso_file <- isoreader:::make_iso_file_data_structure()
  expect_warning(iso_get_standards_info(iso_file), "read without extracting the method info")
  
  # test data
  iso_file$read_options$method_info <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                       method_info = list(standards = data_frame(standard = "test a"))))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                       method_info = list(standards = data_frame(standard = "test a"))))
  
  expect_message(iso_get_standards_info(c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(iso_get_standards_info(c(iso_file1, iso_file2), quiet = TRUE))
  expect_equal(iso_get_standards_info(c(iso_file1, iso_file2)), 
               data <- bind_rows(mutate(iso_file1$method_info$standards, file_id="a"), 
                                 mutate(iso_file2$method_info$standards, file_id="b")))
  # include file info
  iso_file1 <- modifyList(iso_file1, list(file_info = list(test_info = "x")))
  iso_file2 <- modifyList(iso_file2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_get_standards_info(c(iso_file1, iso_file2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_get_standards_info(
      c(isoreader:::make_iso_file_data_structure(), iso_file1, iso_file2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})


## check resistors aggreation ====

test_that("test that aggregating of resistors works", {
  
  iso_file <- isoreader:::make_iso_file_data_structure()
  expect_warning(iso_get_resistors_info (iso_file), "read without extracting the method info")
  
  # test data
  iso_file$read_options$method_info <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                       method_info = list(resistors = data_frame(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11)))))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                       method_info = list(resistors = data_frame(cup = 1:3, R.Ohm = c(3e9, 1e11, 1e12)))))
  
  expect_message(iso_get_resistors_info (c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(iso_get_resistors_info (c(iso_file1, iso_file2), quiet = TRUE))
  expect_equal(iso_get_resistors_info (c(iso_file1, iso_file2)), 
               data <- bind_rows(mutate(iso_file1$method_info$resistors, file_id="a"), 
                                 mutate(iso_file2$method_info$resistors, file_id="b")))
  # include file info
  iso_file1 <- modifyList(iso_file1, list(file_info = list(test_info = "x")))
  iso_file2 <- modifyList(iso_file2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_get_resistors_info (c(iso_file1, iso_file2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_get_resistors_info (
      c(isoreader:::make_iso_file_data_structure(), iso_file1, iso_file2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})


## check vendor data table aggreation ====

test_that("test that aggregating of vendor data table works", {
  
  iso_file <- isoreader:::make_iso_file_data_structure()
  expect_warning(iso_get_vendor_data_table(iso_file), "read without extracting the vendor data table")
  
  # test data
  iso_file$read_options$vendor_data_table <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a")))
  iso_file1$vendor_data_table <- data_frame(column1 = "col1 a", column2 = "col2 a", col_a_only = "test a")
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b")))
  iso_file2$vendor_data_table <- data_frame(column1 = "col1 b", column2 = "col2 b")
  
  # unit information
  expect_warning(iso_get_vendor_data_table(iso_file1, with_units = TRUE), "do not have unit information")
  expect_message(iso_get_vendor_data_table(iso_file1, with_units = FALSE, quiet = FALSE), "aggregating")
  expect_silent(iso_get_vendor_data_table(iso_file1, with_units = FALSE, quiet = TRUE))
  
  attr(iso_file1$vendor_data_table, "units") <- attr(iso_file2$vendor_data_table, "units") <- 
    data_frame(column = c("column1", "column2", "col_a_only"), units = c("[1]", "[2]", ""))
  
  # selecting subsets
  expect_warning(agg <- iso_get_vendor_data_table(c(iso_file1, iso_file2), select = "bla"), "unknown column")
  expect_equal(names(agg), "file_id")
  expect_equal(iso_get_vendor_data_table(c(iso_file1, iso_file2), select = c(file_id, column1), with_units = FALSE) %>% 
                 names(), c("file_id", "column1"))
  
  # aggregated with and without units
  expect_message(agg <- iso_get_vendor_data_table(c(iso_file1, iso_file2), with_units = TRUE, quiet = FALSE), "aggregating")
  expect_equal(agg, 
               bind_rows(mutate(iso_file1$vendor_data_table, file_id="a"),
                              mutate(iso_file2$vendor_data_table, file_id="b")) %>% 
                 rename(`column1 [1]` = column1, `column2 [2]` = column2))
  expect_equal(iso_get_vendor_data_table(c(iso_file1, iso_file2), with_units = FALSE), 
               bind_rows(mutate(iso_file1$vendor_data_table, file_id="a"),
                         mutate(iso_file2$vendor_data_table, file_id="b")))
  
  # include file info
  iso_file1 <- modifyList(iso_file1, list(file_info = list(test_info = "x")))
  iso_file2 <- modifyList(iso_file2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_get_vendor_data_table(c(iso_file1, iso_file2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_get_vendor_data_table(
      c(isoreader:::make_iso_file_data_structure(), iso_file1, iso_file2),
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})
