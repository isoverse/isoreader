# file info ====

context("File info")

test_that("test that standard file information can be recovered from iso_files", {
  
  expect_true(iso_is_file(iso_file <- make_iso_file_data_structure("NA")))
  iso_file$read_options$file_info <- TRUE
  
  expect_equal(iso_get_file_info(iso_file)$file_id, "NA")
  expect_equal(iso_get_file_info(iso_file)$file_root, NA_character_)
  expect_equal(iso_get_file_info(iso_file)$file_path, NA_character_)
  expect_equal(iso_get_file_info(iso_file)$file_subpath, NA_character_)
  expect_equal(iso_get_file_info(iso_file)$file_datetime, NA_integer_)
  
  iso_file$file_info$file_id <- "id"
  iso_file$file_info$file_root <- "root"
  iso_file$file_info$file_path <- "path"
  iso_file$file_info$file_subpath <- "subpath"
  iso_file$file_info$file_datetime <- parse_datetime("2010-11-12 13:14:15")
  
  expect_equal(iso_get_file_info(iso_file)$file_id, iso_file$file_info$file_id)
  expect_equal(iso_get_file_info(iso_file)$file_root, iso_file$file_info$file_root)
  expect_equal(iso_get_file_info(iso_file)$file_path, iso_file$file_info$file_path)
  expect_equal(iso_get_file_info(iso_file)$file_subpath, iso_file$file_info$file_subpath)
  
  # Note: this also tests the timezone switch to local timezone that happens in aggregation
  expect_equal(iso_get_file_info(iso_file)$file_datetime, 
               lubridate::with_tz(iso_file$file_info$file_datetime, tz = Sys.timezone()))
  
})

context("Data aggregation")

## check read options ====

test_that("test that read option checks work properly", {
  iso_file <- make_iso_file_data_structure("NA")
  expect_warning(check_read_options(iso_file, "raw_data"), "read without extracting the raw data")
  iso_file <- modifyList(iso_file, list(read_options = list(raw_data = TRUE)))
  expect_silent(check_read_options(iso_file, "raw_data"))
  expect_warning(check_read_options(iso_file, "file_info"), "read without extracting the file info")
})

## check aggregate functions' errors ====

test_that("test that aggregation functions refuse to work with non iso_files", {
  expect_error(iso_get_data_summary(1), "encountered incompatible data type")
  expect_error(iso_get_raw_data(1), "encountered incompatible data type")
  expect_error(iso_get_file_info(1), "encountered incompatible data type")
  expect_error(iso_get_standards_info(1), "encountered incompatible data type")
  expect_error(iso_get_vendor_data_table(1), "encountered incompatible data type")
})

## check aggregation helpers ====

test_that("test that unnesting of aggregated data works properly", {
  
  df <- tibble(int = list(5L), dbl = list(4.2), chr = list("chr"), lgl = list(TRUE))
  
  # simple unnest
  expect_equal(unnest_aggregated_data_frame(df), unnest(df, cols = everything()))
  # check on datetime (not quite the same due to integer --> datetime conversion and back)
  dt <- Sys.time()
  expect_true((unnest_aggregated_data_frame(tibble(dt = list(dt)))$dt - dt) < 10)
  # unnest even with NULLs present
  expect_equal(
    bind_rows(df, select(df, -int)) %>% unnest_aggregated_data_frame(),
    bind_rows(unnest(df, cols = everything()), unnest(select(df, -int), cols = everything()))
  )
  # don't unnest mixed type columns (throw warning instead)
  expect_warning(
    dt_unnest <- unnest_aggregated_data_frame(bind_rows(df, mutate(df, dbl=chr))),
    "different value types"
  )
  expect_equal(
    dt_unnest$dbl,
    bind_rows(unnest(df, cols = c(int, chr, lgl)), 
              unnest(mutate(df, dbl=chr), cols = c(int, chr, lgl)))$dbl
  )
  # don't unnest multi value columns
  df2 <- mutate(df, chr = map(chr, ~c("ch1", "ch2")))
  expect_equal(
    unnest_aggregated_data_frame(bind_rows(df, df2))$chr,
    c(df$chr, df2$chr)
  )
  # replace missing entries with NA instead (for string)
  expect_equal(
    unnest_aggregated_data_frame(bind_rows(select(df, -chr), df2))$chr,
    c(NA_character_, df2$chr)
  )
  # replace missing entries with NA instead (for integer)
  df2 <- mutate(df, int = map(int, ~c(1L,2L)))
  expect_equal(
    unnest_aggregated_data_frame(bind_rows(select(df, -int), df2))$int,
    c(NA_integer_, df2$int)
  )
  # replace missing entries with NA instead (for double)
  df2 <- mutate(df, dbl = map(dbl, ~c(1.0, 4.2)))
  expect_equal(
    unnest_aggregated_data_frame(bind_rows(select(df, -dbl), df2))$dbl,
    c(NA_real_, df2$dbl)
  )
  
})

## check data summary ====

test_that("test that data summary is accessible", {
  
  iso_file <- make_cf_data_structure("NA")
  expect_true(is.data.frame(iso_get_data_summary(iso_file)))
  
  # test data
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a")))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b")))
  expect_equal(nrow(iso_get_data_summary(c(iso_file1, iso_file2))), 2)
  
  # FIXME: continue here testing the individual get...infos functions
})

## check get file info works =====

test_that("test that aggregating file info works", {

  iso_file <- make_iso_file_data_structure("NA")
  expect_warning(iso_get_file_info(iso_file), "read without extracting the file info")
  
  # test data
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- iso_file2 <- iso_file
  iso_file1$file_info <- mutate(iso_file1$file_info, file_id = "a", test_info = "x", multi_value = list(1:2), only_a = TRUE)
  iso_file2$file_info <- mutate(iso_file2$file_info, file_id = "b", test_info = "y", multi_value = list(1:3))
  
  expect_message(iso_get_file_info(c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(agg <- iso_get_file_info(c(iso_file1, iso_file2), quiet = TRUE))
  expect_equal(names(agg), unique(names(iso_file1$file_info), names(iso_file2$file_info)))
  expect_equal(iso_get_file_info(c(iso_file1, iso_file2)) %>% unnest(multi_value) ,
               bind_rows(unnest(iso_file1$file_info, multi_value), 
                         unnest(iso_file2$file_info, multi_value)))
  
  # check select functionality
  expect_equal(names(iso_get_file_info(iso_file1, select = c("file_datetime", "only_a"))), c("file_id", "file_datetime", "only_a"))
  expect_equal(names(iso_get_file_info(iso_file1, select = c(file_datetime, only_a))), c("file_id", "file_datetime", "only_a"))
  expect_equal(names(iso_get_file_info(iso_file1, select = c(x = file_datetime, y = only_a))), c("file_id", "x", "y"))
  expect_equal(names(iso_get_file_info(iso_file1, select = starts_with("file"))), c("file_id", "file_root", "file_path", "file_subpath", "file_datetime"))
  expect_error(iso_get_file_info(iso_file1, select = c(x = file_id)), "renaming.*file_id.*may lead to unpredictable")
  expect_equal(names(iso_get_file_info(iso_file1, select = c(starts_with("file")))), c("file_id", "file_root", "file_path", "file_subpath", "file_datetime"))
  # note: not sure how to implement but this should probably throw a warning
  expect_equal(names(iso_get_file_info(iso_file2, select = c("file_datetime", "only_a"))), c("file_id", "file_datetime"))
})


## check raw data aggregation ========

test_that("test that aggregeting raw data works", {
  
  iso_file <- make_iso_file_data_structure("NA")
  expect_warning(iso_get_raw_data(iso_file), "read without extracting the raw data")
  
  # test data
  iso_file$read_options$raw_data <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a")))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b")))
  iso_file1$raw_data <- dplyr::tibble(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), `r46/44` = v46.mV/v44.mV)
  iso_file2$raw_data <- dplyr::tibble(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), v45.mV = runif(10))
  
  expect_message(iso_get_raw_data(c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(iso_get_raw_data(c(iso_file1, iso_file2), quiet = TRUE))
  expect_equal(iso_get_raw_data(c(iso_file1, iso_file2)), 
               data <- bind_rows(mutate(iso_file1$raw_data, file_id="a"), 
                                 mutate(iso_file2$raw_data, file_id = "b")))
  
  expect_equal(iso_get_raw_data(c(iso_file1, iso_file2), gather = TRUE), 
               data %>% gather(column, value, starts_with("v"), starts_with("r")) %>% 
                 left_join(tibble(
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
      c(make_iso_file_data_structure("NA"), iso_file1, iso_file2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
})


## check standards aggreation ====

test_that("test that aggregating of methods standards works", {
  
  iso_file <- make_iso_file_data_structure("NA")
  expect_warning(iso_get_standards_info(iso_file), "read without extracting the method info")
  
  # test data
  iso_file$read_options$method_info <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                       method_info = list(standards = tibble(standard = "test a"))))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                       method_info = list(standards = tibble(standard = "test a"))))
  
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
      c(make_iso_file_data_structure("NA"), iso_file1, iso_file2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})


## check resistors aggreation ====

test_that("test that aggregating of resistors works", {
  
  iso_file <- make_iso_file_data_structure("NA")
  expect_warning(iso_get_resistors_info (iso_file), "read without extracting the method info")
  
  # test data
  iso_file$read_options$method_info <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                       method_info = list(resistors = tibble(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11)))))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                       method_info = list(resistors = tibble(cup = 1:3, R.Ohm = c(3e9, 1e11, 1e12)))))
  
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
      c(make_iso_file_data_structure("NA"), iso_file1, iso_file2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})


## check vendor data table aggreation ====

test_that("test that aggregating of vendor data table works", {
  
  expect_error(iso_get_vendor_data_table(make_iso_file_data_structure("NA")), "only dual inlet and continuous flow files")
  expect_error(iso_get_vendor_data_table(make_scan_data_structure("NA")), "scan files don't have")
  iso_file <- make_cf_data_structure("NA")
  expect_warning(iso_get_vendor_data_table(iso_file), "read without extracting the vendor data table")
  
  # test data
  iso_file$read_options$vendor_data_table <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a")))
  iso_file1$vendor_data_table <- tibble(column1 = "col1 a", column2 = 1, col_a_only = "test a")
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b")))
  iso_file2$vendor_data_table <- tibble(column1 = "col1 b", column2 = 2)
  
  # unit information
  expect_warning(iso_get_vendor_data_table(iso_file1, with_units = TRUE), "DEPRECATED")
  expect_equal(
    # the same if there are no units
    iso_get_vendor_data_table(iso_file1, with_explicit_units = TRUE), 
    iso_get_vendor_data_table(iso_file1, with_explicit_units = FALSE)
  )
  expect_message(iso_get_vendor_data_table(iso_file1, quiet = FALSE), "aggregating")
  expect_silent(iso_get_vendor_data_table(iso_file1, quiet = TRUE))
  
  attr(iso_file1$vendor_data_table, "units") <- attr(iso_file2$vendor_data_table, "units") <- 
    tibble(column = c("column2"), units = c("[2]"))
  iso_file1$vendor_data_table <- convert_df_units_attr_to_implicit_units(iso_file1$vendor_data_table)
  iso_file2$vendor_data_table <- convert_df_units_attr_to_implicit_units(iso_file2$vendor_data_table)
  
  # aggregated with and without units
  expect_message(agg <- iso_get_vendor_data_table(c(iso_file1, iso_file2), with_explicit_units = TRUE, quiet = FALSE), "aggregating")
  expect_equal(agg, 
               vctrs::vec_rbind(mutate(iso_file1$vendor_data_table, file_id="a"),
                              mutate(iso_file2$vendor_data_table, file_id="b")) %>% 
                 iso_make_units_explicit())
  expect_equal(iso_get_vendor_data_table(c(iso_file1, iso_file2), with_explicit_units = FALSE), 
               vctrs::vec_rbind(mutate(iso_file1$vendor_data_table, file_id="a"),
                         mutate(iso_file2$vendor_data_table, file_id="b")))
  
  # selecting/renaming specific columns
  isofiles <- c(iso_file1, iso_file2)
  expect_warning(agg <- iso_get_vendor_data_table(isofiles, select = c(bla, column1)), "unknown column")
  expect_equal(names(agg), c("file_id", "column1"))
  expect_equal(names(iso_get_vendor_data_table(isofiles, select = c(file_id, column1))), c("file_id", "column1"))
  expect_equal(names(iso_get_vendor_data_table(isofiles, select = c(x = file_id, y = column1))), c("x", "y"))
  expect_equal(names(iso_get_vendor_data_table(isofiles, select = c(z = starts_with("file")))), c("z"))
  expect_equal(names(iso_get_vendor_data_table(isofiles, select = c())), c("file_id"))
  
  # include file info
  iso_file1 <- modifyList(iso_file1, list(file_info = list(test_info = "x")))
  iso_file2 <- modifyList(iso_file2, list(file_info = list(test_info = "y")))
  isofiles <- c(iso_file1, iso_file2)
  expect_true("test_info" %in% names(agg <- iso_get_vendor_data_table(isofiles, include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  expect_equal(names(iso_get_vendor_data_table(isofiles, select = column1, include_file_info = c(a = test_info))), c("file_id", "a", "column1"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_get_vendor_data_table(
      c(make_cf_data_structure("NA"), iso_file1, iso_file2),
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})

## check overall get data =====

test_that("test that total data aggregation works", {
  
  # general warning messages
  iso_file <- make_cf_data_structure("NA")
  expect_warning(data <- iso_get_data(iso_file), "read without extracting the file info")
  expect_warning(data <- iso_get_data(iso_file), "read without extracting the raw data")
  expect_warning(data <- iso_get_data(iso_file), "read without extracting the vendor data table")
  expect_warning(data <- iso_get_data(iso_file), "read without extracting the method info")
  
  # total get_data structure
  expect_equal(names(data), c("file_id", "file_type", "file_info", 
                              "raw_data", "vendor_data_table", "standards", "resistors", 
                              "has_file_info", "has_raw_data", "has_vendor_data_table", "has_standards", "has_resistors"))
  expect_equal(data %>% select(starts_with("has")),
               tibble(has_file_info = TRUE, has_raw_data = FALSE, has_vendor_data_table = FALSE, has_standards = FALSE, has_resistors = FALSE))  
  expect_equal(data$file_type, "continuous_flow")
  
  # info messages
  iso_file$read_options$file_info <- TRUE
  iso_file$read_options$method_info <- TRUE
  iso_file$read_options$raw_data <- TRUE
  iso_file$read_options$vendor_data_table <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a")))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b")))
  expect_message(iso_get_data(iso_file), "aggregating all data from 1 data file")
  expect_message(iso_get_data(c(iso_file1, iso_file2)), "aggregating all data from 2 data file")
  
  # vendor data table
  iso_file1$vendor_data_table <- tibble(column1 = "col1 a", column2 = "col2 a")
  expect_equal(iso_get_data(c(iso_file1, iso_file2))$has_vendor_data_table, c(TRUE, FALSE))
  iso_file2$vendor_data_table <- tibble(column1 = "col1 b", column2 = "col2 b")
  expect_equal(iso_get_data(c(iso_file1, iso_file2))$has_vendor_data_table, c(TRUE, TRUE))
  expect_true(is_tibble(out <- iso_get_data(c(iso_file1, iso_file2)) %>% unnest(vendor_data_table)))
  expect_equal(select(out,column1, column2), bind_rows(iso_file1$vendor_data_table, iso_file2$vendor_data_table) )
  expect_true(is_tibble(out <- iso_get_data(c(iso_file1, iso_file2), include_vendor_data_table = c(x = column1)) %>% unnest(vendor_data_table)))
  expect_equal(out$x, bind_rows(iso_file1$vendor_data_table, iso_file2$vendor_data_table)$column1)
  
  # file_info
  iso_file1$file_info$test <- 42
  expect_true(is_tibble(out <- iso_get_data(c(iso_file1, iso_file2), include_file_info = c(x = test)) %>% unnest(file_info)))
  expect_equal(out$x, c(42, NA_real_))
  
  # raw data
  iso_file1$raw_data <- tibble(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), `r46/44` = v46.mV/v44.mV)
  expect_equal(iso_get_data(c(iso_file1, iso_file2))$has_raw_data, c(TRUE, FALSE))
  iso_file2$raw_data <- tibble(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), v45.mV = runif(10))
  expect_equal(iso_get_data(c(iso_file1, iso_file2))$has_raw_data, c(TRUE, TRUE))
  expect_true(is_tibble(out <- iso_get_data(c(iso_file1, iso_file2), include_file_info = c(x = test)) %>% unnest(raw_data)))
  expect_equal(select(out, -file_id, -file_type, -starts_with("has"), 
                      -file_info, -vendor_data_table, -standards, -resistors), 
               bind_rows(iso_file1$raw_data, iso_file2$raw_data))
  
  # standards
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                         method_info = list(standards = tibble(standard = "test a"))))
  expect_equal(iso_get_data(c(iso_file1, iso_file2))$has_standards, c(TRUE, FALSE))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                         method_info = list(standards = tibble(standard = "test a"))))
  expect_equal(iso_get_data(c(iso_file1, iso_file2))$has_standards, c(TRUE, TRUE))
  expect_true(is_tibble(out <- iso_get_data(c(iso_file1, iso_file2)) %>% unnest(standards)))
  expect_equal(select(out, standard), bind_rows(iso_file1$method_info$standards, iso_file2$method_info$standards))

  # resistors
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                         method_info = list(resistors = tibble(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11)))))
  expect_equal(iso_get_data(c(iso_file1, iso_file2))$has_resistors, c(TRUE, FALSE))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                         method_info = list(resistors = tibble(cup = 1:3, R.Ohm = c(3e9, 1e11, 1e12)))))
  expect_equal(iso_get_data(c(iso_file1, iso_file2))$has_resistors, c(TRUE, TRUE))
  expect_true(is_tibble(out <- iso_get_data(c(iso_file1, iso_file2)) %>% unnest(resistors)))
  expect_equal(select(out, cup, R.Ohm), bind_rows(iso_file1$method_info$resistors, iso_file2$method_info$resistors))
      
})
