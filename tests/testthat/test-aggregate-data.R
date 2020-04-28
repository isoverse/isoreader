# file info ====

context("File info")

test_that("test that standard file information can be recovered from iso_files", {
  
  expect_true(iso_is_file(iso_file <- make_iso_file_data_structure("NA")))
  iso_file$read_options$file_info <- TRUE
  
  expect_equal(iso_get_file_info(iso_file)$file_id, "NA")
  expect_equal(iso_get_file_info(iso_file)$file_root, NA_character_)
  expect_equal(iso_get_file_info(iso_file)$file_path, NA_character_)
  expect_equal(iso_get_file_info(iso_file)$file_subpath, NA_character_)
  expect_equal(iso_get_file_info(iso_file)$file_datetime, as_datetime(NA, tz = Sys.timezone()))
  
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
  expect_error(iso_get_standards(1), "encountered incompatible data type")
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
  expect_true(identical(
    iso_get_file_info(c(iso_file1, iso_file2)) %>% unnest(multi_value),
    bind_rows(unnest(iso_file1$file_info, multi_value), 
              unnest(iso_file2$file_info, multi_value)) %>% 
      mutate(file_datetime = as_datetime(file_datetime, tz = Sys.timezone()))
  ))
  
  # check select functionality
  expect_equal(names(iso_get_file_info(iso_file1, select = c("file_datetime", "only_a"))), c("file_id", "file_datetime", "only_a"))
  expect_equal(names(iso_get_file_info(iso_file1, select = c(file_datetime, only_a))), c("file_id", "file_datetime", "only_a"))
  expect_equal(names(iso_get_file_info(iso_file1, select = c(x = file_datetime, y = only_a))), c("file_id", "x", "y"))
  expect_equal(names(iso_get_file_info(iso_file1, select = starts_with("file"))), c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", "file_size"))
  expect_warning(iso_get_file_info(iso_file1, select = c(DNE)), "unknown column")
  expect_equal(names(iso_get_file_info(c(iso_file1, iso_file2), select = c(starts_with("file"), "only_a"))), c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", "file_size", "only_a"))
  # note: not sure how to implement but this should probably throw a warning
  expect_warning(out <- names(iso_get_file_info(iso_file2, select = c("file_datetime", "only_a"))), "unknown column")
  expect_equal(out, c("file_id", "file_datetime"))
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
  iso_file1$raw_data <-
    tibble::tibble(
      tp = 1:10,
      time.s = tp * 0.2,
      v44.mV = runif(10),
      v46.mV = runif(10),
      i47.mA = runif(10),
      vC1.mV = runif(10),
      `r46/44` = v46.mV / v44.mV,
      `d46/44.permil` = runif(10),
      `x45.mA` = runif(10)
    )
  iso_file2$raw_data <-
    tibble::tibble(
      tp = 1:10,
      time.s = tp * 0.2,
      v44.mV = runif(10),
      v46.mV = runif(10),
      v45.mV = runif(10)
    )
  
  expect_message(iso_get_raw_data(c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(iso_get_raw_data(c(iso_file1, iso_file2), quiet = TRUE))
  expect_equal(iso_get_raw_data(c(iso_file1, iso_file2)), 
               data <- bind_rows(mutate(iso_file1$raw_data, file_id="a"), 
                                 mutate(iso_file2$raw_data, file_id = "b")) %>% 
                 select(file_id, everything()))
  
  expect_equal(iso_get_raw_data(c(iso_file1, iso_file2), gather = TRUE), 
               data %>% tidyr::pivot_longer(matches("^[virdx]"), names_to = "column", values_to = "value", values_drop_na = TRUE) %>% 
                 left_join(tibble(
                   column = c("v44.mV", "v46.mV", "i47.mA", "vC1.mV", "r46/44", "d46/44.permil", "x45.mA", "v45.mV"),
                   category = c("mass", "mass", "mass", "channel", "ratio", "delta", "other", "mass"),
                   data = c("44", "46", "47", "1", "46/44", "d46/44", "x45", "45"),
                   units = c("mV", "mV", "mA", "mV", NA_character_, "permil", "mA", "mV")
                 ), by = "column") %>% 
                 select(file_id, tp, time.s, data, units, value, category)
  )
  
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
  
  expect_error(iso_get_standards(make_iso_file_data_structure("NA")), "only dual inlet and continuous flow files")
  expect_error(iso_get_standards(make_scan_data_structure("NA")), "scan files don't have")
  
  iso_file <- make_di_data_structure("NA")
  expect_warning(iso_get_standards_info(iso_file), "deprecated")
  expect_warning(iso_get_standards(iso_file), "read without extracting the method info")
  
  # test data
  iso_file$read_options$method_info <- TRUE
  iso_file$read_options$file_info <- TRUE
  ref_ratios <- tibble(reference = "x", element = "X")
  iso_file1 <- modifyList(iso_file, list(
    file_info = list(file_id = "a"),
    method_info = list(
      standards = tibble(
        standard = "test a", reference = "x"
      ),
      reference_ratios = ref_ratios
    )
  ))
  iso_file2 <-
    modifyList(iso_file, list(
      file_info = list(file_id = "b"),
      method_info = list(
        standards = tibble(
          standard = "test b", reference = "x"
        ),
        reference_ratios = ref_ratios
      )
    ))
  
  expect_message(iso_get_standards(c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(iso_get_standards(c(iso_file1, iso_file2), quiet = TRUE))
  
  # select_specific columns
  expect_equal(iso_get_standards(c(iso_file1, iso_file2), select = file_id:reference), 
               data <- bind_rows(mutate(iso_file1$method_info$standards, file_id="a"), 
                                 mutate(iso_file2$method_info$standards, file_id="b")) %>% 
                 select(file_id, everything()))
  expect_equal(iso_get_standards(c(iso_file1, iso_file2)), 
               left_join(data, ref_ratios, by = "reference"))
  expect_equal(iso_get_standards(c(iso_file1, iso_file2), select = c(file_id)), select(data, file_id))
  expect_equal(iso_get_standards(c(iso_file1, iso_file2), select = c()), select(data, file_id))
  expect_equal(iso_get_standards(c(iso_file1, iso_file2), select = NULL), select(data, file_id))
  expect_warning(iso_get_standards(c(iso_file1, iso_file2), select = c(file_id, DNA)), "unknown column")
  
  # include file info
  iso_file1 <- modifyList(iso_file1, list(file_info = list(test_info = "x")))
  iso_file2 <- modifyList(iso_file2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_get_standards(c(iso_file1, iso_file2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_get_standards(
      c(make_di_data_structure("NA"), iso_file1, iso_file2), 
      include_file_info = c("test_info")))$test_info %>% unique(),
    c("x", "y")
  )
  
})


## check resistors aggreation ====

test_that("test that aggregating of resistors works", {
  
  iso_file <- make_iso_file_data_structure("NA")
  expect_warning(iso_get_resistors_info(iso_file), "deprecated")
  expect_warning(iso_get_resistors (iso_file), "read without extracting the method info")
  
  # test data
  iso_file$read_options$method_info <- TRUE
  iso_file$read_options$file_info <- TRUE
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                       method_info = list(resistors = tibble(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11)))))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                       method_info = list(resistors = tibble(cup = 1:3, R.Ohm = c(3e9, 1e11, 1e12)))))
  
  expect_message(iso_get_resistors (c(iso_file1, iso_file2), quiet = FALSE), "aggregating")
  expect_silent(iso_get_resistors (c(iso_file1, iso_file2), quiet = TRUE))
  expect_equal(iso_get_resistors (c(iso_file1, iso_file2)), 
               data <- bind_rows(mutate(iso_file1$method_info$resistors, file_id="a"), 
                                 mutate(iso_file2$method_info$resistors, file_id="b")) %>% 
                 select(file_id, everything()))
  
  # select specific columns
  expect_equal(iso_get_resistors(c(iso_file1, iso_file2), select = c(-cup)), select(data, -cup))
  expect_equal(iso_get_resistors(c(iso_file1, iso_file2), select = c()), select(data, file_id))
  expect_equal(iso_get_resistors(c(iso_file1, iso_file2), select = NULL), select(data, file_id))
  expect_warning(iso_get_resistors(c(iso_file1, iso_file2), select = c(file_id, test)), "unknown column")
  
  # include file info
  iso_file1 <- modifyList(iso_file1, list(file_info = list(test_info = "x")))
  iso_file2 <- modifyList(iso_file2, list(file_info = list(test_info = "y")))
  expect_true("test_info" %in% names(agg <- iso_get_resistors (c(iso_file1, iso_file2), include_file_info = c("test_info"))))
  expect_equal(unique(agg$test_info), c("x", "y"))
  
  # make sure that files that have no raw data do not get added back in by including file info
  expect_equal(
    suppressWarnings(iso_get_resistors (
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
                 iso_make_units_explicit() %>% 
                 select(file_id, everything()))
  expect_equal(iso_get_vendor_data_table(c(iso_file1, iso_file2), with_explicit_units = FALSE), 
               vctrs::vec_rbind(mutate(iso_file1$vendor_data_table, file_id="a"),
                         mutate(iso_file2$vendor_data_table, file_id="b")) %>% 
                 select(file_id, everything()))
  
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
  expect_warning(iso_get_data(iso_file), "deprecated")
  expect_warning(iso_get_all_data(iso_file), "read without extracting the file info")
  expect_warning(iso_get_all_data(iso_file), "read without extracting the raw data")
  expect_warning(iso_get_all_data(iso_file), "read without extracting the vendor data table")
  expect_warning(iso_get_all_data(iso_file), "read without extracting the method info")
  
  # total get_data structure
  iso_file$read_options$file_info <- TRUE
  iso_file$read_options$method_info <- TRUE
  iso_file$read_options$raw_data <- TRUE
  iso_file$read_options$vendor_data_table <- TRUE
  expect_equal( # default
    names(iso_get_all_data(iso_file)), 
    c("file_id", "file_type", "file_info", "raw_data", "standards", "resistors", "vendor_data_table")
  )
  expect_equal( # with problems
    names(iso_get_all_data(iso_file, include_problems = everything())), 
    c("file_id", "file_type", "file_info", "raw_data", "standards", "resistors", "vendor_data_table", "problems")
  )
  expect_equal( # without raw data
    names(iso_get_all_data(iso_file, include_raw_data = NULL)), 
    c("file_id", "file_type", "file_info", "standards", "resistors", "vendor_data_table")
  )
  expect_equal( # without file_info
    names(iso_get_all_data(iso_file, include_file_info = NULL)), 
    c("file_id", "file_type", "raw_data", "standards", "resistors", "vendor_data_table")
  )
  expect_equal( # without standards
    names(iso_get_all_data(iso_file, include_standards = NULL)), 
    c("file_id", "file_type", "file_info", "raw_data", "resistors", "vendor_data_table")
  )
  expect_equal( # without resistors
    names(iso_get_all_data(iso_file, include_resistors = NULL)), 
    c("file_id", "file_type", "file_info", "raw_data", "standards", "vendor_data_table")
  )
  expect_equal( # without vendor_data_table
    names(iso_get_all_data(iso_file, include_vendor_data_table = NULL)), 
    c("file_id", "file_type", "file_info", "raw_data", "standards", "resistors")
  )
  expect_equal(iso_get_all_data(iso_file)$file_type, "continuous_flow")
  
  # info messages
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a")))
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b")))
  expect_message(iso_get_all_data(iso_file), "aggregating all data from 1 data file")
  expect_message(iso_get_all_data(c(iso_file1, iso_file2)), "aggregating all data from 2 data file")
  
  # file_info
  expect_warning(iso_get_all_data(c(iso_file1, iso_file2), include_file_info = x), "unknown column")
  iso_file1$file_info$a <- 42
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2), include_file_info = c(x = a)) %>% unnest(file_info) %>% dplyr::pull(x), c(42, NA_real_))
  
  # raw data
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(raw_data) %>% nrow(), 0L)
  iso_file1$raw_data <- tibble(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), `r46/44` = v46.mV/v44.mV)
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(raw_data) %>% dplyr::pull(file_id) %>% unique(), "a")
  iso_file2$raw_data <- tibble(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10), v45.mV = runif(10))
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(raw_data) %>% dplyr::pull(file_id) %>% unique(), c("a", "b"))
  expect_false("v44.mV" %in% (iso_get_all_data(c(iso_file1, iso_file2), include_raw_data = c(-v44.mV)) %>% unnest(raw_data) %>% names()))
  
  # standards
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(standards) %>% nrow(), 0L)
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                         method_info = list(standards = tibble(standard = "test a"))))
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(standards) %>% dplyr::pull(file_id) %>% unique(), "a")
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                         method_info = list(standards = tibble(standard = "test a"))))
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(standards) %>% dplyr::pull(file_id) %>% unique(), c("a", "b"))
  expect_true(is_tibble(out <- iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(standards)))
  expect_equal(select(out, standard), bind_rows(iso_file1$method_info$standards, iso_file2$method_info$standards))
  expect_false("standard" %in% names(iso_get_all_data(c(iso_file1, iso_file2), include_standards = c(-standard)) %>% unnest(standards)))
  
  # resistors
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(resistors) %>% nrow(), 0L)
  iso_file1 <- modifyList(iso_file, list(file_info = list(file_id = "a"), 
                                         method_info = list(resistors = tibble(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11)))))
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(resistors) %>% dplyr::pull(file_id) %>% unique(), "a")
  iso_file2 <- modifyList(iso_file, list(file_info = list(file_id = "b"),
                                         method_info = list(resistors = tibble(cup = 1:3, R.Ohm = c(3e9, 1e11, 1e12)))))
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(resistors) %>% dplyr::pull(file_id) %>% unique(), c("a", "b"))
  expect_true(is_tibble(out <- iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(resistors)))
  expect_equal(select(out, cup, R.Ohm), bind_rows(iso_file1$method_info$resistors, iso_file2$method_info$resistors))
  expect_true(is_tibble(out <- iso_get_all_data(c(iso_file1, iso_file2), include_resistors = c(-cup)) %>% unnest(resistors)))
  expect_false("cup" %in% names(out))
  
  # vendor data table
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(vendor_data_table) %>% nrow(), 0L)
  iso_file1$vendor_data_table <- tibble(column1 = "col1 a", column2 = "col2 a")
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(vendor_data_table) %>% dplyr::pull(file_id) %>% unique(), "a")
  iso_file2$vendor_data_table <- tibble(column1 = "col1 b", column2 = "col2 b")
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(vendor_data_table) %>% dplyr::pull(file_id) %>% unique(), c("a", "b"))
  expect_true(is_tibble(out <- iso_get_all_data(c(iso_file1, iso_file2)) %>% unnest(vendor_data_table)))
  expect_equal(select(out,column1, column2), bind_rows(iso_file1$vendor_data_table, iso_file2$vendor_data_table) )
  expect_true(is_tibble(out <- iso_get_all_data(c(iso_file1, iso_file2), include_vendor_data_table = c(x = column1)) %>% unnest(vendor_data_table)))
  expect_equal(out$x, bind_rows(iso_file1$vendor_data_table, iso_file2$vendor_data_table)$column1)
  
  # problems
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2), include_problems = everything()) %>% unnest(problems) %>% nrow(), 0L)
  iso_file1 <- iso_file1 %>% register_error("test", warn = FALSE)
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2), include_problems = everything()) %>% unnest(problems) %>% dplyr::pull(file_id) %>% unique(), "a")  
  iso_file2 <- iso_file2 %>% register_error("test2", warn = FALSE)
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2), include_problems = everything()) %>% unnest(problems) %>% dplyr::pull(file_id) %>% unique(), c("a", "b"))
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2), include_problems = everything()) %>% select(file_id, problems) %>% unnest(problems),
               iso_get_problems(c(iso_file1, iso_file2)))
  expect_equal(iso_get_all_data(c(iso_file1, iso_file2), include_problems = c(test = type)) %>% select(file_id, problems) %>% unnest(problems),
               iso_get_problems(c(iso_file1, iso_file2)) %>% select(file_id, test = type))
    
})

