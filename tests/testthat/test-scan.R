context("Scan")

test_that("test that supported scan files are correct", {
  initialize_options()
  expect_is(exts <- get_supported_scan_files(), "data.frame")
  expect_equal(exts$extension, c(".scn", ".scan.rds"))
  expect_true(all(exts$func %>% sapply(class) == "character"))
  expect_true(all(exts$func %>% map_lgl(exists, mode = "function", where = asNamespace("isoreader"))))
})

test_that("test that parameter checks are performed", {
  
  expect_error(iso_read_scn (make_di_data_structure("NA")), 
               "data structure must be a \\'scan\\' iso_file")
  
  
})


test_that("test that scn files can be read", {
  
  # skip on CRAN to reduce checktime to below 10 minutes
  skip_on_cran()
  
  # check if tests are enabled
  run_file_tests <- getOption("isoreader.run_file_tests")
  if (!is.null(run_file_tests) && identical(run_file_tests, FALSE)) {
    skip("Currently not testing all scan data files.")
  }
  
  # test specific files
  iso_turn_reader_caching_off()
  iso_turn_datetime_warnings_off()
  
  # testing wrapper
  check_scan_test_file <- function(file_path) {
    expect_true(file.exists(file_path))
    expect_is(scn <- iso_read_scan(file_path), "scan")
    expect_equal(nrow(problems(scn)), 0)
    return(invisible(scn))
  }
  
  # example files bundled with the package
  check_scan_test_file(iso_get_reader_example("peak_shape_scan_example.scn"))
  check_scan_test_file(iso_get_reader_example("background_scan_example.scn"))
  check_scan_test_file(iso_get_reader_example("full_scan_example.scn"))
  check_scan_test_file(iso_get_reader_example("time_scan_example.scn"))
  
  # additional test files (skip on CRAN because test files not includes due to tarball size limits) =====
  skip_on_cran()
  test_data <- "minimal_data" # test_data <- file.path("tests", "testthat", "minimal_data") # direct
  check_scan_test_file(file.path(test_data, "scan_hv_01.scn"))
  check_scan_test_file(file.path(test_data, "scan_hv_02.scn"))
  check_scan_test_file(file.path(test_data, "scan_hv_03.scn"))
  
})

