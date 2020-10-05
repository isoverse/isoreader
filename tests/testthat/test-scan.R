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
  
  # check if tests are enabled
  run_file_tests <- getOption("isoreader.run_file_tests")
  if (!is.null(run_file_tests) && identical(run_file_tests, FALSE)) {
    skip("Currently not testing all scan data files.")
  }
  
  # test specific files
  iso_turn_reader_caching_off()
  
  expect_true(file.exists(file <- iso_get_reader_example("peak_shape_scan_example.scn")))
  expect_is(scan <- iso_read_scan(file), "scan")
  expect_equal(nrow(filter(problems(scan), type != "warning")), 0)
  
  expect_true(file.exists(file <- iso_get_reader_example("background_scan_example.scn")))
  expect_is(scan <- iso_read_scan(file), "scan")
  expect_equal(nrow(filter(problems(scan), type != "warning")), 0)
  
  expect_true(file.exists(file <- iso_get_reader_example("full_scan_example.scn")))
  expect_is(scan <- iso_read_scan(file), "scan")
  expect_equal(nrow(filter(problems(scan), type != "warning")), 0)
  
  expect_true(file.exists(file <- iso_get_reader_example("time_scan_example.scn")))
  expect_is(scan <- iso_read_scan(file), "scan")
  expect_equal(nrow(filter(problems(scan), type != "warning")), 0)
  
  # additional test files =====
  test_folder <- file.path("test_data") # test_folder <- file.path("tests", "testthat", "test_data") # direct
  
  # testing wrapper
  check_scan_test_file <- function(file) {
    file_path <- get_isoreader_test_file(file, local_folder = test_folder)
    expect_true(file.exists(file_path))
    expect_is(scn <- iso_read_scan(file_path), "scan")
    expect_equal(nrow(filter(problems(scn), type != "warning")), 0)
    return(invisible(scn))
  }
  
  check_scan_test_file("scan_hv_01.scn")
  check_scan_test_file("scan_hv_02.scn")
  check_scan_test_file("scan_hv_03.scn")
  
})

