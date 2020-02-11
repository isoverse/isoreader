context("Scan")

test_that("test that supported scan files are correct", {
  initialize_options()
  expect_is(exts <- get_supported_scan_files(), "data.frame")
  expect_equal(exts$extension, c(".scn"))
  expect_true(all(exts$func %>% sapply(class) == "character"))
  expect_true(all(exts$func %>% map_lgl(exists, mode = "function", where = asNamespace("isoreader"))))
})

test_that("test that parameter checks are performed", {
  
  # flow iarc
  expect_error(iso_read_scn (make_di_data_structure("NA")), 
               "data structure must be a \\'scan\\' iso_file")
  
  
})


test_that("test that scn files can be read", {
  # test specific files
  
  # FIXME: re-enable for commits
  #skip("Currently not testing all scan data files.")
  # FIXME: run as one batch to make use of parallel processing
  
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
  
  test_folder <- file.path("test_data") 
  
  expect_true(file.exists(file <- file.path(test_folder, "scan_hv_01.scn")))
  expect_is(scan <- iso_read_scan(file), "scan")
  expect_equal(nrow(filter(problems(scan), type != "warning")), 0)

  
})

