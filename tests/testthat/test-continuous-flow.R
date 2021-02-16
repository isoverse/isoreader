context("Continuous flow")

test_that("test that supported cf files are correct", {
  initialize_options()
  expect_is(exts <- get_supported_cf_files(), "data.frame")
  expect_equal(exts$extension, c(".cf", ".cf.rds", ".dxf", ".iarc"))
  expect_true(all(exts$func %>% sapply(class) == "character"))
  expect_true(all(exts$func %>% map_lgl(exists, mode = "function", where = asNamespace("isoreader"))))
})

test_that("test that parameter checks are performed", {
  
  # flow iarc
  expect_error(iso_read_flow_iarc (make_di_data_structure("NA")), 
               "data structure must be a \\'continuous_flow\\' iso_file")
  
  
})


test_that("test that continous flow files can be read", {
  
  # skip on CRAN to reduce checktime to below 10 minutes
  skip_on_cran()
  
  # test specific files
  iso_turn_reader_caching_off()

  expect_true(file.exists(file <- iso_get_reader_example("continuous_flow_example.cf")))
  expect_is(cf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_true(length(cf$vendor_data_table %>% iso_get_units() %>% na.omit()) == 17)
  expect_equal(nrow(problems(cf)), 0)
  expect_equal(nrow(iso_get_file_info(cf)), 1)
  expect_equal(ncol(iso_get_file_info(cf)), 21)
  expect_equal(nrow(iso_get_raw_data(cf)), 8605)
  expect_equal(ncol(iso_get_raw_data(cf)), 5)
  expect_equal(nrow(iso_get_resistors(cf)), 2)
  expect_equal(ncol(iso_get_resistors(cf)), 4)
  expect_equal(nrow(iso_get_standards(cf)), 3)
  expect_equal(ncol(iso_get_standards(cf)), 9)
  expect_equal(nrow(iso_get_vendor_data_table(cf)), 19)
  expect_equal(ncol(iso_get_vendor_data_table(cf)), 26)
  
  expect_true(file.exists(file <- iso_get_reader_example("continuous_flow_example.dxf")))
  expect_is(cf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(cf)), 0)
  expect_equal(nrow(iso_get_file_info(cf)), 1)
  expect_equal(ncol(iso_get_file_info(cf)), 21)
  expect_equal(nrow(iso_get_raw_data(cf)), 2435)
  expect_equal(ncol(iso_get_raw_data(cf)), 9)
  expect_equal(nrow(iso_get_resistors(cf)), 6)
  expect_equal(ncol(iso_get_resistors(cf)), 4)
  expect_equal(nrow(iso_get_standards(cf)), 7)
  expect_equal(ncol(iso_get_standards(cf)), 9)
  expect_equal(nrow(iso_get_vendor_data_table(cf)), 6)
  expect_equal(ncol(iso_get_vendor_data_table(cf)), 61)
  
  # skip if optional dependencies are not installed
  skip_if_not_installed("xml2")
  skip_if_not_installed("rhdf5")
  expect_true(file.exists(file <- iso_get_reader_example("continuous_flow_example.iarc")))
  expect_is(iarc <- iso_read_continuous_flow(file), "iso_file_list")
  expect_equal(nrow(problems(iarc)), 0)
  
})

test_that("test that additional continous flow files can be read", {
  
  # additional test files (skip on CRAN because test files not includes due to tarball size limits) =====
  skip_on_cran()
  test_folder <- file.path("test_data") # test_folder <- file.path("tests", "testthat", "test_data") # direct
  iso_turn_reader_caching_off()

  # testing wrapper
  check_continuous_flow_test_file <- function(file) {
    file_path <- get_isoreader_test_file(file, local_folder = test_folder)
    expect_true(file.exists(file_path))
    expect_is(dxf <- iso_read_continuous_flow(file_path), "continuous_flow")
    expect_equal(nrow(problems(dxf)), 0)
    return(invisible(dxf))
  }
  
  dxf1 <- check_continuous_flow_test_file("dxf_example_H_01.dxf")
  check_continuous_flow_test_file("dxf_example_HO_01.dxf")
  check_continuous_flow_test_file("dxf_example_HO_02.dxf")
  check_continuous_flow_test_file("dxf_example_C_01.dxf")
  check_continuous_flow_test_file("dxf_example_CN_01.dxf")
  check_continuous_flow_test_file("dxf_example_CNS_01.dxf")
  dxf2 <- check_continuous_flow_test_file("dxf_example_N2_01.dxf")
  check_continuous_flow_test_file("cf_example_CN_01.cf")
  check_continuous_flow_test_file("cf_example_H_01.cf")
  check_continuous_flow_test_file("cf_example_H_02.cf")
  check_continuous_flow_test_file("cf_example_H_03.cf")
  cf1 <- check_continuous_flow_test_file("cf_example_H_04.cf")
  check_continuous_flow_test_file("cf_example_H_05.cf")
  check_continuous_flow_test_file("cf_example_H_06.cf")
  check_continuous_flow_test_file("cf_example_H_07.cf")
  
  # test re-reading =======
  # NOTE: ideally this should also include an iarc file
  iso_files <- c(dxf1, dxf2, cf1)
  expect_true(iso_is_continuous_flow(reread_dxf <- reread_iso_files(iso_files)))
  expect_equal(nrow(problems(reread_dxf)), 0)
  
  
  # test parallel processing ======
  # multisession
  file_paths <-
    file.path(test_folder,
              c("dxf_example_H_01.dxf", "dxf_example_HO_01.dxf", "dxf_example_HO_02.dxf", "dxf_example_CNS_01.dxf", "dxf_example_N2_01.dxf"))
  
  expect_message(files <- iso_read_continuous_flow(file_paths, parallel = TRUE, parallel_plan = future::multisession, parallel_cores = future::availableCores()),
                 sprintf("preparing to read 5 data files.*setting up %.0f parallel processes", min(5, future::availableCores())))
  expect_equal(nrow(problems(files)), 0)
  expect_warning(iso_read_continuous_flow(file_paths, parallel = TRUE, parallel_plan = future::multisession, parallel_cores = future::availableCores() + 1),
                 sprintf("%.0f cores.*requested.*only %.0f.*available", future::availableCores() + 1, future::availableCores()))
  
  # multiproccess
  expect_message(files <- iso_read_continuous_flow(file_paths, parallel = TRUE, parallel_plan = future::multiprocess),
                 "preparing to read 5 data files.*setting up.*parallel processes")
  expect_equal(nrow(problems(files)), 0)
  
})

