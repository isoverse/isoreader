context("Dual Inlet Files")

# data structures =========

test_that("test that supported di files are correct", {
  initialize_options()
  expect_is(exts <- get_supported_di_files(), "data.frame")
  expect_equal(exts$extension, c(".did", ".caf", ".txt", ".di.rda", ".di.rds"))
  expect_true(all(exts$func %>% sapply(class) == "character"))
  expect_true(all(exts$func %>% map_lgl(exists, mode = "function", where = asNamespace("isoreader"))))
})

test_that("test that parameter checks are performed", {

  expect_error(iso_read_did(make_cf_data_structure("NA")), 
               "data structure must be a \\'dual_inlet\\' iso_file")
  
})

# nu files ======

test_that("test that nu file processor works properly", {
  
  expect_error(group_lines(list()))
  expect_error(group_lines(""))
  
})

# actual files ========

test_that("test that did files can be read", {
  
  # skip on CRAN to reduce checktime to below 10 minutes
  skip_on_cran()
  
  # check if tests are enabled
  run_file_tests <- getOption("isoreader.run_file_tests")
  if (!is.null(run_file_tests) && identical(run_file_tests, FALSE)) {
    skip("Currently not testing all dual inlet data files.")
  }
  
  # test specific files
  iso_turn_reader_caching_off()
  
  # .did
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_example.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  expect_true(length(did$vendor_data_table %>% iso_get_units() %>% na.omit()) == 0)
  
  # .caf
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_example.caf")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  expect_true(length(did$vendor_data_table %>% iso_get_units() %>% na.omit()) == 8)
  
  # .txt (nu)
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_nu_example.txt")))
  expect_is(did <- iso_read_dual_inlet(file, nu_masses = 49:44, read_cache = FALSE), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  expect_warning(did <- iso_read_dual_inlet(file, read_cache = FALSE), "encountered 1 problem\\.")
  expect_true(stringr::str_detect(iso_get_problems(did)$details, fixed("found 6 channels but 0 masses were specified")))
  expect_equal(nrow(problems(did)), 1)
  
  # additional test files (skip on CRAN because test files not includes due to tarball size limits) =====
  skip_on_cran()
  
  # testing wrapper
  check_dual_inlet_test_file <- function(file, ...) {
    file_path <- get_isoreader_test_file(file, local_folder = test_data)
    expect_true(file.exists(file_path))
    expect_is(did <- iso_read_dual_inlet(file_path, ...), "dual_inlet")
    expect_equal(nrow(problems(did)), 0)
    return(invisible(did))
  }
  
  # .did files
  test_data <- file.path("test_data") # test_data <- file.path("tests", "testthat", "test_data") # direct
  check_dual_inlet_test_file("did_example_air.did")
  check_dual_inlet_test_file("did_example_CO2_clumped_01.did")
  check_dual_inlet_test_file("did_example_many_cycles.did")
  check_dual_inlet_test_file("did_example_unicode.did")
  check_dual_inlet_test_file("did_ultra_example.did")
  check_dual_inlet_test_file("caf_example_CO2_01.caf")
  
  # minimal files
  test_data <- file.path("minimal_data") # test_data <- file.path("tests", "testthat", "minimal_data") # direct
  did1 <- check_dual_inlet_test_file("minimal_01.did")
  did2 <- check_dual_inlet_test_file("minimal_02.did")
  did3 <- check_dual_inlet_test_file("minimal_03.did")
  did4 <- check_dual_inlet_test_file("minimal_04.did")
  expect_true(iso_is_file_list(dids <- c(did1, did2, did3, did4)))
  expect_true(dids %>% iso_get_file_info(select = c(Comment, starts_with("MS"))) %>% 
                mutate(MSIT_correct = Comment == paste(MS_integration_time.s, "sec")) %>% 
                { all(.$MSIT_correct) })
  
})
