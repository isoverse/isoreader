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
  
  
  # check if tests are enabled
  run_file_tests <- getOption("isoreader.run_file_tests")
  if (!is.null(run_file_tests) && identical(run_file_tests, FALSE)) {
    skip("Currently not testing all dual inlet data files.")
  }
  
  # test specific files
  iso_turn_reader_caching_off()
  
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_example.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  expect_true(length(did$vendor_data_table %>% iso_get_units() %>% na.omit()) == 0)
  
  expect_true(file.exists(file <- file.path("test_data", "did_example_CO2_clumped_01.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "did_example_many_cycles.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "did_example_unicode.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)

  expect_true(file.exists(file <- file.path("test_data", "did_ultra_example.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  # minimal files
  expect_true(dir.exists(files <- file.path("test_data", "minimal_files")))
  expect_true(iso_is_file_list(dids <- iso_read_dual_inlet(files)))
  expect_true(dids %>% iso_get_file_info(select = c(Comment, starts_with("MS"))) %>% 
                mutate(MSIT_correct = Comment == paste(MS_integration_time.s, "sec")) %>% 
                { all(.$MSIT_correct) })
  
  # .caf files
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_example.caf")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  expect_true(length(did$vendor_data_table %>% iso_get_units() %>% na.omit()) == 0)
  
  expect_true(file.exists(file <- file.path("test_data", "caf_example_CO2_01.caf")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  # nu files
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_nu_example.txt")))
  expect_is(did <- iso_read_dual_inlet(file, nu_masses = 49:44, read_cache = FALSE), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  expect_message(capture.output(did <- iso_read_dual_inlet(file, read_cache = FALSE)), "6 channels but 0 masses")
  expect_equal(nrow(problems(did)), 1)
  
})
