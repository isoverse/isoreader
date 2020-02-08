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
  # test specific files
  
  # FIXME: re-enable for commits
  #skip("Currently not testing all dual inlet data files.")
  # FIXME: run as one batch to make use of parallel processing
  
  iso_turn_reader_caching_off()
  
  # .did files
  
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_example.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "did_example_CO2_clumped_01.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "did_example_many_cycles.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "did_example_unicode.did")))
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
  
  expect_true(file.exists(file <- file.path("test_data", "caf_example_CO2_01.caf")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  # nu files
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_nu_example.txt")))
  expect_is(did <- iso_read_dual_inlet(file, nu_masses = 49:44), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
})
