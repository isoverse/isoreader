context("Continuous flow")

test_that("test that supported cf files are correct", {
  isoreader:::initialize_options()
  expect_is(exts <- isoreader:::get_supported_cf_files(), "data.frame")
  expect_equal(exts$extension, c(".cf", ".dxf", ".iarc", ".cf.rds", ".cf.rda"))
  expect_true(all(exts$func %>% sapply(class) == "character"))
  expect_true(all(exts$func %>% map_lgl(exists, mode = "function", where = asNamespace("isoreader"))))
})

test_that("test that parameter checks are performed", {
  
  # flow iarc
  expect_error(isoreader:::iso_read_flow_iarc (isoreader:::make_di_data_structure()), 
               "data structure must be a \\'continuous_flow\\' iso_file")
  
  
})


test_that("test that dxf files can be read", {
  # test specific files
  
  # FIXME: re-enable for commits
  # skip("Currently not testing all continuous flow data files.")
  # FIXME: run as one batch to make use of parallel processing
  
  iso_turn_reader_caching_off()
  
  expect_true(file.exists(file <- iso_get_reader_example("linearity_example.dxf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)

  expect_true(file.exists(file <- iso_get_reader_example("continuous_flow_example.cf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
  expect_true(file.exists(file <- iso_get_reader_example("continuous_flow_example.dxf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "dxf_example_H_01.dxf")))
  expect_is(dxf1 <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf1)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "dxf_example_HO_01.dxf")))
  expect_is(dxf2 <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf2)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "cf_example_H_01.cf")))
  expect_is(dxf3 <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf3)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "cf_example_H_02.cf")))
  expect_is(dxf4 <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf4)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "cf_example_H_03.cf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "cf_example_H_04.cf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "cf_example_H_05.cf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "cf_example_H_06.cf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "cf_example_H_07.cf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
  # test re-reading
  # NOTE: ideally this should also include an iarc file
  iso_files <- c(dxf1, dxf2, dxf3)
  expect_equal(
    iso_files %>% get_reread_filepaths(),
    iso_files %>% sapply(function(i) i$file_info$file_path) %>% as.character()
  )
  expect_true(iso_is_continuous_flow(reread_dxf <- iso_reread_files(iso_files)))
  expect_equal(nrow(problems(reread_dxf)), 0)
  
  
})

