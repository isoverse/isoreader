context("Dual Inlet Files")

test_that("test that supported di files are correct", {
  expect_is(exts <- isoreader:::get_supported_di_files(), "data.frame")
  expect_equal(exts$extension, c("did", "di.rda"))
  expect_true(all(exts$fun %>% sapply(class) == "character"))
  
  # check for existence in names sapce
  expect_true(all(exts$fun %>% sapply(exists, where = environment(get_supported_file_types))))
  expect_true(all(exts$fun %>% sapply(get, envir = environment(get_supported_file_types)) %>% sapply(class) == "function"))
})

test_that("test that parameter checks are performed", {

  expect_error(isoreader:::isoread_did(isoreader:::make_cf_data_structure()), 
               "data structure must be a \\'dual_inlet\\' isofile")
  
})

test_that("test that did files can be read", {
  # test specific files
  
  #skip("Currently not testing all dual inlet data files.")
  
  turn_caching_off()
  
  expect_true(file.exists(file <- isoreader_example("dual_inlet_example.did")))
  expect_is(did <- read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  expect_true(file.exists(file <- file.path("test_data", "did_example_CO2_clumped_01.did")))
  expect_is(did <- read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
})
