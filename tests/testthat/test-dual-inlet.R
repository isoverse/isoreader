context("Dual Inlet Files")

test_that("test that supported di files are correct", {
  expect_is(exts <- isoreader:::get_supported_di_files(), "data.frame")
  expect_equal(exts$extension, c("did", "feather.zip"))
  expect_true(all(exts$fun %>% sapply(class) == "character"))
  
  # check for existence in names sapce
  expect_true(exists("show_supported_file_types"))
  expect_true(all(exts$fun %>% sapply(exists, where = environment(show_supported_file_types))))
  expect_true(all(exts$fun %>% sapply(get, envir = environment(show_supported_file_types)) %>% sapply(class) == "function"))
})

test_that("test that parameter checks are performed", {

  expect_error(isoreader:::isoread_did(isoreader:::make_cf_data_structure()), 
               "data structure must be a \\'dual_inlet\\' isofile")
  
})

test_that("test that did file can be read", {
  # test specific files
  
  expect_true(file.exists(file <- system.file(package = "isoreader", "extdata", "dual_inlet_example.did")))
  expect_is(did <- isoread_dual_inlet(file, cache = FALSE), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  
  expect_true(file.exists("additional_dual_inlet_example_01.did"))
  expect_is(isoread_dual_inlet("additional_dual_inlet_example_01.did", cache = FALSE), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
})