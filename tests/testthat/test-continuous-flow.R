context("Continuous flow")

test_that("test that supported cf files are correct", {
  expect_is(exts <- isoreader:::get_supported_cf_files(), "data.frame")
  expect_equal(exts$extension, c("cf", "dxf", "iarc", "feather.zip"))
  expect_true(all(exts$fun %>% sapply(class) == "character"))
  
  # check for existence in namespace
  expect_true(exists("show_supported_file_types"))
  expect_true(all(exts$fun %>% sapply(exists, where = environment(show_supported_file_types))))
  expect_true(all(exts$fun %>% sapply(get, envir = environment(show_supported_file_types)) %>% sapply(class) == "function"))
})

test_that("test that parameter checks are performed", {
  
  # flow iarc
  expect_error(isoreader:::isoread_flow_iarc (isoreader:::make_di_data_structure()), 
               "data structure must be a \\'continuous_flow\\' isofile")
  
  
})


test_that("test that did file can be read", {
  # test specific files
  
  expect_true(file.exists(file <- system.file(package = "isoreader", "extdata", "continuous_flow_example.dxf")))
  expect_is(dxf <- isoread_continuous_flow(file, cache = FALSE)[[1]], "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
  expect_true(file.exists(file <- system.file(package = "isoreader", "extdata", "peak_jump_example.dxf")))
  expect_is(dxf <- isoread_continuous_flow(file, cache = FALSE)[[1]], "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  
})

