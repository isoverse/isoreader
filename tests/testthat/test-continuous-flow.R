context("Continuous flow")

test_that("test that supported cf files are correct", {
  expect_is(exts <- isoreader:::get_supported_cf_files(), "data.frame")
  expect_equal(exts$extension, c("cf", "dxf", "iarc"))
  expect_true(all(exts$fun %>% sapply(class) == "character"))
  
  # check for existence in namespace
  expect_true(exists("show_supported_file_types"))
  expect_true(all(exts$fun %>% sapply(exists, where = environment(show_supported_file_types))))
  expect_true(all(exts$fun %>% sapply(get, envir = environment(show_supported_file_types)) %>% sapply(class) == "function"))
})