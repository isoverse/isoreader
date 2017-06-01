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

  expect_error(isoreader:::isoread_did(structure(list(), class = "isofile")), 
               "data structure must have class \\'isofile\\' and \\'dual_inlet\\'")
  expect_error(isoreader:::isoread_did(structure(list(), class = "dual_inlet")), 
               "data structure must have class \\'isofile\\' and \\'dual_inlet\\'")
  expect_error(isoreader:::isoread_did(structure(list(), class = c("dual_inlet", "isofile"))), 
               "not in data: \\'file_info\\', \\'raw_data\\'")
  expect_error(isoreader:::isoread_did(structure(list(file_info = list(), raw_data=data_frame()), 
                                                 class = c("dual_inlet", "isofile"))), 
               "not in data: \\'file_id\\', \\'file_path\\'")

})
