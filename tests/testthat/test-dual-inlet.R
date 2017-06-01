context("Dual Inlet Files")

test_that("test that supported di files are correct", {
  expect_is(exts <- get_supported_di_files(), "data.frame")
  expect_equal(exts$extension, c("did"))
  expect_true(all(exts$fun %>% sapply(class) == "character"))
  expect_true(all(exts$fun %>% sapply(exists)))
  expect_true(all(exts$fun %>% sapply(get) %>% sapply(class) == "function"))
})

test_that("test that parameter checks are performed", {

  expect_error(isoreader:::isoread_did(structure(list(), class = "isofile")), 
               "data structure must have class \\'isofile\\' and \\'dual_inlet\\'")
  expect_error(isoreader:::isoread_did(structure(list(), class = "dual_inlet")), 
               "data structure must have class \\'isofile\\' and \\'dual_inlet\\'")
  expect_error(isoreader:::isoread_did(structure(list(), class = c("dual_inlet", "isofile"))), 
               "not in data: \\'file_info\\', \\'mass_data\\'")
  expect_error(isoreader:::isoread_did(structure(list(file_info = list(), mass_data=data_frame()), 
                                                 class = c("dual_inlet", "isofile"))), 
               "not in data: \\'file_name\\', \\'file_path\\'")

})