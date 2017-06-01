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
  expect_error(isoreader:::isoread_flow_iarc (structure(list(), class = "isofile")), 
               "data structure must have class \\'isofile\\' and \\'continuous_flow\\'")
  expect_error(isoreader:::isoread_flow_iarc(structure(list(), class = "dual_inlet")), 
               "data structure must have class \\'isofile\\' and \\'continuous_flow\\'")
  expect_error(isoreader:::isoread_flow_iarc(structure(list(), class = c("continuous_flow", "isofile"))), 
               "not in data: \\'file_info\\', \\'raw_data\\'")
  expect_error(isoreader:::isoread_flow_iarc(structure(list(file_info = list(), raw_data=data_frame()), 
                                                 class = c("continuous_flow", "isofile"))), 
               "not in data: \\'file_id\\', \\'file_path\\'")
  
})

