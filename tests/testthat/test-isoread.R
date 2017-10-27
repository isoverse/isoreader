context("Test basic isoread_files coordinator function")

test_that("test that parameter checks are performed when reading binary file", {
  # make sure adequate parameter supplied
  expect_error(isoreader:::isoread_files(), "missing")
  expect_error(isoreader:::isoread_files(supported_extensions = data_frame()), "not in data\\: \\'extension\\', \\'fun\\'")
  expect_error(isoreader:::isoread_files(
    supported_extensions = data_frame(extension = "did", fun = list(isoreader:::isoread_did)), 
    data_structure = structure(list())), "data structure must include class \\'isofile\\'")
  expect_error(isoreader:::isoread_files(
    supported_extensions = isoreader:::get_supported_di_files(), # func tested in test-dual-inlet 
    data_structure = structure(list(), class = "isofile")), 
    "not in data: \\'file_info\\'")
  expect_error(isoreader:::isoread_files(
    supported_extensions = isoreader:::get_supported_di_files(), # func tested in test-dual-inlet 
    data_structure = isoreader:::make_di_data_structure()), 
    "file path\\(s\\) required")
})
  

test_that("test that checks are run when re-reading isofiles", {
  
  expect_warning(reread_isofiles(make_cf_data_structure()), "no longer exist at the referenced location")
  expect_error(reread_isofiles(make_cf_data_structure(), stop_if_missing = TRUE), "no longer exist at the referenced location")
  expect_error(reread_isofiles_archive("test.csv"), "unrecognized file type")
  expect_error(reread_isofiles_archive("DNE.dxf"), "file\\(s\\) do not exist")
})