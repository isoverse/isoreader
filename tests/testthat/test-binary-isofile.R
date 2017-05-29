context("read binary iso file")

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
  