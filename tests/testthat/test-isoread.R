context("Test basic iso_read_files coordinator function")

test_that("test that parameter checks are performed when reading binary file", {
  # make sure adequate parameter supplied
  expect_error(isoreader:::iso_read_files(), "missing")
  expect_error(isoreader:::iso_read_files(supported_extensions = data_frame()), "not in data\\: \\'extension\\', \\'fun\\'")
  expect_error(isoreader:::iso_read_files(
    supported_extensions = data_frame(extension = "did", fun = list(isoreader:::iso_read_did)), 
    data_structure = structure(list())), "data structure must include class \\'iso_file\\'")
  expect_error(isoreader:::iso_read_files(
    supported_extensions = isoreader:::get_supported_di_files(), # func tested in test-dual-inlet 
    data_structure = structure(list(), class = "iso_file")), 
    "not in data: \\'file_info\\'")
  expect_error(isoreader:::iso_read_files(
    supported_extensions = isoreader:::get_supported_di_files(), # func tested in test-dual-inlet 
    data_structure = isoreader:::make_di_data_structure()), 
    "file path\\(s\\) required")
})
  

test_that("test that checks are run when re-reading iso_files", {
  
  expect_warning(iso_reread_files(make_cf_data_structure()), "no longer exist at the referenced location")
  expect_error(iso_reread_files(make_cf_data_structure(), stop_if_missing = TRUE), "no longer exist at the referenced location")
  expect_error(iso_reread_archive("test.csv"), "unrecognized file type")
  expect_error(iso_reread_archive("DNE.dxf"), "file\\(s\\) do not exist")
})


test_that("test that read file event expression works", {
  
  minimal_files <- file.path("test_data", "minimal_files") %>% 
    list.files(pattern = "\\.did", full.names = TRUE)
  isoreader::set_read_file_event_expr({ print(file_n*-1) })
  expect_output(iso_read_dual_inlet(minimal_files[1:3], quiet = TRUE), "-1.*-2.*-3")
  isoreader::set_read_file_event_expr({})
})