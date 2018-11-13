context("Test basic iso_read_files coordinator function")

test_that("test that file reader registration works", {
  
  isoreader:::initialize_options()
  expect_error(iso_register_continuous_flow_file_reader(".new", nrow), "please provide the function name")
  expect_equal(iso_register_continuous_flow_file_reader(".new", "nrow") %>% dplyr::filter(extension == ".new") %>% nrow(), 1)
  expect_equal(iso_register_continuous_flow_file_reader(".new", "nrow") %>% dplyr::filter(extension == ".new") %>% nrow(), 1)
  expect_error(iso_register_continuous_flow_file_reader(".new", "mean"), "already exists")
  expect_warning(new <- iso_register_continuous_flow_file_reader(".new", "mean", overwrite = TRUE), "will be overwritte")
  expect_equal(new %>% dplyr::filter(extension == ".new") %>% nrow(), 1)
  expect_error(iso_register_continuous_flow_file_reader(".new2", "THISFUNCTIONDOESNOTEXIST"), "could not find function")
  .GlobalEnv$iso_is_file <- function() stop("testing")
  expect_error(iso_register_continuous_flow_file_reader(".new2", "iso_is_file"), "exists in more than one environment")
  expect_equal(iso_register_continuous_flow_file_reader(".new2", "iso_is_file", env = "isoreader") %>% 
                 dplyr::filter(extension == ".new") %>% nrow(), 1)
})

test_that("test that parameter checks are performed when reading file", {
  # make sure adequate parameter supplied
  expect_error(isoreader:::iso_read_files(), "missing")
  expect_error(isoreader:::iso_read_files(supported_extensions = data_frame()), "not in data\\: \\'extension\\', \\'func\\'")
  expect_error(isoreader:::iso_read_files(
    supported_extensions = isoreader:::get_supported_di_files(), 
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
  expect_error(iso_reread_archive("test.csv"), "unexpected file extension")
  expect_error(iso_reread_archive("DNE.cf.rda"), "file\\(s\\) do not exist")
})


test_that("test that file event expressions work", {
  
  minimal_files <- file.path("test_data", "minimal_files") %>% 
    list.files(pattern = "\\.did", full.names = TRUE)
  
  isoreader:::set_read_file_event_expr({ print(file_n*-1) })
  expect_output(iso_read_dual_inlet(minimal_files[1:3], quiet = TRUE), "-1.*-2.*-3")
  isoreader:::set_read_file_event_expr({})
  
  isoreader:::set_finish_file_event_expr({ print(file_n^2) })
  expect_output(iso_read_dual_inlet(minimal_files[1:3], quiet = TRUE), "1.*4.*9")
  isoreader:::set_finish_file_event_expr({})
})