context("Test basic iso_read_files coordinator function")

test_that("test that file reader registration works", {
  
  initialize_options()
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
  expect_error(iso_read_files(), "missing")
  expect_error(iso_read_files(supported_extensions = data_frame()), "not in data\\: \\'extension\\', \\'func\\'")
  expect_error(iso_read_files(
    supported_extensions = get_supported_di_files(), 
    data_structure = structure(list())), "data structure must include class \\'iso_file\\'")
  expect_error(iso_read_files(
    supported_extensions = get_supported_di_files(), # func tested in test-dual-inlet 
    data_structure = structure(list(), class = "iso_file")), 
    "not in data: \\'file_info\\'")
  expect_error(iso_read_files(
    supported_extensions = get_supported_di_files(), # func tested in test-dual-inlet 
    data_structure = make_di_data_structure()), 
    "file path\\(s\\) required")
})

test_that("old file checks are run when reading stored collecionts", {

  expect_message(iso_read_continuous_flow(file.path("test_data", "collection_old.cf.rds"), quiet = TRUE), "version mismatch")
  expect_message(iso_read_dual_inlet(file.path("test_data", "collection_old.di.rda"), quiet = TRUE), "version mismatch")
  expect_message(iso_read_dual_inlet(file.path("test_data", "collection_old.di.rda"), quiet = TRUE), "deprecated")

})

test_that("test that checks are run when re-reading iso_files", {
  
  expect_message(iso_reread_files(make_cf_data_structure()), "no longer exist at the referenced location")
  expect_error(iso_reread_files(make_cf_data_structure(), stop_if_missing = TRUE), "no longer exist at the referenced location")
  expect_error(iso_reread_storage("test.csv"), "unexpected file extension")
  expect_error(iso_reread_storage("DNE.cf.rds"), "file\\(s\\) do not exist")
 
})

test_that("test that re-reads are working properly", {
  
  # re-read of files
  files <- iso_read_continuous_flow(file.path("test_data", c("cf_example_H_01.cf", "cf_example_H_02.cf")))
  expect_true(iso_is_file_list(files))
  expect_true(iso_is_file(files[[1]]))
  expect_message(re_file <- iso_reread_files(files[[1]]), "re-reading 1 data file")
  expect_true(iso_is_file(re_file))
  expect_message(re_files <- iso_reread_files(files), "re-reading 2 data file")
  expect_true(iso_is_file_list(re_files))
  
  # re-read old collection (should save as new .rds instead)
  collection_path <- file.path("test_data", "collection_old.di.rda")
  expect_message(new_path <- iso_reread_storage(collection_path), "R Data Archives .* deprecated")
  expect_equal(new_path, stringr::str_replace(collection_path, "rda$", "rds"))
  expect_equal(iso_read_dual_inlet(new_path) %>% iso_get_problems() %>% nrow(), 0)
  # re-read the new collection
  expect_message(new_path2 <- iso_reread_storage(new_path), "re-reading")
  expect_equal(new_path, new_path2)
  expect_equal(iso_read_dual_inlet(new_path2) %>% iso_get_problems() %>% nrow(), 0)
  if (file.exists(new_path2)) file.remove(new_path2)
})


test_that("test that file event expressions work", {
  
  minimal_files <- file.path("test_data", "minimal_files") %>% 
    list.files(pattern = "\\.did", full.names = TRUE)
  
  set_read_file_event_expr({ print(file_n*-1) })
  expect_output(iso_read_dual_inlet(minimal_files[1:3], quiet = TRUE), "-1.*-2.*-3")
  set_read_file_event_expr({})
  
  set_finish_file_event_expr({ print(file_n^2) })
  expect_output(iso_read_dual_inlet(minimal_files[1:3], quiet = TRUE), "1.*4.*9")
  set_finish_file_event_expr({})
})