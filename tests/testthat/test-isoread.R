context("Test basic iso_read_files coordinator function")

# file reader registration ======

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

# parameter checks ======

test_that("test that parameter checks are performed when reading file", {
  # make sure adequate parameter supplied
  expect_error(iso_read_files(), "missing")
  expect_error(iso_read_files(supported_extensions = tibble()), "not in data\\: \\'extension\\', \\'func\\'")
  expect_error(iso_read_files(
    supported_extensions = get_supported_di_files(), 
    data_structure = structure(list())), "data structure must include class \\'iso_file\\'")
  expect_error(iso_read_files(
    supported_extensions = get_supported_di_files(), # func tested in test-dual-inlet 
    data_structure = structure(list(), class = "iso_file")), 
    "not in data: \\'file_info\\'")
  expect_error(iso_read_files(
    supported_extensions = get_supported_di_files(), # func tested in test-dual-inlet 
    data_structure = make_di_data_structure("NA")), 
    "file path\\(s\\) required")
})

# file checks on old collections =======

test_that("old file checks are run when reading stored collecionts", {

  root <- "."
  expect_message(iso_read_continuous_flow(file.path(root, "test_data", "collection_old.cf.rds"), quiet = TRUE), "version mismatch")
  expect_message(iso_read_dual_inlet(file.path(root, "test_data", "collection_old.di.rda"), quiet = TRUE), "version mismatch")
  expect_message(iso_read_dual_inlet(file.path(root, "test_data", "collection_old.di.rda"), quiet = TRUE), "deprecated")

})

# re-reads =======

test_that("test that checks are run when re-reading iso_files", {
  
  expect_message(iso_reread_files(make_cf_data_structure("NA")), "not exist at.*referenced location")
  expect_error(iso_reread_files(make_cf_data_structure("NA"), stop_if_missing = TRUE), "not exist at.*referenced location")
  expect_error(iso_reread_storage("test.csv"), "unexpected file extension")
  expect_error(iso_reread_storage("DNE.cf.rds"), "file\\(s\\) do not exist")
 
})

test_that("test that re-reads are working properly", {
  
  # read fiels
  test_folder <- "test_data" # test_folder <- file.path("tests", "testthat", "test_data") # for direct testing
  files <- iso_read_continuous_flow(file.path(test_folder, c("cf_example_H_01.cf", "cf_example_H_02.cf", "cf_example_H_03.cf")))
  expect_true(iso_is_file_list(files))
  expect_true(iso_is_file(files[[1]]))
  files[2] <- register_warning(files[2], warn = FALSE, "warning")
  files[3] <- register_error(files[3], warn = FALSE, "error")
  
  # errors
  expect_error(iso_reread_files(tibble()), "can only re-read iso")
  
  # re-read single file
  expect_message(re_file <- iso_reread_files(files[[1]], read_cache = TRUE), "re-reading all \\(1/1\\) data file")
  expect_true(iso_is_file(re_file))
  
  # re-read all files (not from cache)
  expect_message(re_files <- iso_reread_files(files), "re-reading all \\(3/3\\) data file")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  expect_equal(nrow(re_files %>% iso_get_problems()), 0L)
  
  # re-read from cache
  expect_message(re_files <- iso_reread_files(files, read_cache = TRUE), "from cache")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  
  # re-read only errors + warnings
  expect_message(re_files <- iso_reread_files(files, read_cache = TRUE, reread_files_without_problems = FALSE), 
                 "re-reading 2/3 data file.*with errors or warnings")
  expect_message(re_files <- iso_reread_files_with_problems(files, read_cache = TRUE), 
                 "re-reading 2/3 data file.*with errors or warnings")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  
  # re-read only good
  expect_message(re_files <- iso_reread_files(files, read_cache = TRUE, reread_files_with_warnings = FALSE, reread_files_with_errors = FALSE), 
                 "re-reading 1/3 data file.*without warnings or errors")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  
  # re-read with changed root
  expect_message(files %>% iso_set_file_root(root = "DNE") %>% iso_reread_files(), "Warning.*3 file.*not exist")
  expect_error(files %>% iso_set_file_root(root = "DNE") %>% iso_reread_files(stop_if_missing = TRUE), "3 file.*not exist")
  expect_message(
    files %>% 
    iso_set_file_root(root = test_folder, remove_embedded_root = test_folder) %>% iso_reread_files(read_cache = TRUE),
    "re-reading all \\(3/3\\) data file"
  )
  
  # re-read old collection (should save as new .rds instead)
  test_folder <- "test_data" # test_folder <- file.path("tests", "testthat", "test_data")
  collection_path <- file.path(test_folder, "collection_old.di.rda")
  expect_message(new_path <- iso_reread_storage(collection_path), "R Data Archives .* deprecated")
  expect_equal(new_path, stringr::str_replace(collection_path, "rda$", "rds"))
  expect_message(iso_reread_storage(collection_path), "version mismatch")
  expect_message(iso_reread_storage(collection_path), "Consider re-reading")
  expect_message(iso_reread_storage(collection_path), "not exist at.* referenced location")
  # re-read the new collection
  expect_message(new_path2 <- iso_reread_storage(new_path), "re-reading")
  expect_equal(new_path, new_path2)
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

