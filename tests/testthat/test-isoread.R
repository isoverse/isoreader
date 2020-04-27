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


# read version checks and re-reads =======


test_that("test that version checking and re-reads are working properly", {
  
  # test folder
  test_folder <- "test_data" # test_folder <- file.path("tests", "testthat", "test_data") # for direct testing
  test_files <- file.path(test_folder, c("scan_hv_01.scn", "scan_hv_02.scn", "scan_hv_03.scn"))
  
  # version warnings for files
  cache_dir <- default("cache_dir")
  isoreader:::set_default("cache_dir", file.path(test_folder, "cache_files"))
  expect_message(
    capture.output(suppressWarnings(iso_read_scan(test_files))), 
    "running backwards compatibility checks")
  expect_warning(
    capture.output(files <- iso_read_scan(test_files)), 
    "some files.*outdated cache or storage")
  expect_true(nrow(problems(files)) == 3)
  expect_true(is_iso_object_outdated(files))
  isoreader:::set_default("cache_dir", cache_dir)
  
  # expected errors
  expect_message(iso_reread_files(make_cf_data_structure("NA")), "not exist at.*referenced location")
  expect_error(iso_reread_files(make_cf_data_structure("NA"), stop_if_missing = TRUE), "not exist at.*referenced location")
  expect_error(iso_reread_storage("test.csv"), "unexpected file extension")
  expect_error(iso_reread_storage("DNE.cf.rds"), "file\\(s\\) do not exist")
  expect_error(iso_reread_files(tibble()), "can only re-read iso")
  
  # read files
  files <- iso_read_scan(test_files)
  expect_true(nrow(problems(files)) == 0)
  expect_false(is_iso_object_outdated(files))
  expect_true(iso_is_file_list(files))
  expect_true(iso_is_file(files[[1]]))
  
  # set versions, warnings, errors
  files[[1]]$version <- NULL
  files[[2]]$version <- as.package_version("0.5.0")
  files[2] <- register_warning(files[2], warn = FALSE, "warning")
  files[3] <- register_error(files[3], warn = FALSE, "error")
  expect_true(is_iso_object_outdated(files))
  expect_true(nrow(problems(files)) > 0)
  
  # re-read single file
  expect_message(re_file <- iso_reread_files(files[[1]]), "re-reading all data files \\(1/1\\)")
  expect_true(iso_is_file(re_file))
  expect_true(nrow(problems(re_file)) == 0)
  expect_false(is_iso_object_outdated(re_file))
  
  # re-read all files
  expect_message(re_files <- iso_reread_files(files), "re-reading all.*\\(3/3\\)")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  expect_true(nrow(problems(re_files)) == 0)
  expect_false(is_iso_object_outdated(re_files))
  
  # reread outdated only
  expect_message(re_files <- iso_reread_files(files, reread_only_outdated_files = TRUE), "re-reading all outdated.* \\(2/3\\)")
  expect_message(re_files <- iso_reread_files_outdated(files), "re-reading all outdated.* \\(2/3\\)")
  expect_true(iso_is_file_list(re_files))
  expect_false(is_iso_object_outdated(re_files))
  expect_true(nrow(problems(files)) > 0)
 
  # re-read only errors + warnings
  expect_message(re_files <- iso_reread_files(files, reread_files_without_problems = FALSE), 
                 "re-reading.*with errors or warnings.*2/3")
  expect_message(re_files <- iso_reread_files_with_problems(files), 
                 "re-reading.*with errors or warnings.*2/3")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  expect_true(nrow(problems(re_files)) == 0)
  expect_true(is_iso_object_outdated(re_files))
  
  # re-read only good
  expect_message(re_files <- iso_reread_files(files, reread_files_with_warnings = FALSE, reread_files_with_errors = FALSE), 
                 "re-reading.*without warnings or errors.*1/3")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  expect_true(nrow(problems(re_files)) > 0)
  expect_false(is_iso_object_outdated(re_files[[1]]))
  expect_true(is_iso_object_outdated(re_files))
  
  # re-read with changed root
  expect_message(files %>% iso_set_file_root(root = "DNE") %>% iso_reread_files(), "Warning.*3 file.*not exist")
  expect_error(files %>% iso_set_file_root(root = "DNE") %>% iso_reread_files(stop_if_missing = TRUE), "3 file.*not exist")
  expect_message(
    files %>% 
    iso_set_file_root(root = test_folder) %>% iso_reread_files(),
    "re-reading all.*3/3"
  )
  
  # re-read outdated RDA collection
  rda_path <- file.path(tempdir(), "test.di.rda")
  cat(42, file = rda_path)
  expect_message(
    tryCatch(iso_reread_storage(rda_path), error = function(e){}, warning = function(w) {message(w$message)}),
    "R Data Archives .* deprecated"
  )
  unlink(rda_path)
  
  # re-read outdated rds storage
  test_storage <- file.path(test_folder, "scan_storage_old.scan.rds")
  expect_message(
    capture.output(suppressWarnings(iso_read_scan(test_storage))), 
    "running backwards compatibility checks")
  expect_warning(
    capture.output(files <- iso_read_scan(test_storage)), 
    "some files.*outdated cache or storage")
  expect_true(nrow(problems(files)) == 2)
  expect_true(is_iso_object_outdated(files))
  expect_false(is_iso_object_outdated(files[[3]]))
  
  temp_storage <- file.path(tempdir(), basename(test_storage))
  expect_message(
    files %>% iso_set_file_root(test_folder) %>% iso_save(temp_storage),
    "exporting data from 3.*files.*into R Data Storage"
  )
  expect_message( # first read --> reread 2/3
    iso_reread_storage_outdated(temp_storage),
    "re-reading all outdated.*2/3"
  )
  expect_message( # 2nd read --> none left to reread
    iso_reread_storage_outdated(temp_storage),
    "re-reading no data.*0/3"
  )
  expect_message( # re-read all
    iso_reread_storage(temp_storage),
    "re-reading all.*3/3"
  )
  unlink(temp_storage)
  
})

# file event expressions =====

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

