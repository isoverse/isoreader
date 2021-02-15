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
  rm("iso_is_file", envir = .GlobalEnv)
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

# cached file paths ======

test_that("test that cached file path hashes work okay", {
  
  # skip on CRAN to reduce checktime to below 10 minutes
  skip_on_cran()
  
  data_folder <- "minimal_data" # data_folder <- file.path("tests", "testthat", "minimal_data") # for direct testing
  origin_file <- get_isoreader_test_file("minimal_01.did", data_folder)
  
  # exact same file in different locations
  temp_file <- file.path(tempdir(), "minimal_01.did")
  expect_true(file.copy(origin_file, to = temp_file, overwrite = TRUE, copy.date = TRUE))
  cache_paths <- generate_cache_filepaths(c(origin_file, temp_file))
  expect_true(identical(cache_paths[1], cache_paths[2]))
  unlink(temp_file)
  
  # exact same file but different names
  temp_file <- file.path(tempdir(), "minimal_01-2.did")
  expect_true(file.copy(origin_file, to = temp_file, overwrite = TRUE, copy.date = TRUE))
  cache_paths <- generate_cache_filepaths(c(origin_file, temp_file))
  expect_false(identical(cache_paths[1], cache_paths[2]))
  unlink(temp_file)
  
  # exact same file but copy date updated
  temp_file <- file.path(tempdir(), "minimal_01.did")
  expect_true(file.copy(origin_file, to = temp_file, overwrite = TRUE, copy.date = FALSE))
  cache_paths <- generate_cache_filepaths(c(origin_file, temp_file))
  expect_false(identical(cache_paths[1], cache_paths[2]))
  unlink(temp_file)
  
  # exact same file and name but differnet sizes
  temp_file <- file.path(tempdir(), "minimal_01.did")
  expect_true(file.copy(origin_file, to = temp_file, overwrite = TRUE, copy.date = TRUE))
  cat("42", file = temp_file, append = TRUE)
  cache_paths <- generate_cache_filepaths(c(origin_file, temp_file))
  expect_false(identical(cache_paths[1], cache_paths[2]))
  unlink(temp_file)
  
})


# read version checks and re-reads =======


test_that("test that version checking and re-reads are working properly", {
  
  # skip on CRAN to reduce checktime to below 10 minutes
  skip_on_cran()
  
  # test folder
  data_folder <- "minimal_data" # data_folder <- file.path("tests", "testthat", "minimal_data") # for direct testing
  test_files <- c(
    get_isoreader_test_file("scan_hv_01.scn", local_folder = data_folder),
    get_isoreader_test_file("scan_hv_02.scn", local_folder = data_folder),
    get_isoreader_test_file("scan_hv_03.scn", local_folder = data_folder)
  )
  
  # expected errors
  expect_message(reread_iso_files(make_cf_data_structure("NA")), "not exist at.*referenced location")
  expect_error(reread_iso_files(make_cf_data_structure("NA"), stop_if_missing = TRUE), "not exist at.*referenced location")
  expect_error(reread_iso_files(tibble()), "can only re-read iso")
  
  # read files
  iso_turn_datetime_warnings_off() # required since these are scan files
  files <- iso_read_scan(test_files, read_cache = FALSE)
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
  expect_true(nrow(problems(files)) == 2)
  
  # save "old" cached files and rds file (for version warning tests)
  temp_cache <- file.path(tempdir(), "cache_files")
  temp_storage <- file.path(tempdir(), "scan_storage_old.scan.rds")
  dir.create(temp_cache, showWarnings = FALSE)
  save_files <- files %>% iso_set_file_root(remove_embedded_root = data_folder)
  readr::write_rds(save_files[[1]], file.path(temp_cache, basename(generate_cache_filepaths(test_files)[1])))
  readr::write_rds(save_files[[2]], file.path(temp_cache, basename(generate_cache_filepaths(test_files)[2])))
  readr::write_rds(save_files[[3]], file.path(temp_cache, basename(generate_cache_filepaths(test_files)[3])))
  iso_save(save_files, filepath = temp_storage)
  
  # version warnings for files
  isoreader:::set_default("cache_dir", temp_cache)
  expect_message(
    capture.output(cached_files <- suppressWarnings(iso_read_scan(test_files))), 
    "running compatibility checks")
  expect_warning(
    capture.output(cached_files <- iso_read_scan(test_files)), 
    "some files.*outdated cache")
  expect_true(nrow(problems(cached_files)) == 4)
  expect_true(is_iso_object_outdated(cached_files))
  isoreader:::set_default("cache_dir", "cache")
  unlink(temp_cache, recursive = TRUE)
  
  # re-read single file
  expect_message(re_file <- iso_reread_all_files(files[[1]]), "found 1.*re-reading 1/1")
  expect_true(iso_is_file(re_file))
  expect_true(nrow(problems(re_file)) == 0)
  expect_false(is_iso_object_outdated(re_file))
  
  # re-read all files
  expect_message(re_files <- iso_reread_all_files(files), "found 3.*re-reading 3/3")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  expect_true(nrow(problems(re_files)) == 0)
  expect_false(is_iso_object_outdated(re_files))
  
  # re-read outdated files
  isoreader:::cache_iso_file(files[[1]], generate_cache_filepaths(test_files)[1])
  isoreader:::cache_iso_file(files[[2]], generate_cache_filepaths(test_files)[2])
  expect_message(re_files <- iso_reread_outdated_files(files, cache = FALSE), "cached file is outdated.*scan_hv_01")
  expect_message(re_files <- iso_reread_outdated_files(files, cache = FALSE), "cached file is outdated.*scan_hv_02")
  expect_equal(names(re_files), names(files))
  expect_true(nrow(problems(re_files)) == 1)
  expect_true(nrow(problems(re_files[1:2])) == 0)
  expect_false(is_iso_object_outdated(re_files))
  expect_message(iso_reread_outdated_files(files), "found 2 outdated.*re-reading 2/3") # updates cache
  expect_false(is_iso_object_outdated(iso_read_scan(test_files))) # check updated cache
  
  # re-read changed files (i.e. no appropriate cache file exists)
  expect_message(re_files <- iso_reread_changed_files(files), "found 0 changed.*re-reading 0/3")
  unlink(generate_cache_filepaths(test_files)[2])
  expect_message(re_files <- iso_reread_changed_files(files), "found 1 changed.*re-reading 1/3")
  expect_equal(names(re_files), names(files))
  expect_true(nrow(problems(re_files[[2]])) == 0)
  expect_false(is_iso_object_outdated(re_files[[2]]))
  expect_warning(iso_reread_files(files), "renamed.*iso_reread_changed_files")
  
  # reread outdated only
  expect_message(re_files <- iso_reread_outdated_files(files), "found 2 outdated.*re-reading 2/3")
  expect_true(iso_is_file_list(re_files))
  expect_false(is_iso_object_outdated(re_files))
  expect_true(nrow(problems(files)) == 2)
 
  # re-read only problematic
  expect_message(re_files <- iso_reread_problem_files(files), "found 1.*with errors.*re-reading 1/3")
  expect_true(iso_is_file_list(re_files))
  expect_equal(names(re_files), names(files))
  expect_true(nrow(problems(re_files)) == 1)
  expect_true(is_iso_object_outdated(re_files))
  
  # re-read with changed root
  expect_message(files %>% iso_set_file_root(root = "DNE") %>% reread_iso_files(), "Warning.*3 file.*not exist")
  expect_error(files %>% iso_set_file_root(root = "DNE") %>% reread_iso_files(stop_if_missing = TRUE), "3 file.*not exist")
  expect_message(
    files %>% 
    iso_set_file_root(root = data_folder, remove_embedded_root = data_folder) %>% iso_reread_all_files(),
    "found 3.*re-reading 3/3"
  )
  
  # re-read outdated rds storage
  expect_message(
    capture.output(suppressWarnings(iso_read_scan(temp_storage))), 
    "running compatibility checks")
  expect_warning(
    capture.output(files <- iso_read_scan(temp_storage)), 
    "some files.*outdated cache")
  expect_true(nrow(problems(files)) == 4)
  expect_true(is_iso_object_outdated(files))
  expect_false(is_iso_object_outdated(files[[3]]))
  expect_message(re_files <- files %>% iso_set_file_root(data_folder) %>% iso_reread_outdated_files(),
                 "found 2 outdated.*re-reading 2/3")
  expect_false(is_iso_object_outdated(re_files))
  expect_true(nrow(problems(re_files)) == 1)
  expect_message(re_files <- files %>% iso_set_file_root(data_folder) %>% iso_reread_changed_files(),
                 "found 0 changed.*re-reading 0/3")
  expect_message(re_files <- files %>% iso_set_file_root(data_folder) %>% iso_reread_problem_files(),
                 "found 1.*with errors.*re-reading 1/3")
  expect_true(is_iso_object_outdated(re_files))
  expect_true(nrow(problems(re_files)) == 3)
  expect_message(re_files <- files %>% iso_set_file_root(data_folder) %>% iso_reread_problem_files(reread_files_with_warnings = TRUE),
                 "found 3.*with warnings or errors.*re-reading 3/3")
  expect_false(is_iso_object_outdated(re_files))
  expect_true(nrow(problems(re_files)) == 0)
  unlink(temp_storage)
  
})

# file event expressions =====

test_that("test that file event expressions work", {
  
  # skip on CRAN to reduce checktime to below 10 minutes
  skip_on_cran()
  
  data_folder <- file.path("minimal_data")
  minimal_files <- c(
    get_isoreader_test_file("minimal_01.did", local_folder = data_folder),
    get_isoreader_test_file("minimal_02.did", local_folder = data_folder),
    get_isoreader_test_file("minimal_03.did", local_folder = data_folder),
    get_isoreader_test_file("minimal_04.did", local_folder = data_folder)
  )
  
  set_read_file_event_expr({ print(file_n*-1) })
  expect_output(iso_read_dual_inlet(minimal_files[1:3], quiet = TRUE), "-1.*-2.*-3")
  set_read_file_event_expr({})
  
  set_finish_file_event_expr({ print(file_n^2) })
  expect_output(iso_read_dual_inlet(minimal_files[1:3], quiet = TRUE), "1.*4.*9")
  set_finish_file_event_expr({})
})

