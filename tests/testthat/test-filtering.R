context("File info manipulation")

test_that("Test that filtering by file info works", {
  
  iso_file1 <- make_di_data_structure()
  iso_file1$read_options$file_info <- TRUE
  iso_file1$file_info$new_info <- 42
  iso_file2 <- iso_file3 <- iso_file1
  iso_file1$file_info$file_id <- "A"
  iso_file2$file_info$file_id <- "B"
  iso_file3$file_info$file_id <- "C"
  
  # errors
  expect_error(filter(iso_file1), "no applicable method")
  expect_error(iso_filter_files(iso_file1), "can only filter collections")
  
  # messaging
  iso_files <- c(iso_file1, iso_file2, iso_file3)
  expect_equal(filter(iso_files), iso_files)
  expect_silent(filter(iso_files))
  expect_message(iso_filter_files(iso_files), "applying.*filter")
  expect_silent(iso_filter_files(iso_files, quiet = TRUE))
  
  # filtering
  expect_equal(filter(iso_files, new_info == 42), iso_files)
  expect_equal(filter(iso_files, file_id == "A"), c(iso_file1))
  expect_null(filter(iso_files, file_id == "DNE"))
  expect_error(filter(iso_files, dne == 5), "not.*found")
  
  # filter and iso_filter_files equivalence
  expect_equal(filter(iso_files, file_id != "A"), iso_filter_files(iso_files, file_id != "A"))
})


test_that("Test that removing files with errors works properly", {
  
  # iso_filter_files_with_problems
  iso_file <- make_iso_file_data_structure()
  expect_message(iso_warn <- register_warning(iso_file, "test warning"))
  expect_message(iso_err <- register_error(iso_file, "test error"))
  iso_file$file_info$file_id <- "A"
  iso_warn$file_info$file_id <- "B"
  iso_err$file_info$file_id <- "C"
  iso_files <- c(iso_file, iso_err, iso_warn)
  expect_error(iso_filter_files_with_problems(42), "provide a list of iso_files")
  expect_equal(iso_filter_files_with_problems(iso_files, remove_files_with_errors = FALSE, remove_files_with_warnings = FALSE), iso_files)
  expect_message(iso_filter_files_with_problems(iso_files, quiet = FALSE), "removing")
  expect_silent(iso_filter_files_with_problems(iso_files, quiet = TRUE))
  expect_equal(iso_filter_files_with_problems(iso_files) %>% # default parameters
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A", "B"))
  expect_equal(iso_filter_files_with_problems(iso_files, remove_files_with_errors = FALSE, remove_files_with_warnings = TRUE) %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A", "C"))
  expect_equal(iso_filter_files_with_problems(iso_files, remove_files_with_warnings = TRUE) %>% 
                 sapply(function(x) x$file_info$file_id) %>% as.character(), 
               c("A"))
  
  # deprecated iso_omit_files_with_problems
  expect_warning(iso_omit_files_with_problems(iso_files), "renamed.*will be removed")
  
})