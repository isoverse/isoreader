context("File info operations")

# selecting / renaming ======

test_that("Test that selecting/renaming file info works", {
  
  iso_file1 <- make_di_data_structure()
  iso_file1$read_options$file_info <- TRUE
  iso_file1$file_info$new_info <- 42
  iso_file2 <- iso_file3 <- iso_file1
  iso_file1$file_info$file_id <- "A"
  iso_file2$file_info$file_id <- "B"
  iso_file2$file_info$new_info2 <- 2
  iso_file3$file_info$file_id <- "C"
  iso_file3$file_info$new_info3 <- 3
  iso_files <- c(iso_file1, iso_file2, iso_file3)
  
  # select error checks
  expect_error(iso_select_file_info(42), "only select.*iso files")
  expect_error(iso_select_file_info(iso_file1, new = file_id), "renaming.*not allowed")
  expect_error(iso_select_file_info(iso_files, new = file_id), "renaming.*not allowed")
  expect_error(iso_select_file_info(iso_files, y = new_info, y = new_info2), "unresolvable naming conflict")
  
  # select info message
  expect_message(iso_select_file_info(iso_file1), "keeping 1") # always file_info
  expect_silent(iso_select_file_info(iso_file1, quiet = TRUE))
  expect_silent(select(iso_file1)) 
  expect_message(iso_select_file_info(iso_file1, newer_info = new_info), "keeping 2 file info.*across 1 isofile")
  expect_message(iso_select_file_info(iso_file1, newer_info = new_info), "new_info.*renamed.*newer_info.*in 1 file")
  expect_message(iso_select_file_info(iso_files, newer_info = new_info2), "keeping 2 file info.*across 3 isofile")
  expect_message(iso_select_file_info(iso_files, newer_info = new_info), "new_info.*renamed.*newer_info.*in 3 file")
  expect_message(iso_select_file_info(iso_files, y = new_info2, y = new_info3), "keeping 3 file info.*across 3 isofile")
  expect_message(iso_select_file_info(iso_files, y = new_info2, y = new_info3), "new_info2.*renamed.*y.*in 1 file")
  expect_message(iso_select_file_info(iso_files, y = new_info2, y = new_info3), "new_info3.*renamed.*y.*in 1 file")
  
  # select outcomes
  expect_equal(
    iso_select_file_info(iso_files, y = new_info2, y = new_info3) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), y = c(NA, 2, 3))
  )
  expect_equal(
    iso_select_file_info(iso_files, -starts_with("file"), -new_info) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), new_info2 = c(NA, 2, NA), new_info3 = c(NA, NA, 3))
  )
  expect_equal(
    iso_select_file_info(iso_files, newer_info = new_info) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), newer_info = 42)
  )
  expect_equal(
    iso_select_file_info(iso_files, newest_info = new_info, new_info2, new_info2 = new_info3) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), newest_info = 42, new_info2 = c(NA, 2, 3))
  )
  
  # rename error checks
  expect_error(iso_rename_file_info(42), "only rename.*iso files")
  expect_error(iso_rename_file_info(iso_file1, new = file_id), "renaming.*not allowed")
  expect_error(iso_rename_file_info(iso_files, new = file_id), "renaming.*not allowed")
  expect_error(iso_rename_file_info(iso_files, new_info = new_info2), "unresolvable naming conflict")
  expect_error(iso_rename_file_info(iso_files, y = new_info, y = new_info2), "unresolvable naming conflict")
  
  # rename info message
  expect_message(iso_rename_file_info(iso_file1), "renaming 0")
  expect_silent(iso_rename_file_info(iso_file1, quiet = TRUE))
  expect_silent(rename(iso_file1)) 
  expect_message(iso_rename_file_info(iso_file1, newer_info = new_info), "renaming 1 file info.*across 1 isofile")
  expect_message(iso_rename_file_info(iso_file1, newer_info = new_info), "new_info.*to.*newer_info.*in 1 file")
  expect_message(iso_rename_file_info(iso_files, newer_info = new_info2), "renaming 1 file info.*across 3 isofile")
  expect_message(iso_rename_file_info(iso_files, newer_info = new_info), "new_info.*to.*newer_info.*in 3 file")
  expect_message(iso_rename_file_info(iso_files, y = new_info2, y = new_info3), "renaming 2 file info.*across 3 isofile")
  expect_message(iso_rename_file_info(iso_files, y = new_info2, y = new_info3), "new_info2.*to.*y.*in 1 file")
  expect_message(iso_rename_file_info(iso_files, y = new_info2, y = new_info3), "new_info3.*to.*y.*in 1 file")
  
  # rename outcomes
  expect_equal(
    iso_rename_file_info(iso_files, y = new_info2, y = new_info3) %>% iso_get_file_info() %>% select(file_id, new_info, y),
    tibble(file_id = c("A", "B", "C"), new_info = 42, y = c(NA, 2, 3))
  )
  expect_equal(
    iso_rename_file_info(iso_files, newer_info = new_info) %>% iso_get_file_info() %>% select(file_id, newer_info),
    tibble(file_id = c("A", "B", "C"), newer_info = 42)
  )
  expect_equal(
    iso_rename_file_info(iso_files, newest_info = new_info, new_info2 = new_info3) %>% iso_get_file_info() %>% select(file_id, newest_info, new_info2),
    tibble(file_id = c("A", "B", "C"), newest_info = 42, new_info2 = c(NA, 2, 3))
  )
  
})

# filtering ======

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


