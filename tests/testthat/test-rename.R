context("File info manipulation")

test_that("Test that renaming file info works", {
  
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
  
  # error checks
  expect_error(iso_rename_file_info(42), "only rename.*iso files")
  expect_error(iso_rename_file_info(iso_file1, new = file_id), "trying to rename.*core file info")
  expect_error(iso_rename_file_info(iso_file1, new = file_root), "trying to rename.*core file info")
  expect_error(iso_rename_file_info(iso_file1, new = file_path), "trying to rename.*core file info")
  expect_error(iso_rename_file_info(iso_file1, new = file_subpath), "trying to rename.*core file info")
  expect_error(iso_rename_file_info(iso_file1, new = file_datetime), "trying to rename.*core file info")
  expect_error(iso_rename_file_info(iso_files, new = file_id), "trying to rename.*core file info")
  expect_error(iso_rename_file_info(iso_files, new_info = new_info2), "unresolvable naming conflict")
  expect_error(iso_rename_file_info(iso_files, y = new_info, y = new_info2), "unresolvable naming conflict")
  
  # info message
  expect_message(iso_rename_file_info(iso_file1), "renaming 0")
  expect_silent(iso_rename_file_info(iso_file1, quiet = TRUE))
  expect_silent(rename(iso_file1)) 
  expect_message(iso_rename_file_info(iso_file1, newer_info = new_info), "renaming 1 file info.*across 1 isofile")
  expect_message(iso_rename_file_info(iso_file1, newer_info = new_info), "new_info.*to.*newer_info.*in 1 file")
  expect_message(iso_rename_file_info(iso_files, newer_info = new_info2), "renaming 1 file info.*across 3 isofile")
  expect_message(iso_rename_file_info(iso_files, newer_info = new_info), "new_info.*to.*newer_info.*in 3 file")
  expect_message(iso_rename_file_info(iso_files, y = new_info2, y = new_info3), "renaming 2 file.*across 3 isofile")
  expect_message(iso_rename_file_info(iso_files, y = new_info2, y = new_info3), "new_info2.*to.*y.*in 1 file")
  expect_message(iso_rename_file_info(iso_files, y = new_info2, y = new_info3), "new_info3.*to.*y.*in 1 file")
  
  # outcomes
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