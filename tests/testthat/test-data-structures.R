context("Data Structures")

# basic iso_file data structure is correct ====
test_that("test that basic iso_file data structure is correct", {
  expect_is(iso_file <- isoreader:::make_iso_file_data_structure(), "iso_file")
  expect_equal(names(iso_file), c("version", "read_options", "file_info", "method_info", "raw_data", "vendor_data_table"))
  expect_equal(names(iso_file$read_options), c("file_info", "method_info", "raw_data", "vendor_data_table"))
  expect_equal(iso_file$version, packageVersion("isoreader"))
  expect_false(iso_is_file(42))
  expect_false(iso_is_file(iso_as_file_list()))
  expect_true(iso_is_file(iso_file))
  expect_false(iso_is_object(42))
  expect_true(iso_is_object(iso_file))
})

# dual inlet data structure is correct ====
test_that("test that dual inlet data structure is correct", {
  expect_is(di <- isoreader:::make_di_data_structure(), "iso_file")
  expect_is(di, "dual_inlet")
  expect_false(iso_is_dual_inlet(42))
  expect_false(iso_is_continuous_flow(di))
  expect_true(iso_is_dual_inlet(di))
  expect_true({
    di1 <- di2 <- di
    di1$file_info$file_id <- "A"
    di2$file_info$file_id <- "B"
    iso_is_dual_inlet(c(di1, di2))
  })
  # FIXME: expand
})

# continuous data structure is correct ====
test_that("test that continuous data structure is correct", {
  expect_is(cf <- isoreader:::make_cf_data_structure(), "iso_file")
  expect_is(cf, "continuous_flow")
  expect_false(iso_is_continuous_flow(42))
  expect_false(iso_is_dual_inlet(cf))
  expect_true(iso_is_continuous_flow(cf))
  expect_true({
    cf1 <- cf2 <- cf
    cf1$file_info$file_id <- "A"
    cf2$file_info$file_id <- "B"
    iso_is_continuous_flow(c(cf1, cf2))
  })
  # FIXME: expand
})

# iso_file list checks work ====
test_that("test that iso_file list checks work", {
  expect_is(iso_files <- iso_as_file_list(), "iso_file_list")
  expect_error(iso_as_file_list(1, error = "test"), "encountered incompatible data type")
  expect_false(iso_is_file_list(42))
  expect_false(iso_is_file_list(isoreader:::make_iso_file_data_structure()))
  expect_true(iso_is_file_list(iso_files))
  expect_true(iso_is_object(iso_files))
})

# can set file path for data structures ====
test_that("can set file path for data structures", {
  expect_error(isoreader:::set_ds_file_path(data_frame()), "can only set path for iso_file data structures")
  expect_silent(ds <- isoreader:::make_iso_file_data_structure())
  expect_error(isoreader:::set_ds_file_path(ds, "DOESNOTEXIST"), "does not exist")
  
  # default
  expect_is(ds <- isoreader:::set_ds_file_path(ds, system.file("extdata", package = "isoreader")), "iso_file")
  expect_equal(ds$file_info$file_path, system.file("extdata", package = "isoreader"))
  expect_equal(ds$file_info$file_id, basename(system.file("extdata", package = "isoreader")))
  expect_equal(ds$file_info$file_subpath, NA_character_)
  
  # custom id and subpath
  expect_is(ds <- isoreader:::set_ds_file_path(ds, system.file("extdata", package = "isoreader"),
                                               "my_id", "subpath"), "iso_file")
  expect_equal(ds$file_info$file_path, system.file("extdata", package = "isoreader"))
  expect_equal(ds$file_info$file_id, "my_id")
  expect_equal(ds$file_info$file_subpath, "subpath")
})


# can update read options ====
test_that("test that can update read options", {
  expect_is(iso_file <- isoreader:::make_iso_file_data_structure(), "iso_file")
  expect_equal(iso_file$read_options, list(file_info = FALSE, method_info = FALSE, raw_data = FALSE, vendor_data_table = FALSE))
  expect_equal(isoreader:::update_read_options(iso_file, not_an_option = FALSE)$read_options,
               list(file_info = FALSE, method_info = FALSE, raw_data = FALSE, vendor_data_table = FALSE))
  expect_equal(isoreader:::update_read_options(iso_file, read_file_info = TRUE, raw_data = TRUE)$read_options,
               list(file_info = TRUE, method_info = FALSE, raw_data = TRUE, vendor_data_table = FALSE))
})


# isofils objects can be combined and subset ====
test_that("test that isofils objects can be combined and subset", {
  
  expect_is(iso_file <- isoreader:::make_iso_file_data_structure(), "iso_file")
  expect_equal({ iso_fileA <- iso_file %>% { .$file_info$file_id <- "A"; . }; iso_fileA$file_info$file_id }, "A")
  expect_equal({ iso_fileB <- iso_file %>% { .$file_info$file_id <- "B"; . }; iso_fileB$file_info$file_id }, "B")
  expect_equal({ iso_fileC <- iso_file %>% { .$file_info$file_id <- "C"; . }; iso_fileC$file_info$file_id }, "C")
  
  # combinining iso_files
  expect_error(c(iso_fileA, 5), "can only combine iso_file and iso_file\\_list")
  expect_error(c(isoreader:::make_cf_data_structure(), isoreader:::make_di_data_structure()), 
               "can only combine iso_file objects with the same data type")
  expect_is(iso_filesAB <- c(iso_fileA, iso_fileB), "iso_file_list")
  expect_is(iso_filesABC <- c(iso_fileA, iso_fileB, iso_fileC), "iso_file_list")
  expect_equal(c(iso_filesAB, iso_fileC), c(iso_fileA, iso_fileB, iso_fileC))
  
  ## problems combining identical files (without discarding duplicates!)
  expect_warning(iso_filesABA <- iso_as_file_list(iso_fileA, iso_fileB, iso_fileA, discard_duplicates = FALSE), 
                 "duplicate files kept, may interfere with data processing")
  expect_is(iso_filesABA, "iso_file_list")
  expect_equal(problems(iso_filesABA) %>% select(file_id, type), data_frame(file_id = "A", type = "warning"))
  expect_equal(names(iso_filesABA), c("A", "B", "A"))
  expect_equal(problems(iso_filesABA[[1]]) %>% select(type), data_frame(type = "warning"))
  expect_equal(problems(iso_filesABA[[2]]) %>% select(type), data_frame(type = character(0)))
  expect_equal(problems(iso_filesABA[[3]]) %>% select(type), data_frame(type = "warning"))
  expect_equal(problems(c(iso_fileA, iso_fileA)), problems(c(iso_fileA, iso_fileA, iso_fileA)))
  
  ## combining identical files (with discarding duplicates, i.e. default behavior)
  expect_warning(iso_filesABA <- c(iso_fileA, iso_fileB, iso_fileA), 
                 "duplicate files encountered, only first kept")
  expect_equal(problems(iso_filesABA) %>% select(file_id, type), data_frame(file_id = "A", type = "warning"))
  expect_equal(names(iso_filesABA), c("A", "B"))
  
  ## propagating problems
  expect_is(
    iso_filesAB_probs <- c(
      isoreader:::register_warning(iso_fileA, "warning A", warn=FALSE),
      isoreader:::register_warning(iso_fileB, "warning B", warn=FALSE)),
    "iso_file_list"
  )
  expect_equal(problems(iso_filesAB_probs) %>% select(file_id, details),
               data_frame(file_id = c("A", "B"), details = paste("warning", c("A", "B"))))
  expect_warning(iso_files_ABB_probs <- c(iso_filesAB_probs, iso_fileB), "duplicate files encountered")
  expect_equal(problems(iso_files_ABB_probs) %>% select(file_id, details),
               data_frame(file_id = c("A", "B", "B"), details = c("warning A", "warning B", 
                          "duplicate files encountered, only first kept: B")))
  
  # subsetting iso_files
  expect_is(iso_filesAB[2], "iso_file_list")
  expect_is(iso_filesABC[1:2], "iso_file_list")
  expect_is(iso_filesABC[c("A", "C")], "iso_file_list")
  expect_equal(iso_filesABC[c(1,3)], iso_filesABC[c("A", "C")])
  expect_is(iso_filesAB[[2]], "iso_file")
  expect_is(iso_filesAB[['B']], "iso_file")
  expect_equal(iso_filesAB[[2]], iso_filesAB[['B']])
  expect_equal(iso_filesAB[[2]], iso_filesAB$B)
  expect_equal(iso_filesAB[['B']], iso_filesAB$B)
  
  ## ignore out of range indices
  expect_equal(iso_filesABC[2:3], iso_filesABC[2:5])
  expect_equal(iso_filesABC[c("B", "C")], iso_filesABC[c("B", "C", "D")]) 
  
  # assigning by indices
  expect_equal( { iso_files <- iso_filesAB; iso_files[[3]] <- iso_fileC; names(iso_files)}, names(iso_filesABC))
  expect_equal( { iso_files <- iso_filesAB; iso_files[[2]] <- iso_fileC; names(iso_files) }, names(c(iso_fileA, iso_fileC)))
  expect_equal( { iso_files <- iso_filesAB; iso_files[1] <- iso_filesABC[3]; names(iso_files) }, names(c(iso_fileC, iso_fileB)))
  expect_equal( { iso_files <- iso_filesAB; iso_files[[1]] <- iso_filesABC[[3]]; names(iso_files) }, names(c(iso_fileC, iso_fileB)))
  
  ## warnings from assignments
  expect_warning( { iso_files <- iso_filesAB; iso_files[1] <- iso_files[2]}, "duplicate files")
  expect_equal(names(iso_files), "B")
  
  # convertion to list
  expect_equal(as.list(iso_filesABC) %>% class(), "list")
  expect_equal(as.list(iso_filesABC)[[1]], iso_filesABC[[1]])
})



