context("Data Structures")

# basic isofile data structure is correct ====
test_that("test that basic isofile data structure is correct", {
  expect_is(isofile <- isoreader:::make_isofile_data_structure(), "isofile")
  expect_equal(names(isofile), c("version", "read_options", "file_info", "method_info", "raw_data", "vendor_data_table"))
  expect_equal(names(isofile$read_options), c("file_info", "method_info", "raw_data", "vendor_data_table"))
  expect_equal(isofile$version, packageVersion("isoreader"))
  expect_false(iso_is_file(42))
  expect_false(iso_is_file(iso_as_file_list()))
  expect_true(iso_is_file(isofile))
  expect_false(iso_is_object(42))
  expect_true(iso_is_object(isofile))
})

# dual inlet data structure is correct ====
test_that("test that dual inlet data structure is correct", {
  expect_is(di <- isoreader:::make_di_data_structure(), "isofile")
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
  expect_is(cf <- isoreader:::make_cf_data_structure(), "isofile")
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

# isofile list checks work ====
test_that("test that isofile list checks work", {
  expect_is(isofiles <- iso_as_file_list(), "isofile_list")
  expect_error(iso_as_file_list(1, error = "test"), "encountered incompatible data type")
  expect_false(iso_is_file_list(42))
  expect_false(iso_is_file_list(isoreader:::make_isofile_data_structure()))
  expect_true(iso_is_file_list(isofiles))
  expect_true(iso_is_object(isofiles))
})

# can set file path for data structures ====
test_that("can set file path for data structures", {
  expect_error(isoreader:::set_ds_file_path(data_frame()), "can only set path for isofile data structures")
  expect_silent(ds <- isoreader:::make_isofile_data_structure())
  expect_error(isoreader:::set_ds_file_path(ds, "DOESNOTEXIST"), "does not exist")
  
  # default
  expect_is(ds <- isoreader:::set_ds_file_path(ds, system.file("extdata", package = "isoreader")), "isofile")
  expect_equal(ds$file_info$file_path, system.file("extdata", package = "isoreader"))
  expect_equal(ds$file_info$file_id, basename(system.file("extdata", package = "isoreader")))
  expect_equal(ds$file_info$file_subpath, NA_character_)
  
  # custom id and subpath
  expect_is(ds <- isoreader:::set_ds_file_path(ds, system.file("extdata", package = "isoreader"),
                                               "my_id", "subpath"), "isofile")
  expect_equal(ds$file_info$file_path, system.file("extdata", package = "isoreader"))
  expect_equal(ds$file_info$file_id, "my_id")
  expect_equal(ds$file_info$file_subpath, "subpath")
})


# can update read options ====
test_that("test that can update read options", {
  expect_is(isofile <- isoreader:::make_isofile_data_structure(), "isofile")
  expect_equal(isofile$read_options, list(file_info = FALSE, method_info = FALSE, raw_data = FALSE, vendor_data_table = FALSE))
  expect_equal(isoreader:::update_read_options(isofile, not_an_option = FALSE)$read_options,
               list(file_info = FALSE, method_info = FALSE, raw_data = FALSE, vendor_data_table = FALSE))
  expect_equal(isoreader:::update_read_options(isofile, read_file_info = TRUE, raw_data = TRUE)$read_options,
               list(file_info = TRUE, method_info = FALSE, raw_data = TRUE, vendor_data_table = FALSE))
})


# isofils objects can be combined and subset ====
test_that("test that isofils objects can be combined and subset", {
  
  expect_is(isofile <- isoreader:::make_isofile_data_structure(), "isofile")
  expect_equal({ isofileA <- isofile %>% { .$file_info$file_id <- "A"; . }; isofileA$file_info$file_id }, "A")
  expect_equal({ isofileB <- isofile %>% { .$file_info$file_id <- "B"; . }; isofileB$file_info$file_id }, "B")
  expect_equal({ isofileC <- isofile %>% { .$file_info$file_id <- "C"; . }; isofileC$file_info$file_id }, "C")
  
  # combinining isofiles
  expect_error(c(isofileA, 5), "can only combine isofile and isofile\\_list")
  expect_error(c(isoreader:::make_cf_data_structure(), isoreader:::make_di_data_structure()), 
               "can only combine isofile objects with the same data type")
  expect_is(isofilesAB <- c(isofileA, isofileB), "isofile_list")
  expect_is(isofilesABC <- c(isofileA, isofileB, isofileC), "isofile_list")
  expect_equal(c(isofilesAB, isofileC), c(isofileA, isofileB, isofileC))
  
  ## problems combining identical files (without discarding duplicates!)
  expect_warning(isofilesABA <- iso_as_file_list(isofileA, isofileB, isofileA, discard_duplicates = FALSE), 
                 "duplicate files kept, may interfere with data processing")
  expect_is(isofilesABA, "isofile_list")
  expect_equal(problems(isofilesABA) %>% select(file_id, type), data_frame(file_id = "A", type = "warning"))
  expect_equal(names(isofilesABA), c("A", "B", "A"))
  expect_equal(problems(isofilesABA[[1]]) %>% select(type), data_frame(type = "warning"))
  expect_equal(problems(isofilesABA[[2]]) %>% select(type), data_frame(type = character(0)))
  expect_equal(problems(isofilesABA[[3]]) %>% select(type), data_frame(type = "warning"))
  expect_equal(problems(c(isofileA, isofileA)), problems(c(isofileA, isofileA, isofileA)))
  
  ## combining identical files (with discarding duplicates, i.e. default behavior)
  expect_warning(isofilesABA <- c(isofileA, isofileB, isofileA), 
                 "duplicate files encountered, only first kept")
  expect_equal(problems(isofilesABA) %>% select(file_id, type), data_frame(file_id = "A", type = "warning"))
  expect_equal(names(isofilesABA), c("A", "B"))
  
  ## propagating problems
  expect_is(
    isofilesAB_probs <- c(
      isoreader:::register_warning(isofileA, "warning A", warn=FALSE),
      isoreader:::register_warning(isofileB, "warning B", warn=FALSE)),
    "isofile_list"
  )
  expect_equal(problems(isofilesAB_probs) %>% select(file_id, details),
               data_frame(file_id = c("A", "B"), details = paste("warning", c("A", "B"))))
  expect_warning(isofiles_ABB_probs <- c(isofilesAB_probs, isofileB), "duplicate files encountered")
  expect_equal(problems(isofiles_ABB_probs) %>% select(file_id, details),
               data_frame(file_id = c("A", "B", "B"), details = c("warning A", "warning B", 
                          "duplicate files encountered, only first kept: B")))
  
  # subsetting isofiles
  expect_is(isofilesAB[2], "isofile_list")
  expect_is(isofilesABC[1:2], "isofile_list")
  expect_is(isofilesABC[c("A", "C")], "isofile_list")
  expect_equal(isofilesABC[c(1,3)], isofilesABC[c("A", "C")])
  expect_is(isofilesAB[[2]], "isofile")
  expect_is(isofilesAB[['B']], "isofile")
  expect_equal(isofilesAB[[2]], isofilesAB[['B']])
  expect_equal(isofilesAB[[2]], isofilesAB$B)
  expect_equal(isofilesAB[['B']], isofilesAB$B)
  
  ## ignore out of range indices
  expect_equal(isofilesABC[2:3], isofilesABC[2:5])
  expect_equal(isofilesABC[c("B", "C")], isofilesABC[c("B", "C", "D")]) 
  
  # assigning by indices
  expect_equal( { isofiles <- isofilesAB; isofiles[[3]] <- isofileC; names(isofiles)}, names(isofilesABC))
  expect_equal( { isofiles <- isofilesAB; isofiles[[2]] <- isofileC; names(isofiles) }, names(c(isofileA, isofileC)))
  expect_equal( { isofiles <- isofilesAB; isofiles[1] <- isofilesABC[3]; names(isofiles) }, names(c(isofileC, isofileB)))
  expect_equal( { isofiles <- isofilesAB; isofiles[[1]] <- isofilesABC[[3]]; names(isofiles) }, names(c(isofileC, isofileB)))
  
  ## warnings from assignments
  expect_warning( { isofiles <- isofilesAB; isofiles[1] <- isofiles[2]}, "duplicate files")
  expect_equal(names(isofiles), "B")
  
  # convertion to list
  expect_equal(as.list(isofilesABC) %>% class(), "list")
  expect_equal(as.list(isofilesABC)[[1]], isofilesABC[[1]])
})



