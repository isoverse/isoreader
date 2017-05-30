context("Data Structures")

test_that("test that dual inlet data structure is correct", {
  expect_is(isoreader:::make_di_data_structure(), "isofile")
  expect_is(isoreader:::make_di_data_structure(), "dual_inlet")
  # FIXME: expand
})

test_that("test that continuous data structure is correct", {
  expect_is(isoreader:::make_cf_data_structure(), "isofile")
  expect_is(isoreader:::make_cf_data_structure(), "continuous_flow")
  # FIXME: expand
})

test_that("can set file path for data structures", {
  expect_error(isoreader:::set_ds_file_path(data_frame()), "can only set path for isofile data structures")
  expect_silent(ds <- isoreader:::make_cf_data_structure())
  expect_error(isoreader:::set_ds_file_path(ds, "DOESNOTEXIST"), "does not exist")
  expect_is(ds <- isoreader:::set_ds_file_path(ds, system.file("extdata", package = "isoreader")), "isofile")
  expect_equal(ds$file_info$file_path, system.file("extdata", package = "isoreader"))
  expect_equal(ds$file_info$file_name, basename(system.file("extdata", package = "isoreader")))
})

