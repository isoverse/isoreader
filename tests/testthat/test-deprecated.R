context("Deprecated functions")

test_that("test that deprecated functions give the appropriate error", {
  
  expect_warning(tryCatch(iso_get_data(), error = function(e){}), "deprecated")
  expect_warning(tryCatch(iso_get_standards_info(), error = function(e){}), "deprecated")
  expect_warning(tryCatch(iso_get_resistors_info(), error = function(e){}), "deprecated")
  expect_warning(tryCatch(iso_omit_files_with_problems(), error = function(e){}), "deprecated")
  expect_warning(tryCatch(iso_export_to_excel(), error = function(e){}), "deprecated")
  expect_warning(tryCatch(iso_export_to_feather(), error = function(e){}), "deprecated")
  
})
