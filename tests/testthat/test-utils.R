context("Utility functions")

test_that("test that retrieving file paths works correctly", {
  
  expect_error(isoreader:::retrieve_file_paths())
  expect_error(isoreader:::retrieve_file_paths("DOESNOTEXIST"), "not exist")
  expect_error(isoreader:::retrieve_file_paths(c("DOESNOTEXIST", "NOTEITHER")), "not exist")
  expect_error(isoreader:::retrieve_file_paths(system.file("extdata", package = "isoreader")), "no extensions")
  expect_error(isoreader:::retrieve_file_paths(system.file("extdata", package = "isoreader") %>% list.files(full.names = T), "did"), 
               "do not have one of the supported extensions")
  
  # check expected result
  expect_identical(
    direct_list <- system.file("extdata", package = "isoreader") %>% list.files(full.names = T, pattern = "\\.(dxf|did)$"),
    system.file("extdata", package = "isoreader") %>% isoreader:::retrieve_file_paths(c("did", "dxf"))
  )
  expect_identical(
    system.file("extdata", package = "isoreader") %>% list.files(full.names = T, pattern = "\\.(dxf|did)$") %>% {.[c(2,1,3:length(.))]},
    c(direct_list[2], system.file("extdata", package = "isoreader")) %>% isoreader:::retrieve_file_paths(c("did", "dxf"))
  )
  
  
})