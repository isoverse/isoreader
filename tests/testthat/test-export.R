context("Export functions")

test_that("test that export to rda works properly", {
  expect_error(iso_export_to_rda(42), "can only export iso files")
  expect_error(iso_export_to_rda(isoreader:::make_cf_data_structure()), "no filepath provided")
  expect_error(iso_export_to_rda(isoreader:::make_cf_data_structure(), file.path("DOESNOTEXIST", "test")), 
               "folder .* does not exist")
  
  # test data
  cf <- isoreader:::make_cf_data_structure()
  cf$file_info$file_id <- "A"
  cf$raw_data <- data_frame(time = 1:10, m44 = runif(10), m45 = runif(10))
  cf$method_info$standards <- data_frame(standard = "test a")
  cf$method_info$resistors <- data_frame(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11))
  cf$vendor_data_table <- data_frame(x = 1:5, y = letters[1:5]) %>% { attr(., "units") <- c(x="a", y = "b"); . }
  filepath <- file.path(tempdir(), "test")
  
  # export and reimport single file
  expect_message(cf_out <- iso_export_to_rda(cf, filepath, quiet = FALSE), "exporting data .* into R Data Archive")
  expect_equal(cf, cf_out)
  expect_true(file.exists(str_c(filepath, ".cf.rda")))
  expect_message(cf_back <- iso_read_continuous_flow(str_c(filepath, ".cf.rda"), quiet = FALSE), "reading file")
  expect_equal(cf, cf_back)  
  expect_true(file.remove(str_c(filepath, ".cf.rda")))
  
  # export and reimport multiple isofiles
  cf2 <- cf
  cf2$file_info$file_id <- "B"
  cfs <- c(cf, cf2)
  expect_message(cfs_out <- iso_export_to_rda(cfs, filepath, quiet = FALSE), "exporting data .* into R Data Archive")
  expect_equal(cfs, cfs_out)
  expect_true(file.exists(str_c(filepath, ".cf.rda")))
  expect_message(cfs_back <- iso_read_continuous_flow(str_c(filepath, ".cf.rda"), quiet = FALSE), "reading file")
  expect_equal(cfs, cfs_back)  
  expect_true(file.remove(str_c(filepath, ".cf.rda")))
  
})

library(readxl)
test_that("test that export to Excel works properly", {
  expect_error(iso_export_to_excel(42), "can only export iso files")
  expect_error(iso_export_to_excel(isoreader:::make_cf_data_structure()), "no filepath provided")
  expect_error(iso_export_to_excel(isoreader:::make_cf_data_structure(), file.path("DOESNOTEXIST", "test")), 
               "folder .* does not exist")
  
  # test data
  cf <- isoreader:::make_cf_data_structure()
  cf$file_info$file_id <- "A"
  cf$read_options <- list(file_info = TRUE, method_info = TRUE, raw_data = TRUE, vendor_data_table = TRUE)
  cf$raw_data <- data_frame(time = (1:10)*0.1, m44 = (1:10)*0.2, m45 = (1:10)*0.3)
  cf$method_info$standards <- data_frame(standard = "test a")
  cf$method_info$resistors <- data_frame(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11))
  cf$vendor_data_table <- data_frame(x = 1:5, y = letters[1:5]) %>% { attr(., "units") <- data_frame(column=c("x", "y"), units = ""); . }
  filepath <- file.path(tempdir(), "test")
  
  # export and check
  expect_message(cf_out <- iso_export_to_excel(cf, filepath, quiet = FALSE), "exporting data .* into Excel")
  expect_equal(cf, cf_out)
  expect_true(file.exists(str_c(filepath, ".cf.xlsx")))
  # note for comparisons: rounding is necessary because storage is not perfect numerically
  expect_equal(iso_aggregate_raw_data(cf) %>% 
                 mutate(time = signif(time), m44 = signif(m44), m45 = signif(m45)), 
               read_excel(str_c(filepath, ".cf.xlsx"), "raw data") %>% 
                 mutate(time = signif(time), m44 = signif(m44), m45 = signif(m45))) 
  expect_equal(iso_aggregate_file_info(cf), 
               read_excel(str_c(filepath, ".cf.xlsx"), "file info",
                          col_types = c("text", "text", "text", "logical"))) 
  expect_equal(iso_aggregate_vendor_data_table(cf), 
               read_excel(str_c(filepath, ".cf.xlsx"), "vendor data table") %>% 
                 mutate(x = as.integer(x))) 
  # TODO: also test the standards and resistor values (from the same tab)
  expect_true(file.remove(str_c(filepath, ".cf.xlsx")))
})


library(feather)
test_that("test that export to Feather works properly", {
  expect_error(iso_export_to_feather(42), "can only export iso files")
  expect_error(iso_export_to_feather(isoreader:::make_cf_data_structure()), "no filepath provided")
  expect_error(iso_export_to_feather(isoreader:::make_cf_data_structure(), file.path("DOESNOTEXIST", "test")), 
               "folder .* does not exist")
  
  # test data
  cf <- isoreader:::make_cf_data_structure()
  cf$file_info$file_id <- "A"
  cf$read_options <- list(file_info = TRUE, method_info = TRUE, raw_data = TRUE, vendor_data_table = TRUE)
  cf$raw_data <- data_frame(time = (1:10)*0.1, m44 = (1:10)*0.2, m45 = (1:10)*0.3)
  cf$method_info$standards <- data_frame(standard = "test a")
  cf$method_info$resistors <- data_frame(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11))
  cf$vendor_data_table <- data_frame(x = 1:5, y = letters[1:5]) %>% { attr(., "units") <- data_frame(column=c("x", "y"), units = ""); . }
  filepath <- file.path(tempdir(), "test")
  
  # export and check
  expect_message(cf_out <- iso_export_to_feather(cf, filepath, quiet = FALSE), "exporting data .* into .cf.feather")
  expect_equal(cf, cf_out)
  expect_true(file.exists(str_c(filepath, "_raw_data.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_file_info.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-standards.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-resistors.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_vendor_data_table.cf.feather")))
  # note for comparisons: rounding is NOT necessary because storage is equivalent to values in R
  expect_equal(iso_aggregate_raw_data(cf), read_feather(str_c(filepath, "_raw_data.cf.feather")))
  expect_equal(iso_aggregate_file_info(cf), read_feather(str_c(filepath, "_file_info.cf.feather")))
  expect_equal(iso_aggregate_standards_info(cf), read_feather(str_c(filepath, "_method_info-standards.cf.feather")))
  expect_equal(iso_aggregate_resistors_info(cf), read_feather(str_c(filepath, "_method_info-resistors.cf.feather")))
  expect_equal(iso_aggregate_vendor_data_table(cf), read_feather(str_c(filepath, "_vendor_data_table.cf.feather")))
  expect_true(all(file.remove(list.files(dirname(filepath), pattern = "\\.cf\\.feather$", full.names = TRUE))))
})
