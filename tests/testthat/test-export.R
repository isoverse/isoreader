context("Export functions")

di_example <- iso_read_dual_inlet(iso_get_reader_example("dual_inlet_example.did"))
cf_example <- iso_read_continuous_flow(iso_get_reader_example("continuous_flow_example.cf"))
scan_example <- iso_read_scan(iso_get_reader_example("peak_shape_scan_example.scn"))

test_that("test that export to rda works properly", {
  expect_error(iso_save(42), "can only export iso files")
  expect_error(iso_save(make_cf_data_structure("NA")), "no filepath provided")
  expect_error(iso_save(make_cf_data_structure("NA"), file.path("DOESNOTEXIST", "test")), 
               "folder .* does not exist")
  
  # test data
  cf <- make_cf_data_structure("NA")
  cf$file_info$file_id <- "A"
  cf$file_info$file_root <- "."
  cf$file_info$file_path <- "test"
  cf$file_info$vector_test <- list(1:3)
  cf$raw_data <- tibble(time = 1:10, m44 = runif(10), m45 = runif(10))
  cf$method_info$standards <- tibble(standard = "test a")
  cf$method_info$resistors <- tibble(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11))
  cf$vendor_data_table <- 
    tibble(x = iso_double_with_units(1:5, units = "a"), 
           y = letters[1:5])
  filepath <- file.path(tempdir(), "test")
  
  # export and reimport single file
  expect_message(cf_out <- iso_save(cf, filepath, quiet = FALSE), "exporting data .* into R Data Storage")
  expect_equal(cf$raw_data, cf_out$raw_data)
  expect_true(file.exists(paste0(filepath, ".cf.rds")))
  expect_message(cf_back <- iso_read_continuous_flow(paste0(filepath, ".cf.rds"), quiet = FALSE), "reading file")
  expect_equal(cf$raw_data, cf_back$raw_data)
  expect_equal(cf$file_info %>% unnest(vector_test), cf_back$file_info %>% unnest(vector_test))
  expect_equal(cf$method_info$standards, cf_back$method_info$standards)
  expect_equal(cf$method_info$resistors, cf_back$method_info$resistors)
  expect_equal(cf$vendor_data_table, cf_back$vendor_data_table)
  expect_true(file.remove(str_c(filepath, ".cf.rds")))
  
  # export and reimport multiple iso_files
  cf$file_info$vector_test <- NULL
  cf2 <- cf
  cf2$file_info$file_id <- "B"
  cfs <- c(cf, cf2)
  expect_message(cfs_out <- iso_save(cfs, filepath, quiet = FALSE), "exporting data .* into R Data Storage")
  expect_equal(cfs, cfs_out)
  expect_true(file.exists(str_c(filepath, ".cf.rds")))
  expect_message(cfs_back <- iso_read_continuous_flow(str_c(filepath, ".cf.rds"), quiet = FALSE), "reading file")
  expect_equal(cfs, cfs_back)  
  expect_true(file.remove(str_c(filepath, ".cf.rds")))
  
  # export real data files - dual inlet
  expect_message(iso_save(di_example, filepath, quiet = FALSE), "exporting data .* into R Data Storage")
  expect_true(file.exists(str_c(filepath, ".di.rds")))
  expect_message(di_example_back <- iso_read_dual_inlet(str_c(filepath, ".di.rds"), quiet = FALSE), "reading file")
  expect_equal(di_example$raw_data, di_example_back$raw_data)  
  expect_equal(di_example$file_info %>% unnest_aggregated_data_frame() %>% unnest(measurement_info), 
               di_example_back$file_info %>% unnest_aggregated_data_frame() %>% unnest(measurement_info))
  expect_equal(di_example$method_info$standards, di_example_back$method_info$standards)
  expect_equal(di_example$method_info$resistors, di_example_back$method_info$resistors)
  expect_equal(di_example$vendor_data_table, di_example_back$vendor_data_table)
  expect_true(file.remove(str_c(filepath, ".di.rds")))
  
  # export real data files - continuous flow
  expect_message(iso_save(cf_example, filepath, quiet = FALSE), "exporting data .* into R Data Storage")
  expect_true(file.exists(str_c(filepath, ".cf.rds")))
  expect_message(cf_example_back <- iso_read_continuous_flow(str_c(filepath, ".cf.rds"), quiet = FALSE), "reading file")
  expect_equal(cf_example$raw_data, cf_example_back$raw_data)  
  expect_equal(cf_example$file_info %>% unnest_aggregated_data_frame(), 
               cf_example_back$file_info %>% unnest_aggregated_data_frame())
  expect_equal(cf_example$method_info$standards, cf_example_back$method_info$standards)
  expect_equal(cf_example$method_info$resistors, cf_example_back$method_info$resistors)
  expect_equal(cf_example$vendor_data_table, cf_example_back$vendor_data_table)
  expect_true(file.remove(str_c(filepath, ".cf.rds")))
})

library(readxl)
test_that("test that export to Excel works properly", {
  expect_error(iso_export_to_excel(42), "can only export iso files")
  expect_error(iso_export_to_excel(make_cf_data_structure("NA")), "no filepath provided")
  expect_error(iso_export_to_excel(make_cf_data_structure("NA"), file.path("DOESNOTEXIST", "test")), 
               "folder .* does not exist")
  
  # test data
  cf <- make_cf_data_structure("NA")
  cf$file_info$file_id <- "A"
  cf$file_info$vector_test <- list(1:3)
  cf$read_options <- list(file_info = TRUE, method_info = TRUE, raw_data = TRUE, vendor_data_table = TRUE)
  cf$raw_data <- tibble(time = (1:10)*0.1, m44 = (1:10)*0.2, m45 = (1:10)*0.3)
  cf$method_info$standards <- tibble(standard = "test a")
  cf$method_info$resistors <- tibble(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11))
  cf$vendor_data_table <- tibble(x = 1:5, y = letters[1:5]) %>% { attr(., "units") <- tibble(column=c("x", "y"), units = ""); . }
  filepath <- file.path(tempdir(), "test")
  
  # export and check
  expect_message(cf_out <- iso_export_to_excel(cf, filepath, quiet = FALSE), "exporting data .* into Excel")
  expect_equal(names(cf), names(cf_out))
  expect_true(is_tibble(cf$file_info))
  expect_true(is_tibble(cf_out$file_info))
  expect_equal(as.list(cf$file_info), as.list(cf_out$file_info))
  expect_equal(cf$raw_data, cf_out$raw_data)
  expect_equal(cf$resistors, cf_out$resistors)
  expect_equal(cf$vendor_data_table, cf_out$vendor_data_table)
  expect_true(file.exists(str_c(filepath, ".cf.xlsx")))
  # note for comparisons: rounding is necessary because storage is not perfect numerically
  expect_equal(iso_get_raw_data(cf) %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif), 
               readxl::read_excel(str_c(filepath, ".cf.xlsx"), "raw data") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif)) 
  expect_equal(iso_get_file_info(cf) %>% collapse_list_columns(), 
               readxl::read_excel(str_c(filepath, ".cf.xlsx"), "file info",
                          col_types = c("text", "text", "text", "text", "numeric", "text")) %>% 
                 mutate(file_datetime = as.integer(file_datetime))) 
  expect_equal(iso_get_vendor_data_table(cf), 
               readxl::read_excel(str_c(filepath, ".cf.xlsx"), "vendor data table") %>% 
                 mutate(x = as.integer(x))) 
  # TODO: also test the standards and resistor values (from the same tab)
  expect_true(file.remove(str_c(filepath, ".cf.xlsx")))
  
  # export real data files - dual inlet
  expect_message(iso_export_to_excel(di_example, filepath, quiet = FALSE), "exporting data .* into Excel")
  expect_true(file.exists(str_c(filepath, ".di.xlsx")))
  expect_equal(iso_get_raw_data(di_example) %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif), 
               read_excel(str_c(filepath, ".di.xlsx"), "raw data") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif)) 
  expect_equal(iso_get_file_info(di_example) %>% collapse_list_columns() %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif) %>% 
                 dplyr::select_if(function(x) !is.na(x)) %>% 
                 select(-file_datetime), # never exactly identical, 
               read_excel(str_c(filepath, ".di.xlsx"), "file info") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif) %>% 
                 dplyr::select_if(function(x) !is.na(x)) %>% 
                 select(-file_datetime))
  expect_equal(iso_get_vendor_data_table(di_example) %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif), 
               read_excel(str_c(filepath, ".di.xlsx"), "vendor data table") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif)) 
  expect_true(file.remove(str_c(filepath, ".di.xlsx")))
  
  # export real data files - continuous flow
  expect_message(iso_export_to_excel(cf_example, filepath, quiet = FALSE), "exporting data .* into Excel")
  expect_true(file.exists(str_c(filepath, ".cf.xlsx")))
  expect_equal(iso_get_raw_data(cf_example) %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif), 
               read_excel(str_c(filepath, ".cf.xlsx"), "raw data") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif)) 
  expect_equal(iso_get_file_info(cf_example) %>% collapse_list_columns() %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif) %>% 
                 dplyr::select_if(function(x) !is.na(x)) %>% 
                 select(-file_datetime), # never exactly identical, 
               read_excel(str_c(filepath, ".cf.xlsx"), "file info") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif) %>% 
                 dplyr::select_if(function(x) !is.na(x)) %>% 
                 select(-file_datetime))
  expect_equal(iso_get_vendor_data_table(cf_example) %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif) %>% 
                 iso_strip_units(), 
               read_excel(str_c(filepath, ".cf.xlsx"), "vendor data table") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif)) 
  expect_true(file.remove(str_c(filepath, ".cf.xlsx")))
  
  # export real data files with explicit units
  expect_message(iso_export_to_excel(cf_example, filepath, with_explicit_units = TRUE, quiet = FALSE), "exporting data .* into Excel")
  expect_true(file.exists(str_c(filepath, ".cf.xlsx")))
  expect_equal(iso_get_vendor_data_table(cf_example, with_explicit_units = TRUE) %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif), 
               read_excel(str_c(filepath, ".cf.xlsx"), "vendor data table") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif)) 
  expect_true(file.remove(str_c(filepath, ".cf.xlsx")))
  
  # export real data files - scan
  expect_message(iso_export_to_excel(scan_example, filepath, quiet = FALSE), "exporting data .* into Excel")
  expect_true(file.exists(str_c(filepath, ".scan.xlsx")))
  expect_equal(iso_get_raw_data(scan_example) %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif), 
               read_excel(str_c(filepath, ".scan.xlsx"), "raw data") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif)) 
  expect_equal(iso_get_file_info(scan_example) %>% collapse_list_columns() %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif) %>% 
                 dplyr::select_if(function(x) !is.na(x)) %>% 
                 select(-file_datetime), # never exactly identical, 
               read_excel(str_c(filepath, ".scan.xlsx"), "file info") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif) %>% 
                 dplyr::select_if(function(x) !is.na(x)) %>% 
                 select(-file_datetime))
  expect_equal(iso_get_resistors_info(scan_example) %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif), 
               read_excel(str_c(filepath, ".scan.xlsx"), "method info") %>% 
                 dplyr::mutate_if(.predicate = is.numeric, .funs = signif)) 
  expect_true(file.remove(str_c(filepath, ".scan.xlsx")))
  
  
})


library(feather)
test_that("test that export to Feather works properly", {
  expect_error(iso_export_to_feather(42), "can only export iso files")
  expect_error(iso_export_to_feather(make_cf_data_structure("NA")), "no filepath provided")
  expect_error(iso_export_to_feather(make_cf_data_structure("NA"), file.path("DOESNOTEXIST", "test")), 
               "folder .* does not exist")
  
  # test data
  cf <- make_cf_data_structure("NA")
  cf$file_info$file_id <- "A"
  cf$file_info$vector_test <- list(1:3)
  cf$read_options <- list(file_info = TRUE, method_info = TRUE, raw_data = TRUE, vendor_data_table = TRUE)
  cf$raw_data <- tibble(time = (1:10)*0.1, m44 = (1:10)*0.2, m45 = (1:10)*0.3)
  cf$method_info$standards <- tibble(standard = "test a")
  cf$method_info$resistors <- tibble(cup = 1:3, R.Ohm = c(1e9, 1e10, 1e11))
  cf$vendor_data_table <- tibble(x = 1:5, y = letters[1:5]) %>% { attr(., "units") <- tibble(column=c("x", "y"), units = ""); . } %>% convert_df_units_attr_to_implicit_units()
  filepath <- file.path(tempdir(), "test")
  
  # export and check
  expect_message(cf_out <- iso_export_to_feather(cf, filepath, quiet = FALSE), "exporting data .* into .cf.feather")
  expect_equal(names(cf), names(cf_out))
  expect_true(is_tibble(cf$file_info))
  expect_true(is_tibble(cf_out$file_info))
  expect_equal(as.list(cf$file_info), as.list(cf_out$file_info))
  expect_equal(cf$raw_data, cf_out$raw_data)
  expect_equal(cf$resistors, cf_out$resistors)
  expect_equal(cf$vendor_data_table, cf_out$vendor_data_table)
  expect_true(file.exists(str_c(filepath, "_raw_data.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_file_info.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-standards.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-resistors.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_vendor_data_table.cf.feather")))
  # note for comparisons: rounding is NOT necessary because storage is equivalent to values in R
  expect_equal(iso_get_raw_data(cf), read_feather(str_c(filepath, "_raw_data.cf.feather")))
  expect_equal(iso_get_file_info(cf) %>% collapse_list_columns(), read_feather(str_c(filepath, "_file_info.cf.feather")))
  expect_equal(iso_get_standards_info(cf), read_feather(str_c(filepath, "_method_info-standards.cf.feather")))
  expect_equal(iso_get_resistors_info (cf), read_feather(str_c(filepath, "_method_info-resistors.cf.feather")))
  expect_equal(iso_get_vendor_data_table(cf), read_feather(str_c(filepath, "_vendor_data_table.cf.feather")))
  expect_true(all(file.remove(list.files(dirname(filepath), pattern = "\\.cf\\.feather$", full.names = TRUE))))
  
  # export real data files - dual inlet
  expect_message(iso_export_to_feather(di_example, filepath, quiet = FALSE), "exporting data .* into .di.feather")
  expect_true(file.exists(str_c(filepath, "_raw_data.di.feather")))
  expect_true(file.exists(str_c(filepath, "_file_info.di.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-standards.di.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-resistors.di.feather")))
  expect_true(file.exists(str_c(filepath, "_vendor_data_table.di.feather")))
  # note for comparisons: rounding is NOT necessary because storage is equivalent to values in R
  expect_equal(iso_get_raw_data(di_example), read_feather(str_c(filepath, "_raw_data.di.feather")))
  expect_equal(iso_get_file_info(di_example) %>% collapse_list_columns(), read_feather(str_c(filepath, "_file_info.di.feather")))
  expect_equal(iso_get_standards_info(di_example), read_feather(str_c(filepath, "_method_info-standards.di.feather")))
  expect_equal(iso_get_resistors_info (di_example), read_feather(str_c(filepath, "_method_info-resistors.di.feather")))
  expect_equal(iso_get_vendor_data_table(di_example), read_feather(str_c(filepath, "_vendor_data_table.di.feather")))
  expect_true(all(file.remove(list.files(dirname(filepath), pattern = "\\.di\\.feather$", full.names = TRUE))))
  
  # export real data files - continuous flow
  expect_message(iso_export_to_feather(cf_example, filepath, quiet = FALSE), "exporting data .* into .cf.feather")
  expect_true(file.exists(str_c(filepath, "_raw_data.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_file_info.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-standards.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-resistors.cf.feather")))
  expect_true(file.exists(str_c(filepath, "_vendor_data_table.cf.feather")))
  # note for comparisons: rounding is NOT necessary because storage is equivalent to values in R
  expect_equal(iso_get_raw_data(cf_example), read_feather(str_c(filepath, "_raw_data.cf.feather")))
  expect_equal(iso_get_file_info(cf_example) %>% collapse_list_columns(), read_feather(str_c(filepath, "_file_info.cf.feather")))
  expect_equal(iso_get_standards_info(cf_example), read_feather(str_c(filepath, "_method_info-standards.cf.feather")))
  expect_equal(iso_get_resistors_info (cf_example), read_feather(str_c(filepath, "_method_info-resistors.cf.feather")))
  expect_equal(iso_get_vendor_data_table(cf_example) %>% iso_strip_units(), 
               read_feather(str_c(filepath, "_vendor_data_table.cf.feather")))
  expect_true(all(file.remove(list.files(dirname(filepath), pattern = "\\.cf\\.feather$", full.names = TRUE))))
  
  # export with explicit units
  expect_message(iso_export_to_feather(cf_example, filepath, with_explicit_units = TRUE, quiet = FALSE), "exporting data .* into .cf.feather")
  expect_true(file.exists(str_c(filepath, "_vendor_data_table.cf.feather")))
  expect_equal(iso_get_vendor_data_table(cf_example, with_explicit_units = TRUE), 
               read_feather(str_c(filepath, "_vendor_data_table.cf.feather")))
  expect_true(all(file.remove(list.files(dirname(filepath), pattern = "\\.cf\\.feather$", full.names = TRUE))))
  
  # export real data files - scan
  expect_message(iso_export_to_feather(scan_example, filepath, quiet = FALSE), "exporting data .* into .scan.feather")
  expect_true(file.exists(str_c(filepath, "_raw_data.scan.feather")))
  expect_true(file.exists(str_c(filepath, "_file_info.scan.feather")))
  expect_true(file.exists(str_c(filepath, "_method_info-resistors.scan.feather")))
  # note for comparisons: rounding is NOT necessary because storage is equivalent to values in R
  expect_equal(iso_get_raw_data(scan_example), read_feather(str_c(filepath, "_raw_data.scan.feather")))
  expect_equal(iso_get_file_info(scan_example) %>% collapse_list_columns(), read_feather(str_c(filepath, "_file_info.scan.feather")))
  expect_equal(iso_get_resistors_info (scan_example), read_feather(str_c(filepath, "_method_info-resistors.scan.feather")))
  expect_true(all(file.remove(list.files(dirname(filepath), pattern = "\\.scan\\.feather$", full.names = TRUE))))
  
  
})
