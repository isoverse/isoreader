context("Ratio calculations")

test_that("test that ratios can be calculated", {
  
  # parameter tests
  expect_error(iso_calculate_ratios(42), "can only calculate ratios for iso files")
  isofile <- isoreader:::make_isofile_data_structure()
  expect_error(iso_calculate_ratios(isofile), "no ratios provided for ratio calculations")
  expect_error(iso_calculate_ratios(isofile, ratios = c("42")), "invalid ratio")
  expect_warning(iso_calculate_ratios(isofile, ratios = c("44/42")), "read without extracting the raw data")
  
  # test data
  isofile$read_options$raw_data <- TRUE
  isofile$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10))
  expect_message(iso_calculate_ratios(isofile, ratios = c("46/44"), quiet = FALSE), "calculating ratio")
  expect_silent(iso_calculate_ratios(isofile, ratios = c("46/44"), quiet = TRUE))
  expect_true(iso_is_file(isofile_w_ratio <- iso_calculate_ratios(isofile, ratios = c("46/44"))))
  expect_equal(isofile_w_ratio$raw_data$`r46/44`, with(isofile$raw_data, v46.mV/v44.mV))
  
  # multiple files
  isofiles <- c(modifyList(isofile, list(file_info = list(file_id = "a"))),
                modifyList(isofile, list(file_info = list(file_id = "b"))))
  isofiles$b$raw_data$v45.mV <- (1:10)*runif(10)
  expect_true(iso_is_file_list(isofiles_w_ratios <- iso_calculate_ratios(isofiles, ratios = c("46/44", "45/44"))))
  expect_equal(isofiles_w_ratios$a$raw_data$`r46/44`, with(isofiles$a$raw_data, v46.mV/v44.mV))
  expect_false("r45/44" %in% names(isofiles_w_ratios$a$raw_data)) # not in a
  expect_equal(isofiles_w_ratios$b$raw_data$`r46/44`, with(isofiles$b$raw_data, v46.mV/v44.mV))
  expect_equal(isofiles_w_ratios$b$raw_data$`r45/44`, with(isofiles$b$raw_data, v45.mV/v44.mV))
  
})