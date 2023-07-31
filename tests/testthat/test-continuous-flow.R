context("Continuous flow")

# data structures ========

test_that("test that supported cf files are correct", {
  initialize_options()
  expect_is(exts <- get_supported_cf_files(), "data.frame")
  expect_equal(exts$extension, c(".cf", ".cf.rds", ".dxf", ".iarc"))
  expect_true(all(exts$func |> sapply(class) == "character"))
  expect_true(all(exts$func |> map_lgl(exists, mode = "function", where = asNamespace("isoreader"))))
})

test_that("test that parameter checks are performed", {
  
  # flow iarc
  expect_error(iso_read_flow_iarc (make_di_data_structure("NA")), 
               "data structure must be a \\'continuous_flow\\' iso_file")
  
  
})


# example files ========

test_that("test that continous flow files can be read", {
  
  # skip on CRAN to reduce check time to below 10 minutes
  skip_on_cran()
  
  # skip if flag is set
  if (identical(getOption("isoreader.skip_file_tests"), TRUE))
    skip("Currently not testing continuous flow example files.")
  
  # start tests for files
  iso_turn_reader_caching_off()

  # .cf file
  expect_true(file.exists(file <- iso_get_reader_example("continuous_flow_example.cf")))
  expect_is(cf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(cf)), 0)
  expect_equal(nrow(iso_get_file_info(cf)), 1)
  expect_equal(
    names(iso_get_file_info(cf)), 
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Comment", 
      "Preparation", "Pre Script", "Post Script", "Method", "H3 Factor", 
      "MS_integration_time.s")
  )
  expect_equal(nrow(iso_get_raw_data(cf)), 8605)
  expect_equal(names(iso_get_raw_data(cf)), c("file_id", "tp", "time.s", "v2.mV", "v3.mV"))
  expect_equal(nrow(iso_get_resistors(cf)), 2)
  expect_equal(names(iso_get_resistors(cf)), c("file_id", "cup", "R.Ohm", "mass"))
  expect_equal(nrow(iso_get_standards(cf)), 3)
  expect_equal(
    names(iso_get_standards(cf)), 
    c("file_id", "standard", "gas", "delta_name", "delta_value", 
      "reference", "element", "ratio_name", "ratio_value")
  )
  expect_equal(nrow(iso_get_vendor_data_table(cf)), 19)
  expect_equal(
    names(iso_get_vendor_data_table(cf)), 
    c("file_id", "Nr.", "Start", "Rt", "End", "Ampl 2", "Ampl 3", 
      "BGD 2", "BGD 3", "rIntensity 2", "rIntensity 3", "rIntensity All", 
      "Intensity 2", "Intensity 3", "Intensity All", "List First Peak", 
      "rR 3H2/2H2", "Is Ref.?", "R 3H2/2H2", "Ref. Name", "rd 3H2/2H2", 
      "d 3H2/2H2", "R 2H/1H", "d 2H/1H", "AT% 2H/1H", "Rps 3H2/2H2"
    )
  )
  expect_equal(
    iso_get_units(iso_get_vendor_data_table(cf)) |> as.character(),
    c(NA, NA, "s", "s", "s", "mV", "mV", "mV", "mV", "mVs", "mVs", 
      "mVs", "Vs", "Vs", "Vs", NA, NA, NA, NA, NA, "permil", "permil", 
      NA, "permil", "%", NA)
  )
  
  # .dxf file
  expect_true(file.exists(file <- iso_get_reader_example("continuous_flow_example.dxf")))
  expect_is(dxf <- iso_read_continuous_flow(file), "continuous_flow")
  expect_equal(nrow(problems(dxf)), 0)
  expect_equal(nrow(iso_get_file_info(dxf)), 1)
  expect_equal(
    names(iso_get_file_info(dxf)), 
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
     "file_size", "Row", "Peak Center", "Check Ref. Dilution", "H3 Stability", 
     "H3 Factor", "Type", "EA Method", "Identifier 1", "Identifier 2", 
     "Analysis", "Comment", "Preparation", "Method", "measurement_info", 
     "MS_integration_time.s")
  )
  expect_equal(nrow(iso_get_raw_data(dxf)), 2435)
  expect_equal(
    names(iso_get_raw_data(dxf)), 
    c("file_id", "tp", "time.s", "v28.mV", "v29.mV", "v30.mV", "v44.mV", "v45.mV", "v46.mV"))
  expect_equal(nrow(iso_get_resistors(dxf)), 6)
  expect_equal(names(iso_get_resistors(dxf)), c("file_id", "cup", "R.Ohm", "mass"))
  expect_equal(nrow(iso_get_standards(dxf)), 7)
  expect_equal(
    names(iso_get_standards(dxf)), 
    c("file_id", "standard", "gas", "delta_name", "delta_value", 
      "reference", "element", "ratio_name", "ratio_value")
  )
  expect_equal(nrow(iso_get_vendor_data_table(dxf)), 6)
  expect_equal(
    names(iso_get_vendor_data_table(dxf)), 
    c("file_id", "Nr.", "Start", "Rt", "End", "Ampl 28", "Ampl 29", 
      "Ampl 30", "Ampl 44", "Ampl 45", "Ampl 46", "BGD 28", "BGD 29", 
      "BGD 30", "BGD 44", "BGD 45", "BGD 46", "rIntensity 28", "rIntensity 29", 
      "rIntensity 30", "rIntensity All", "Intensity 28", "Intensity 29", 
      "Intensity 30", "Intensity All", "Sample Dilution", "List First Peak", 
      "rR 29N2/28N2", "Is Ref.?", "R 29N2/28N2", "Ref. Name", "rd 29N2/28N2", 
      "d 29N2/28N2", "R 15N/14N", "d 15N/14N", "AT% 15N/14N", "Rps 29N2/28N2", 
      "rIntensity 44", "rIntensity 45", "rIntensity 46", "Intensity 44", 
      "Intensity 45", "Intensity 46", "rR 45CO2/44CO2", "rR 46CO2/44CO2", 
      "R 45CO2/44CO2", "rd 45CO2/44CO2", "d 45CO2/44CO2", "R 46CO2/44CO2", 
      "rd 46CO2/44CO2", "d 46CO2/44CO2", "R 13C/12C", "d 13C/12C", 
      "AT% 13C/12C", "R 18O/16O", "d 18O/16O", "AT% 18O/16O", "R 17O/16O", 
      "d 17O/16O", "Rps 45CO2/44CO2", "Rps 46CO2/44CO2")
  )
  expect_equal(
    iso_get_units(iso_get_vendor_data_table(dxf)) |> as.character(),
    c(NA, NA, "s", "s", "s", "mV", "mV", "mV", "mV", "mV", "mV", 
      "mV", "mV", "mV", "mV", "mV", "mV", "mVs", "mVs", "mVs", "mVs", 
      "Vs", "Vs", "Vs", "Vs", "%", NA, NA, NA, NA, NA, "permil", "permil", 
      NA, "permil", "%", NA, "mVs", "mVs", "mVs", "Vs", "Vs", "Vs", 
      NA, NA, NA, "permil", "permil", NA, "permil", "permil", NA, "permil", 
      "%", NA, "permil", "%", NA, NA, NA, NA)
  )
  
  # skip if optional dependencies are not installed
  skip_if_not_installed("xml2")
  skip_if_not_installed("rhdf5")
  expect_true(file.exists(file <- iso_get_reader_example("continuous_flow_example.iarc")))
  expect_is(iarc <- iso_read_continuous_flow(file), "iso_file_list")
  expect_equal(nrow(problems(iarc)), 0)
  
})

# additional test files ======

test_that("test that additional continous flow files can be read", {
  
  # skip on CRAN (test files too big and check time too long)
  skip_on_cran()
  
  # skip if flag is set
  if (identical(getOption("isoreader.skip_file_tests"), TRUE))
    skip("Currently not testing additional continuous flow files.")
  
  # start tests for files
  iso_turn_reader_caching_off()
  
  
  test_folder <- file.path("test_data") # test_folder <- file.path("tests", "testthat", "test_data") # direct
  iso_turn_reader_caching_off()

  # testing wrapper
  check_continuous_flow_test_file <- function(file, file_info_cols = NULL, data_table_nrow = NULL, data_table_col_units = NULL, n_problems = 0) {
    file_path <- get_isoreader_test_file(file, local_folder = test_folder)
    expect_true(file.exists(file_path))
    if (n_problems > 0)
      expect_warning(file <- iso_read_continuous_flow(file_path, read_cache = FALSE))
    else
      expect_is(file <- iso_read_continuous_flow(file_path, read_cache = FALSE), "continuous_flow")
    expect_equal(nrow(problems(file)), n_problems)
    expect_equal(nrow(file$file_info), 1)
    if (!is.null(file_info_cols)) 
      expect_equal(names(file$file_info), file_info_cols)
    else dput(names(file$file_info))
    if (!is.null(data_table_nrow)) 
      expect_equal(nrow(file$vendor_data_table), data_table_nrow)
    else cat(nrow(file$vendor_data_table), ",\n")
    if (!is.null(data_table_col_units)) 
      expect_equal(iso_get_units(file$vendor_data_table), data_table_col_units)
    else dput(iso_get_units(file$vendor_data_table))
    return(invisible(file))
  }
  
  dxf1 <- check_continuous_flow_test_file(
    "dxf_example_H_01.dxf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Row", "Peak Center", "Check Ref. Dilution", "H3 Stability", 
      "H3 Factor", "Conditioning", "Seed Oxidation", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Method", 
      "measurement_info", "MS_integration_time.s"),
    25,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV",
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs",
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs",
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `Sample Dilution` = "%",
      `List First Peak` = NA, `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA,
      `Ref. Name` = NA, `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil",
      `R 2H/1H` = NA, `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA
    )
  )
  
  dxf2 <- check_continuous_flow_test_file(
    "dxf_example_H_02.dxf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Row", "Peak Center", "Check Ref. Dilution", "H3 Stability", 
      "H3 Factor", "Conditioning", "Seed Oxidation", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Preparation", "Method", 
      "measurement_info", "MS_integration_time.s"),
    53,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV",
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs",
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs",
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `Sample Dilution` = "%",
      `List First Peak` = NA, `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA,
      `Ref. Name` = NA, `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil",
      `R 2H/1H` = NA, `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA,
      `Master Peak` = NA, `DeltaDelta 3H2/2H2` = "permil"
    ),
    n_problems = 1L
  )
  expect_equal(problems(dxf2)$type, "warning")
  expect_true(stringr::str_detect(problems(dxf2)$details, "has multiple precisions"))
  
  check_continuous_flow_test_file(
    "dxf_example_HO_01.dxf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Row", "Peak Center", "Check Ref. Dilution", "H3 Stability", 
      "H3 Factor", "Amount", "Type", "Identifier 1", "Identifier 2", 
      "Analysis", "Method", "measurement_info", "MS_integration_time.s"
    ),
    6,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
      `Ampl 3` = "mV", `Ampl 28` = "mV", `Ampl 29` = "mV", `Ampl 30` = "mV", 
      `BGD 2` = "mV", `BGD 3` = "mV", `BGD 28` = "mV", `BGD 29` = "mV", 
      `BGD 30` = "mV", `rIntensity 2` = "mVs", `rIntensity 3` = "mVs", 
      `rIntensity All` = "mVs", `Intensity 2` = "Vs", `Intensity 3` = "Vs", 
      `Intensity All` = "Vs", `Sample Dilution` = "%", `List First Peak` = NA, 
      `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, `Ref. Name` = NA, 
      `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", `R 2H/1H` = NA, 
      `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA, 
      `rIntensity 28` = "mVs", `rIntensity 29` = "mVs", `rIntensity 30` = "mVs", 
      `Intensity 28` = "Vs", `Intensity 29` = "Vs", `Intensity 30` = "Vs", 
      `rR 29CO/28CO` = NA, `rR 30CO/28CO` = NA, `R 29CO/28CO` = NA, 
      `rd 29CO/28CO` = "permil", `d 29CO/28CO` = "permil", `R 30CO/28CO` = NA, 
      `rd 30CO/28CO` = "permil", `d 30CO/28CO` = "permil", `R 13C/12C` = NA, 
      `d 13C/12C` = "permil", `AT% 13C/12C` = "%", `R 18O/16O` = NA, 
      `d 18O/16O` = "permil", `AT% 18O/16O` = "%", `R 17O/16O` = NA, 
      `d 17O/16O` = NA, `Rps 29CO/28CO` = NA, `Rps 30CO/28CO` = NA)
  )
  
  check_continuous_flow_test_file(
    "dxf_example_HO_02.dxf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Row", "Peak Center", "Check Ref. Dilution", "H3 Stability", 
      "H3 Factor", "Type", "EA Method", "AS Sample", "AS Method", "Sequence Flag", 
      "Identifier 1", "Identifier 2", "Analysis", "Method", "measurement_info", 
      "MS_integration_time.s"),
    5,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
       `Ampl 3` = "mV", `Ampl 28` = "mV", `Ampl 29` = "mV", `Ampl 30` = "mV", 
       `BGD 2` = "mV", `BGD 3` = "mV", `BGD 28` = "mV", `BGD 29` = "mV", 
       `BGD 30` = "mV", `rIntensity 2` = "mVs", `rIntensity 3` = "mVs", 
       `rIntensity All` = "mVs", `Intensity 2` = "Vs", `Intensity 3` = "Vs", 
       `Intensity All` = "Vs", `Sample Dilution` = "%", `List First Peak` = NA, 
       `rR 3H2/2H2` = NA, `Rps 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, 
       `Ref. Name` = NA, `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", 
       `R 2H/1H` = NA, `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `rIntensity 28` = "mVs", 
       `rIntensity 29` = "mVs", `rIntensity 30` = "mVs", `Intensity 28` = "Vs", 
       `Intensity 29` = "Vs", `Intensity 30` = "Vs", `rR 29CO/28CO` = NA, 
       `rR 30CO/28CO` = NA, `R 29CO/28CO` = NA, `rd 29CO/28CO` = "permil", 
       `d 29CO/28CO` = "permil", `R 30CO/28CO` = NA, `rd 30CO/28CO` = "permil", 
       `d 30CO/28CO` = "permil", `R 13C/12C` = NA, `d 13C/12C` = "permil", 
       `AT% 13C/12C` = "%", `R 18O/16O` = NA, `d 18O/16O` = "permil", 
       `AT% 18O/16O` = "%", `R 17O/16O` = NA, `d 17O/16O` = NA, `Rps 29CO/28CO` = NA, 
       `Rps 30CO/28CO` = NA)
  )
  
  check_continuous_flow_test_file(
    "dxf_example_C_01.dxf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Row", "Peak Center", "Check Ref. Dilution", "H3 Stability", 
      "H3 Factor", "Identifier 1", "Analysis", "Method", "measurement_info", 
      "MS_integration_time.s"),
    8,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 44` = "mV", 
      `Ampl 45` = "mV", `Ampl 46` = "mV", `BGD 44` = "mV", `BGD 45` = "mV", 
      `BGD 46` = "mV", `rIntensity 44` = "mVs", `rIntensity 45` = "mVs", 
      `rIntensity 46` = "mVs", `rIntensity All` = "mVs", `Intensity 44` = "Vs", 
      `Intensity 45` = "Vs", `Intensity 46` = "Vs", `Intensity All` = "Vs", 
      `Sample Dilution` = "%", `List First Peak` = NA, `rR 45N2O/44N2O` = NA, 
      `rR 46N2O/44N2O` = NA, `Is Ref.?` = NA, `R 45N2O/44N2O` = NA, 
      `Ref. Name` = NA, `rd 45N2O/44N2O` = "permil", `d 45N2O/44N2O` = "permil", 
      `R 46N2O/44N2O` = NA, `rd 46N2O/44N2O` = "permil", `d 46N2O/44N2O` = "permil", 
      `R 18O/16O` = NA, `d 18O/16O` = "permil", `AT% 15N/14N` = "%", 
      `R 15N/14N` = NA, `d 15N/14N` = "permil", `AT% 18O/16O` = "%", 
      `R 17O/16O` = NA, `d 17O/16O` = NA, `Rps 46N2O/44N2O` = NA, `Rps 45N2O/44N2O` = NA
    )
  )
  
  check_continuous_flow_test_file(
    "dxf_example_CN_01.dxf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Row", "Peak Center", "Amount", "Type", "Identifier 1", 
      "Identifier 2", "Analysis", "Comment", "Method", "measurement_info", 
      "MS_integration_time.s"),
    5,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 28` = "mV", 
      `Ampl 29` = "mV", `Ampl 30` = "mV", `Ampl 44` = "mV", `Ampl 45` = "mV", 
      `Ampl 46` = "mV", `BGD 28` = "mV", `BGD 29` = "mV", `BGD 30` = "mV", 
      `BGD 44` = "mV", `BGD 45` = "mV", `BGD 46` = "mV", `rIntensity 28` = "mVs", 
      `rIntensity 29` = "mVs", `rIntensity 30` = "mVs", `rIntensity All` = "mVs", 
      `Intensity 28` = "Vs", `Intensity 29` = "Vs", `Intensity 30` = "Vs", 
      `Intensity All` = "Vs", `List First Peak` = NA, `rR 29N2/28N2` = NA, 
      `Is Ref.?` = NA, `Conflo K Fac` = NA, `Amt%` = "%", `R 29N2/28N2` = NA, 
      `Ref. Name` = NA, `rd 29N2/28N2` = "permil", `d 29N2/28N2` = "permil", 
      `R 15N/14N` = NA, `d 15N/14N` = "permil", `AT% 15N/14N` = "%", 
      `Rps 29N2/28N2` = NA, `Rps 30N2/28N2` = NA, `rIntensity 44` = "mVs", 
      `rIntensity 45` = "mVs", `rIntensity 46` = "mVs", `Intensity 44` = "Vs", 
      `Intensity 45` = "Vs", `Intensity 46` = "Vs", `rR 45CO2/44CO2` = NA, 
      `rR 46CO2/44CO2` = NA, `R 45CO2/44CO2` = NA, `rd 45CO2/44CO2` = "permil", 
      `d 45CO2/44CO2` = "permil", `R 46CO2/44CO2` = NA, `rd 46CO2/44CO2` = "permil", 
      `d 46CO2/44CO2` = "permil", `R 13C/12C` = NA, `d 13C/12C` = "permil", 
      `AT% 13C/12C` = "%", `R 18O/16O` = NA, `d 18O/16O` = "permil", 
      `AT% 18O/16O` = "%", `R 17O/16O` = NA, `d 17O/16O` = NA, `Rps 45CO2/44CO2` = NA, 
      `Rps 46CO2/44CO2` = NA)
  )
  
  check_continuous_flow_test_file(
    "dxf_example_CNS_01.dxf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Row", "Peak Center", "Check Ref. Dilution", "H3 Stability", 
      "H3 Factor", "Amount", "Type", "EA Method", "Identifier 1", "Identifier 2", 
      "Analysis", "Method", "measurement_info", "MS_integration_time.s"
    ),
    5,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 28` = "mV", 
      `Ampl 29` = "mV", `Ampl 44` = "mV", `Ampl 45` = "mV", `Ampl 46` = "mV", 
      `Ampl 64` = "mV", `Ampl 66` = "mV", `BGD 28` = "mV", `BGD 29` = "mV", 
      `BGD 44` = "mV", `BGD 45` = "mV", `BGD 46` = "mV", `BGD 64` = "mV", 
      `BGD 66` = "mV", `rIntensity 28` = "mVs", `rIntensity 29` = "mVs", 
      `rIntensity All` = "mVs", `Intensity 28` = "Vs", `Intensity 29` = "Vs", 
      `Intensity All` = "Vs", `Sample Dilution` = "%", `List First Peak` = NA, 
      `rR 29N2/28N2` = NA, `Rps 29N2/28N2` = NA, `Is Ref.?` = NA, `R 29N2/28N2` = NA, 
      `Ref. Name` = NA, `rd 29N2/28N2` = "permil", `d 29N2/28N2` = "permil", 
      `R 15N/14N` = NA, `d 15N/14N` = "permil", `AT% 15N/14N` = "%", 
      `d 15N/14N Blk corr` = "permil", `AT% 15N/14N Blk corr` = "%", 
      `rIntensity 44` = "mVs", `rIntensity 45` = "mVs", `rIntensity 46` = "mVs", 
      `Intensity 44` = "Vs", `Intensity 45` = "Vs", `Intensity 46` = "Vs", 
      `rR 45CO2/44CO2` = NA, `rR 46CO2/44CO2` = NA, `Rps 45CO2/44CO2` = NA, 
      `Rps 46CO2/44CO2` = NA, `R 45CO2/44CO2` = NA, `rd 45CO2/44CO2` = "permil", 
      `d 45CO2/44CO2` = "permil", `R 46CO2/44CO2` = NA, `rd 46CO2/44CO2` = "permil", 
      `d 46CO2/44CO2` = "permil", `R 13C/12C` = NA, `d 13C/12C` = "permil", 
      `AT% 13C/12C` = "%", `R 18O/16O` = NA, `d 18O/16O` = "permil", 
      `AT% 18O/16O` = "%", `R 17O/16O` = NA, `d 17O/16O` = NA, `rIntensity 64` = "mVs", 
      `rIntensity 66` = "mVs", `Intensity 64` = "Vs", `Intensity 66` = "Vs", 
      `rR 66SO2/64SO2` = NA, `Rps 66SO2/64SO2` = NA, `R 66SO2/64SO2` = NA, 
      `rd 66SO2/64SO2` = "permil", `d 66SO2/64SO2` = "permil", `AT% 34S/32S` = "%", 
      `R 34S/32S` = NA, `d 34S/32S` = "permil")
  )
  
  dxf2 <- check_continuous_flow_test_file(
    "dxf_example_N2_01.dxf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Row", "Peak Center", "AS Sample", "AS Method", 
      "Identifier 1", "Identifier 2", "Analysis", "Comment", "Method", 
      "measurement_info", "MS_integration_time.s"),
    8,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 28` = "mV", 
      `Ampl 29` = "mV", `Ampl 30` = "mV", `BGD 28` = "mV", `BGD 29` = "mV", 
      `BGD 30` = "mV", `rIntensity 28` = "mVs", `rIntensity 29` = "mVs", 
      `rIntensity 30` = "mVs", `rIntensity All` = "mVs", `Intensity 28` = "Vs", 
      `Intensity 29` = "Vs", `Intensity 30` = "Vs", `Intensity All` = "Vs", 
      `List First Peak` = NA, `rR 29N2/28N2` = NA, `R 30N2/28N2` = NA, 
      `R 29N2/28N2` = NA, `rR 30N2/28N2` = NA, `Is Ref.?` = NA, `Ref. Name` = NA, 
      `rd 29N2/28N2` = "permil", `d 29N2/28N2` = "permil", `rd 30N2/28N2` = "permil", 
      `d 30N2/28N2` = "permil")
  )
  
  check_continuous_flow_test_file(
    "cf_example_CN_01.cf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "Amount", "Type", "Port", 
      "Identifier 1", "Identifier 2", "Analysis", "Comment", "Preparation", 
      "Pre Script", "Post Script", "Method", "MS_integration_time.s"
    ),
    6,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 28` = "mV", 
      `Ampl 29` = "mV", `Ampl 30` = "mV", `Ampl 44` = "mV", `Ampl 45` = "mV", 
      `Ampl 46` = "mV", `BGD 28` = "mV", `BGD 29` = "mV", `BGD 30` = "mV", 
      `BGD 44` = "mV", `BGD 45` = "mV", `BGD 46` = "mV", `rIntensity 28` = "mVs", 
      `rIntensity 29` = "mVs", `rIntensity 30` = "mVs", `rIntensity All` = "mVs", 
      `Intensity 28` = "Vs", `Intensity 29` = "Vs", `Intensity 30` = "Vs", 
      `Intensity All` = "Vs", `List First Peak` = NA, `rR 29N2/28N2` = NA, 
      `Is Ref.?` = NA, `Conflo K Fac` = NA, `Amt%` = "%", `R 29N2/28N2` = NA, 
      `Ref. Name` = NA, `rd 29N2/28N2` = "permil", `d 29N2/28N2` = "permil", 
      `R 15N/14N` = NA, `d 15N/14N` = "permil", `AT% 15N/14N` = "%", 
      `rIntensity 44` = "mVs", `rIntensity 45` = "mVs", `rIntensity 46` = "mVs", 
      `Intensity 44` = "Vs", `Intensity 45` = "Vs", `Intensity 46` = "Vs", 
      `rR 45CO2/44CO2` = NA, `rR 46CO2/44CO2` = NA, `R 45CO2/44CO2` = NA, 
      `rd 45CO2/44CO2` = "permil", `d 45CO2/44CO2` = "permil", `R 46CO2/44CO2` = NA, 
      `rd 46CO2/44CO2` = "permil", `d 46CO2/44CO2` = "permil", `R 13C/12C` = NA, 
      `d 13C/12C` = "permil", `AT% 13C/12C` = "%", `R 18O/16O` = NA, 
      `d 18O/16O` = "permil", `AT% 18O/16O` = "%")
  )
  
  check_continuous_flow_test_file(
    "cf_example_H_01.cf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Comment", 
      "Preparation", "Pre Script", "Post Script", "Method", "H3 Factor", 
      "MS_integration_time.s"),
    19,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs", 
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs", 
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `List First Peak` = NA, 
      `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, `Ref. Name` = NA, 
      `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", `R 2H/1H` = NA, 
      `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA)
  )
  
  check_continuous_flow_test_file(
    "cf_example_H_02.cf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Comment", 
      "Preparation", "Pre Script", "Post Script", "Method", "H3 Factor", 
      "MS_integration_time.s"),
    19,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs", 
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs", 
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `List First Peak` = NA, 
      `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, `Ref. Name` = NA, 
      `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", `R 2H/1H` = NA, 
      `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA)
  )
  
  check_continuous_flow_test_file(
    "cf_example_H_03.cf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Comment", 
      "Preparation", "Pre Script", "Post Script", "Method", "H3 Factor", 
      "MS_integration_time.s"),
    19,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs", 
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs", 
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `List First Peak` = NA, 
      `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, `Ref. Name` = NA, 
      `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", `R 2H/1H` = NA, 
      `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA)
  )
  
  cf1 <- check_continuous_flow_test_file(
    "cf_example_H_04.cf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Comment", 
      "Preparation", "Pre Script", "Post Script", "Method", "H3 Factor", 
      "MS_integration_time.s"),
    19,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs", 
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs", 
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `List First Peak` = NA, 
      `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, `Ref. Name` = NA, 
      `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", `R 2H/1H` = NA, 
      `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA, 
      `Master Peak` = NA, `DeltaDelta 3H2/2H2` = "permil")
  )
  check_continuous_flow_test_file(
    "cf_example_H_05.cf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Comment", 
      "Preparation", "Pre Script", "Post Script", "Method", "H3 Factor", 
      "MS_integration_time.s"),
    13,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs", 
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs", 
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `List First Peak` = NA, 
      `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, `Ref. Name` = NA, 
      `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", `R 2H/1H` = NA, 
      `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA)
  )
  check_continuous_flow_test_file(
    "cf_example_H_06.cf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Comment", 
      "Preparation", "Pre Script", "Post Script", "Method", "H3 Factor", 
      "MS_integration_time.s"),
    19,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs", 
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs", 
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `List First Peak` = NA, 
      `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, `Ref. Name` = NA, 
      `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", `R 2H/1H` = NA, 
      `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA)
  )
  check_continuous_flow_test_file(
    "cf_example_H_07.cf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime", 
      "file_size", "Line", "Peak Center", "GC Method", "AS Sample", 
      "AS Method", "Identifier 1", "Identifier 2", "Analysis", "Comment", 
      "Preparation", "Pre Script", "Post Script", "Method", "H3 Factor", 
      "MS_integration_time.s"),
    10,
    c(Nr. = NA, Start = "s", Rt = "s", End = "s", `Ampl 2` = "mV", 
      `Ampl 3` = "mV", `BGD 2` = "mV", `BGD 3` = "mV", `rIntensity 2` = "mVs", 
      `rIntensity 3` = "mVs", `rIntensity All` = "mVs", `Intensity 2` = "Vs", 
      `Intensity 3` = "Vs", `Intensity All` = "Vs", `List First Peak` = NA, 
      `rR 3H2/2H2` = NA, `Is Ref.?` = NA, `R 3H2/2H2` = NA, `Ref. Name` = NA, 
      `rd 3H2/2H2` = "permil", `d 3H2/2H2` = "permil", `R 2H/1H` = NA, 
      `d 2H/1H` = "permil", `AT% 2H/1H` = "%", `Rps 3H2/2H2` = NA)
  )
  
  ## test re-reading =======
  # NOTE: ideally this should also include an iarc file
  iso_files <- c(dxf1, dxf2, cf1)
  expect_true(iso_is_continuous_flow(reread_dxf <- reread_iso_files(iso_files)))
  expect_equal(nrow(problems(reread_dxf)), 0)
  
  ## test parallel processing ======
  # multisession
  file_paths <-
    file.path(test_folder,
              c("dxf_example_H_01.dxf", "dxf_example_HO_01.dxf", "dxf_example_HO_02.dxf", "dxf_example_CNS_01.dxf", "dxf_example_N2_01.dxf"))
  
  expect_message(files <- iso_read_continuous_flow(file_paths, parallel = TRUE, parallel_plan = future::multisession, parallel_cores = future::availableCores()),
                 sprintf("preparing to read 5 data files.*setting up %.0f parallel processes", min(5, future::availableCores())))
  expect_equal(nrow(problems(files)), 0)
  expect_warning(iso_read_continuous_flow(file_paths, parallel = TRUE, parallel_plan = future::multisession, parallel_cores = future::availableCores() + 1),
                 sprintf("%.0f cores.*requested.*only %.0f.*available", future::availableCores() + 1, future::availableCores()))
  
  # multicore
  expect_message(files <- iso_read_continuous_flow(file_paths, parallel = TRUE, parallel_plan = future::multicore),
                 "preparing to read 5 data files.*setting up.*parallel processes")
  expect_equal(nrow(problems(files)), 0)
  
})

