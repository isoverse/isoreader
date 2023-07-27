context("Dual Inlet Files")

# data structures =========

test_that("test that supported di files are correct", {
  initialize_options()
  expect_is(exts <- get_supported_di_files(), "data.frame")
  expect_equal(exts$extension, c(".caf", ".di.rds", ".did", ".txt"))
  expect_true(all(exts$func |> sapply(class) == "character"))
  expect_true(all(exts$func |> map_lgl(exists, mode = "function", where = asNamespace("isoreader"))))
})

test_that("test that parameter checks are performed", {

  expect_error(iso_read_did(make_cf_data_structure("NA")),
               "data structure must be a \\'dual_inlet\\' iso_file")

})

# nu files ======

test_that("test that nu file processor works properly", {

  expect_error(group_lines(list()))
  expect_error(group_lines(""))

})

# actual files ========

test_that("test that dual inlet files can be read", {

  # skip on CRAN to reduce checktime to below 10 minutes
  skip_on_cran()

  # test specific files
  iso_turn_reader_caching_off()

  # .did
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_example.did")))
  expect_is(did <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  expect_equal(nrow(did$file_info), 1)
  expect_equal(
    names(did$file_info),
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Line", "Peak Center", "Pressadjust", "Background",
      "Identifier 1", "Identifier 2", "Analysis", "Method", "measurement_info",
      "MS_integration_time.s")
  )
  expect_equal(nrow(did$vendor_data_table), 7)
  expect_equal(
    iso_get_units(did$vendor_data_table),
    c(cycle = NA_character_, `d 45CO2/44CO2` = NA_character_, `d 46CO2/44CO2` = NA_character_,
      `d 13C/12C` = NA_character_, `d 18O/16O` = NA_character_, `d 17O/16O` = NA_character_,
      `AT% 13C/12C` = NA_character_, `AT% 18O/16O` = NA_character_)
  )

  # .caf
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_example.caf")))
  expect_is(caf <- iso_read_dual_inlet(file), "dual_inlet")
  expect_equal(nrow(problems(caf)), 0)
  expect_equal(nrow(caf$file_info), 1)
  expect_equal(
    names(caf$file_info),
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Line", "Peak Center", "Pressadjust", "Background",
      "Reference Refill", "Weight [mg]", "Sample", "Identifier 1",
      "Identifier 2", "Analysis", "Comment", "Preparation", "Pre Script",
      "Post Script", "Method", "MS_integration_time.s")
  )
  expect_equal(nrow(caf$vendor_data_table), 8)
  expect_equal(
    iso_get_units(caf$vendor_data_table),
    c(cycle = NA, `d 45CO2/44CO2` = "permil", `d 46CO2/44CO2` = "permil",
      `d 13C/12C` = "permil", `d 18O/16O` = "permil", `d 17O/16O` = " ",
      `d 47CO2/44CO2` = "permil", `d 48CO2/44CO2` = "permil", `d 49CO2/44CO2` = "permil"
    )
  )

  # .txt (nu)
  expect_true(file.exists(file <- iso_get_reader_example("dual_inlet_nu_example.txt")))
  expect_is(did <- iso_read_dual_inlet(file, nu_masses = 49:44, read_cache = FALSE), "dual_inlet")
  expect_equal(nrow(problems(did)), 0)
  expect_warning(did <- iso_read_dual_inlet(file, read_cache = FALSE), "encountered 1 problem\\.")
  expect_true(stringr::str_detect(iso_get_problems(did)$details, fixed("found 6 channels but 0 masses were specified")))
  expect_equal(nrow(problems(did)), 1)

})

test_that("test that additional dual inlet files can be read", {

  # additional test files (skip on CRAN because test files not includes due to tarball size limits) =====
  skip_on_cran()
  iso_turn_reader_caching_off()

  # testing wrapper
  check_dual_inlet_test_file <- function(file, file_info_cols = NULL, data_table_nrow = NULL, data_table_col_units = NULL, ...) {
    file_path <- get_isoreader_test_file(file, local_folder = test_data)
    expect_true(file.exists(file_path))
    expect_is(file <- iso_read_dual_inlet(file_path, ...), "dual_inlet")
    expect_equal(nrow(problems(file)), 0)
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

  # .did files
  test_data <- file.path("test_data") # test_data <- file.path("tests", "testthat", "test_data") # direct
  check_dual_inlet_test_file(
    "did_example_air.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Background", "Pressadjust",
      "Identifier 1", "Identifier 2", "Analysis", "Comment", "Method",
      "measurement_info", "MS_integration_time.s"),
    15,
    c(cycle = NA_character_, `d 32O2/28N2` = NA_character_, `d 32O2/29N2` = NA_character_,
      `d 40Ar/28N2` = NA_character_, `d 32O2/40Ar` = NA_character_,
      `d 44CO2/28N2` = NA_character_, `d 44CO2/40Ar` = NA_character_
    )
  )
  check_dual_inlet_test_file(
    "did_example_CO2_clumped_01.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Background", "Pressadjust",
      "Reference Refill", "Identifier 1", "Identifier 2", "Analysis",
      "Comment", "Method", "measurement_info", "MS_integration_time.s"
    ),
    7,
    c(cycle = NA_character_, `d 45CO2/44CO2` = NA_character_, `d 46CO2/44CO2` = NA_character_,
      `d 13C/12C` = NA_character_, `d 18O/16O` = NA_character_, `d 17O/16O` = NA_character_,
      `AT% 13C/12C` = NA_character_, `AT% 18O/16O` = NA_character_)
  )
  check_dual_inlet_test_file(
    "did_example_many_cycles.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Background", "Pressadjust",
      "Reference Refill", "Identifier 1", "Identifier 2", "Analysis",
      "Comment", "Preparation", "Method", "measurement_info", "MS_integration_time.s"
    ),
    50,
    c(cycle = NA_character_, `d 45CO2/44CO2` = NA_character_, `d 46CO2/44CO2` = NA_character_,
      `d 13C/12C` = NA_character_, `d 18O/16O` = NA_character_, `d 17O/16O` = NA_character_,
      `AT% 13C/12C` = NA_character_, `AT% 18O/16O` = NA_character_)
  )
  check_dual_inlet_test_file(
    "did_example_unicode.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Background", "Pressadjust",
      "Reference Refill", "Line", "Sample", "Weight [mg]", "Identifier 1",
      "Analysis", "Comment", "Preparation", "Method", "measurement_info",
      "MS_integration_time.s"),
    60,
    c(cycle = NA_character_, `d 45CO2/44CO2` = NA_character_, `d 46CO2/44CO2` = NA_character_,
      `d 47CO2/44CO2` = NA_character_, `d 48CO2/44CO2` = NA_character_,
      `d 49CO2/44CO2` = NA_character_, `d 13C/12C` = NA_character_,
      `d 18O/16O` = NA_character_, `d 17O/16O` = NA_character_, `AT% 13C/12C` = NA_character_,
      `AT% 18O/16O` = NA_character_)
  )
  check_dual_inlet_test_file(
    "did_ultra_example.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Switch Cup Positions", "Baseline",
      "Background", "Pressadjust", "Identifier 1", "Analysis", "Method",
      "measurement_info", "MS_integration_time.s"),
    10,
    c(cycle = NA_character_, `d 17CH4/16CH4` = NA_character_)
  )
  check_dual_inlet_test_file(
    "caf_example_CO2_01.caf",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Line", "Peak Center", "Pressadjust", "Background",
      "Reference Refill", "Weight [mg]", "Sample", "Identifier 1",
      "Identifier 2", "Analysis", "Comment", "Preparation", "Pre Script",
      "Post Script", "Method", "MS_integration_time.s"),
    8,
    c(cycle = NA, `d 45CO2/44CO2` = "permil", `d 46CO2/44CO2` = "permil",
      `d 13C/12C` = "permil", `d 18O/16O` = "permil", `d 17O/16O` = " ",
      `d 47CO2/44CO2` = "permil", `d 48CO2/44CO2` = "permil", `d 49CO2/44CO2` = "permil"
    )
  )

  # minimal files
  test_data <- file.path("minimal_data") # test_data <- file.path("tests", "testthat", "minimal_data") # direct
  did1 <- check_dual_inlet_test_file(
    "minimal_01.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Background", "Pressadjust",
      "Reference Refill", "Identifier 1", "Identifier 2", "Analysis",
      "Comment", "Method", "measurement_info", "MS_integration_time.s"
    ),
    7,
    c(cycle = NA_character_, `d 45CO2/44CO2` = NA_character_, `d 46CO2/44CO2` = NA_character_,
      `d 13C/12C` = NA_character_, `d 18O/16O` = NA_character_, `d 17O/16O` = NA_character_,
      `AT% 13C/12C` = NA_character_, `AT% 18O/16O` = NA_character_)
  )
  did2 <- check_dual_inlet_test_file(
    "minimal_02.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Background", "Pressadjust",
      "Reference Refill", "Identifier 1", "Identifier 2", "Analysis",
      "Comment", "Method", "measurement_info", "MS_integration_time.s"
    ),
    7,
    c(cycle = NA_character_, `d 45CO2/44CO2` = NA_character_, `d 46CO2/44CO2` = NA_character_,
      `d 13C/12C` = NA_character_, `d 18O/16O` = NA_character_, `d 17O/16O` = NA_character_,
      `AT% 13C/12C` = NA_character_, `AT% 18O/16O` = NA_character_)
  )
  did3 <- check_dual_inlet_test_file(
    "minimal_03.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Background", "Pressadjust",
      "Reference Refill", "Identifier 1", "Identifier 2", "Analysis",
      "Comment", "Method", "measurement_info", "MS_integration_time.s"
    ),
    7,
    c(cycle = NA_character_, `d 45CO2/44CO2` = NA_character_, `d 46CO2/44CO2` = NA_character_,
      `d 13C/12C` = NA_character_, `d 18O/16O` = NA_character_, `d 17O/16O` = NA_character_,
      `AT% 13C/12C` = NA_character_, `AT% 18O/16O` = NA_character_)
  )
  did4 <- check_dual_inlet_test_file(
    "minimal_04.did",
    c("file_id", "file_root", "file_path", "file_subpath", "file_datetime",
      "file_size", "Row", "Peak Center", "Background", "Pressadjust",
      "Reference Refill", "Identifier 1", "Identifier 2", "Analysis",
      "Comment", "Method", "measurement_info", "MS_integration_time.s"
    ),
    7,
    c(cycle = NA_character_, `d 45CO2/44CO2` = NA_character_, `d 46CO2/44CO2` = NA_character_,
      `d 13C/12C` = NA_character_, `d 18O/16O` = NA_character_, `d 17O/16O` = NA_character_,
      `AT% 13C/12C` = NA_character_, `AT% 18O/16O` = NA_character_)
  )
  expect_true(iso_is_file_list(dids <- c(did1, did2, did3, did4)))
  expect_true(dids |> iso_get_file_info(select = c(Comment, starts_with("MS"))) |>
                mutate(MSIT_correct = Comment == paste(MS_integration_time.s, "sec")) |>
                dplyr::pull(MSIT_correct) |>
                all())

})
