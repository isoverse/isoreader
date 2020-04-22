context("Utility functions")

# package version comparisons =====

test_that("package version comparisons works correctly", {
  
  # versions < 1.0
  expect_true(same_as_isoreader_version(
    version = package_version(list(major = "0", minor = "9.0")),
    isoreader_version = package_version(list(major = "0", minor = "9.1"))
  ))
  expect_false(same_as_isoreader_version(
    version = package_version(list(major = "0", minor = "8.0")),
    isoreader_version = package_version(list(major = "0", minor = "9.0"))
  ))
  
  # versions > 1.0
  expect_true(same_as_isoreader_version(
    version = package_version(list(major = "1", minor = "0.0")),
    isoreader_version = package_version(list(major = "1", minor = "0.1"))
  ))
  expect_true(same_as_isoreader_version(
    version = package_version(list(major = "1", minor = "0.0")),
    isoreader_version = package_version(list(major = "1", minor = "1.1"))
  ))
  expect_false(same_as_isoreader_version(
    version = package_version(list(major = "1", minor = "0.0")),
    isoreader_version = package_version(list(major = "2", minor = "0.0"))
  ))
  
  
})

# example files =====

test_that("retrieving example files works correctly", {
  
  expect_true(is.data.frame(iso_get_reader_examples()))
  expect_error(iso_get_reader_example("DNE"), "does not exist")
  expect_equal(system.file(package = "isoreader", "extdata", "linearity_example.dxf"), path <- iso_get_reader_example("linearity_example.dxf"))
  expect_true(file.exists(path))
  
})

# file extensions helpers ======

test_that("test that file extension helpers work correctly", {
  
  # get file extension
  expect_equal(get_file_ext("test.csv"), ".csv")
  expect_equal(get_file_ext("test.x.csv"), ".csv")
  
  # match file extension
  expect_equal(match_file_ext("test.csv", c(".txt", ".csv")), ".csv")
  expect_equal(match_file_ext("test.CSV", c(".txt", ".csv")), ".csv")
  expect_equal(match_file_ext("test.csv", c(".txt", ".CSV")), ".CSV")
  expect_equal(match_file_ext("test.CSV", c(".txt", ".CSV")), ".CSV")
  expect_equal(match_file_ext("test.csv", c(".txt", "csv", ".csv")), ".csv")
  expect_equal(match_file_ext("test.dne", c(".txt", ".csv")), NA_character_)
  
  # match supported filed types
  expect_error(match_to_supported_file_types())
  expect_error(match_to_supported_file_types(tibble(), tibble()))
  expect_error(
    match_to_supported_file_types(tibble(path = "test.txt"), tibble(extension = ".csv")),
    "unexpected file extension"
  )
  expect_equal(
    match_to_supported_file_types(
      tibble(path = c("test.csv", "test.dxf")), 
      tibble(extension = c(".dxf", "t.dxf", ".csv"))),
    tibble(path = c("test.csv", "test.dxf"), extension = c(".csv", "t.dxf"))
  )
})

# path segmentation ======= 

test_that("test that path segmentation works correctly", {
  
  # has common start
  expect_equal(has_common_start(list(1:5, 1:3), 1:2), c(TRUE, TRUE))
  expect_equal(has_common_start(list(1:5, 1:3), 1:3), c(TRUE, TRUE))
  expect_equal(has_common_start(list(1:5, 1:3), 1:4), c(TRUE, FALSE))
  expect_equal(has_common_start(list(1:5, 1:3), 1:6), c(FALSE, FALSE))
  expect_equal(has_common_start(list(1:5, 1:3), 2L), c(FALSE, FALSE))
  expect_equal(has_common_start(list(1:5, 2:3), 2L), c(FALSE, TRUE))
  
  # get common/different from start
  expect_equal(find_common_different_from_start(list(1:5, 1:3)), 
               list(common = 1:3, different = list(4:5, character(0))))
  expect_equal(find_common_different_from_start(list(1:5, 1:4, 1:3)), 
               list(common = 1:3, different = list(4:5, 4, character(0))))
  expect_equal(find_common_different_from_start(list(1:3, 1:4, 1:5)),
               list(common = 1:3, different = list(character(0), 4, 4:5)))
  expect_equal(find_common_different_from_start(list(1:5, c(1:3, 5:4))), 
               list(common = 1:3, different = list(4:5, 5:4)))
  expect_equal(find_common_different_from_start(list(1:5, 0:5)), 
               list(common = character(0), different = list(1:5, 0:5)))
  expect_equal(find_common_different_from_start(list(1:5, 2)), 
               list(common = character(0), different = list(1:5, 2)))
  expect_equal(find_common_different_from_start(list(1:5, 0), empty = "test"), 
               list(common = "test", different = list(1:5, 0)))
  expect_equal(find_common_different_from_start(list(1:5, integer(0))), 
               list(common = character(0), different = list(1:5, integer(0))))
  
  # check whether something is a folder
  expect_equal(is_folder(getwd()), TRUE)
  expect_equal(is_folder(list.files(include.dirs = FALSE, pattern = "\\.")[1]), FALSE)
  expect_error(is_folder("DNE"), "do not exist")
  expect_equal(is_folder("DNE", check_existence = FALSE), TRUE)
  expect_equal(is_folder("DNE.ext", check_existence = FALSE), FALSE)
  
  # get path folders
  expect_equal(get_path_segments(file.path("A", "B", "C")), c("A", "B", "C"))
  expect_equal(get_path_segments(file.path("A", ".", "B", "C")), c("A", "B", "C"))
  expect_equal(get_path_segments(file.path(".", "A", "B", "C")), c("A", "B", "C"))
  expect_equal(get_path_segments(file.path("A", "A", "A")), c("A", "A", "A"))
  expect_equal(get_path_segments(""), "")
  expect_equal(get_path_segments(file.path(".")), character(0))
  
  expect_equal(
    setdiff(get_path_segments(system.file("extdata", package = "isoreader")), 
            get_path_segments(system.file(package = "isoreader"))),
    "extdata"
  )
  
})

# relative path shifting =======

test_that("relative path shortening works correctly", {
  
  # errors
  expect_error(iso_find_absolute_path_roots(c(".", ".", "."), c(".", ".")), "one entry or be of the same length")
  
  # shoretning of sequential ././.
  expect_equal(iso_shorten_relative_paths(file.path(".", ".", "A", "B", ".", "C")), 
               tibble(root = ".", path = file.path("A", "B", "C")))
  
  # shortening of relative paths
  expect_equal(iso_shorten_relative_paths(file.path("A", "B", "C"), "A"), 
               tibble(root = "A", path = file.path("B", "C")))
  expect_equal(iso_shorten_relative_paths(file.path("A", "B", "C"), file.path("A", "B")), 
               tibble(root = file.path("A", "B"), path = "C"))
  expect_equal(iso_shorten_relative_paths(file.path("A", "C", "D"), file.path("A", "B")), 
               tibble(root = "A", path = file.path("C", "D")))
  expect_equal(iso_shorten_relative_paths(file.path("A", ".", ".", "B", "C"), file.path(".", "A", "B")), 
               tibble(root = file.path("A", "B"), path = "C"))
  expect_equal(iso_shorten_relative_paths(file.path("A", "B", "C"), "B"), 
               tibble(root = ".", path = file.path("A", "B", "C")))
  expect_equal(iso_shorten_relative_paths(file.path("A", "B", "C"), file.path("A", "B", "C")), 
               tibble(root = file.path("A", "B", "C"), path = "."))
  expect_equal(iso_shorten_relative_paths(file.path("A", "B", "C"), file.path("A", "B", "C", "D")), 
               tibble(root = file.path("A", "B", "C"), path = "."))
  
  # path and root absolute - stay the same
  expect_equal(iso_shorten_relative_paths(getwd(), system.file(package = "base")),
               tibble(root = system.file(package = "base"), path = getwd()))
  
  # root gets shortened to wd if a subpath
  expect_equal(iso_shorten_relative_paths(file.path("A", "B"), getwd()),
               tibble(root = ".", path = file.path("A", "B")))
  
  expect_equal(iso_shorten_relative_paths(file.path("A", "B"), file.path(getwd(), "A")),
               tibble(root = "A", path = "B"))
  
  # no shortening for absolute paths (only roots)
  expect_equal(iso_shorten_relative_paths(getwd(), getwd()), 
               tibble(root = ".", path = getwd()))
  
  # mixed test
  expect_equal(
    iso_shorten_relative_paths(
      c(file.path("A", "B", "C"), file.path("A", "C"), file.path("B", "C"), getwd()), file.path("A", "B")),
    tibble(root = c(file.path("A", "B"), "A", ".", file.path("A", "B")),
               path = c("C", "C", c(file.path("B", "C"), getwd())))
  )
  
  # combined rel and abs
  expect_equal(
    iso_root_paths(
      c(file.path("A", "B", "C"), file.path("A", "C"), file.path("B", "C"), getwd()), file.path("A", "B"), check_existence = FALSE),
    tibble(root = c(file.path("A", "B"), "A", ".", getwd()),
               path = c("C", "C", c(file.path("B", "C"), ".")))
  )
  
})

# absolute path roots =======

test_that("test that root folder finding works correctly", {
 
  # identifty path roots
  expect_error(iso_find_absolute_path_roots("DNE"), "do not exist")
  expect_error(iso_find_absolute_path_roots(file.path(getwd(), "DNE")), "do not exist")
  expect_error(iso_find_absolute_path_roots(".", "DNE"), "do not exist")
  expect_error(iso_find_absolute_path_roots(c(".", ".", "."), c(".", ".")), "one entry or be of the same length")
  expect_error(iso_find_absolute_path_roots(""), "empty paths .* are not valid")
  expect_equal(iso_find_absolute_path_roots(c()), tibble(root = character(0), path = character(0)))
  
  # general checks on relative paths (should remain unchanged)
  test_folder <- "test_data" # test_folder <- file.path("tests", "testthat", "test_data") # for direct testing
  expect_equal(iso_find_absolute_path_roots(test_folder), tibble(root = ".", path = test_folder))
  expect_equal(iso_find_absolute_path_roots(".", root = test_folder), tibble(root = test_folder, path = "."))
  expect_equal(iso_find_absolute_path_roots(c(test_folder, ".")), tibble(root = ".", path = c(test_folder, ".")))
  expect_equal(iso_find_absolute_path_roots(c(test_folder, "."), "."), tibble(root = ".", path = c(test_folder, ".")))
  expect_equal(iso_find_absolute_path_roots(c(test_folder, "."), c(".", ".")), tibble(root = ".", path = c(test_folder, ".")))
  expect_equal(iso_find_absolute_path_roots(c(test_folder, "."), c(".", getwd())), tibble(root = c(".", getwd()), path = c(test_folder, ".")))
  expect_equal(iso_find_absolute_path_roots(c(".", test_folder)), tibble(root = ".", path = c(".", test_folder)))
  expect_equal(iso_find_absolute_path_roots("cf_example_H_01.cf", test_folder), tibble(root = test_folder, path = "cf_example_H_01.cf"))
  expect_equal(
    iso_find_absolute_path_roots(c(test_folder, file.path(test_folder, "cf_example_H_01.cf"))), 
    tibble(root = ".", path = c(test_folder, file.path(test_folder, "cf_example_H_01.cf"))))
  
  # absolute paths that fit the relative path
  expect_equal(iso_find_absolute_path_roots(getwd()), tibble(root = ".", path = "."))
  expect_equal(iso_find_absolute_path_roots(file.path(getwd(), test_folder)), tibble(root = ".", path = test_folder))
  expect_equal(iso_find_absolute_path_roots(c(file.path(getwd(), test_folder), test_folder)), tibble(root = ".", path = c(test_folder, test_folder)))
  expect_equal(
    iso_find_absolute_path_roots(c(file.path(getwd(), test_folder), test_folder), c(test_folder, ".")), 
    tibble(root = c(test_folder, "."), path = c(".", test_folder)))
  expect_equal(
    iso_find_absolute_path_roots(c(file.path(getwd(), test_folder), file.path(getwd(), test_folder, "cf_example_H_01.cf")), test_folder),
    tibble(root = test_folder, path = c(".", "cf_example_H_01.cf")))
  expect_equal(
    iso_find_absolute_path_roots(c(file.path(getwd(), test_folder), file.path(getwd(), test_folder, "cf_example_H_01.cf")), c(".", test_folder)),
    tibble(root = c(".", test_folder), path = c(test_folder, "cf_example_H_01.cf")))
  
  # add absolute paths that don't fit the relative path (this don't work interactively if package installed in current path with devtools)
  td <- system.file(package = "isoreader")
  expect_equal(
    iso_find_absolute_path_roots(c(td, file.path(getwd(), test_folder), file.path(test_folder, "cf_example_H_01.cf"))), 
    tibble(root = c(td, ".", "."), path = c(".", test_folder, file.path(test_folder, "cf_example_H_01.cf")))
  )
  expect_equal(
    iso_find_absolute_path_roots(
      c(system.file(package = "isoreader"),
        system.file("extdata", package = "isoreader"),
        system.file("extdata", "dual_inlet_example.did", package = "isoreader"))
    ),
    tibble(
      root = system.file(package = "isoreader"),
      path = c(".", "extdata", file.path("extdata", "dual_inlet_example.did"))
    )
  )
  
  expect_equal(
    iso_find_absolute_path_roots(
      c(system.file("extdata", package = "isoreader"),
        system.file("extdata", "dual_inlet_example.did", package = "isoreader")),
    ),
    tibble(
      root = system.file("extdata", package = "isoreader"),
      path = c(".", "dual_inlet_example.did")
    )
  )
  
  # check that file isn't included in common path
  expect_equal(
    iso_find_absolute_path_roots(
      c(system.file("extdata", "dual_inlet_example.did", package = "isoreader"),
        system.file("extdata", "dual_inlet_example.did", package = "isoreader"))
    ),
    tibble(
      root = system.file("extdata", package = "isoreader"),
      path = c("dual_inlet_example.did", "dual_inlet_example.did")
    )
  )
  
})

# expanding file paths ======

test_that("test that retrieving file paths works correctly", {
  
  expect_error(iso_expand_paths())
  expect_error(iso_expand_paths("DNE"), "do not exist")
  expect_error(iso_expand_paths(file.path(getwd(), "DNE")), "do not exist")
  expect_error(iso_expand_paths(".", root = "DNE"), "do not exist")
  expect_error(iso_expand_paths(c(".", ".", "."), root = c(".", ".")), "one entry or be of the same length")
  expect_error(iso_expand_paths(""), "empty paths .* are not valid")
  expect_error(iso_expand_paths(system.file("extdata", package = "isoreader")), "no extensions")
  expect_error(iso_expand_paths(system.file("extdata", package = "isoreader") %>% list.files(full.names = TRUE), "did"), 
               "do not have one of the supported extensions")
  
  # check expected result
  direct_list <-  system.file("extdata", package = "isoreader") %>% list.files(full.names = T, pattern = "\\.(dxf|did|cf)$")
  expect_identical(
    tibble(root = ".", path = direct_list),
    system.file("extdata", package = "isoreader") %>% iso_expand_paths(c("did", "dxf", "cf"))
  )
  expect_identical(
    tibble(root = ".", path = direct_list %>% {.[c(2,1,3:length(.))]}),
    c(direct_list[2], system.file("extdata", package = "isoreader")) %>% iso_expand_paths(c("did", "dxf", "cf"))
  )
  
  expect_warning(
    iso_expand_paths(c("."), "did", c(".", getwd())),
   "some files from different folders have identical file names" 
  )
  
})

test_that("test that column name checks work correctly", {
  expect_error(col_check("x", tibble(y=1:5)), "not in data")
  expect_error(col_check(c("x", "y"), tibble(x=1:5)), "not in data")
  expect_silent(col_check("x", tibble(x=1:5)))
  expect_silent(col_check(c("x", "y"), tibble(x=1:5, y="test")))
})


test_that("test that get support file types are listed", {
  expect_true(is.data.frame(iso_get_supported_file_types()))
  expect_equal(
    iso_get_supported_file_types() %>% names(),
    c("extension", "software", "description", "type", "call")
  )
})

test_that("logging works correctly", {
  iso_turn_info_messages_on()
  expect_message(log_message("test"), "Info: test")
  expect_message(log_warning("test"), "Warning: test")
  
  iso_turn_info_messages_off()
  expect_silent(log_message("test"))
  expect_message(log_warning("test"), "Warning: test")
  
  iso_turn_info_messages_on()
})

test_that("test that error catching works correctly", {
  expect_equal( {suppressMessages(iso_turn_debug_on(catch_errors = TRUE)); default(debug)}, TRUE)
  expect_message(y <- exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal( {suppressMessages(iso_turn_debug_on(catch_errors = FALSE)); default(debug)}, TRUE)
  expect_error(exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal( {suppressMessages(iso_turn_debug_off()); default(debug)}, FALSE)
  expect_message(y <- exec_func_with_error_catch(function(x) stop("problem"), 1), "problem")
  expect_equal(problems(y) %>% select(type, details), tibble(type = "error", details = "problem"))
})


test_that("test that info concatenation works", {
  expect_equal(get_info_message_concat(1:2), "'1', '2'")
  expect_equal(get_info_message_concat(1:2, quotes = FALSE), "1, 2")
  expect_equal(get_info_message_concat(prefix = "hello ", 1:2), "hello '1', '2'")
  expect_equal(get_info_message_concat(1:2, suffix = " world"), "'1', '2' world")
  expect_equal(get_info_message_concat(c()), "")
  expect_equal(get_info_message_concat(NULL), "")
  expect_equal(get_info_message_concat(rlang::quo(NULL)), "")
  expect_equal(get_info_message_concat(rlang::expr(NULL)), "")
  expect_equal(get_info_message_concat(quo(xyz)), "'xyz'")
  expect_equal(get_info_message_concat(rlang::expr(xyz)), "'xyz'")
  expect_equal(get_info_message_concat(rlang::exprs(a = xyz, abc)), "'xyz', 'abc'")
  expect_equal(get_info_message_concat(rlang::exprs(a = xyz, abc), include_names = TRUE), "'a'='xyz', 'abc'")
  expect_equal(get_info_message_concat(rlang::exprs(a = xyz, b = abc), include_names = TRUE, quotes = FALSE, names_sep = " -> "), "a -> xyz, b -> abc")
  expect_equal(get_info_message_concat(rlang::quos(a = xyz, b = abc), include_names = TRUE, quotes = FALSE, names_sep = " -> "), "a -> xyz, b -> abc")
})

# cached file paths ======

test_that("test that cached file path hashes work okay", {
  
  test_folder <- "test_data" # test_folder <- file.path("tests", "testthat", "test_data") # for direct testing
  
  file_paths <- file.path(test_folder, c(
    "cache_test.did",
    file.path("cache_test1", "cache_test.did"),
    file.path("cache_test2", "cache_test.did"),
    file.path("cache_test3", "cache_test.did")
  ))
  
  cache_paths <- generate_cache_filepaths(file_paths)
  
  # exact same file in different locations
  expect_true(identical(cache_paths[1], cache_paths[2]))
  
  # same names but different file sizes
  expect_false(identical(cache_paths[1], cache_paths[3]))
  
  # same names but different modified different dates
  expect_false(identical(cache_paths[1], cache_paths[4]))

  
})
