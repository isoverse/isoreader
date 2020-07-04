context("File info operations")

# selecting / renaming ======

test_that("Test that selecting/renaming file info works", {
  
  iso_file1 <- make_di_data_structure("NA")
  iso_file1$read_options$file_info <- TRUE
  iso_file1$file_info$new_info <- 42
  iso_file2 <- iso_file3 <- iso_file1
  iso_file1$file_info$file_id <- "A"
  iso_file2$file_info$file_id <- "B"
  iso_file2$file_info$new_info2 <- 2
  iso_file3$file_info$file_id <- "C"
  iso_file3$file_info$new_info3 <- 3
  iso_files <- c(iso_file1, iso_file2, iso_file3)
  
  # select error checks
  expect_error(iso_select_file_info(42), "not defined")
  expect_error(iso_select_file_info(iso_file1, new = file_id), "renaming.*not allowed")
  expect_error(iso_select_file_info(iso_files, new = file_id), "renaming.*not allowed")
  expect_warning(iso_select_file_info(iso_files, y = DNE), "doesn't exist")
  expect_warning(iso_select_file_info(iso_files, y = DNE, file_specific = TRUE), "doesn't exist")
  expect_error(iso_select_file_info(iso_files, y = new_info, y = new_info2), "renamed columns must be unique")
  expect_message(iso_select_file_info(iso_files, y = new_info, y = new_info2, file_specific = TRUE, cols_must_exist = FALSE), 
                 "selecting/renaming")
  
  # select info message
  expect_message(iso_select_file_info(iso_file1), "keeping only 'file_id'") # always file_info
  expect_silent(iso_select_file_info(iso_file1, quiet = TRUE))
  expect_silent(select(iso_file1)) 
  expect_message(iso_select_file_info(iso_file1, newer_info = new_info), "1 data file.*'new_info'->'newer_info'")
  expect_message(iso_select_file_info(iso_file1, newer_info = new_info, file_specific = TRUE), 
                 "1 data file.*'new_info'->'newer_info'")
  expect_message(iso_select_file_info(iso_files, newer_info = new_info2), "3 data file.*'new_info2'->'newer_info'")
  expect_message(iso_select_file_info(iso_files, newer_info = new_info2, file_specific = TRUE), 
                 "2 file.*'file_id'.*1 file.*'file_id', 'new_info2'->'newer_info'")
  expect_message(iso_select_file_info(iso_files, newer_info = new_info), "3 data file.*'new_info'->'newer_info'")
  expect_message(iso_select_file_info(iso_files, newer_info = new_info, file_specific = TRUE), 
                 "3 file.*'new_info'->'newer_info'")
  expect_message(iso_select_file_info(iso_files, y = new_info2, y = new_info3, file_specific = TRUE), 
                 "1 file.*'file_id'.*1 file.*'file_id', 'new_info2'->'y'.*1 file.*'file_id', 'new_info3'->'y'")
  
  # select outcomes
  expect_equal(
    iso_select_file_info(iso_files, y = new_info2, y = new_info3, file_specific = TRUE) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), y = c(NA, 2, 3))
  )
  expect_equal(
    iso_select_file_info(iso_files, -starts_with("file"), -new_info) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), new_info2 = c(NA, 2, NA), new_info3 = c(NA, NA, 3))
  )
  expect_equal(
    iso_select_file_info(iso_files, -starts_with("file"), -new_info, file_specific = TRUE) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), new_info2 = c(NA, 2, NA), new_info3 = c(NA, NA, 3))
  )
  expect_equal(
    iso_select_file_info(iso_files, newer_info = new_info) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), newer_info = 42)
  )
  expect_equal(
    iso_select_file_info(iso_files, newer_info = new_info, file_specific = TRUE) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), newer_info = 42)
  )
  expect_equal(
    iso_select_file_info(iso_files, newest_info = new_info, new_info2, new_info2 = new_info3, file_specific = TRUE) %>% iso_get_file_info(),
    tibble(file_id = c("A", "B", "C"), newest_info = 42, new_info2 = c(NA, 2, 3))
  )
  
  # rename error checks
  expect_error(iso_rename_file_info(42), "not defined")
  expect_error(iso_rename_file_info(iso_file1, new = file_id), "renaming.*not allowed")
  expect_error(iso_rename_file_info(iso_files, new = file_id), "renaming.*not allowed")
  expect_error(iso_rename_file_info(iso_files, new_info = new_info2), class = "vctrs_error_names_must_be_unique", "must be unique")
  expect_error(iso_rename_file_info(iso_files, y = new_info, y = new_info2), "must be unique")
  
  # rename info message
  expect_message(iso_rename_file_info(iso_file1), "renaming.*1 data file")
  expect_silent(iso_rename_file_info(iso_file1, quiet = TRUE))
  expect_silent(rename(iso_file1)) 
  expect_message(iso_rename_file_info(iso_file1, newer_info = new_info), "renaming.*1.*file.*'new_info'->'newer_info'")
  expect_message(iso_rename_file_info(iso_files, newer_info = new_info2), "renaming.*3.*file.*'new_info2'->'newer_info'")
  expect_error(iso_rename_file_info(iso_files, y = new_info2, y = new_info3), "renamed columns must be unique")
  expect_message(iso_rename_file_info(iso_files, y = new_info2, y = new_info3, file_specific = TRUE), 
                 "1 file.*'new_info2'->'y'.*1 file.*'new_info3'->'y'")
  
  # rename outcomes
  expect_equal(
    iso_rename_file_info(iso_files, y = new_info2, y = new_info3, file_specific = TRUE) %>%
      iso_get_file_info() %>% select(file_id, new_info, y),
    tibble(file_id = c("A", "B", "C"), new_info = 42, y = c(NA, 2, 3))
  )
  expect_equal(
    iso_rename_file_info(iso_files, newer_info = new_info) %>% iso_get_file_info() %>% select(file_id, newer_info),
    tibble(file_id = c("A", "B", "C"), newer_info = 42)
  )
  expect_equal(
    iso_rename_file_info(iso_files, newest_info = new_info, new_info2 = new_info3, file_specific = TRUE) %>% iso_get_file_info() %>% select(file_id, newest_info, new_info2),
    tibble(file_id = c("A", "B", "C"), newest_info = 42, new_info2 = c(NA, 2, 3))
  )
  
})

# filtering ======

test_that("Test that filtering by file info works", {
  
  iso_file1 <- make_di_data_structure("NA")
  iso_file1$read_options$file_info <- TRUE
  iso_file1$file_info$new_info <- 42
  iso_file2 <- iso_file3 <- iso_file1
  iso_file1$file_info$file_id <- "A"
  iso_file2$file_info$file_id <- "B"
  iso_file3$file_info$file_id <- "C"
  
  # errors
  expect_error(iso_filter_files(42), "not defined")
  
  # messaging
  iso_files <- c(iso_file1, iso_file2, iso_file3)
  expect_silent(filter(iso_files))
  expect_message(iso_filter_files(iso_files), "applying.*filter")
  expect_silent(iso_filter_files(iso_files, quiet = TRUE))
  
  # filtering
  expect_equal(filter(iso_files), iso_files)
  expect_equal(filter(iso_files, new_info == 42), iso_files)
  expect_equal(filter(iso_file1, new_info == 42), iso_file1)
  expect_null(filter(iso_files, new_info != 42))
  expect_null(filter(iso_file1, new_info != 42))
  expect_equal(filter(iso_files, file_id == "A"), c(iso_file1))
  expect_null(filter(iso_files, file_id == "DNE"))
  expect_error(filter(iso_files, dne == 5), "not.*found", class = "dplyr_error")
  
  # filter and iso_filter_files equivalence
  expect_equal(filter(iso_files, file_id != "A"), iso_filter_files(iso_files, file_id != "A"))
})

# mutating ====

test_that("Test that mutating file info works", {
  
  iso_file1 <- make_di_data_structure("NA")
  iso_file1$read_options$file_info <- TRUE
  iso_file1$file_info$new_info <- 42
  iso_file2 <- iso_file3 <- iso_file1
  iso_file1$file_info$file_id <- "A"
  iso_file2$file_info$file_id <- "B"
  iso_file2$file_info$new_info2 <- 2
  iso_file3$file_info$file_id <- "C"
  iso_file3$file_info$new_info3 <- 3
  
  # errors
  expect_error(iso_mutate_file_info(42), "not defined")
  
  # messaging
  iso_files <- c(iso_file1, iso_file2, iso_file3)
  expect_silent(mutate(iso_files))
  expect_message(iso_mutate_file_info(iso_files), "mutating")
  expect_silent(iso_mutate_file_info(iso_files, quiet = TRUE))
  
  # mutating
  expect_equal(
    mutate(iso_files) %>% iso_get_file_info(), 
    iso_files %>% iso_get_file_info())
  expect_equal(
    mutate(iso_files, new_info = as.character(new_info)) %>% iso_get_file_info(), 
    iso_files %>% iso_get_file_info() %>% mutate(new_info = as.character(new_info)))
  expect_equal(
    mutate(iso_files, new_info = iso_double_with_units(1:3, "s")) %>% iso_get_file_info(), 
    iso_files %>% iso_get_file_info() %>% mutate(new_info = iso_double_with_units(1:3, "s")))
  
  expect_true(
    iso_is_file_list(
      mutated_iso_files <- 
        mutate(iso_files, newest_info = case_when(new_info2 == 2 ~ 20, new_info3 == 3 ~ 30, TRUE ~ 00)))
  )
  expect_equal(
    mutated_iso_files %>% iso_get_file_info(),
    iso_files %>% iso_get_file_info() %>% 
      mutate(newest_info = case_when(new_info2 == 2 ~ 20, new_info3 == 3 ~ 30, TRUE ~ 00))
  )
  
  # check on data types
  expect_true(is.character(mutated_iso_files[[1]]$file_info$file_id))
  expect_true(is.na(mutated_iso_files[[1]]$file_info$file_root))
  expect_true(is.list(mutated_iso_files[[1]]$file_info$new_info))
  expect_true(is.list(mutated_iso_files[[1]]$file_info$newest_info))
  
  # mutate and iso_mutate_file_info equivalence
  expect_equal(
    mutate(iso_files, newest_info = "A") %>% iso_get_file_info(), 
    iso_mutate_file_info(iso_files, newest_info = "A") %>% iso_get_file_info())
  
  
  # file root update =====
  expect_error(iso_set_file_root(iso_files, root = NULL), "must supply.*file root")
  expect_error(iso_set_file_root(iso_files, root = "root", remove_embedded_root = 1:2), "only.*single value")
  
  # just setting file_root itself
  expect_message(rerooted_files <- iso_set_file_root(iso_files, root = "test"), 
                 "setting file root for 3 data file.*to.*test")
  expect_equal(iso_get_file_info(rerooted_files, c(file_root, file_path)),
               iso_get_file_info(iso_files, c(file_root, file_path)) %>% mutate(file_root = "test"))
  
  # with removing embedded root
  iso_files[[1]]$file_info$file_path <- "A/B/C/a.cf"
  iso_files[[2]]$file_info$file_path <- "A/B/C/D/b.cf"
  iso_files[[3]]$file_info$file_path <- "A/B/c.df"
  
  expect_warning(rerooted_files <- iso_set_file_root(iso_files, remove_embedded_root = "DNE"), 
                 "3/3 file paths do not include the embedded root")
  expect_equal(iso_get_file_info(rerooted_files, c(file_root, file_path)),
               iso_get_file_info(iso_files, c(file_root, file_path)) %>% mutate(file_root = "."))
  
  expect_warning(rerooted_files <- iso_set_file_root(iso_files, root = "test", remove_embedded_root = "A/B/C"), 
                 "1/3 file paths do not include the embedded root")
  expect_equal(iso_get_file_info(rerooted_files, c(file_root, file_path)),
               iso_get_file_info(iso_files, c(file_root, file_path)) %>% 
                 mutate(file_path = str_replace(file_path, "A/B/C/", ""), file_root = "test"))
  
  expect_message(rerooted_files <- iso_set_file_root(iso_files, remove_embedded_root = "A/B"), 
                 "setting file root for 3 data file.*removing embedded root.*A/B")
  expect_message(rerooted_files <- iso_set_file_root(iso_files, root = "test", remove_embedded_root = "././A/B"), 
                 "setting file root for 3 data file.*to.*test.*removing embedded root.*A/B")
  expect_equal(iso_get_file_info(rerooted_files, c(file_root, file_path)),
               iso_get_file_info(iso_files, c(file_root, file_path)) %>% 
                 mutate(file_path = str_replace(file_path, "A/B/", ""), file_root = "test"))

  
    
})




# parse info =======

test_that("Test that file info parsing works", {
  
  iso_file1 <- make_di_data_structure("NA")
  iso_file1$read_options$file_info <- TRUE
  iso_file1$file_info$new_info <- 42.0
  iso_file2 <- iso_file3 <- iso_file1
  iso_file1$file_info$file_id <- "A"
  iso_file2$file_info$file_id <- "B"
  iso_file2$file_info$new_info2 <- "2"
  iso_file3$file_info$file_id <- "C"
  iso_file3$file_info$new_info3 <- "2019-01-01 01:01"
  iso_files <- c(iso_file1, iso_file2, iso_file3)
  
  # errors
  expect_error(iso_parse_file_info(42), "not defined")
  expect_error(iso_parse_file_info(iso_files, number = new_info, integer = new_info),
               "cannot convert.*to multiple formats")  
  
  # warnings
  expect_warning(iso_parse_file_info(iso_files, integer = new_info),
                 "missing automatic parsers")
  expect_warning(iso_parse_file_info(iso_files, datetime = new_info),
                 "missing automatic parsers")
  expect_warning(iso_parse_file_info(iso_files, integer = new_info3),
                 "parsing failure")
  expect_warning(iso_parse_file_info(iso_files, double = new_info3),
                 "parsing failure")
  expect_warning(iso_parse_file_info(iso_files, datetime = new_info2),
                 "parsing failure")
    
  # messages
  expect_message(no_effect_isos <- iso_parse_file_info(iso_files), "parsing 0.*for 3 data file")
  expect_silent(iso_parse_file_info(iso_files, quiet = TRUE))
  expect_message(text_isos <- iso_parse_file_info(iso_files, text = starts_with("new")), 
                 "parsing 1.*for 3 data file")
  expect_message(iso_parse_file_info(iso_files, text = starts_with("new")), 
                 "to text.*new_info")
  expect_message(iso_parse_file_info(iso_files, text = starts_with("new")), 
                 "already the target data type.*new_info2.*new_info3")
  expect_message(number_isos <- iso_parse_file_info(iso_files, number = c(new_info2, new_info3)),
                 "to number.*new_info2.*new_info3")
  expect_message(integer_isos <- iso_parse_file_info(iso_files, integer = new_info2),
                 "to integer.*new_info2")
  expect_message(double_isos <- iso_parse_file_info(iso_files, double = new_info2),
                 "to double.*new_info2")
  expect_message(datetime_isos <- iso_parse_file_info(iso_files, datetime = new_info3),
                 "to datetime.*new_info3")
  
  # outcome
  expect_equal(iso_get_file_info(no_effect_isos), iso_get_file_info(iso_files))
  expect_equal(iso_get_file_info(text_isos), iso_get_file_info(iso_files) %>% 
                 mutate(new_info = as.character(new_info)))
  expect_equal(iso_get_file_info(number_isos), iso_get_file_info(iso_files) %>% 
                 mutate(new_info2 = parse_number(new_info2),
                        new_info3 = parse_number(new_info3)))
  expect_equal(iso_get_file_info(integer_isos), iso_get_file_info(iso_files) %>% 
                 mutate(new_info2 = parse_integer(new_info2)))
  expect_equal(iso_get_file_info(double_isos), iso_get_file_info(iso_files) %>% 
                 mutate(new_info2 = parse_double(new_info2)))
  expect_equal(iso_get_file_info(datetime_isos)$new_info3, (iso_get_file_info(iso_files) %>% 
                 mutate(new_info3 = parse_datetime(new_info3) %>% lubridate::with_tz(Sys.timezone())))$new_info3)
  
})

# add info ========


test_that("Test that file info addition works", {
  
  # test drive data set
  file_info <- tibble(
    file_id = letters[1:6],
    y = rep(LETTERS[1:2], each = 3),
  )
  new_info <- tibble(
    file_id = c(NA, NA, letters[2:4]),
    y = c(LETTERS[1:2], NA, "", "X"),
    info = paste("new", 1:length(y))
  )
  
  # errors
  expect_error(iso_add_file_info(), "without parameters")
  expect_error(iso_add_file_info(42), "not defined")
  expect_error(iso_add_file_info(tibble()), "no new_file_info supplied")
  expect_error(iso_add_file_info(tibble(), tibble()), "specify at least one set of join_by")
  expect_error(iso_add_file_info(tibble(), tibble(), "x"), "file_id column")
  expect_error(iso_add_file_info(file_info, new_info, "NA"), "join_by.*must exist.*missing.*NA")
  expect_error(iso_add_file_info(file_info, select(new_info, -y), "y"), "join_by.*must exist.*missing in new file info.*y")
  expect_error(iso_add_file_info(select(file_info, -y), new_info, "y"), "join_by.*must exist.*missing in existing file info.*y")
  expect_error(
    iso_add_file_info(file_info, new_info[c(1,1,2), ], by1 = "y"),
    "would create duplicate entries")
  expect_warning(
    out <- iso_add_file_info(file_info, select(new_info, -info), "y"),
    "no new information.*returning data unchanged"
  )
  expect_equal(out, file_info)
  
  # actual join (sequential join)
  expect_message(
    df_out <- iso_add_file_info(file_info, new_info, by1 = "y", by2 = "file_id"),
    "adding.*'info'.*to 6 data file.*joining by 'y' then 'file_id'"
  )
  expect_message(
    iso_add_file_info(file_info, new_info, by1 = "y", by2 = "file_id"),
    "'y' join.*2/3 new info.*matched 6.*3.*also matched.*subsequent joins" 
  )
  expect_message(
    iso_add_file_info(file_info, new_info, by1 = "y", by2 = "file_id"),
    "'file_id' join.*3/3 new info.*matched 3" 
  )
  
  # actual join (including double column join)
  expect_message(
    iso_add_file_info(file_info, new_info, by1 = "y", by2 = c("file_id", "y")),
    "adding.*to 6 data file.*joining by 'y' then 'file_id'\\+'y'" 
  )
  expect_message(
    iso_add_file_info(file_info, new_info, by1 = "y", by2 = c("file_id", "y")),
    "'y' join.*2/3 new info.*matched 6" 
  )
  expect_message(
    iso_add_file_info(file_info, new_info, by1 = "y", by2 = c("file_id", "y")),
    "'file_id'\\+'y' join.*0/1 new info.*matched 0" 
  )
  
  # sequential join including a logical column join
  iso_add_file_info(
    mutate(file_info, y = rep(c(TRUE, FALSE), each = 3)), 
    mutate(new_info, y = c(TRUE, FALSE, NA, NA, NA)), 
    by1 = "y", 
    by2 = "file_id"
  )
  
  # test with isofiles (not just in data frame)
  template <- make_cf_data_structure("NA")
  template$read_options$file_info <- TRUE
  iso_files <- map(split(file_info, seq(nrow(file_info))), ~{ x <- template; x$file_info <- .x; x }) %>% 
    iso_as_file_list()
  
  expect_message(
    iso_files_out <- iso_add_file_info(iso_files, new_info, by1 = "y", by2 = "file_id"),
    "adding.*'info'.*to 6 data file.*joining by 'y' then 'file_id'"
  )
  expect_message(
    iso_add_file_info(iso_files, new_info, by1 = "y", by2 = "file_id"),
    "'y' join.*2/3 new info.*matched 6.*3.*also matched.*subsequent joins" 
  )
  expect_message(
    iso_add_file_info(iso_files, new_info, by1 = "y", by2 = "file_id"),
    "'file_id' join.*3/3 new info.*matched 3" 
  )
  # check that iso files and df derived are the same
  expect_equal(iso_files_out %>% iso_get_file_info(), df_out)
  
  
})

