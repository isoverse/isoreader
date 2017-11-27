context("Standard and non-standard evaluation")

test_that("Default calls are resolved", {
  # resolve defaults in a list of quos
  expect_equal(resolve_defaults(quo(default(quiet))), FALSE)
  expect_equal(resolve_defaults(list(quo(default(quiet)), quo(x))), list(FALSE, quo(x)))
})

test_that("Getting column names (with # and type requirement checks) works", {
  
  df <- as_data_frame(mtcars) %>% tibble::rownames_to_column()
  
  # basic errors
  expect_error(get_column_names(), "no data frame supplied")
  expect_error(get_column_names(5), "not a data frame")
  expect_error(get_column_names(df, a = quo(x)), "unknown column")
  
  # single column per identifier
  expect_equal(get_column_names(df, a = quo(mpg), b = quo(wt)), list(a="mpg", b="wt"))
  expect_equal(get_column_names(df, a = quo("mpg"), b = quo("wt")), list(a="mpg", b="wt"))
  
  # no columns per identifier
  expect_error(get_column_names(df, a = quo(mpg), n_reqs = list(a = "wrong")), "unknown number requirement")
  expect_error(get_column_names(df, a = quo(NULL)), "not .* the correct number of columns")
  expect_error(get_column_names(df, a = quo(NULL), n_reqs = list(a = "+")), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(NULL), n_reqs = list(a = "*")), list(a = character(0)))
  expect_equal(get_column_names(df, a = quo(NULL), n_reqs = list(a = "?")), list(a = character(0)))
  expect_equal(get_column_names(df, a = quo(mpg), n_reqs = list(a = "?")), list(a = "mpg"))
  expect_error(get_column_names(df, a = quo(NULL), b = quo(NULL), n_reqs = list(a = "+")), "not .* the correct number of columns")
  
  # multiple columns in identifier
  expect_error(get_column_names(df, a = quo(c(mpg, wt))), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), n_reqs = list(a = "*")), list(a = c("mpg", "wt")))
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), n_reqs = list(a = "+")), list(a = c("mpg", "wt")))
  expect_error(get_column_names(df, a = quo(c(mpg, wt)), b = quo(c(mpg, wt)), n_reqs = list(a = "+")), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), b = quo(starts_with("d")), n_reqs = list(a = "+", b="*")),
               list(a = c("mpg", "wt"), b = c("disp", "drat")))
  
  # allow missing columns
  expect_warning(get_column_names(df, a = quo(x), n_reqs = list(a = "*"), cols_must_exist = FALSE), "unknown column")
  expect_warning(found <- get_column_names(df, a = quo(c(mpg, x)), n_reqs = list(a = "*"), cols_must_exist = FALSE), "unknown column")
  expect_equal(found, list(a = "mpg"))
  
  # type checks
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "DNE")), "unknown type")
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "list")), "not .* the correct column types")
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "integer")), "not .* the correct column types")
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "character")), "not .* the correct column types")
  expect_error(get_column_names(df, a = quo(c(mpg, rowname)), n_reqs = list(a = "+"), type_reqs = list(a = "character")), "not .* the correct column types")
  expect_equal(get_column_names(df, a = quo(mpg), type_reqs = list(a = "numeric")), list(a = "mpg"))
  expect_equal(get_column_names(df, a = quo(c(mpg, cyl)), n_reqs = list(a = "+"), type_reqs = list(a = "numeric")), list(a = c("mpg", "cyl")))
  expect_equal(get_column_names(df, a = quo(rowname), type_reqs = list(a = "character")), list(a = "rowname"))
  expect_error(get_column_names(nest(df, -rowname), a = quo(data), type_reqs = list(a = "character")), "not .* the correct column types")
  expect_equal(get_column_names(nest(df, -rowname), a = quo(data), type_reqs = list(a = "list")), list(a = "data"))
})

test_that("Test that quos to text conversion works", {
  
  expect_error(quos_to_text(quo(a^2)), "not .* valid")
  expect_error(quos_to_text(quo(NULL)), "not .* valid")
  expect_error(quos_to_text(quo(~mean)), "not .* valid")
  expect_error(quos_to_text(quos(a^2, NULL, mean)), "not .* valid")
  expect_equal(quos_to_text(quo("a")), "a")
  expect_equal(quos_to_text(quo(a)), "a")
  expect_equal(quos_to_text(quos(a, "b")) %>% unlist(use.name = FALSE), c("a", "b"))
  expect_equal(quos_to_text(quos(x = a)), list(x="a"))
  
})