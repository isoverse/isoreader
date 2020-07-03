context("Standard and non-standard evaluation")

test_that("Getting column names (with # and type requirement checks) works", {
  
  df <- dplyr::as_tibble(mtcars) %>% tibble::rownames_to_column()
  
  # basic errors
  expect_error(get_column_names(), "no data frame supplied")
  expect_error(get_column_names(5), "not a data frame")
  expect_error(get_column_names(df, a = quo(x)), "unknown column")
  
  # error messages
  expect_error(get_column_names(df, quo(x)), "'x'.*unknown column")
  expect_error(get_column_names(df, x = quo(x)), "'x'.*unknown column")
  expect_error(get_column_names(df, x = quo(c(x))), "'x = c\\(x\\)'.*unknown column")
  expect_error(get_column_names(df, a = quo(x)), "'a = x'.*unknown column")
  set_default("x", quo(y))
  expect_error(get_column_names(df, a = quo(default(x))), "'a = y'.*unknown column")
  expect_error(get_column_names(df, a = quo(c(b = mpg, b = cyl))), "renamed columns must be unique")
  expect_error(get_column_names(df, a = quo(c(b = mpg, b = DNE))), "unknown column")
  expect_warning(get_column_names(df, a = quo(c(b = mpg, b = DNE)), cols_must_exist = FALSE), "unknown column")
  
  # single column per identifier
  expect_equal(get_column_names(df, a = quo(mpg), b = expr(wt)), list(a=c(mpg = "mpg"), b = c(wt = "wt")))
  expect_equal(get_column_names(df, a = quo("mpg"), b = expr("wt")), list(a=c(mpg = "mpg"), b = c(wt = "wt")))
  
  # with defaults
  set_default("x", quo(mpg)); set_default("y", quo(wt))
  expect_equal(get_column_names(df, a = quo(default(x)), b = expr(default(y))), list(a=c(mpg = "mpg"), b = c(wt = "wt")))
  set_default("x", expr(mpg)); set_default("y", expr(wt))
  expect_equal(get_column_names(df, a = quo(default(x)), b = expr(default(y))), list(a=c(mpg = "mpg"), b = c(wt = "wt")))
  set_default("x", quo("mpg")); set_default("y", quo("wt"))
  expect_equal(get_column_names(df, a = quo(default(x)), b = expr(default(y))), list(a=c(mpg = "mpg"), b = c(wt = "wt")))
  set_default("x", "mpg"); set_default("y", "wt")
  expect_equal(get_column_names(df, a = quo(default(x)), b = expr(default(y))), list(a=c(mpg = "mpg"), b = c(wt = "wt")))
  
  # no columns per identifier
  expect_error(get_column_names(df, a = quo(mpg), n_reqs = list(a = "wrong")), "unknown number requirement")
  expect_error(get_column_names(df, a = quo(NULL)), "not .* the correct number of columns")
  expect_error(get_column_names(df, a = quo(NULL), n_reqs = list(a = "+")), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(NULL), n_reqs = list(a = "*")), list(a = rlang::set_names(character(0), character(0))))
  expect_equal(get_column_names(df, a = quo(NULL), n_reqs = list(a = "?")), list(a = rlang::set_names(character(0), character(0))))
  expect_equal(get_column_names(df, a = quo(mpg), n_reqs = list(a = "?")), list(a = c(mpg = "mpg")))
  expect_error(get_column_names(df, a = quo(NULL), b = quo(NULL), n_reqs = list(a = "+")), "not .* the correct number of columns")
  
  # multiple columns in identifier
  expect_error(get_column_names(df, a = quo(c(mpg, wt))), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), n_reqs = list(a = "*")), list(a = c(mpg = "mpg", wt = "wt")))
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), n_reqs = list(a = "+")), list(a = c(mpg = "mpg", wt = "wt")))
  expect_error(get_column_names(df, a = quo(c(mpg, wt)), b = quo(c(mpg, wt)), n_reqs = list(a = "+")), "not .* the correct number of columns")
  expect_equal(get_column_names(df, a = quo(c(mpg, wt)), b = quo(starts_with("d")), n_reqs = list(a = "+", b="*")),
               list(a = c(mpg = "mpg", wt = "wt"), b = c(disp = "disp", drat = "drat")))
  expect_equal(get_column_names(df, a = expr(c(mpg, wt)), b = expr(starts_with("d")), n_reqs = list(a = "+", b="*")),
               list(a = c(mpg = "mpg", wt = "wt"), b = c(disp = "disp", drat = "drat")))
  
  # named columns
  expect_equal(get_column_names(df, a = quo(c(x = mpg)), b = quo(wt)), list(a=c(x = "mpg"), b = c(wt = "wt")))
  expect_equal(get_column_names(df, a = quo(c(x = mpg, y = "wt")), n_reqs = list(a = "*")), list(a = c(x = "mpg", y = "wt")))
  expect_equal(get_column_names(df, a = quo(c(mpg, y = wt)), n_reqs = list(a = "*")), list(a = c(mpg = "mpg", y = "wt")))
  expect_equal(get_column_names(df, a = quo(c(x = starts_with("c"))), n_reqs = list(a = "*")), list(a = c(x1 = "cyl", x2 = "carb")))
  
  # expected error from conversion to expressions inside get_column_names (may not work interactively!)
  ..test.. <- "c"
  expect_error(get_column_names(df, a = quo(c(x = starts_with(..test..))), n_reqs = list(a = "*")), "'..test..' not found")
  expect_error(get_column_names(df, a = expr(c(x = starts_with(..test..))), n_reqs = list(a = "*")), "'..test..' not found")
  expect_equal(get_column_names(df, a = quo(c(x = starts_with(!!..test..))), n_reqs = list(a = "*")), list(a = c(x1 = "cyl", x2 = "carb")))
  expect_equal(get_column_names(df, a = expr(c(x = starts_with(!!..test..))), n_reqs = list(a = "*")), list(a = c(x1 = "cyl", x2 = "carb")))
  
  # allow missing columns
  expect_warning(get_column_names(df, a = quo(x), n_reqs = list(a = "*"), cols_must_exist = FALSE), "unknown column")
  expect_warning(found <- get_column_names(df, a = quo(c(mpg, x)), n_reqs = list(a = "*"), cols_must_exist = FALSE), "unknown column")
  expect_equal(found, list(a = c(mpg = "mpg")))
  
  # type checks
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "DNE")), "unknown type")
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "list")), "not .* the correct column types")
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "integer")), "not .* the correct column types")
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "character")), "not .* the correct column types")
  expect_error(get_column_names(df, a = quo(mpg), type_reqs = list(a = "logical")), "not .* the correct column types")
  expect_error(get_column_names(df, a = quo(c(mpg, rowname)), n_reqs = list(a = "+"), type_reqs = list(a = "character")), "not .* the correct column types")
  expect_equal(get_column_names(df, a = quo(mpg), type_reqs = list(a = "numeric")), list(a = c(mpg = "mpg")))
  expect_equal(get_column_names(df, a = quo(c(mpg, cyl)), n_reqs = list(a = "+"), type_reqs = list(a = "numeric")), list(a = c(mpg = "mpg", cyl = "cyl")))
  expect_equal(get_column_names(df, a = quo(rowname), type_reqs = list(a = "character")), list(a = c(rowname = "rowname")))
  expect_error(get_column_names(nest(df, data = c(-rowname)), a = quo(data), type_reqs = list(a = "character")), "not .* the correct column types")
  expect_equal(get_column_names(nest(df, data = c(-rowname)), a = quo(data), type_reqs = list(a = "list")), list(a = c(data = "data")))
})
