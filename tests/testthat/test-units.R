context("Units")

# isodat units processing ====

test_that("test that raw isodat units are processed correctly", {
  no_permil <- as.raw(c(0x5b, 0x00, 0x25, 0x00, 0x5d, 0x00))
  w_permil <- as.raw(c(0x5b, 0x00, 0x30, 0x20, 0x5d, 0x00))
  per_mil <- as.raw(c(0x70, 0x00, 0x65, 0x00, 0x72, 0x00, 0x20, 0x00, 0x6d, 0x00, 0x69, 0x00, 0x6c, 0x00))
  expect_equal(process_isodat_units(no_permil), "[%]")
  expect_equal(process_isodat_units(w_permil), "[permil]")
  expect_equal(process_isodat_units(per_mil), "permil")
  expect_equal(process_isodat_units(c(w_permil, no_permil, w_permil, per_mil)), "[permil][%][permil]permil")
})

# iso_with_units ====

test_that("test that units class works properly", {
  
  # generic with units
  expect_error(iso_with_units("test"), "cannot add units to.*character")
  expect_is(iso_with_units(pi), "iso_double_with_units")
  expect_is(iso_with_units(42L), "iso_double_with_units")
  
  # double with units
  data <- seq(1., 5., by = 0.5)
  x <- iso_double_with_units(data, "permil")
  y <- iso_double_with_units(data, "not permil")
  
  # types and casting
  # NA
  expect_is(vctrs::vec_ptype2(x, NA), "iso_double_with_units")
  expect_is(vctrs::vec_ptype2(NA, x), "iso_double_with_units")
  expect_equal(vctrs::vec_c(x, NA), iso_double_with_units(c(data, NA), "permil"))
  expect_equal(vctrs::vec_c(NA, x), iso_double_with_units(c(NA, data), "permil"))
  # double with units
  expect_is(vctrs::vec_ptype2(x, x), "iso_double_with_units")
  expect_equal(vctrs::vec_c(x, x), iso_double_with_units(c(data, data), "permil"))
  expect_warning(out <- vctrs::vec_ptype2(x, y), "different units")
  expect_is(out, "numeric")
  expect_warning(out <- vctrs::vec_c(x, y), "different units")
  expect_equal(out, c(data, data))
  expect_is(iso_double_with_units(x), "iso_double_with_units")
  expect_equal(iso_double_with_units(x) %>% iso_get_units(), "undefined units")
  expect_equal(iso_double_with_units(x, units = "new units") %>% iso_get_units(), "new units")
  # double
  expect_is(vctrs::vec_ptype2(x, double()), "numeric")
  expect_is(vctrs::vec_ptype2(double(), x), "numeric")
  expect_is(vctrs::vec_cast(x, double()), "numeric")
  expect_equal(vctrs::vec_cast(pi, x), iso_double_with_units(pi, "permil"))
  expect_equal(vctrs::vec_c(x, 4.2), c(as.numeric(x), 4.2))
  expect_equal(vctrs::vec_c(4.2, x), c(4.2, as.numeric(x)))
  # integer
  expect_is(vctrs::vec_ptype2(x, integer()), "numeric")
  expect_is(vctrs::vec_ptype2(integer(), x), "numeric")
  expect_is(vctrs::vec_cast(x, integer()), "integer")
  expect_equal(vctrs::vec_cast(42L, x), iso_double_with_units(42, "permil"))
  expect_equal(vctrs::vec_c(x, 42L), c(as.numeric(x), 42.))
  expect_equal(vctrs::vec_c(42L, x), c(42., as.numeric(x)))
  # others
  expect_error(vctrs::vec_ptype2(x, logical()))
  expect_error(vctrs::vec_ptype2(logical(), x))
  expect_error(vctrs::vec_ptype2(x, character()))
  expect_error(vctrs::vec_ptype2(character(), x))
  expect_error(vctrs::vec_ptype2(x, factor()))
  expect_error(vctrs::vec_ptype2(factor(), x))
  
  # units
  expect_equal(iso_get_units(42), NA_character_)
  expect_equal(iso_get_units(iso_double_with_units()), "undefined units")
  expect_equal(iso_get_units(x), "permil")
  expect_equal(iso_get_units(y), "not permil")
  expect_equal(data.frame(x = x, y = y, z = 42) %>% iso_get_units(), 
               c(x = "permil", y = "not permil", z = NA_character_))
  
  # strip units
  expect_equal(iso_strip_units(42.), 42.)
  expect_equal(iso_strip_units(42) %>% iso_get_units(), NA_character_)
  expect_equal(iso_strip_units(x), data)
  expect_equal(iso_strip_units(x) %>% iso_get_units(), NA_character_)
  expect_equal(data.frame(x = x, y = y, z = 42) %>% iso_strip_units() %>% iso_get_units(), 
               c(x = NA_character_, y = NA_character_, z = NA_character_))
  expect_equal(data.frame(x = x, y = y, z = 42) %>% iso_strip_units(),
               data.frame(x = data, y = data, z = 42))
  
  # is this what should happen for lists?
  expect_equal(list(x = x, y = y, z = 42) %>% iso_get_units(), NA_character_)
  expect_equal(list(x = x, y = y, z = 42) %>% purrr::map_chr(iso_get_units), c(x = "permil", y = "not permil", z = NA_character_))
  expect_equal(list(x = x, y = y, z = 42) %>% iso_strip_units() %>% purrr::map_chr(iso_get_units), c(x = "permil", y = "not permil", z = NA_character_))
  
  # implicit / explicit units
  expect_error(iso_make_units_explicit(42), "only.*data frames")
  expect_equal(iso_make_units_explicit(tibble()), tibble())
  expect_is(out <- iso_make_units_explicit(tibble(x = x, y = y)), "tbl_df")
  expect_equal(names(out), c("x [permil]", "y [not permil]"))
  expect_equal(iso_get_units(out), c(`x [permil]` = NA_character_, `y [not permil]` = NA_character_))
  expect_equal(out[[1]], data)
  expect_equal(out[[2]], data)
  expect_is(out <- iso_make_units_explicit(tibble(x = x, y = y), prefix = ".", suffix = ""), "tbl_df")
  expect_equal(names(out), c("x.permil", "y.not permil"))
  expect_equal(
    iso_make_units_explicit(tibble(x = x, y = y)),
    iso_make_units_explicit(tibble(x = x, y = y)) %>% iso_make_units_explicit()
  )
  expect_error(iso_make_units_implicit(42), "only.*data frames")
  expect_error(iso_make_units_implicit(tibble(), prefix = ""), "must be at least 1")
  expect_equal(iso_make_units_implicit(tibble()), tibble())
  expect_is(out <- iso_make_units_implicit(tibble(`x [permil]` = data, `y [not permil]` = data, other = data)), "tbl_df")
  expect_equal(names(out), c("x", "y", "other"))
  expect_equal(iso_get_units(out), c(x = "permil", y = "not permil", other = NA_character_))
  expect_equal(out$x, x)
  expect_equal(out$y, y)
  expect_equal(out$other, data)
  expect_is(out <- iso_make_units_implicit(tibble(x.permil = data), prefix = ".", suffix = ""), "tbl_df")
  expect_equal(names(out), "x")
  expect_equal(iso_get_units(out), c(x = "permil"))
  expect_equal(iso_make_units_implicit(tibble(`x [permil]` = data, `y [not permil]` = data, other = data)) %>% iso_make_units_explicit(), 
               tibble(`x [permil]` = data, `y [not permil]` = data, other = data))
  expect_equal(iso_make_units_explicit(tibble(x = x, y = y)) %>% iso_make_units_implicit(), tibble(x = x, y = y))
  
  # printout
  expect_equal(vctrs::vec_ptype_full(x), "double in 'permil'")
  expect_equal(vctrs::vec_ptype_abbr(x), "dbl[permil]")
  expect_equal(format(x), format(as.numeric(x)))
  
  # data frames
  expect_equal(vctrs::vec_rbind(tibble(x = x), tibble(x = x))$x, c(x, x))
  expect_equal(vctrs::vec_rbind(tibble(x = x), tibble(x = data))$x, c(data, data))
  expect_warning(
    out <- vctrs::vec_rbind(tibble(x = x), tibble(x = y)),
    "different units"
  )
  expect_equal(out, tibble(x = c(data, data)))
  expect_equal( vctrs::vec_rbind(tibble(x = x), tibble(y = y))$x, vctrs::vec_c(x, rep(NA, length(y))))
  expect_equal( vctrs::vec_rbind(tibble(x = x), tibble(y = y))$y, vctrs::vec_c(rep(NA, length(x)), y))
  expect_equal(
    tibble(a = c("a", "b"), x = purrr::map(a, ~x)) %>% tidyr::unnest(x),
    tibble(a = rep(c("a", "b"), each = length(data)), x = iso_double_with_units(c(data, data), "permil"))
  )
  expect_equal(
    tibble(a = c("a", "b"), x = purrr::map(a, ~tibble(x=x, y=y))) %>% tidyr::unnest(x),
    tibble(a = rep(c("a", "b"), each = length(data)), x = iso_double_with_units(c(data, data), "permil"), y = iso_double_with_units(c(data, data), "not permil"))
  )
  
  # arithmetic
  # plus
  expect_warning(out <- x+y, "different units")
  expect_equal(out, data + data)
  expect_equal(x + x, iso_double_with_units(data + data, "permil"))
  expect_equal(+x, iso_double_with_units(data, "permil"))
  # minus
  expect_warning(out <- x - 2 * y, "different units")
  expect_equal(out, as.numeric(x) - 2*as.numeric(y))
  expect_equal(x - 2*x, iso_double_with_units(-data, "permil"))
  expect_equal(-x, iso_double_with_units(-data, "permil"))
  # multiply
  expect_warning(out <- x * y, "different units")
  expect_equal(out, data * data)
  expect_warning(out <- x * x, "don't know how to calculate")
  expect_equal(out, data * data)
  expect_equal(2 * x, iso_double_with_units(2 * data, "permil"))
  expect_equal(x * 2, iso_double_with_units(2 * data, "permil"))
  # divide
  expect_warning(out <- x / y, "different units")
  expect_equal(out, data / data)
  expect_equal(x / x, data / data)
  expect_equal(x / 2, iso_double_with_units(data / 2, "permil"))
  expect_warning(out <- 2 / x, "don't know how to calculate")
  expect_equal(out, 2 / data)
  # power
  expect_warning(out <- x ^ y, "different units")
  expect_equal(out, data ^ data)
  expect_warning(out <- x ^ x, "don't know how to calculate")
  expect_equal(out, data ^ data)
  
})

test_that("test that vendor data table units conversion works", {
  
  units <- tibble(column = c("x", "y", "z ?"), units = c("s", "", "[V]"))
  df <- tibble(x = 1:5, y = 1:5, `z ?` = 1:5)
  attr(df, "units") <- units
  
  expect_equal(convert_df_units_attr_to_implicit_units(df)$x, iso_double_with_units(1:5, "s"))
  expect_equal(convert_df_units_attr_to_implicit_units(df)$y, 1:5)
  expect_equal(convert_df_units_attr_to_implicit_units(df)$`z ?`, iso_double_with_units(1:5, "V"))
  expect_null(attr(convert_df_units_attr_to_implicit_units(df), "units"))
  expect_warning(convert_df_units_attr_to_implicit_units({df2<-df; df2$x <- "a"; df2}),
                 "encountered non-numeric data table columns")
  
})


# iso_format ====

test_that("test that iso_format works properly", {
  
  expect_error(iso_format(1:5, 1), "unequal lengths")
  x <- 1:2
  expect_equal(
    iso_format(x, b = iso_double_with_units(pi * 1:2, "V"), signif = 3),
    c("x: 1\nb: 3.14V", "x: 2\nb: 6.28V")
  )
  expect_equal(
    iso_format(a = x, b = iso_double_with_units(pi * 1:2, "V"), signif = 3),
    c("a: 1\nb: 3.14V", "a: 2\nb: 6.28V")
  )
  expect_equal(
    iso_format(a = x, b = iso_double_with_units(pi * 1:2, "V"), signif = 4),
    c("a: 1\nb: 3.142V", "a: 2\nb: 6.283V")
  )
  expect_equal(
    iso_format(x, iso_double_with_units(pi * 1:2, "V"), signif = 3, format_names = NULL),
    c("1\n3.14V", "2\n6.28V")
  )
  expect_equal(
    iso_format(x, iso_double_with_units(pi * 1:2, "V"), signif = 3, format_names = NULL, format_units = NULL),
    c("1\n3.14", "2\n6.28")
  )
  
})

