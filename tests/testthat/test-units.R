context("Units")

test_that("test that units class works properly", {
  
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
  # double
  expect_is(vctrs::vec_ptype2(x, double()), "numeric")
  expect_is(vctrs::vec_ptype2(double(), x), "numeric")
  expect_is(vctrs::vec_cast(x, double()), "numeric")
  expect_equal(vctrs::vec_c(x, 4.2), c(as.numeric(x), 4.2))
  expect_equal(vctrs::vec_c(4.2, x), c(4.2, as.numeric(x)))
  # integer
  expect_is(vctrs::vec_ptype2(x, integer()), "numeric")
  expect_is(vctrs::vec_ptype2(integer(), x), "numeric")
  expect_is(vctrs::vec_cast(x, integer()), "integer")
  expect_equal(vctrs::vec_c(x, 42L), c(as.numeric(x), 42.))
  expect_equal(vctrs::vec_c(42L, x), c(42., as.numeric(x)))
  # others
  expect_error(vctrs::vec_ptype2(x, logical()))
  expect_error(vctrs::vec_ptype2(logical(), x))
  expect_error(vctrs::vec_ptype2(x, character()))
  expect_error(vctrs::vec_ptype2(character(), x))
  expect_error(vctrs::vec_ptype2(x, factor()))
  expect_error(vctrs::vec_ptype2(factor(), x))
  
  # printout
  expect_equal(vctrs::vec_ptype_full(x), "double in 'permil'")
  expect_equal(vctrs::vec_ptype_abbr(x), "dbl[permil]")
  expect_equal(format(x), format(as.numeric(x)))
  
  # data frames
  
  
  # arithmetic
  
  
})