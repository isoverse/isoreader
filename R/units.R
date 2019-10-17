# Class Definitions ======

# double with units constructor
new_iso_double_with_units <- function(x = double(), units = "undefined units") {
  vctrs::vec_assert(x, ptype = double())
  vctrs::vec_assert(units, ptype = character(), size = 1)
  if (is.na(units[1])) stop("units must be set (NA is not permissible)", call. = FALSE)
  vctrs::new_vctr(x, units = units, class = "iso_double_with_units")
}

iso_double_with_units <- function(x = double(), units = "undefined units") {
  x <- vctrs::vec_cast(x, double())
  units <- vctrs::vec_recycle(vctrs::vec_cast(units, character()), 1L)
  new_iso_double_with_units(x, units = units)
}

#' @importFrom methods setOldClass
methods::setOldClass(c("iso_double_with_units", "vctrs_vctr"))

# check if something is a double with units
iso_is_double_with_units <- function(x) {
  inherits(x, "iso_double_with_units")
}

# retrieve the units attribute
iso_get_units <- function(x) attr(x, "units")

# check for identical units (convenience function)
check_units_identical <- function(x, y, warn_if_not = FALSE) {
  check <- identical(attr(x, "units"), attr(y, "units"))
  if (!check && warn_if_not) {
    glue::glue(
      "don't know how to reconcile different units '{iso_get_units(x)}' and ",
      "'{iso_get_units(y)}', converting to double without units to continue") %>% 
      warning(call. = FALSE, immediate. = TRUE)
  }
  return(check)
}

# formatting during printout
vec_ptype_full.iso_double_with_units <- function(x, ...) {
  sprintf("%s in '%s'", vctrs::vec_ptype_full(vctrs::vec_data(x), ...), iso_get_units(x))
}
format.iso_double_with_units <- function(x, ...) {
  format(vctrs::vec_data(x), ...)
}
vec_ptype_abbr.iso_double_with_units <- function(x, ...) {
  if (check_units_identical(x, new_iso_double_with_units())) x_units <- "undef"
  else x_units <- iso_get_units(x)
  sprintf("%s[%s]", vctrs::vec_ptype_abbr(vctrs::vec_data(x), ...), x_units)
}

# Combinations and Casting ======
#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 iso_double_with_units
#' @export
#' @export vec_ptype2.iso_double_with_units
vec_ptype2.iso_double_with_units <- function(x, y, ...) UseMethod("vec_ptype2.iso_double_with_units", y)
#' @method vec_ptype2.iso_double_with_units default
#' @export
vec_ptype2.iso_double_with_units.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @importFrom vctrs vec_cast
#' @method vec_cast iso_double_with_units
#' @export
#' @export vec_cast.iso_double_with_units
vec_cast.iso_double_with_units <- function(x, to, ...) UseMethod("vec_cast.iso_double_with_units")
#' @method vec_cast.iso_double_with_units default
#' @export
vec_cast.iso_double_with_units.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

# combining doubles with units: only allow it if they have the same units
#' @method vec_ptype2.iso_double_with_units iso_double_with_units
#' @export
vec_ptype2.iso_double_with_units.iso_double_with_units <- function(x, y, ...) {
  if (check_units_identical(x, y, warn_if_not = TRUE)) {
    # units are the same, keep it a double with units
    new_iso_double_with_units(units = iso_get_units(x))
  } else {
    # convert to a double without units
    double()
  }
}

#' @method vec_cast.iso_double_with_units iso_double_with_units
#' @export
vec_cast.iso_double_with_units.iso_double_with_units <- function(x, to, ...)  {
  if (check_units_identical(x, to, warn_if_not = TRUE)) {
    # has the right units so there isn't anything to do for casting
    return(x)
  } else {
    # convert to a double without units
    return(vctrs::vec_data(x))
  } 
}

# combining a double with units with a double without units yields a double without units
#' @method vec_ptype2.iso_double_with_units double
#' @export
vec_ptype2.iso_double_with_units.double <- function(x, y, ...) double()
#' @importFrom vctrs vec_ptype2.double
#' @method vec_ptype2.double iso_double_with_units
#' @export
vec_ptype2.double.iso_double_with_units <- function(x, y, ...) double()

# combining a double with units with an integer yields a double without units
#' @method vec_ptype2.iso_double_with_units integer
#' @export
vec_ptype2.iso_double_with_units.integer <- function(x, y, ...) double()
#' @importFrom vctrs vec_ptype2.integer
#' @method vec_ptype2.integer iso_double_with_units
#' @export
vec_ptype2.integer.iso_double_with_units <- function(x, y, ...) double()

# no other vec_c combinations are formally allowed for now
# since double with units + character/logical/factor should really not happen

# cast from double with units to any other format makes it behave like a double
# this allows c() to work as if it was a double (but other vec_c combinations are not allowed)
#' @importFrom vctrs vec_cast.double
#' @method vec_cast.double iso_double_with_units
#' @export
vec_cast.double.iso_double_with_units <- function(x, to, ...) vctrs::vec_data(x)
#' @importFrom vctrs vec_cast.integer
#' @method vec_cast.integer iso_double_with_units
#' @export
vec_cast.integer.iso_double_with_units <- function(x, to, ...) as.integer(vctrs::vec_data(x))
#' @importFrom vctrs vec_cast.character
#' @method vec_cast.character iso_double_with_units
#' @export
vec_cast.character.iso_double_with_units <- function(x, to, ...) as.character(vctrs::vec_data(x))
#' @importFrom vctrs vec_cast.logical
#' @method vec_cast.logical iso_double_with_units
#' @export
vec_cast.logical.iso_double_with_units <- function(x, to, ...) as.logical(vctrs::vec_data(x))
#' @importFrom vctrs vec_cast.factor
#' @method vec_cast.factor iso_double_with_units
#' @export
vec_cast.factor.iso_double_with_units <- function(x, to, ...) as.factor(vctrs::vec_data(x))

# Arithmetic =======

#' @importFrom vctrs vec_arith
#' @method vec_arith iso_double_with_units
#' @export
#' @export vec_arith.iso_double_with_units
vec_arith.iso_double_with_units <- function(op, x, y, ...) {
  UseMethod("vec_arith.iso_double_with_units", y)
}
#' @method vec_arith.iso_double_with_units default
#' @export
vec_arith.iso_double_with_units.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

downcast_for_unknown_op <- function(op, x, y, warn = TRUE) {
  glue::glue(
    "don't know how to calculate <{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}>, ",
    "converting to double without units to continue"
  ) %>% warning(call. = FALSE, immediate. = TRUE)
  vec_arith_base(op, x, y)
}

# combining two units objects (if their units are identical)
# allow + and -, and / looses the units
# all other cases convert to double with a warning
#' @method vec_arith.iso_double_with_units iso_double_with_units
#' @export
vec_arith.iso_double_with_units.iso_double_with_units <- function(op, x, y, ...) {
  if (check_units_identical(x, y, warn_if_not = TRUE)) {
    switch(
      op,
      "+" = ,
      "-" = new_iso_double_with_units(vec_arith_base(op, x, y), units = iso_get_units(x)),
      "/" = vec_arith_base(op, x, y),
      # downcast to double
      downcast_for_unknown_op(op, x, y)
    )
  } else {
    # downcast to double
    downcast_for_unknown_op(op, x, y, warn = FALSE)
  }
}

# combining a units object with a number
# allow division and multiplication
# all other cases convert to double with a warning
#' @method vec_arith.iso_double_with_units numeric
#' @export
vec_arith.iso_double_with_units.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_iso_double_with_units(vec_arith_base(op, x, y), units = iso_get_units(x)),
    downcast_for_unknown_op(op, x, y)
  )
}
# combining a number with a units object
# allow multiplication, everything else convert to double with warning
#' @importFrom vctrs vec_arith.numeric
#' @method vec_arith.numeric iso_double_with_units
#' @export
vec_arith.numeric.iso_double_with_units <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_iso_double_with_units(vec_arith_base(op, x, y), units = iso_get_units(y)),
    downcast_for_unknown_op(op, x, y)
  )
}

# missing scenarios for unary +x and -x
#' @method vec_arith.iso_double_with_units MISSING
#' @export
vec_arith.iso_double_with_units.MISSING <- function(op, x, y, ...) {
  switch(op, 
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
