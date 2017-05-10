#' get the scaling factor for a simple si unit prefix
#' does not currently process compound units (e.g. m/s) or powers (e.g. cm2 s-1)
#' @param unit the unit to find the suffix for (e.g. mm, kg, ms, nA, kV)
#' @param suffix the expected suffix (e.g. m, g, s, A, V)
get_si_prefix_scaling <- function(unit, suffix) {
  # supported prefixes
  prefix <- c(f = 1e-15, p = 1e-12, n = 1e-9, "\U00B5" = 1e-6, m = 1e-3, 1,
              k = 1e3, M = 1e6, G = 1e9, T = 1e12)

  # generate pattern
  prefix_pattern <- prefix %>% names() %>% str_c(collapse="|")
  pattern <- sprintf("^(%s)%s$", prefix_pattern, suffix)
  prefixes <- unit %>% str_match(pattern) %>% { .[,2] }
  if (any(is.na(prefixes))) {
    stop("Encountered unrecognized units: ", unit[is.na(prefixes)] %>% str_c(collapse = ", "),
         ". Supported are for this suffix: ", prefix %>% names() %>% str_c(suffix) %>% str_c(collapse = ", "),
         call. = FALSE)
  }

  # scaling
  prefix[sapply(prefixes, function(i) which(i == names(prefix)))]
}
