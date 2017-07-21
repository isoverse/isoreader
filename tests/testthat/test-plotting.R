context("Plotting functions")

test_that("test that raw data plot throws appropriate errors", {
  expect_error(isoplot_raw_data(42), "can only plot iso files")
})

test_that("test that plot continuous flow works properly", {
  
  expect_error(isoplot_continuous_flow(42), "can only plot continuous flow")
  expect_is(cf <- isoreader:::make_cf_data_structure() %>% isoreader:::update_read_options(read_raw_data = TRUE), "continuous_flow")
  expect_error(isoplot_raw_data(cf), "no raw data in supplied isofiles")
  
  # make test raw data
  cf$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10))
  expect_error(isoplot_raw_data(cf, masses = c(), ratios = c()), "must specify at least one mass or ratio")
  expect_error(isoplot_raw_data(cf, ratios = c("42")), "invalid ratio")
  expect_error(isoplot_raw_data(cf, ratios = c("3/2")), "mass\\(es\\) not available in the provided isofiles")
  expect_error(cf %>% {.$raw_data$time.min = 1:10; .} %>% isoplot_raw_data(.), "unclear which column is the time column")
  expect_error(isoplot_raw_data(cf, time_interval = 55), "time interval needs to be a vector with two numeric entries")
  expect_error(isoplot_raw_data(cf, panels = "42"), "unknown layout specification")
  expect_error(isoplot_raw_data(cf, colors = "42"), "unknown layout specification")
  expect_error(isoplot_raw_data(cf, linetypes = "42"), "unknown layout specification")
  expect_is(p <- isoplot_raw_data(cf, ratios = c("46/44")), "ggplot")
  
  
})

test_that("test that plot dual inlet works properly", {
  
  expect_error(isoplot_dual_inlet(42), "can only plot dual inlet")
  expect_is(di <- isoreader:::make_di_data_structure() %>% isoreader:::update_read_options(read_raw_data = TRUE), "dual_inlet")
  expect_error(isoplot_raw_data(di), "no raw data in supplied isofiles")
  
  # make test raw data
  di$raw_data <- data_frame(type = rep(c("standard", "sample"), each = 5), cycle = rep(1:5, times = 2), v44.mV = runif(10), v46.mV = runif(10))
  expect_error(isoplot_raw_data(di, panels = "42"), "unknown layout specification")
  expect_error(isoplot_raw_data(di, colors = "42"), "unknown layout specification")
  expect_error(isoplot_raw_data(di, linetypes = "42"), "unknown layout specification")
  expect_error(isoplot_raw_data(di, shapes = "42"), "unknown layout specification")
  expect_is(p <- isoplot_raw_data(di, ratios = c("46/44")), "ggplot")
  
  
})