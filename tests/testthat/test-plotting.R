context("Plotting functions")

test_that("test that raw data plot throws appropriate errors", {
  expect_error(plot_raw_data(42), "can only plot iso files")
})

test_that("test that plot continuous flow works properly", {
  
  expect_error(plot_continuous_flow(42), "can only plot continuous flow")
  expect_is(cf <- isoreader:::make_cf_data_structure() %>% isoreader:::update_read_options(read_raw_data = TRUE), "continuous_flow")
  expect_error(plot_raw_data(cf), "no raw data in supplied isofiles")
  
  # make test raw data
  cf$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10))
  
  # test for errors
  expect_error(plot_raw_data(cf, c("42")), "not available in the provided isofiles")
  expect_error(cf %>% {.$raw_data$time.min = 1:10; .} %>% plot_raw_data(.), "unclear which column is the time column")
  expect_error(plot_raw_data(cf, time_interval = 55), "time interval needs to be a vector with two numeric entries")
  expect_error(plot_raw_data(cf, panel_by = "42"), "unknown layout specification")
  expect_error(plot_raw_data(cf, color_by = "42"), "unknown layout specification")
  expect_error(plot_raw_data(cf, linetype_by = "42"), "unknown layout specification")
  
  # generate plot
  cf <- calculate_ratios(cf, "46/44")
  expect_message(p <- plot_raw_data(cf, c("46/44", "44"), quiet = FALSE), "plotting data")
  expect_true(is.ggplot(p))
  expect_silent(plot_raw_data(cf, "44", quiet = TRUE))
  expect_true(all(p$data$data %in% c("44 [mV]", "46/44"))) # only these datas selected
  expect_true(identical(p$data$data %>% levels(), c("46/44", "44 [mV]")))
  
  # aesthetics, mapping, panelling formatting tests - defaults first
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group")))
  expect_equal(as.character(p$mapping$colour), "file_id")
  expect_equal(as.character(p$mapping$x), "time")
  expect_equal(as.character(p$mapping$y), "value")
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "data")
  expect_equal(names(p$facet$params$cols), NULL)
  
  # then custom specifications
  expect_true(is.ggplot(p <- plot_raw_data(cf, panel_by = "none", color_by = "data", linetype_by = "file")))
  expect_true(all(p$data$data %in% c("44 [mV]", "46 [mV]", "46/44"))) # all selected by default
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "linetype")))
  expect_equal(as.character(p$mapping$colour), "data")
  expect_equal(as.character(p$mapping$linetype), "file_id")
  expect_equal(class(p$facet)[1], "FacetNull")
  expect_true(is.ggplot(p <- plot_raw_data(cf, "44", panel_by = "file", color_by = "none", linetype_by = "data")))
  expect_true(all(names(p$mapping) %in% c("x", "y", "group", "linetype")))
  expect_equal(as.character(p$mapping$linetype), "data")
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "file_id")
  expect_equal(names(p$facet$params$cols), NULL)
  
})

test_that("test that plot dual inlet works properly", {
  
  expect_error(plot_dual_inlet(42), "can only plot dual inlet")
  expect_is(di <- isoreader:::make_di_data_structure() %>% isoreader:::update_read_options(read_raw_data = TRUE), "dual_inlet")
  expect_error(plot_raw_data(di), "no raw data in supplied isofiles")

  # make test raw data
  di$raw_data <- data_frame(type = rep(c("standard", "sample"), each = 5), cycle = rep(1:5, times = 2), v44.mV = runif(10), v46.mV = runif(10))
  
  # test for errors
  expect_error(plot_raw_data(di, panel_by = "42"), "unknown layout specification")
  expect_error(plot_raw_data(di, color_by = "42"), "unknown layout specification")
  expect_error(plot_raw_data(di, linetype_by = "42"), "unknown layout specification")
  expect_error(plot_raw_data(di, shape_by = "42"), "unknown layout specification")
  
  # generate plot
  di <- calculate_ratios(di, "46/44")
  expect_message(p <- plot_raw_data(di, c("46/44", "44"), quiet = FALSE), "plotting data")
  expect_true(is.ggplot(p))
  expect_silent(plot_raw_data(di, "44", quiet = TRUE))
  expect_true(all(p$data$data %in% c("44 [mV]", "46/44"))) # only these datas selected
  expect_true(identical(p$data$data %>% levels(), c("46/44", "44 [mV]")))
  
  # aesthetics, mapping, panelling formatting tests - defaults first
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "shape")))
  expect_equal(as.character(p$mapping$colour), "file_id")
  expect_equal(as.character(p$mapping$x), "cycle")
  expect_equal(as.character(p$mapping$y), "value")
  expect_equal(as.character(p$mapping$shape), "type")
  expect_equal(class(p$facet)[1], "FacetWrap")
  expect_equal(names(p$facet$params$facets), "data")
  
  # then custom specifications
  expect_true(is.ggplot(p <- plot_raw_data(di, panel_by = "none", color_by = "data", linetype_by = "file", shape_by = "none")))
  expect_true(all(p$data$data %in% c("44 [mV]", "46 [mV]", "46/44"))) # all selected by default
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "linetype")))
  expect_equal(as.character(p$mapping$colour), "data")
  expect_equal(as.character(p$mapping$linetype), "file_id")
  expect_equal(class(p$facet)[1], "FacetNull")
  expect_true(is.ggplot(p <- plot_raw_data(di, "44", panel_by = "file", color_by = "SA|STD", linetype_by = "data", shape_by = "file")))
  expect_true(all(names(p$mapping) %in% c("x", "y", "group", "colour", "linetype", "shape")))
  expect_equal(as.character(p$mapping$colour), "type")
  expect_equal(as.character(p$mapping$linetype), "data")
  expect_equal(as.character(p$mapping$shape), "file_id")
  expect_equal(class(p$facet)[1], "FacetWrap")
  expect_equal(names(p$facet$params$facets), "file_id")
})