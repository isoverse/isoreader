context("Plotting functions")

test_that("test that raw data plot throws appropriate errors", {
  expect_error(iso_plot_raw_data(42), "can only plot iso files")
})

test_that("test that plot continuous flow works properly", {
  
  expect_error(iso_plot_continuous_flow_data(42), "can only plot continuous flow")
  expect_is(cf <- isoreader:::make_cf_data_structure() %>% 
              isoreader:::update_read_options(read_file_info = TRUE, read_raw_data = TRUE), "continuous_flow")
  expect_error(iso_plot_raw_data(cf), "no raw data in supplied iso_files")
  
  # make test raw data
  cf$raw_data <- data_frame(tp = 1:10, time.s = tp*0.2, v44.mV = runif(10), v46.mV = runif(10))
  
  # test for errors
  expect_error(iso_plot_raw_data(cf, c("42")), "not available in the provided iso_files")
  expect_error(cf %>% {.$raw_data$time.min = 1:10; .} %>% iso_plot_raw_data(.), "unclear which column is the time column")
  expect_error(iso_plot_raw_data(cf, time_interval = 55), "time interval needs to be a vector with two numeric entries")
  expect_error(iso_plot_raw_data(cf, panel = DNE), "unknown column")
  expect_error(iso_plot_raw_data(cf, color = DNE), "not.*valid")
  expect_error(iso_plot_raw_data(cf, linetype = DNE), "not.*valid")
  expect_error(iso_plot_raw_data(cf, label = DNE), "not.*valid")
  
  # generate plot
  cf <- iso_calculate_ratios(cf, "46/44")
  expect_message(p <- iso_plot_raw_data(cf, c("46/44", "44"), quiet = FALSE), "plotting data")
  expect_true(is.ggplot(p))
  expect_silent(iso_plot_raw_data(cf, "44", quiet = TRUE))
  expect_true(all(p$data$data %in% c("44 [mV]", "46/44"))) # only these datas selected
  expect_true(identical(p$data$data %>% levels(), c("46/44", "44 [mV]")))
  
  # aesthetics, mapping, panelling formatting tests - defaults first
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "label")))
  expect_true("file_id" %in% as.character(p$mapping$colour))
  expect_true("time" %in% as.character(p$mapping$x))
  expect_true("value" %in% as.character(p$mapping$y))
  expect_true("file_id" %in% as.character(p$mapping$label))
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "data")
  expect_equal(names(p$facet$params$cols) %>% length(), 0)
  
  # then custom specifications
  expect_true(is.ggplot(p <- iso_plot_raw_data(cf, panel = NULL, color = data, linetype = file_id)))
  expect_true(all(p$data$data %in% c("44 [mV]", "46 [mV]", "46/44"))) # all selected by default
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "linetype", "label")))
  expect_true("data" %in% as.character(p$mapping$colour))
  expect_true("file_id" %in% as.character(p$mapping$linetype))
  expect_equal(class(p$facet)[1], "FacetNull")
  expect_true(is.ggplot(p <- iso_plot_raw_data(cf, "44", panel = file_id, color = NULL, linetype = data)))
  expect_true(all(names(p$mapping) %in% c("x", "y", "group", "linetype", "label")))
  expect_true("data" %in% as.character(p$mapping$linetype))
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "file_id")
  expect_equal(names(p$facet$params$cols) %>% length(), 0)
  
})

test_that("test that plot dual inlet works properly", {
  
  expect_error(iso_plot_dual_inlet_data(42), "can only plot dual inlet")
  expect_is(di <- isoreader:::make_di_data_structure() %>% isoreader:::update_read_options(read_raw_data = TRUE, read_file_info = TRUE), "dual_inlet")
  expect_error(iso_plot_raw_data(di), "no raw data in supplied iso_files")

  # make test raw data
  di$raw_data <- data_frame(type = rep(c("standard", "sample"), each = 5), cycle = rep(1:5, times = 2), v44.mV = runif(10), v46.mV = runif(10))
  
  # test for errors
  expect_error(iso_plot_raw_data(di, panel = DNE), "unknown column")
  expect_error(iso_plot_raw_data(di, panel = DNE ~ data), "unknown column")
  expect_error(iso_plot_raw_data(di, panel = data ~ DNE), "unknown column")
  expect_error(iso_plot_raw_data(di, color = DNE), "not.*valid")
  expect_error(iso_plot_raw_data(di, linetype = DNE), "not.*valid")
  expect_error(iso_plot_raw_data(di, shape = DNE), "not.*valid")
  expect_error(iso_plot_raw_data(di, label = DNE), "not.*valid")
  
  # generate plot
  di <- iso_calculate_ratios(di, "46/44")
  expect_message(p <- iso_plot_raw_data(di, c("46/44", "44"), quiet = FALSE), "plotting data")
  expect_true(is.ggplot(p))
  expect_silent(iso_plot_raw_data(di, "44", quiet = TRUE))
  expect_true(all(p$data$data %in% c("44 [mV]", "46/44"))) # only these datas selected
  expect_true(identical(p$data$data %>% levels(), c("46/44", "44 [mV]")))
  
  # aesthetics, mapping, panelling formatting tests - defaults first
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "shape", "label")))
  expect_true("file_id" %in% as.character(p$mapping$colour))
  expect_true("cycle" %in% as.character(p$mapping$x))
  expect_true("value" %in% as.character(p$mapping$y))
  expect_true("type" %in% as.character(p$mapping$shape))
  expect_true("file_id" %in% as.character(p$mapping$label))
  expect_equal(class(p$facet)[1], "FacetWrap")
  expect_equal(names(p$facet$params$facets), "data")
  
  # then custom specifications
  expect_true(is.ggplot(p <- iso_plot_raw_data(di, panel = NULL, color = data, linetype = file_id, shape = NULL)))
  expect_true(all(p$data$data %in% c("44 [mV]", "46 [mV]", "46/44"))) # all selected by default
  expect_true(all(names(p$mapping) %in% c("colour", "x", "y", "group", "linetype", "label")))
  expect_true("data" %in% as.character(p$mapping$colour))
  expect_true("file_id" %in% as.character(p$mapping$linetype))
  expect_equal(class(p$facet)[1], "FacetNull")
  expect_true(is.ggplot(p <- iso_plot_raw_data(di, "44", panel = file_id, color = type, linetype = data, shape = file_id)))
  expect_true(all(names(p$mapping) %in% c("x", "y", "group", "colour", "linetype", "shape", "label")))
  expect_true("type" %in% as.character(p$mapping$colour))
  expect_true("data" %in% as.character(p$mapping$linetype))
  expect_true("file_id" %in% as.character(p$mapping$shape))
  expect_equal(class(p$facet)[1], "FacetWrap")
  expect_equal(names(p$facet$params$facets), "file_id")
  
  expect_true(is.ggplot(p <- iso_plot_raw_data(di, "44", panel = file_id ~ data)))
  expect_equal(class(p$facet)[1], "FacetGrid")
  expect_equal(names(p$facet$params$rows), "file_id")
  expect_equal(names(p$facet$params$cols), "data")
  
})

