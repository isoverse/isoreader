# plotting functions ==== 

#' Plot raw data from isoreader files
#' 
#' Convenience function for making standard plots for raw isoreader data. Calls \code{\link{plot_continuous_flow}} and \code{\link{plot_dual_inlet}} for data specific plotting (see those functions for parameter details).
#' 
#' @inheritParams aggregate_raw_data
#' @param ... parameters for the data specific plotting functions
#' @family plot functions
#' @export
plot_raw_data <- function(isofiles, ..., quiet = setting("quiet")) {
  if(!is_iso_object(isofiles)) stop("can only plot iso files or lists of iso files", call. = FALSE)
  
  isofiles <- as_isofile_list(isofiles)
  
  if (!quiet) sprintf("Info: plotting data from %d data file(s)", length(isofiles)) %>% message()
  
  if (is_continuous_flow(isofiles))
    plot_continuous_flow (isofiles, ...)
  else if (is_dual_inlet(isofiles))
    plot_dual_inlet (isofiles, ...)
  else
    stop("plotting of this type of isofiles not yet supported", call. = FALSE) 
}

#' Plot chromatogram from continuous flow data
#'
#' @inheritParams plot_raw_data
#' @param datasets which masses and ratios to plot (e.g. c("44", "45", "45/44")), if omitted, all available masses and ratios are plotted. Note that ratios should be calculated using \code{\link{calculate_ratios}} prior to plotting.
#' @param time_interval which time interval to plot
#' @param time_interval_units which units the time interval is in, default is "seconds"
#' @param normalize whether to normalize all dataset (default is FALSE, i.e. no normalization). If TRUE, normalizes each trace across all files. Normalizing always scales such that each trace fills the entire height of the plot area. Note that zooming (if \code{zoom} is set) is applied after normalizing.
#' @param zoom if not set, automatically scales to the maximum range in the selected time_interval in each panel. If set, scales by the indicated factor, i.e. values > 1 are zoom in, values < 1 are zoom out, baseline always remains the anchor point. Note that for overlay plots (\code{panel_bu = "none"}) zooming is relative to the max in each panel (potentially across different dataset). Also note that zooming only affects masses, ratios are not zoomed.
#' @param panel_by whether to panel data by anything, options are "none" (overlay all), "dataset" (by mass/ratio dataset), "file" (panel by files). The default is "dataset"
#' @param color_by whether to color data by anything, options are the same as for \code{panel_by} but the default is "file"
#' @param linetype_by whether to differentiate data by linetype, options are the same as for \code{panel_by} but the default is "none". Note that a limited number of linetype_by (6) is defined by default and the plot will fail if a higher number is required unless specified using \code{\link[ggplot2]{scale_linetype}}
#' @family plot functions
#' @export
plot_continuous_flow <- function(
  isofiles, datasets, time_interval = c(), time_interval_units = "seconds", normalize = FALSE, zoom = NULL, 
  panel_by = "dataset", color_by = "file", linetype_by = "none") {
  
  # checks and defaults
  if(!is_continuous_flow(isofiles)) stop("can only plot continuous flow isofiles", call. = FALSE)
  if (!all(ok <- c(panel_by, color_by, linetype_by) %in% c("none", "dataset", "file")))
    stop("unknown layout specification: '", str_c(c(panel_by, color_by, linetype_by)[!ok], collapse = "', '"),
         "'. Please use 'none', 'dataset' or 'file' for panel_by, color_by and linetype_by specifications.", call. = FALSE)
  
  # global vars
  #time <- type <- column <- value <- file_id <- label_with_units <- NULL
  #is_ratio <- max_signal <- baseline <- cutoff <- discard <- change <- border <- gap <- NULL
  
  # collect raw data
  raw_data <- aggregate_raw_data(isofiles, gather = TRUE, quiet = TRUE)
  if (nrow(raw_data) == 0) stop("no raw data in supplied isofiles", call. = FALSE)
  
  # only work with desired datasets (masses and ratios)
  datasets <- if(missing(datasets)) unique(raw_data$dataset) else as.character(datasets)
  if ( length(missing <- setdiff(datasets, unique(raw_data$dataset))) > 0 ) 
    stop("dataset(s) not available in the provided isofiles: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- filter(raw_data, dataset %in% datasets)
  
  # time column
  time_pattern <- "^time\\.(.*)$"
  time_column <- str_subset(names(raw_data), time_pattern)
  if (length(time_column) != 1) 
    stop("unclear which column is the time column, consider an explicit 'convert_time' call before plotting, found: ", 
         str_c(time_column, collapse = ", "), call. = FALSE)
  time_unit <- str_match(time_column, time_pattern) %>% {.[2] }
  raw_data$time <- raw_data[[time_column]]
  
  # time interval
  if (length(time_interval) == 2) {
    time_interval <- scale_time(time_interval, to = time_unit, from = time_interval_units)
  } else if (length(time_interval) > 0 && length(time_interval) != 2)
    stop("time interval needs to be a vector with two numeric entries, found: ", str_c(time_interval, collapse = ", "), call. = FALSE)
  time_unit <- sprintf("[%s]", time_unit)
  
  # border extrapolation function
  extrapolate_border <- function(cutoff, border, change, time, value) {
    cutoff <- unique(cutoff)
    if (length(cutoff) != 1) stop("problematic cutoff", call. = FALSE)
    bi <- which(border) # border indices
    bi2 <- bi + 1 - change[bi] # end point of border
    bi1 <- bi - change[bi] # start point of border
    border_time <- (cutoff - value[bi1])/(value[bi2] - value[bi1]) * (time[bi2] - time[bi1]) + time[bi1]
    time[bi] <- border_time
    return(time)
  }
  
  # normalize data
  normalize_data <- function(data) {
    group_by(data, dataset) %>%
      mutate(value = (value - min(value, na.rm = TRUE))/
               (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>% 
      ungroup()
  }
  
  # plot data
  zoom_grouping <- if(panel_by == "dataset") "dataset" else if (panel_by == "file") "file_id" else c()
  plot_data <- 
    raw_data %>% 
    # make ratio identification simple
    mutate(is_ratio = category == "ratio") %>% 
    arrange(time) %>% 
    # find global zoom cutoffs per panel before time filtering (don't consider ratios)
    { 
      if (!is.null(zoom)) {
        group_by_(., .dots = zoom_grouping) %>% 
          mutate(
            baseline = value[!is_ratio] %>% { if(length(.) == 0) NA else min(.) },
            max_signal = value[!is_ratio] %>% { if(length(.) == 0) NA else max(.) }) %>%
          ungroup() 
      } else . 
    } %>% 
    # time filtering
    { if (length(time_interval) == 2) filter(., time >= time_interval[1], time <= time_interval[2]) else . } %>% 
    # normalizing
    {
      if (normalize) {
        normalize_data(.) %>% 
          # zooming always based on the full (0 to 1) interval
          mutate(baseline = 0, max_signal = 1)
      } else .
    } %>% 
    # zooming
    { 
      if (!is.null(zoom)) {
        # extrapolate data (multi-panel requires this instead of coord_cartesian)
        group_by(., file_id, dataset) %>% 
          mutate(
            cutoff = 1/zoom * max_signal + (1 - 1/zoom) * baseline, # cutoffs
            discard = ifelse(!is_ratio, value > cutoff, FALSE), # never zoom ratios
            change = c(0, diff(discard)), # check for where the cutoffs
            border = change == 1 | c(change[-1], 0) == -1, # identify borders
            gap = c(0, change[-n()]) == 1, # identify gaps next to one border so ggplot knows the line is interrupted
            time = extrapolate_border(cutoff, border, change, time, value), # calculate time at border
            value = ifelse(border, cutoff, ifelse(gap, NA, value)) # assign values
          ) %>% 
          ungroup() %>% 
          filter(!discard | border | gap) %>% 
          { # re-normalize after zooming if normalizing is turned on
            if (normalize) normalize_data(.)
            else .
          }
      } else . 
    } %>% 
    # dataset with units and in correct order
    mutate(
      dataset_with_units = ifelse(!is.na(units), str_c(dataset, " [", units, "]"), dataset)
    ) %>% {
      dataset_levels <- deframe(select(., dataset, dataset_with_units))[datasets]
      mutate(., dataset = factor(dataset_with_units, levels = dataset_levels))
    }
  
  # generate plot
  p <- plot_data %>% 
    ggplot() + 
    aes(time, value, group = paste(file_id, dataset)) +
    geom_line() +
    scale_x_continuous(str_c("Time ", time_unit), expand = c(0, 0)) +
    scale_y_continuous(if(normalize) "Normalized Signal" else "Signal", expand = c(0, 0)) +
    theme_bw()
  
  # display full time scale
  if (length(time_interval) == 2)
    p <- p + expand_limits(x = time_interval)
  
  # normalize plot y axis
  if (normalize)
    p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  # labels
  datasets_label <- "Dataset"
  files_label <- "File"
  
  # paneling
  if ( panel_by == "dataset") 
    p <- p + facet_grid(dataset~., scales = "free_y") 
  else if (panel_by == "file") 
    p <- p + facet_grid(file_id~., scales = "free_y")

  # color_by
  if (color_by == "dataset") 
    p <- p %+% aes(color = dataset) + labs(color = datasets_label)
  else if (color_by == "file") 
    p <- p %+% aes(color = file_id) + labs(color = files_label)
  
  # linetype_by
  if (linetype_by == "dataset") 
    p <- p %+% aes(linetype = dataset) + labs(linetype = datasets_label)
  else if (linetype_by == "file") 
    p <- p %+% aes(linetype = file_id) + labs(linetype = files_label)
  
  # return plot
  return(p)
}

#' Plot mass data from dual inlet files
#' 
#' @inheritParams plot_continuous_flow
#' @param panel_by whether to panel data by anything, options are "none" (overlay all), "dataset" (by mass/ratio dataset), "file" (panel by files), "SA|STD" (panel by sample|standard). The default is "dataset"
#' @param shape_by whether to shape data points by anything, options are the same as for panel_by but the default is "SA|STD" (sample|standard)
#' @note normalization is not useful for dual inlet data, except potentially between standard and sample - however, for this it is more meaningful to simply plot the relevant ratios together
plot_dual_inlet <- function(
  isofiles, datasets, 
  panel_by = "dataset", color_by = "file", linetype_by = "none", shape_by = "SA|STD") {
  
  # checks
  if(!is_dual_inlet(isofiles)) stop("can only plot dual inlet isofiles", call. = FALSE)
  if (!all(ok <- c(panel_by, color_by, linetype_by, shape_by) %in% c("none", "dataset", "file", "SA|STD")))
    stop("unknown layout specification: '", str_c(c(panel_by, color_by, linetype_by, shape_by)[!ok], collapse = "', '"),
         "'. Please use 'none', 'dataset', 'file' or 'SA|STD' for panel_by, color_by, shape_by and linetype_by specifications.", call. = FALSE)
  
  # global vars
  #time <- type <- column <- value <- file_id <- label_with_units <- NULL
  #is_ratio <- max_signal <- baseline <- cutoff <- discard <- change <- border <- gap <- NULL
  
  # collect raw data
  raw_data <- aggregate_raw_data(isofiles, gather = TRUE, quiet = TRUE)
  if (nrow(raw_data) == 0) stop("no raw data in supplied isofiles", call. = FALSE)

  # only work with desired datasets (masses and ratios)
  datasets <- if(missing(datasets)) unique(raw_data$dataset) else as.character(datasets)
  if ( length(missing <- setdiff(datasets, unique(raw_data$dataset))) > 0 ) 
    stop("dataset(s) not available in the provided isofiles: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- filter(raw_data, dataset %in% datasets)
  
  # plot data
  plot_data <- 
    raw_data %>% 
    # dataset with units and in correct order
    mutate(
      dataset_with_units = ifelse(!is.na(units), str_c(dataset, " [", units, "]"), dataset)
    ) %>% {
      dataset_levels <- deframe(select(., dataset, dataset_with_units))[datasets]
      mutate(., dataset = factor(dataset_with_units, levels = dataset_levels))
    }
  
  # generate plot
  p <- plot_data %>% 
    ggplot() + 
    aes(cycle, value, group = paste(file_id, type, dataset)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous("Cycle", breaks = c(0:max(plot_data$cycle))) +
    scale_y_continuous("Signal") +
    theme_bw() 
  
  # labels
  datasets_label <- "Dataset"
  files_label <- "File"
  types_label <- "Data Type"
  
  # paneling
  if (panel_by == "dataset") 
    p <- p + facet_wrap(~dataset, scales = "free_y") 
  else if (panel_by == "file") 
    p <- p + facet_wrap(~file_id, scales = "free_y") 
  else if (panel_by == "SA|STD")
    p <- p + facet_wrap(~type, scales = "free_y") 
  
  # color_by
  if (color_by == "dataset") 
    p <- p %+% aes(color = dataset) + labs(color = datasets_label)
  else if (color_by == "file") 
    p <- p %+% aes(color = file_id) + labs(color = files_label)
  else if (color_by == "SA|STD") 
    p <- p %+% aes(color = type) + labs(color = types_label)
  
  # linetype_by
  if (linetype_by == "dataset") 
    p <- p %+% aes(linetype = dataset) + labs(linetype = datasets_label)
  else if (linetype_by == "file") 
    p <- p %+% aes(linetype = file_id) + labs(linetype = files_label)
  else if (linetype_by == "SA|STD") 
    p <- p %+% aes(linetype = type) + labs(linetype = types_label)
  
  # shape_by
  if (shape_by == "dataset") 
    p <- p %+% aes(shape = dataset) + labs(shape = datasets_label)
  else if (shape_by == "file") 
    p <- p %+% aes(shape = file_id) + labs(shape = files_label)
  else if (shape_by == "SA|STD") 
    p <- p %+% aes(shape = type) + labs(shape = types_label)
  
  # return plot
  return(p)
}
