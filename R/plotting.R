# plotting functions ==== 

#' Plot raw data from isoreader files
#' 
#' Convenience function for making standard plots for raw isoreader data. Calls \code{\link{iso_plot_continuous_flow}} and \code{\link{iso_plot_dual_inlet}} for data specific plotting (see those functions for parameter details).
#' 
#' @inheritParams iso_aggregate_raw_data
#' @param ... parameters for the data specific plotting functions
#' @family plot functions
#' @export
iso_plot_raw_data <- function(isofiles, ..., quiet = default(quiet)) {
  if(!iso_is_object(isofiles)) stop("can only plot iso files or lists of iso files", call. = FALSE)
  
  isofiles <- iso_as_file_list(isofiles)
  
  if (!quiet) sprintf("Info: plotting data from %d data file(s)", length(isofiles)) %>% message()
  
  if (iso_is_continuous_flow(isofiles))
    iso_plot_continuous_flow (isofiles, ...)
  else if (iso_is_dual_inlet(isofiles))
    iso_plot_dual_inlet (isofiles, ...)
  else
    stop("plotting of this type of isofiles not yet supported", call. = FALSE) 
}

#' Plot chromatogram from continuous flow data
#'
#' @inheritParams iso_plot_raw_data
#' @param data which masses and ratios to plot (e.g. c("44", "45", "45/44")), if omitted, all available masses and ratios are plotted. Note that ratios should be calculated using \code{\link{iso_calculate_ratios}} prior to plotting.
#' @param time_interval which time interval to plot
#' @param time_interval_units which units the time interval is in, default is "seconds"
#' @param normalize whether to normalize all data (default is FALSE, i.e. no normalization). If TRUE, normalizes each trace across all files. Normalizing always scales such that each trace fills the entire height of the plot area. Note that zooming (if \code{zoom} is set) is applied after normalizing.
#' @param zoom if not set, automatically scales to the maximum range in the selected time_interval in each panel. If set, scales by the indicated factor, i.e. values > 1 are zoom in, values < 1 are zoom out, baseline always remains the bottom anchor point, zooming is always relative to the max in the plot (even if that is outside visible frame). Note that for overlay plots (\code{panel_bu = "none"}) zooming is relative to the max in each panel (potentially across different data traces). Also note that zooming only affects masses, ratios are not zoomed.
#' @param panel_by whether to panel data by anything, options are "none" (overlay all), "data" (by mass/ratio data), "file" (panel by files). The default is "data"
#' @param color_by whether to color data by anything, options are the same as for \code{panel_by} but the default is "file"
#' @param linetype_by whether to differentiate data by linetype, options are the same as for \code{panel_by} but the default is "none". Note that a limited number of linetype_by (6) is defined by default and the plot will fail if a higher number is required unless specified using \code{\link[ggplot2]{scale_linetype}}
#' @family plot functions
#' @export
iso_plot_continuous_flow <- function(
  isofiles, data = c(), time_interval = c(), time_interval_units = "seconds", normalize = FALSE, zoom = NULL, 
  panel_by = "data", color_by = "file", linetype_by = "none") {
  
  # checks and defaults
  if(!iso_is_continuous_flow(isofiles)) stop("can only plot continuous flow isofiles", call. = FALSE)
  if (!all(ok <- c(panel_by, color_by, linetype_by) %in% c("none", "data", "file")))
    stop("unknown layout specification: '", str_c(c(panel_by, color_by, linetype_by)[!ok], collapse = "', '"),
         "'. Please use 'none', 'data' or 'file' for panel_by, color_by and linetype_by specifications.", call. = FALSE)
  
  # global vars
  time <- type <- value <- file_id <- category <- data_without_units <- NULL
  is_ratio <- max_signal <- baseline <- cutoff <- discard <- change <- border <- gap <- NULL
  
  # collect raw data
  raw_data <- iso_aggregate_raw_data(isofiles, gather = TRUE, quiet = TRUE)
  if (nrow(raw_data) == 0) stop("no raw data in supplied isofiles", call. = FALSE)
  
  # only work with desired data (masses and ratios)
  select_data <- if(length(data) == 0) unique(raw_data$data) else as.character(data)
  if ( length(missing <- setdiff(select_data, unique(raw_data$data))) > 0 ) 
    stop("data not available in the provided isofiles: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- filter(raw_data, data %in% select_data)
  
  # time column
  time_pattern <- "^time\\.(.*)$"
  time_column <- str_subset(names(raw_data), time_pattern)
  if (length(time_column) != 1) 
    stop("unclear which column is the time column, consider an explicit 'iso_convert_time' call before plotting, found: ", 
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
  normalize_data <- function(df) {
    group_by(df, data) %>%
      mutate(value = (value - min(value, na.rm = TRUE))/
               (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>% 
      ungroup()
  }
  
  # plot data
  zoom_grouping <- if(panel_by == "data") "data" else if (panel_by == "file") "file_id" else c()
  plot_data <- 
    raw_data %>% 
    # add units to data for proper grouping
    mutate(
      data_without_units = data,
      data = ifelse(!is.na(units), str_c(data, " [", units, "]"), data)
    ) %>% 
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
        group_by(., file_id, data) %>% 
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
    # switch to factors for proper grouping
    mutate(
      data_with_units = ifelse(!is.na(units), str_c(data, " [", units, "]"), data)
    ) %>% {
      data_levels <- deframe(select(., data, data_without_units) %>% unique())
      data_sorting <- sapply(select_data, function(x) which(data_levels == x)) %>% unlist(use.names = F)
      mutate(., data = factor(data, levels = names(data_levels)[data_sorting]))
    }
  
  # generate plot
  p <- plot_data %>% 
    ggplot() + 
    aes(time, value, group = paste(file_id, data)) +
    geom_line() +
    scale_x_continuous(str_c("Time ", time_unit), expand = c(0, 0)) +
    scale_y_continuous(if(normalize) "Normalized Signal" else "Signal", expand = c(0, 0)) +
    theme_bw()
  
  # zoom ghost points to make sure the zooming frame remains the same (if zoom is set)
  if (!is.null(zoom)) {
    p <- p +
      geom_point(data = function(df) group_by_(df, .dots = zoom_grouping) %>% 
                   summarize(time = mean(time, na.omit = TRUE), value = min(baseline, na.omit = TRUE)) %>% 
                   filter(!is.na(value)),
                 mapping = aes(x = time, y = value), inherit.aes = FALSE,
                 size = 0, alpha = 1, show.legend = FALSE) +
      geom_point(data = function(df) group_by_(df, .dots = zoom_grouping) %>% 
                   summarize(time = mean(time, na.omit = TRUE), value = max(cutoff, na.omit = TRUE)) %>% 
                   filter(!is.na(value)),
                 mapping =aes(x = time, y = value), inherit.aes = FALSE,
                 size = 0, alpha = 1, show.legend = FALSE)
  }

  # display full time scale
  if (length(time_interval) == 2)
    p <- p + expand_limits(x = time_interval)
  
  # normalize plot y axis
  if (normalize)
    p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  # labels
  data_label <- "Data"
  files_label <- "File"
  
  # paneling
  if ( panel_by == "data") 
    p <- p + facet_grid(data~., scales = "free_y") 
  else if (panel_by == "file") 
    p <- p + facet_grid(file_id~., scales = "free_y")

  # color_by
  if (color_by == "data") 
    p <- p %+% aes(color = data) + labs(color = data_label)
  else if (color_by == "file") 
    p <- p %+% aes(color = file_id) + labs(color = files_label)
  
  # linetype_by
  if (linetype_by == "data") 
    p <- p %+% aes(linetype = data) + labs(linetype = data_label)
  else if (linetype_by == "file") 
    p <- p %+% aes(linetype = file_id) + labs(linetype = files_label)
  
  # return plot
  return(p)
}

#' Plot mass data from dual inlet files
#' 
#' @inheritParams iso_plot_continuous_flow
#' @param panel_by whether to panel data by anything, options are "none" (overlay all), "data" (by mass/ratio data), "file" (panel by files), "SA|STD" (panel by sample|standard). The default is "data"
#' @param shape_by whether to shape data points by anything, options are the same as for panel_by but the default is "SA|STD" (sample|standard)
#' @note normalization is not useful for dual inlet data, except potentially between standard and sample - however, for this it is more meaningful to simply plot the relevant ratios together
#' @export
iso_plot_dual_inlet <- function(
  isofiles, data = c(), 
  panel_by = "data", color_by = "file", linetype_by = "none", shape_by = "SA|STD") {
  
  # checks
  if(!iso_is_dual_inlet(isofiles)) stop("can only plot dual inlet isofiles", call. = FALSE)
  if (!all(ok <- c(panel_by, color_by, linetype_by, shape_by) %in% c("none", "data", "file", "SA|STD")))
    stop("unknown layout specification: '", str_c(c(panel_by, color_by, linetype_by, shape_by)[!ok], collapse = "', '"),
         "'. Please use 'none', 'data', 'file' or 'SA|STD' for panel_by, color_by, shape_by and linetype_by specifications.", call. = FALSE)
  
  # global vars
  cycle <- value <- type <- data_without_units <- NULL
  
  # collect raw data
  raw_data <- iso_aggregate_raw_data(isofiles, gather = TRUE, quiet = TRUE)
  if (nrow(raw_data) == 0) stop("no raw data in supplied isofiles", call. = FALSE)

  # only work with desired data (masses and ratios)
  select_data <- if(length(data) == 0) unique(raw_data$data) else as.character(data)
  if ( length(missing <- setdiff(select_data, unique(raw_data$data))) > 0 ) 
    stop("data not available in the provided isofiles: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- filter(raw_data, data %in% select_data)
  
  # plot data
  plot_data <- 
    raw_data %>% 
    # data with units and in correct order
    mutate(
      data_without_units = data,
      data = ifelse(!is.na(units), str_c(data, " [", units, "]"), data)
    ) %>% {
      data_levels <- deframe(select(., data, data_without_units) %>% unique())
      data_sorting <- sapply(select_data, function(x) which(data_levels == x)) %>% unlist(use.names = F)
      mutate(., data = factor(data, levels = names(data_levels)[data_sorting]))
    } 

  # generate plot
  p <- plot_data %>% 
    ggplot() + 
    aes(cycle, value, group = paste(file_id, type, data)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous("Cycle", breaks = c(0:max(plot_data$cycle))) +
    scale_y_continuous("Signal") +
    theme_bw() 
  
  # labels
  data_label <- "Data"
  files_label <- "File"
  types_label <- "Data Type"
  
  # paneling
  if (panel_by == "data") 
    p <- p + facet_wrap(~data, scales = "free_y") 
  else if (panel_by == "file") 
    p <- p + facet_wrap(~file_id, scales = "free_y") 
  else if (panel_by == "SA|STD")
    p <- p + facet_wrap(~type, scales = "free_y") 
  
  # color_by
  if (color_by == "data") 
    p <- p %+% aes(color = data) + labs(color = data_label)
  else if (color_by == "file") 
    p <- p %+% aes(color = file_id) + labs(color = files_label)
  else if (color_by == "SA|STD") 
    p <- p %+% aes(color = type) + labs(color = types_label)
  
  # linetype_by
  if (linetype_by == "data") 
    p <- p %+% aes(linetype = data) + labs(linetype = data_label)
  else if (linetype_by == "file") 
    p <- p %+% aes(linetype = file_id) + labs(linetype = files_label)
  else if (linetype_by == "SA|STD") 
    p <- p %+% aes(linetype = type) + labs(linetype = types_label)
  
  # shape_by
  if (shape_by == "data") 
    p <- p %+% aes(shape = data) + labs(shape = data_label)
  else if (shape_by == "file") 
    p <- p %+% aes(shape = file_id) + labs(shape = files_label)
  else if (shape_by == "SA|STD") 
    p <- p %+% aes(shape = type) + labs(shape = types_label)
  
  # return plot
  return(p)
}

