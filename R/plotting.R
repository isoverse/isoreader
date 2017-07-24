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
#' @note not entirely clear if a normalization parameter would be useful
plot_dual_inlet <- function(
  isofiles, datasets, 
  panel_by = "dataset", color_by = "file", linetype_by = "none", shape_by = "SA|STD") {
  
  # checks
  if(!is_dual_inlet(isofiles)) stop("can only plot dual inlet isofiles", call. = FALSE)
  if (!all(ok <- c(panel_by, color_by, linetype_by, shape_by) %in% c("none", "dataset", "file", "SA|STD")))
    stop("unknown layout specification: '", str_c(c(panel_by, color_by, linetype_by, shape_by)[!ok], collapse = "', '"),
         "'. Please use 'none', 'dataset' or 'file' for panel_by, color_by, shape_by and linetype_by specifications.", call. = FALSE)
  
  # global vars
  #time <- type <- column <- value <- file_id <- label_with_units <- NULL
  #is_ratio <- max_signal <- baseline <- cutoff <- discard <- change <- border <- gap <- NULL
  
  # collect raw data
  raw_data <- aggregate_raw_data(isofiles, gather = TRUE, quiet = TRUE)
  if (nrow(raw_data) == 0) stop("no raw data in supplied isofiles", call. = FALSE)
  
  # collect raw data
  raw_data <- aggregate_raw_data(isofiles, gather = TRUE, quiet = TRUE)
  if (nrow(raw_data) == 0) stop("no raw data in supplied isofiles", call. = FALSE)
  
  # only work with desired datasets (masses and ratios)
  datasets <- if(missing(datasets)) unique(raw_data$dataset) else as.character(datasets)
  if ( length(missing <- setdiff(datasets, unique(raw_data$dataset))) > 0 ) 
    stop("dataset(s) not available in the provided isofiles: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- filter(raw_data, dataset %in% datasets)
  
  
  # masses and ratios
  all_columns <- get_mass_and_ratio_definitions(raw_data, masses, ratios)
  raw_data <- calculate_plot_ratios(raw_data, filter(all_columns, type == "ratio"))
  
  # plot data
  plot_data <- 
    # relevant columns
    raw_data[c("file_id", "type", "cycle", all_columns$column)] %>% 
    # gather everything
    gather(column, value, -file_id, -type, -cycle) %>% 
    filter(!is.na(value)) %>% 
    # normalize
    # {
    #   if (normalize) {
    #     group_by(., file_id, column, type) %>% # what should be grouped by?? can make arguments by panel but since standard and sample are all separate and offset depending on pressure, it's not so clear what makes most sense - can just plot ratios instead of normalizing to anything
    #       mutate(value = (value - min(value, na.rm = TRUE))/
    #                (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>% 
    #       ungroup()
    #   } else .
    # } %>% 
    # labeling information
    left_join(select(all_columns, column, label_with_units), by = "column")
  
  # generate plot
  p <- plot_data %>% 
    ggplot() + 
    aes(cycle, value, group = paste(file_id, type, column)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous("Cycle", breaks = c(0:max(plot_data$cycle))) +
    scale_y_continuous("Signal") +
    theme_bw() 
  
  # normalize plot y axis
  # if (normalize)
  #   p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  # labels
  dataset_label <- "Traces"
  files_label <- "Files"
  types_label <- "Data Type"
   
  # paneling
  if (panel_by == "dataset") 
    p <- p + facet_wrap(~label_with_units, scales = "free_y") 
  else if (panel_by == "files") 
    p <- p + facet_wrap(~file_id, scales = "free_y") 
  else if (panel_by == "types")
    p <- p + facet_wrap(~type, scales = "free_y") 
  
  # color_by
  if (color_by == "dataset") 
    p <- p %+% aes(color = label_with_units) + labs(color = dataset_label)
  else if (color_by == "files") 
    p <- p %+% aes(color = file_id) + labs(color = files_label)
  else if (color_by == "types") 
    p <- p %+% aes(color = type) + labs(color = types_label)
  
  # linetype_by
  if (linetype_by == "dataset") 
    p <- p %+% aes(linetype = label_with_units) + labs(linetype = dataset_label)
  else if (linetype_by == "files") 
    p <- p %+% aes(linetype = file_id) + labs(linetype = files_label)
  else if (linetype_by == "types") 
    p <- p %+% aes(linetype = type) + labs(linetype = types_label)
  
  # shape_by
  if (shape_by == "dataset") 
    p <- p %+% aes(shape = label_with_units) + labs(shape = dataset_label)
  else if (shape_by == "files") 
    p <- p %+% aes(shape = file_id) + labs(shape = files_label)
  else if (shape_by == "types") 
    p <- p %+% aes(shape = type) + labs(shape = types_label)
  
  # return plot
  return(p)
}

# calculation functions for plotting =====

# helper function to process mass and ratio requests for plotting functions
# peforms all the necessary safety checks
# @return a data frame with all requested masses and ratios
get_mass_and_ratio_definitions <- function(raw_data, masses, ratios) {
  
  # global vars
  mass <- column <- ratio <- top <- bot <- label <- NULL
  
  # masses
  mass_column_pattern <- "^[vi](\\d+)\\.(.*)$"
  mass_columns <- names(raw_data) %>% 
    str_subset(mass_column_pattern) %>% 
    str_match(mass_column_pattern) %>%  
    { data_frame(column = .[,1], mass = .[,2], units = .[,3]) }
  mass_lookup <- select(mass_columns, mass, column) %>% deframe()
  
  # get alll masses if none provided
  if (!is.null(masses) &&  is.na(masses)) masses <- mass_columns$mass
  else masses <- as.character(masses)
  
  # safety check
  if(length(masses) == 0 && length(ratios) == 0) stop("must specify at least one mass or ratio", call. = FALSE)
  
  # ratios
  ratio_pattern <- "^(\\d+)/(\\d+)$"
  if (!all(ok <- str_detect(ratios, ratio_pattern))) {
    stop("invalid ratio(s): ", str_c(ratios[!ok], collapse = ", "), call. = FALSE)
  }
  if (length(ratios) > 0) {
    ratio_columns <- ratios %>% 
      str_match(ratio_pattern) %>% 
      { data_frame(column = str_c("ratio.",.[,1]), ratio = .[,1], top = .[,2], bot = .[,3], units = "") }
  } else {
    ratio_columns <- data_frame(column = "", ratio = "", top = "", bot = "", units = "")[0,]
  }
  
  # more safety checks
  all_needed_masses <- unique(c(masses, ratio_columns$top, ratio_columns$bot))
  if ( length(missing <- setdiff(all_needed_masses, mass_columns$mass)) > 0 ) {
    stop("mass(es) not available in the provided isofiles: ", str_c(missing, collapse = ", "), call. = FALSE)
  }
  
  # all data columns to plot (mass and ratio)
  bind_rows(
    mass_columns %>% 
      filter(mass %in% masses) %>% 
      rename(label = mass) %>% 
      mutate(type = "mass"),
    ratio_columns %>% 
      rename(label = ratio) %>% 
      mutate(type = "ratio",
             top = mass_lookup[top],
             bot = mass_lookup[bot])
  ) %>% mutate(
    label_with_units = ifelse(nchar(units) > 0, str_c(label, " [", units, "]"), label)
  )
}

# calculate derived ratios in mass data
# @param ratio_columns is a data frame with columns 'column', 'top' and 'bot' for calculating the resulting ratio of top/bot and storing it in the new column 'column'
calculate_plot_ratios <- function(raw_data, ratio_columns) {
  if(!all(c("column", "top", "bot") %in% names(ratio_columns))) stop("columns missing", call. = FALSE)
  if(any(is.na(ratio_columns$column)) || any(is.na(ratio_columns$top)) || any(is.na(ratio_columns$bot)))
    stop("missing values", call. = FALSE)
 
  # generate ratios
  if (nrow(ratio_columns) > 0) {
    for (i in 1:nrow(ratio_columns)) {
      raw_data[[ratio_columns$column[i]]] <- raw_data[[ratio_columns$top[i]]] / raw_data[[ratio_columns$bot[i]]]
    }
  }
  return(raw_data)
}
