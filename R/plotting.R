#' Plot raw data from isoreader files
#' 
#' Convenience function for making standard plots for raw isoreader data. Calls \code{\link{isoplot_continuous_flow}} and \code{\link{isoplot_dual_inlet}} for data specific plotting (see those functions for parameter details).
#' 
#' @inheritParams isoexport_rda
#' @param ... parameters for the data specific plotting functions
#' @family plot functions
#' @export
isoplot_raw_data <- function(isofiles, ..., quiet = setting("quiet")) {
  if(!is_iso_object(isofiles)) stop("can only plot iso files or lists of iso files", call. = FALSE)
  
  if (!setting("quiet")) 
    sprintf("Info: plotting data from %d data files", length(isofiles)) %>% message()
  
  if (is_continuous_flow(isofiles))
    isoplot_continuous_flow (isofiles, ...)
  else if (is_dual_inlet(isofiles))
    isoplot_dual_inlet (isofiles, ...)
  else
    stop("plotting of this type of isofiles not yet supported", call. = FALSE) 
}

#' Plot chromatogram from continuous flow data
#'
#' @param isofiles collection of continuous flow isofile objects
#' @param masses which masses to plot (e.g. c("45", "44")), NULL (the default) means that all masses will be plotted
#' @param ratios which ratios to plot (e.g. c("45/44", "46/44")), not affected by zoom parameter
#' @param time_interval which time interval to plot (in the units of time of the iso objects)
#' @param normalize whether to normalize all traces (default is FALSE, i.e. no normalization). If TRUE, normalizes each trace across all files. Normalizing always scales such that each trace fills the entire height of the plot area. Note that zooming (if \code{zoom} is set) is applied after normalizing.
#' @param zoom if not set, automatically scales to the maximum range in the selected time_interval in each panel. If set, scales by the indicated factor, i.e. values > 1 are zoom in, values < 1 are zoom out, baseline always remains the anchor point. Note that for overlay plots (\code{panels = "none"}) zooming is relative to the max in each panel (potentially across different traces). Also note that zooming only affects masses, ratios are not zoomed.
#' @param panels whether to panel traces, options are "none" (overlay all), "traces" (by mass/ratio traces), "files" (panel by files). The default is "traces"
#' @param colors whether to color traces, options are the same as for panels but the default is "files"
#' @param linetypes whether to differentiate traces by linetype, options are the same as for panels but the default is "none". Note that a limited number of linetypes (6) is defined by default and the plot will fail if a higher number is required unless specified using \code{\link[ggplot2]{scale_linetype}}
#' @family plot functions
#' @export
isoplot_continuous_flow <- function(
  isofiles, masses = NA, ratios = c(), time_interval = c(), normalize = FALSE, zoom = NULL, 
  panels = c("none", "traces", "files"), colors = c("none", "traces", "files"), linetypes = c("none", "traces", "files")) {
  
  # checks
  if(!is_continuous_flow(isofiles)) stop("can only plot continuous flow isofiles", call. = FALSE)
  if (missing(panels)) panels <- "traces"
  if (missing(colors)) colors <- "files"
  if (missing(linetypes)) linetypes <- "none"
  if (!all(ok <- c(panels, colors, linetypes) %in% c("none", "traces", "files")))
    stop("unknown layout specification ", str_c(c(panels, colors, linetypes)[!ok], collapse = ", "), call. = FALSE)
  if(colors != "none" && colors == linetypes) 
    stop("cannot have the same specification for colors and linetypes", call. = FALSE)
  
  # global vars
  time <- type <- column <- value <- file_id <- label_with_units <- NULL
  is_ratio <- max_signal <- baseline <- cutoff <- discard <- change <- border <- gap <- NULL
  
  # collect raw data
  raw_data <- get_raw_data(isofiles)
  if (nrow(raw_data) == 0) stop("no raw data in supplied isofiles", call. = FALSE)
  
  # masses and ratios
  all_columns <- get_mass_and_ratio_definitions(raw_data, masses, ratios)
  raw_data <- calculate_ratios(raw_data, filter(all_columns, type == "ratio"))

  # time column
  time_pattern <- "^time\\.(.*)$"
  time_column <- str_subset(names(raw_data), time_pattern)
  if (length(time_column) != 1) 
    stop("unclear which column is the time column, found: ", str_c(time_column, collapse = ", "), call. = FALSE)
  time_unit <- str_match(time_column, time_pattern) %>% {sprintf("[%s]", .[2]) }
  raw_data$time <- raw_data[[time_column]]
  
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
    group_by(data, column) %>%
      mutate(value = (value - min(value, na.rm = TRUE))/
               (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>% 
      ungroup()
  }
  
  # plot data
  zoom_grouping <- if(panels == "traces") "column" else if (panels == "files") "file_id" else c()
  plot_data <- 
    # relevant columns
    raw_data[c("file_id", "time", all_columns$column)] %>% 
    # gather everything
    gather(column, value, -file_id, -time) %>% 
    filter(!is.na(value)) %>% 
    mutate(is_ratio = grepl("ratio", column)) %>% 
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
        group_by(., file_id, column) %>% 
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
    # labeling information
    left_join(all_columns, by = "column")
  
  # generate plot
  files_label <- "File"
  traces_label <- "Trace"
  p <- plot_data %>% 
    ggplot() + 
    aes(time, value, group = paste(file_id, column)) +
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
  
  # paneling
  if (panels == "traces") 
    p <- p + facet_grid(label_with_units~., scales = "free_y") 
  else if (panels == "files") 
    p <- p + facet_grid(file_id~., scales = "free_y") 
  
  # colors
  if (colors == "traces") 
    p <- p %+% aes(color = label_with_units) + labs(color = traces_label)
  else if (colors == "files") 
    p <- p %+% aes(color = file_id) + labs(color = files_label)
  
  # linetypes
  if (linetypes == "traces") 
    p <- p %+% aes(linetype = label_with_units) + labs(linetype = traces_label)
  else if (linetypes == "files") 
    p <- p %+% aes(linetype = file_id) + labs(linetype = files_label)
  
  return(p)
}

#' Plot mass data from dual inlet files
#' @inheritParams isoplot_continuous_flow
isoplot_dual_inlet <- function(isofiles, masses = NA, ratios = c()) {
  stop("not yet implemented", call. = FALSE)
}


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
calculate_ratios <- function(raw_data, ratio_columns) {
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

