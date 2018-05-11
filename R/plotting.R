# plotting functions ==== 

#' Plot raw data from isoreader files
#' 
#' Convenience function for making quick standard plots for raw isoreader data. 
#' Calls \code{\link{iso_plot_continuous_flow_data}} and \code{\link{iso_plot_dual_inlet_data}} for data specific plotting (see those functions for parameter details). 
#' For customizing plotting calls, it is easier to use \code{\link{iso_plot_continuous_flow_data}} and \code{\link{iso_plot_dual_inlet_data}} directly.
#' 
#' @inheritParams iso_get_raw_data
#' @param ... parameters for the data specific plotting functions
#' @family plot functions
#' @export
iso_plot_raw_data <- function(iso_files, ..., quiet = default(quiet)) {
  if(!iso_is_object(iso_files)) stop("can only plot iso files or lists of iso files", call. = FALSE)
  
  iso_files <- iso_as_file_list(iso_files)
  
  if (!quiet) sprintf("Info: plotting data from %d data file(s)", length(iso_files)) %>% message()
  
  if (iso_is_continuous_flow(iso_files))
    iso_plot_continuous_flow_data (iso_files, ...)
  else if (iso_is_dual_inlet(iso_files))
    iso_plot_dual_inlet_data (iso_files, ...)
  else
    stop("plotting of this type of iso_files not yet supported", call. = FALSE) 
}

# NOTE: should the color and linetype aesthetics allow any expression?
#' Plot chromatogram from continuous flow data
#'
#' @inheritParams iso_plot_raw_data
#' @param data which masses and ratios to plot (e.g. c("44", "45", "45/44")), if omitted, all available masses and ratios are plotted. Note that ratios should be calculated using \code{\link{iso_calculate_ratios}} prior to plotting.
#' @param time_interval which time interval to plot
#' @param time_interval_units which units the time interval is in, default is "seconds"
#' @param filter any filter condition to apply to the data beyond the masses/ratio selection (param \code{data}) and time interval (param \code{time_interval}). For details on the available data columns see \link{iso_get_raw_data} with parameters \code{gather = TRUE} and \code{include_file_info = everything()} (i.e. all file info is available for plotting aesthetics).
#' @param normalize whether to normalize all data (default is FALSE, i.e. no normalization). If TRUE, normalizes each trace across all files. Normalizing always scales such that each trace fills the entire height of the plot area. Note that zooming (if \code{zoom} is set) is applied after normalizing.
#' @param zoom if not set, automatically scales to the maximum range in the selected time_interval in each panel. If set, scales by the indicated factor, i.e. values > 1 are zoom in, values < 1 are zoom out, baseline always remains the bottom anchor point, zooming is always relative to the max in the plot (even if that is outside visible frame). Note that for overlay plots (\code{panel_bu = "none"}) zooming is relative to the max in each panel (potentially across different data traces). Also note that zooming only affects masses, ratios are not zoomed.
#' @param panel whether to panel data by anything - any data column is possible (see notes in the \code{filter} parameter) but the most commonly used options are \code{panel = NULL} (overlay all), \code{panel = data} (by mass/ratio data), \code{panel = file_id} (panel by files, alternatively use any appropriate file_info column). The default is panelling by \code{data}.
#' @param color whether to color data by anything, options are the same as for \code{panel} but the default is \code{file_id}.
#' @param linetype whether to differentiate data by linetype, options are the same as for \code{panel} but the default is \code{NULL} (i.e. no linetype aesthetic). Note that a limited number of linetypes (6) is defined by default and the plot will fail if a higher number is required unless specified using \code{\link[ggplot2]{scale_linetype}}.
#' @param ... deprecated parameters
#' @family plot functions
#' @export
iso_plot_continuous_flow_data <- function(
  iso_files, data = c(), 
  time_interval = c(), time_interval_units = "seconds", 
  filter = NULL,
  normalize = FALSE, zoom = NULL, 
  panel = data, color = file_id, linetype = NULL,
  ...) {
  
  # safety checks
  if(!iso_is_continuous_flow(iso_files)) 
    stop("iso_plot_continuous_flow_data can only plot continuous flow iso_files", call. = FALSE)
  
  # check for deprecated parameters
  dots <- list(...)
  old <- c("panel_by", "color_by", "linetype_by", "shape_by")
  if (any(old %in% names(dots))) {
    glue("deprecated parameter(s): ",
         "'{collapse(old[old %in% names(dots)], sep=\"', '\")}' ",
         "- please check the function documentation for details on ",
         "the updated parameters") %>% 
      stop(call. = FALSE)
  }
  if (length(dots) > 0) {
    glue("unkown parameter(s): ",
         "'{collapse(names(dots), sep=\"', '\")}' ") %>% 
      stop(call. = FALSE)
  }
  
  # global vars
  time <- type <- value <- file_id <- category <- data_without_units <- NULL
  is_ratio <- max_signal <- baseline <- cutoff <- discard <- change <- border <- gap <- NULL
  
  # collect raw data
  raw_data <- iso_get_raw_data(iso_files, gather = TRUE, quiet = TRUE, include_file_info = everything())
  if (nrow(raw_data) == 0) stop("no raw data in supplied iso_files", call. = FALSE)
  
  # check for column existence
  aes_quos <- list(panel = enquo(panel), color = enquo(color), linetype = enquo(linetype))
  aes_cols <- get_column_names(raw_data, panel = aes_quos$panel, color = aes_quos$color, linetype = aes_quos$linetype, 
                              n_reqs = list(panel = "?", color = "?", linetype = "?"))

  # only work with desired data (masses and ratios)
  select_data <- if(length(data) == 0) unique(raw_data$data) else as.character(data)
  if ( length(missing <- setdiff(select_data, unique(raw_data$data))) > 0 ) 
    stop("data not available in the provided iso_files: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- dplyr::filter(raw_data, data %in% select_data)
  
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
  
  # general filter
  filter_quo <- enquo(filter)
  if (!quo_is_null(filter_quo)) {
    raw_data <- dplyr::filter(raw_data, !!filter_quo)
  }
  
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
  zoom_grouping <- unname(aes_cols$panel)
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
        group_by(., !!sym(zoom_grouping)) %>% 
          mutate(
            baseline = value[!is_ratio] %>% { if(length(.) == 0) NA else min(.) },
            max_signal = value[!is_ratio] %>% { if(length(.) == 0) NA else max(.) }) %>%
          ungroup() 
      } else . 
    } %>% 
    # time filtering
    { if (length(time_interval) == 2) dplyr::filter(., time >= time_interval[1], time <= time_interval[2]) else . } %>% 
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
          dplyr::filter(!discard | border | gap) %>% 
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
                   dplyr::filter(!is.na(value)),
                 mapping = aes(x = time, y = value), inherit.aes = FALSE,
                 size = 0, alpha = 1, show.legend = FALSE) +
      geom_point(data = function(df) group_by_(df, .dots = zoom_grouping) %>% 
                   summarize(time = mean(time, na.omit = TRUE), value = max(cutoff, na.omit = TRUE)) %>% 
                   dplyr::filter(!is.na(value)),
                 mapping =aes(x = time, y = value), inherit.aes = FALSE,
                 size = 0, alpha = 1, show.legend = FALSE)
  }

  # display full time scale
  if (length(time_interval) == 2)
    p <- p + expand_limits(x = time_interval)
  
  # normalize plot y axis
  if (normalize)
    p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  # paneling
  if (!is_empty(aes_cols$panel)) 
    p <- p + facet_grid(new_formula(sym(aes_cols$panel), sym(".")), scales = "free_y")
  
  # color
  if (!is_empty(aes_cols$color))
    p <- p %+% aes_(color = sym(aes_cols$color))
  
  # linetype
  if (!is_empty(aes_cols$linetype)) 
    p <- p %+% aes_(linetype = sym(aes_cols$linetype))
  
  # return plot
  return(p)
}

#' Plot mass data from dual inlet files
#' 
#' @inheritParams iso_plot_continuous_flow_data
#' @param filter any filter condition to apply to the data beyond the masses/ratio selection (param \code{data}) and time interval (param \code{time_interval}). For details on the available data columns see \link{iso_get_raw_data} with parameters \code{gather = TRUE} and \code{include_file_info = everything()} (i.e. all file info is available for plotting aesthetics).
#' @param panel whether to panel data by anything - any data column is possible (see notes in the \code{filter} parameter) but the most commonly used options are \code{panel = NULL} (overlay all), \code{panel = data} (by mass/ratio data), \code{panel = file_id} (panel by files, alternatively use any appropriate file_info column), and \code{panel = type} (panel by sample vs standard). Additionally it is possible to panel two variables against each other (i.e. use a \link[ggplot2]{facet_grid}), e.g. by specifying the formula \code{panel = data ~ file_id} (data in the panel rows, files in the panel columns) or \code{panel = data ~ type}.The default for this parameter is simple panelling by \code{data}. 
#' @param shape whether to shape data points by anything, options are the same as for \code{panel} but the default is \code{type} (sample vs standard).
#' @param ... deprecated parameters
#' @note normalization is not useful for dual inlet data, except potentially between standard and sample - however, for this it is more meaningful to simply plot the relevant ratios together
#' @family plot functions
#' @export
iso_plot_dual_inlet_data <- function(
  iso_files, data = c(), filter = NULL,
  panel = data, color = file_id, linetype = NULL, shape = type,
  ...) {
  
  # checks
  if(!iso_is_dual_inlet(iso_files)) 
    stop("iso_plot_dual_inlet_data can only plot dual inlet iso_files", call. = FALSE)

  # check for deprecated parameters
  dots <- list(...)
  old <- c("panel_by", "color_by", "linetype_by", "shape_by")
  if (any(old %in% names(dots))) {
    glue("deprecated parameter(s): ",
         "'{collapse(old[old %in% names(dots)], sep=\"', '\")}' ",
         "- please check the function documentation for details on ",
         "the updated parameters") %>% 
      stop(call. = FALSE)
  }
  if (length(dots) > 0) {
    glue("unkown parameter(s): ",
         "'{collapse(names(dots), sep=\"', '\")}' ") %>% 
      stop(call. = FALSE)
  }
  
  # global vars
  cycle <- value <- type <- data_without_units <- NULL
  
  # collect raw data
  raw_data <- iso_get_raw_data(iso_files, gather = TRUE, quiet = TRUE, include_file_info = everything())
  if (nrow(raw_data) == 0) stop("no raw data in supplied iso_files", call. = FALSE)

  # check for column existence
  aes_quos <- list(panel = enquo(panel), color = enquo(color), linetype = enquo(linetype), shape = enquo(shape))
  
  
  aes_cols <- get_column_names(
    raw_data, color = aes_quos$color, linetype = aes_quos$linetype, shape = aes_quos$shape, 
    n_reqs = list(color = "?", linetype = "?", shape = "?"))
  
  if (quo_is_null(aes_quos$panel)) {
    # no panel
    aes_cols$panel <- c()
  } else if (quo_is_symbol(aes_quos$panel)) {
    # single symbol --> facet_wrap
    aes_cols <- c(aes_cols, get_column_names(raw_data, panel = aes_quos$panel)) 
  } else {
    # formula --> facet_grid
    aes_cols <- c(aes_cols, get_column_names(
      raw_data, 
      panel_rows = aes_quos$panel %>% quo_expr() %>% f_lhs(),
      panel_cols = aes_quos$panel %>% quo_expr() %>% f_rhs()))
  }
  
  # only work with desired data (masses and ratios)
  select_data <- if(length(data) == 0) unique(raw_data$data) else as.character(data)
  if ( length(missing <- setdiff(select_data, unique(raw_data$data))) > 0 ) 
    stop("data not available in the provided iso_files: ", str_c(missing, collapse = ", "), call. = FALSE)
  raw_data <- dplyr::filter(raw_data, data %in% select_data)
  
  # general filter
  filter_quo <- enquo(filter)
  if (!quo_is_null(filter_quo)) {
    raw_data <- dplyr::filter(raw_data, !!filter_quo)
  }
  
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
  
  # paneling
  if (!quo_is_null(aes_quos$panel)) {
    if (quo_is_symbol(aes_quos$panel))
      p <- p + facet_wrap(new_formula(NULL, sym(aes_cols$panel)), scales = "free_y")
    else
      p <- p + facet_grid(new_formula(sym(aes_cols$panel_rows), sym(aes_cols$panel_cols)), scales = "free_y")
  }
  
  # color
  if (!is_empty(aes_cols$color))
    p <- p %+% aes_(color = sym(aes_cols$color))

  # linetype
  if (!is_empty(aes_cols$linetype)) 
    p <- p %+% aes_(linetype = sym(aes_cols$linetype))
  
  # shape_by
  if (!is_empty(aes_cols$shape)) 
    p <- p %+% aes_(shape = sym(aes_cols$shape))

  # return plot
  return(p)
}

