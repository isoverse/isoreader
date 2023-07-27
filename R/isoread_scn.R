# read isodat .scn file
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_scn <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_scan(ds)) 
    stop("data structure must be a 'scan' iso_file", call. = FALSE)
  
  # read binary file
  ds$source <- get_ds_file_path(ds) |> read_binary_isodat_file()
  
  # get scan file type
  ds <- exec_func_with_error_catch(extract_scn_file_type, ds)
  
  # get mass and cup config
  ds <- exec_func_with_error_catch(extract_scn_mass_cup_info, ds)
  
  # process file info
  if(ds$read_options$file_info) {
    # there does not seem to be creation date stored inside the scn files
    # try to pull it out from the operating system file creation info instead  
    ds <- exec_func_with_error_catch(extract_os_file_creation_datetime, ds)
    # comment
    ds <- exec_func_with_error_catch(extract_scn_file_info, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data && !is.null(ds$source$data$config)) {
    ds <- exec_func_with_error_catch(extract_scn_raw_voltage_data, ds)
  }
  
  # process method info
  if (ds$read_options$method_info && !is.null(ds$source$data$config)) {
    ds <- exec_func_with_error_catch(extract_scn_resistors, ds)
  }

  return(ds)
}

# extract scan file type
extract_scn_file_type <- function(ds) {
  # find type (= x-axis label)
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot identify scan type") |> 
    move_to_C_block_range("CPlotInfo", "CTraceInfo")
  ds$source <- ds$source |> 
    move_to_next_pattern(re_text_x(), re_unicode("Arial"), re_text_x()) |> 
    capture_data_till_pattern("type", "text", re_text_x())
  ds$file_info$type <- ds$source$data$type
  return(ds)
}

# extract file info in scn file
extract_scn_file_info <- function(ds) {
  # find comment
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot extrat comment") |>  
    move_to_C_block_range("CScanStorage", "CBinary")
  ds$source <- ds$source |> 
    move_to_next_pattern(re_x_000(), re_text_x())
  end_pos <- ds$source |> find_next_pattern(re_direct("\xff\xff"))
  
  # comment
  if ((text_length <- end_pos - ds$source$pos - 8) > 0) {
    ds$file_info$comment <- ds$source |> 
      capture_n_data("comment", "text", text_length/2) |> 
      purrr::pluck("comment")
  } else {
    ds$file_info$comment <- NA_character_
  }
  
  return(ds)
}

# extract mass and cup info
extract_scn_mass_cup_info <- function(ds){
  # find masses and cups
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot identify masses/cups") |>  
    move_to_C_block("^CPlotRange", regexp_match = TRUE) |> 
    cap_at_next_pattern(re_unicode("Administrator"))
  
  # masses
  mass_positions <- find_next_patterns(ds$source, re_unicode("Mass"))
  masses <- c()
  cups <- c()
  if (length(mass_positions) > 0) {
    for (pos in mass_positions) {
      ds$source <- ds$source |> move_to_pos(pos) |> 
        capture_data_till_pattern("mass", "text", re_not_null(2)) 
      masses <- c(masses, ds$source$data$mass)
    }
    cups <- c(stringr::str_extract(masses, "C\\d+"))
  } else {
    # cups
    cup_positions <- find_next_patterns(ds$source, re_unicode("Cup"))
    for (pos in cup_positions) {
      ds$source <- ds$source |> move_to_pos(pos) |> 
        capture_data_till_pattern("cup", "text", re_not_null(2)) 
      cups <- c(cups, ds$source$data$cup)
    }
    masses <- rep(NA_character_, length(cups))
  }

  ds$source$data$config <- tibble(
    cup = parse_number(cups) |> as.integer(),
    mass = parse_number(masses) |> as.character(),
    mass_column = ifelse(
      !is.na(.data$mass),
      sprintf("v%s.mV", .data$mass),
      # Note: okay to designate cups in this way?
      sprintf("vC%s.mV", .data$cup)
    )
  ) |> filter(!is.na(.data$cup))
  
  return(ds)
}

# extract voltage data in scn file
extract_scn_raw_voltage_data <- function(ds) {
  
  # data points
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot identify number of scan points") |>  
    move_to_C_block_range("CScanStorage", "CBinary")
  
  ds$source <- ds$source |> 
    move_to_next_pattern(re_x_000(), re_text_x())
  end_pos <- ds$source |> find_next_pattern(re_direct("\xff\xff", label = "xffxff"))
  
  ds$source <- ds$source |> 
    skip_pos(end_pos - ds$source$pos - 8) |> # skip comment 
    capture_n_data("n_points", "integer", 1) |>
    capture_n_data("n_traces", "integer", 1)
  
  # find units
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot identify scan units") |> 
    move_to_C_block_range("CVisualisationData", "CIntegrationUnitScanPart")
  ds$source <- ds$source |> 
    move_to_next_pattern(re_unicode("Arial")) |> 
    move_to_next_pattern(
      # seems to begin with this unique 88 c3 40 sequence
      re_direct("\x88\xc3\x40", label = "x88xc3x40")
    ) |> 
    move_to_next_pattern(
      # but this could be sufficient too if the above turns too specific
      re_x_000(), re_text_x()
    ) |> 
    capture_data_till_pattern("units", "text", re_null(4), re_not_null(1))
  
  # range
  ds$source <- ds$source |>
    set_binary_file_error_prefix("cannot identify scan range") |>
    move_to_C_block("^CPlotRange", regexp_match = TRUE, move_to_end = FALSE) |> 
    skip_pos(16)
  ds$source <- ds$source |>
    capture_n_data("min", "float", 1) |> 
    capture_n_data("max", "float", 1)
  
  # raw data (=voltages)
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot read raw data") |>  
    move_to_C_block_range("CBinary", "CPlotInfo") |> 
    skip_pos(16) |> 
    capture_n_data(
      "voltages", c("float", rep("double", ds$source$data$n_traces)),
      ds$source$data$n_points
    )
  voltages <- dplyr::as_tibble(ds$source$data$voltages)
  
  # safety check
  if (ncol(voltages) - 1L != nrow(ds$source$data$config)) {
    if (default("debug")) {
      log_message("voltages:\n", voltages, prefix = "DEBUG: ")
      log_message("config:\n", ds$source$data$config, prefix = "DEBUG: ")
    }
    glue::glue(
      "inconsistent number of data traces ({ncol(voltages) - 1L}) ",
      "and raw data masses/cups ({nrow(ds$source$data$config)}) recovered") |> 
    stop(call. = FALSE)
  }
  
  # set column names
  voltages <- rlang::set_names(voltages, c("step", ds$source$data$config$mass_column))
  
  # calculate x values from step
  convert_step_to_x <- function(step) {
    type <- ds$file_info$type
    if (!is.null(type) && type == "MagnetCurrent") {
      # x is in steps
      return(step)
    } else if (!is.null(type) && type == "Clock") {
      # x is in sec, step is in msec
      return(step/1000)
    } else {
      # calculate based on max and min
      return(
        ds$source$data$min + (step - min(step)) / 
          diff(range(step)) * (ds$source$data$max - ds$source$data$min)
      )
    }
  }
  
  # set raw data
  ds$raw_data <- voltages |> 
    dplyr::mutate(
      # calculate x values
      x = convert_step_to_x(.data$step),
      # set x units
      x_units = ds$source$data$units
    ) |> 
    dplyr::select("step", "x", "x_units", everything())
  
  return(ds)
  
}


# extract resistor information
# not the same format as other isodat files
extract_scn_resistors <- function(ds) {
  
  ds$source <- ds$source |> 
    move_to_C_block_range("CCupHardwarePart", "CChannelHardwarePart")
  
  cup_positions <- find_next_patterns(ds$source, re_text_x(), re_unicode("Cup"))
  cup_caps <- c(cup_positions[-1], ds$source$max_pos)
  
  cups <- c()
  ohms <- c()
  for (i in 1:length(cup_positions)) {
    ds$source <- ds$source |> move_to_pos(cup_positions[i]) |> 
      cap_at_pos(cup_caps[i]) |> 
      skip_pos(4) |> 
      capture_data_till_pattern("cup", "text", re_x_000()) |> 
      move_to_next_pattern(re_null(16), re_direct("(\xff\xfe\xff\\x00)?"), re_x_000(), re_not_null(1)) |> 
      capture_n_data("R.Ohm", "double", 1)
    cups <- c(cups, ds$source$data$cup)
    ohms <- c(ohms, ds$source$data$R.Ohm)
  }
  
  ds$method_info$resistors <-
    tibble::tibble(
      cup = parse_number(cups) |> as.integer(), 
      R.Ohm = ohms
    ) |>
    dplyr::right_join(
      select(ds$source$data$config, "cup", "mass"),
      by = "cup"
    )
  
  return(ds)
}