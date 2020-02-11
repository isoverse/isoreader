# read isodat .scn file
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_scn <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_scan(ds)) 
    stop("data structure must be a 'scan' iso_file", call. = FALSE)
  
  # read binary file
  ds$binary <- get_ds_file_path(ds) %>% read_binary_file()
  
  # get scan file type
  ds <- exec_func_with_error_catch(extract_scn_file_type, ds)
  
  # process file info
  if(ds$read_options$file_info) {
    # there does not seem to be creation date stored inside the scn files
    # try to pull it out from the operating system file creation info instead  
    ds <- exec_func_with_error_catch(extract_os_file_creation_datetime, ds)
    # comment
    ds <- exec_func_with_error_catch(extract_scn_file_info, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data) {
    ds <- exec_func_with_error_catch(extract_scn_raw_voltage_data, ds)
  }
  return(ds)
  
  # FIXME: should be possible to get resistor information but it's not
  # obvious yet how
  # # process method info
  # if (ds$read_options$method_info) {
  #   ds <- exec_func_with_error_catch(extract_isodat_resistors, ds)
  # }
  
  return(ds)
}

# extract scan file type
extract_scn_file_type <- function(ds) {
  # find type (= x-axis label)
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify scan type") %>% 
    move_to_C_block_range("CPlotInfo", "CTraceInfo")
  ds$binary <- ds$binary %>% 
    move_to_next_pattern(re_block("fef-x"), re_text("Arial"), re_block("fef-x")) %>% 
    capture_data("type", "text", re_block("fef-x"))
  ds$file_info$type <- ds$binary$data$type
  return(ds)
}

# extract file info in scn file
extract_scn_file_info <- function(ds) {
  # find comment
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot extrat comment") %>%  
    move_to_C_block_range("CScanStorage", "CBinary")
  ds$binary <- ds$binary %>% 
    move_to_next_pattern(re_block("x-000"), re_block("fef-x"))
  end_pos <- ds$binary %>% find_next_pattern(re_direct("\xff\xff"))
  
  # comment
  if ((text_length <- end_pos - ds$binary$pos - 8) > 0) {
    ds$file_info$comment <- ds$binary %>% 
      capture_n_data("comment", "text", text_length/2) %>% 
      { .$data$comment }
  } else {
    ds$file_info$comment <- NA_character_
  }
  
  return(ds)
}

# extract voltage data in scn file
extract_scn_raw_voltage_data <- function(ds) {
  
  # data points
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify number of scan points") %>%  
    move_to_C_block_range("CScanStorage", "CBinary")
  
  ds$binary <- ds$binary %>% 
    move_to_next_pattern(re_block("x-000"), re_block("fef-x"))
  end_pos <- ds$binary %>% find_next_pattern(re_direct("\xff\xff", label = "xffxff"))
  
  ds$binary <- ds$binary %>% 
    skip_pos(end_pos - ds$binary$pos - 8) %>% # skip comment 
    capture_n_data("n_points", "integer", 1) %>%
    capture_n_data("n_traces", "integer", 1)
  
  # find units
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify scan units") %>% 
    move_to_C_block_range("CVisualisationData", "CIntegrationUnitScanPart")
  ds$binary <- ds$binary %>% 
    move_to_next_pattern(re_text("Arial")) %>% 
    move_to_next_pattern(
      # seems to begin with this unique 88 c3 40 sequence
      re_direct("\x88\xc3\x40", label = "x88xc3x40")
    ) %>% 
    move_to_next_pattern(
      # but this could be sufficient too if the above turns too specific
      re_block("x-000"), re_block("fef-x")
    ) %>% 
    capture_data("units", "text", re_null(4), re_not_null(1))
  
  # range
  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot identify scan range") %>%
    move_to_C_block("^CPlotRange", regexp_match = TRUE, move_to_end = FALSE) %>% 
    skip_pos(16)
  ds$binary <- ds$binary %>%
    capture_n_data("min", "float", 1) %>% 
    capture_n_data("max", "float", 1)
  
  # find masses and cups
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify masses/cups") %>%  
    move_to_C_block("^CPlotRange", regexp_match = TRUE) %>% 
    cap_at_C_block("CIntegrationUnitGasConfPart")
    
  # masses
  mass_positions <- find_next_patterns(ds$binary, re_text("Mass"))
  masses <- c()
  cups <- c()
  if (length(mass_positions) > 0) {
    for (pos in mass_positions) {
      ds$binary <- ds$binary %>% move_to_pos(pos) %>% 
        capture_data("mass", "text", re_not_null(2)) 
      masses <- c(masses, ds$binary$data$mass)
    }
    cups <- c(stringr::str_extract(masses, "C\\d"))
  } else {
    # cups
    cup_positions <- find_next_patterns(ds$binary, re_text("Cup"))
    for (pos in cup_positions) {
      ds$binary <- ds$binary %>% move_to_pos(pos) %>% 
        capture_data("cup", "text", re_not_null(2)) 
      cups <- c(cups, ds$binary$data$cup)
    }
    masses <- rep(NA_character_, length(cups))
  }
  
  # configuration
  config <- tibble(
    cup = parse_number(cups) %>% as.integer(),
    mass = parse_number(masses) %>% as.character(),
    mass_column = ifelse(
      !is.na(mass),
      sprintf("v%s.mV", mass),
      # Note: should cups be designated in some other way??
      sprintf("v%s.mV", cup)
    )
  )
  
  # raw data (=voltages)
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot read raw data") %>%  
    move_to_C_block_range("CBinary", "CPlotInfo") %>% 
    skip_pos(16) %>% 
    capture_n_data(
      "voltages", c("float", rep("double", ds$binary$data$n_traces)),
      ds$binary$data$n_points
    )
  
  voltages <- ds$binary$data$voltages %>% 
    tibble::as_tibble() %>% 
    setNames(c("step", config$mass_column))
  
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
        ds$binary$data$min + (step - min(step)) / 
          diff(range(step)) * (ds$binary$data$max - ds$binary$data$min)
      )
    }
  }
  
  # set raw data
  ds$raw_data <- voltages %>% 
    dplyr::mutate(
      # calculate x values
      x = convert_step_to_x(step),
      # set units
      units = ds$binary$data$units
    ) %>% 
    dplyr::select(step, x, units, everything())
  
  return(ds)
  
}
