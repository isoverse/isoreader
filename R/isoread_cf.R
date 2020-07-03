# read isodat .cf file
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_cf <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_continuous_flow(ds)) 
    stop("data structure must be a 'continuous_flow' iso_file", call. = FALSE)
  
  # read binary file
  ds$binary <- get_ds_file_path(ds) %>% read_binary_file()
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_old_sequence_line_info, ds)
    # NOTE: measurement info (see dxf) does not seem to be stored in cf files
    ds <- exec_func_with_error_catch(extract_isodat_datetime, ds)
    ds <- exec_func_with_error_catch(extract_H3_factor_info, ds)
    ds <- exec_func_with_error_catch(extract_MS_integration_time_info, ds)
  }
   
  # process raw data
  if (ds$read_option$raw_data) {
    ds <- exec_func_with_error_catch(extract_cf_raw_voltage_data, ds)
  }

  # process method info
  if (ds$read_options$method_info) {
    ds <- exec_func_with_error_catch(
      extract_isodat_reference_values, ds, 
      function(bin) cap_at_pos(bin, find_next_pattern(bin, re_text("Administrator"))))
    ds <- exec_func_with_error_catch(extract_isodat_resistors, ds)
  }
  
  # process pre-evaluated data table
  block <- start <- NULL # global vars
  if (ds$read_options$vendor_data_table) {
    ds <- exec_func_with_error_catch(
      extract_isodat_continuous_flow_vendor_data_table, ds, 
      cap_at_fun = function(bin) {
        C_blocks <- filter(bin$C_blocks, block == "CRawData", start >= bin$pos)
        if (nrow(C_blocks) > 0) {
          cap_at_next_C_block(bin, "CRawData")
        } else {
          cap_at_next_C_block(bin, "CErrorGridStorage")
        }
      })
  }
     
  return(ds)
}

# extract voltage data in cf file
extract_cf_raw_voltage_data <- function(ds) {
  # move to beginning of intensity information (the larger block coming 
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify measured masses") %>%  
    # can have data in multiple positions (e.g. if peak jumping) throughout the rest of the binary
    move_to_C_block("CRawDataScanStorage", reset_cap = TRUE) 
  
  # get trace positions
  gas_positions <- ds$binary %>% 
    find_next_patterns(re_block("fef-0"), re_block("fef-x"), re_text("Trace Data "), re_block("text"), re_null(4), re_block("stx"))
  
  # raw_data
  raw_data <- tibble::tibble()
  
  # loop through gas positions
  for (gas_pos in gas_positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(gas_pos) %>% 
      skip_pos(30) %>% 
      capture_data("gas", "text", re_null(4), re_block("stx"))
    
    gas_config <- ds$binary$data$gas
    
    # data start
    data_start_re <- re_combine(
      re_block("stx"), re_block("fef-0"), re_block("stx"), 
      re_direct(".{4}", size = 4, label = ".{4}"))
    ds$binary <- ds$binary %>% move_to_next_pattern(data_start_re)
    data_start <- ds$binary$pos
    
    # find all masses at end of data
    data_end_re <- re_combine(
      re_direct(".{2}", size = 2, label = ".{2}"), re_block("stx"), 
      re_block("fef-0"), re_block("stx"), re_null(4))
    ds$binary <- ds$binary %>% move_to_next_pattern(data_end_re)
    data_end <- ds$binary$pos - data_end_re$size
    
    mass_re <- re_combine(re_block("fef-x"), re_text("Mass "))
    mass_positions <- ds$binary %>% 
      cap_at_next_pattern(re_text("MS/Clock")) %>% 
      find_next_patterns(mass_re)
    
    masses <- c()
    for (pos in mass_positions) {
      # a bit tricky to capture but this should do the trick reliably
      raw_mass <- 
        ds$binary %>% move_to_pos(pos + mass_re$size) %>% 
        capture_data("mass", "raw", re_block("fef-x"), ignore_trailing_zeros = FALSE) %>% 
        { .$data$mass }
      text_mass <- parse_raw_data(grepRaw("^([0-9]\\x00)+", raw_mass, value = TRUE), type = "text")
      masses <- c(masses, text_mass)
    }
    
    if (is.null(masses)) stop("could not identify measured ions for gas '", gas_config, "'", call. = FALSE)
    masses_columns <- str_c("v", masses, ".mV")
    
    # read in data
    n_data_points <- (data_end - data_start)/(4L + length(masses) * 8L)
    if (n_data_points %% 1 > 0)
      stop("number of data points for ", gas_config, " is not an integer (", n_data_points, ")", call. = FALSE)
    
    ds$binary<- ds$binary %>% 
      move_to_pos(data_start) %>% 
      capture_n_data("voltages", c("float", rep("double", length(masses))), n_data_points)
    voltages <- bind_rows(ds$binary$data$voltages %>% dplyr::as_tibble() %>% rlang::set_names(c("time.s", masses_columns)))
    
    # check for data
    if (nrow(voltages) == 0) 
      stop("could not find raw voltage data for gas ", gas_config, call. = FALSE)
    
    # raw data
    raw_data <- dplyr::bind_rows(raw_data, voltages)
    
  }
  
  # add time point column
  ds$raw_data <-
    raw_data %>% arrange(.data$time.s) %>%
    mutate(tp = 1:n()) %>%
    select(.data$tp, .data$time.s, everything())
  
  return(ds)
}
