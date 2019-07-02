# read isodat .dxf file
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_dxf <- function(ds, options = list()) {

  # safety checks
  if(!iso_is_continuous_flow(ds)) 
    stop("data structure must be a 'continuous_flow' iso_file", call. = FALSE)
  
  # read binary file
  ds$binary <- get_ds_file_path(ds) %>% read_binary_file()
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_sequence_line_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_measurement_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_datetime, ds)
    ds <- exec_func_with_error_catch(extract_H3_factor_info, ds)
    ds <- exec_func_with_error_catch(extract_MS_integration_time_info, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data) {
    ds <- exec_func_with_error_catch(extract_dxf_raw_voltage_data, ds)
  }
  
  # process method info
  if (ds$read_options$method_info) {
    ds <- exec_func_with_error_catch(
      extract_isodat_reference_values, ds, 
      cap_at_fun = function(bin) cap_at_next_C_block(bin, "CResultArray"))
    ds <- exec_func_with_error_catch(extract_isodat_resistors, ds)
  }
  
  # process pre-evaluated data table
  if (ds$read_options$vendor_data_table) {
    ds <- exec_func_with_error_catch(
      extract_isodat_continuous_flow_vendor_data_table, ds, 
      cap_at_fun = function(bin) cap_at_pos(bin, find_next_pattern(bin, re_text("DetectorDataBlock"))))
  }
  
  return(ds)

}


# extract voltage data in dxf file
extract_dxf_raw_voltage_data <- function(ds) {
  
  # move to beginning of intensity information (the larger block coming 
  # afterwards is not always present so not used as max pos here)
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify measured masses") %>%  
    move_to_C_block_range("CEvalDataItemTransferPart", "CBinary") 
  
  configs <- list()
  gas_config_re <- re_text("Overwritten")
  config_positions <- ds$binary %>% find_next_patterns(gas_config_re)
  if (length(config_positions) == 0) stop("could not find gas configurations", call. = FALSE)
  config_caps <- c(config_positions[-1], ds$binary$max_pos)
  
  for(i in 1:length(config_positions)) {

    # find name of gas configuration
    ds$binary <- ds$binary %>% 
      move_to_pos(config_positions[i] + gas_config_re$size) %>% 
      move_to_next_pattern(re_block("etx"), re_text("/"), re_block("fef-0")) %>% 
      move_to_next_pattern(re_block("fef-x"), re_block("text"), re_block("fef-0"), re_block("fef-x"), move_to_end = FALSE) %>% 
      move_to_next_pattern(re_block("fef-x"), max_gap = 0) %>% 
      capture_data("gas", "text", re_block("fef-0"), re_block("fef-x")) 
    
    # make sure we have the right starts and caps for each configuration
    if (ds$binary$data$gas %in% names(configs)) {
      # raise cap on previous
      configs[[ds$binary$data$gas]]$cap <- config_caps[i]
    } else {
      # new config
      configs[[ds$binary$data$gas]] <- list(pos = config_positions[i], cap = config_caps[i], masses = c())
    }
  }  
  
  # find all masses
  for (config in names(configs)) {
    if (default(debug)) 
      log_message("processing config '", config, "' (", configs[[config]]$pos, "-", configs[[config]]$cap, ")")
    ds$binary <- ds$binary %>% 
      move_to_pos(configs[[config]]$pos) %>% 
      cap_at_pos(configs[[config]]$cap)
    
    intensity_id <- 1
    while(!is.null(find_next_pattern(ds$binary, re_text(str_c("rIntensity", intensity_id))))) {
      ds$binary <- ds$binary %>%
        move_to_next_pattern(re_text(str_c("rIntensity", intensity_id))) %>%
        move_to_next_pattern(re_block("fef-x"), re_text("rIntensity "), max_gap = 0) %>%
        capture_data("mass", "text", re_block("fef-x"), move_past_dots = TRUE)
      configs[[config]]$masses <- c(configs[[config]]$masses, ds$binary$data$mass)
      intensity_id <- intensity_id + 1
    }
  }
  
  # find gas config alternative names (sometimes set, sometimes not)
  ds$binary <- ds$binary %>% 
    move_to_C_block_range("CPeakFindParameter", "CResultArray") 
  smoothing_positions <- find_next_patterns(ds$binary, re_text("Smoothing"))
  peak_center_positions <- find_next_patterns(ds$binary, re_text("Peak Center"))
  gas_name_end_re <- re_combine(re_null(4), re_direct("[\x01-\xff]"))
  gas_name_re <- re_combine(re_block("fef-x"), re_block("text0"), gas_name_end_re)
  
  for (pos in smoothing_positions) {
    gas_names <- ds$binary %>% 
      move_to_pos(pos) %>% 
      { cap_at_pos(., find_next_pattern(., re_text("Peak Center"))) } %>% 
      move_to_next_pattern(gas_name_re, move_to_end = FALSE) %>% 
      skip_pos(4) %>% # skip the fef-x at the beginning
      capture_data("gas_name1", "text", gas_name_end_re, data_bytes_max = 50) %>% 
      move_to_next_pattern(gas_name_re, move_to_end = FALSE) %>% 
      skip_pos(4) %>% # skip the fef-x at the beginning
      capture_data("gas_name2", "text", gas_name_end_re, data_bytes_max = 50) %>% 
      { .$data[c("gas_name1", "gas_name2")] }
    
    # update config with alternative name
    if (gas_names$gas_name2 != "" && gas_names$gas_name1 != gas_names$gas_name2 && 
        any(config_idx <- gas_names$gas_name1 == names(configs))) {
      if (default(debug)) 
        glue::glue(
          "renaming config '{gas_names$gas_name1}' to '{gas_names$gas_name2}' ",
          "(non-standard config name)") %>% 
        log_message()
      names(configs)[config_idx] <- gas_names$gas_name2
    }
  }
  
  # move to beginning of original data to get voltages
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover raw voltages") %>% 
    move_to_C_block_range("CAllMoleculeWeights", "CMethod") %>% 
    move_to_next_C_block("CStringArray") %>% 
    move_to_next_pattern(re_text("OrigDataBlock"), re_null(4), re_block("stx"))
  
  # find all data sets
  data_start_re <- re_combine(re_block("fef-0"), re_null(4), re_block("x-000"), re_block("x-000"), re_direct("..", size = 2), re_block("x-000"))
  data_end_re <- re_combine(re_direct(".{4}"), re_null(4), re_block("fef-0"), re_block("stx"))
  gas_config_re <- re_combine(re_block("fef-x"), re_block("text"), re_block("fef-0"))
  voltages <- data_frame()
  positions <- find_next_patterns(ds$binary, data_start_re)
  
  for (pos in positions) {
    # move to beginning of data
    ds$binary <- ds$binary %>% move_to_pos(pos + data_start_re$size + 4L) # 4 byte gap before data
    start_pos <- ds$binary$pos
    
    # find gas configuration name
    gas_data_block_end <- ds$binary %>% 
      move_to_next_pattern(data_end_re) %>% 
      move_to_next_pattern(gas_config_re, move_to_end = FALSE, max_gap = 20) %>% 
      skip_pos(4) %>% # skip the fef-x at the beginning
      capture_data("gas", "text", re_block("fef-0"), data_bytes_max = 50) %>% 
      { list(gas = .$data$gas, pos = .$pos) }
    gas_config <- gas_data_block_end$gas
    
    # debug message
    if (default(debug)) 
      glue::glue("processing data for '{gas_config}' ({start_pos}-{gas_data_block_end$pos})") %>% 
      log_message()
    
    # find gas configuration
    if (!gas_config %in% names(configs)) 
      glue::glue("could not find gas configuration for gas '{gas_config}', ",
                 "available: '{paste(names(configs), collapse = \"', '\")}'") %>% 
      stop(call. = FALSE)
    
    # find gas configuration masses
    masses <- configs[[gas_config]]$masses
    if (is.null(masses)) stop("could not identify measured ions for gas '", gas_config, "'", call. = FALSE)
    masses_columns <- str_c("v", masses, ".mV")
    
    # save voltage data
    ds$binary <- ds$binary %>% 
      capture_data("voltages", c("float", rep("double", length(masses))), data_end_re)
    voltages <- bind_rows(voltages, 
                          ds$binary$data$voltages %>% 
                            as_data_frame() %>% setNames(c("time.s", masses_columns)))
  }
  
  # check for data
  if (nrow(voltages) == 0) stop("could not find raw voltage data", call. = FALSE)
  
  # add time point column
  tp <- time.s <- NULL # global vars
  ds$raw_data <-
    voltages %>% arrange(time.s) %>% 
    mutate(tp = 1:n()) %>% 
    select(tp, time.s, everything())
 
  return(ds) 
}
