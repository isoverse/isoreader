# read isodat .dxf file
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_dxf <- function(ds, options = list()) {

  # safety checks
  if(!iso_is_continuous_flow(ds)) 
    stop("data structure must be a 'continuous_flow' iso_file", call. = FALSE)
  
  # read binary file
  ds$source <- get_ds_file_path(ds) |> read_binary_isodat_file()
  
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
      cap_at_fun = function(bin) cap_at_pos(bin, find_next_pattern(bin, re_unicode("DetectorDataBlock"))))
  }
  
  return(ds)

}


# extract voltage data in dxf file
extract_dxf_raw_voltage_data <- function(ds) {
  
  # search gas configurations
  # (the larger block coming afterwards is not always present so not used as max pos here)
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot identify measured masses") |>  
    move_to_C_block_range("CEvalDataIntTransferPart", "CBinary") 
  
  # find all gas configurations
  configs <- list()
  gas_config_name_re <- re_combine(re_text_x(), re_block("alpha"), re_text_0(), re_text_x())
  config_positions <- ds$source |> find_next_patterns(gas_config_name_re)
  config_caps <- c(config_positions[-1], ds$source$max_pos)
  if (length(config_positions) == 0) return(ds)
  
  for(i in 1:length(config_positions)) {
    # find name of gas configuration
    ds$source <- ds$source |> 
      move_to_pos(config_positions[i]) |> 
      move_to_next_pattern(re_text_x(), max_gap = 0) |> 
      capture_data_till_pattern("gas", "text", re_text_0(), re_text_x()) 
    
    # make sure we have the right starts and caps for each configuration
    if (ds$source$data$gas %in% names(configs)) {
      # raise cap on previous
      configs[[ds$source$data$gas]]$cap <- config_caps[i]
    } else {
      # new config
      configs[[ds$source$data$gas]] <- list(pos = config_positions[i], cap = config_caps[i], masses = c())
    }
  }
  
  # safety check
  if (length(configs) == 0) stop("could not find gas configurations", call. = FALSE)
  
  # find all masses
  for (config in names(configs)) {
    if (default(debug)) 
      log_message("processing config '", config, "' (", configs[[config]]$pos, "-", configs[[config]]$cap, ")")
    ds$source <- ds$source |> 
      move_to_pos(configs[[config]]$pos) |> 
      cap_at_pos(configs[[config]]$cap)
    
    intensity_id <- 1
    while(!is.null(find_next_pattern(ds$source, re_unicode(str_c("rIntensity", intensity_id))))) {
      ds$source <- ds$source |>
        move_to_next_pattern(re_unicode(str_c("rIntensity", intensity_id))) |>
        move_to_next_pattern(re_text_x(), re_unicode("rIntensity "), max_gap = 0) |>
        capture_data_till_pattern("mass", "text", re_text_x(), move_past_dots = TRUE)
      configs[[config]]$masses <- c(configs[[config]]$masses, ds$source$data$mass)
      intensity_id <- intensity_id + 1
    }
  }
  
  # find gas config alternative names (sometimes set, sometimes not)
  ds$source <- ds$source |> 
    move_to_C_block_range("CPeakFindParameter", "CResultArray") 
  smoothing_positions <- find_next_patterns(ds$source, re_unicode("Smoothing"))
  gas_name_end_re <- re_combine(re_null(4), re_direct("[\x01-\xff]", label = "x01-xff"))
  gas_name_re <- re_combine(re_text_x(), re_block("text0"), gas_name_end_re)
  
  for (pos in smoothing_positions) {
    ds$source <- ds$source |>
      move_to_pos(pos, reset_cap = TRUE)
    
    ds$source <- ds$source |>
      cap_at_pos(find_next_pattern(ds$source, re_unicode("Peak Center"))) |>
      move_to_next_pattern(gas_name_re, move_to_end = FALSE) |>
      skip_pos(4) |> # skip the fef-x at the beginning
      capture_data_till_pattern("gas_name1", "text", gas_name_end_re, data_bytes_max = 50)
    gas_name1 <- ds$source$data$gas_name1
    
    # gas name 2
    next_gas_name <- find_next_pattern(ds$source, gas_name_re)
    if (!is.null(next_gas_name) && next_gas_name < ds$source$max_pos) {
      ds$source <- ds$source |> 
        move_to_next_pattern(gas_name_re, move_to_end = FALSE) |>
        skip_pos(4) |> # skip the fef-x at the beginning
        capture_data_till_pattern("gas_name2", "text", gas_name_end_re, data_bytes_max = 50)
      gas_name2 <-  ds$source$data$gas_name2
      
      # update config with alternative name
      if (gas_name2 != "" && gas_name1 != gas_name2 &&
          any(config_idx <- gas_name1 == names(configs))) {
        if (default(debug))
          glue::glue(
            "renaming config '{gas_name1}' to '{gas_name2}' ",
            "(non-standard config name)") |>
          log_message()
        names(configs)[config_idx] <- gas_name2
      }
    }
  }

  # move to beginning of original data to get voltages
  ds$source <- ds$source |>
    set_binary_file_error_prefix("cannot recover raw voltages") |>
    move_to_C_block_range("CAllMoleculeWeights", "CMethod") |>
    move_to_next_C_block("CStringArray") |>
    move_to_next_pattern(re_unicode("OrigDataBlock"), re_null(4), re_block("stx"))

  # find all data sets
  data_start_re <- re_combine(
    re_text_0(), re_null(4), re_x_000(), re_x_000(), 
    re_direct("..", size = 2, label = ".."), re_x_000())
  data_end_re <- re_combine(
    re_direct(".{4}", label = ".{4}"), re_null(4), 
    re_text_0(), re_block("stx"))
  gas_config_re <- re_combine(re_text_x(), re_block("text"), re_text_0())
  voltages <- tibble()
  positions <- find_next_patterns(ds$source, data_start_re)

  for (pos in positions) {
    # move to beginning of data
    ds$source <- ds$source |> move_to_pos(pos + data_start_re$size + 4L) # 4 byte gap before data
    start_pos <- ds$source$pos

    # find gas configuration name
    capture <- ds$source |>
      move_to_next_pattern(data_end_re) |>
      move_to_next_pattern(gas_config_re, move_to_end = FALSE, max_gap = 20) |>
      skip_pos(4) |> # skip the fef-x at the beginning
      capture_data_till_pattern("gas", "text", re_text_0(), data_bytes_max = 50)
    
    gas_data_block_end <- list(gas = capture$data$gas, pos = capture$pos)
    gas_config <- gas_data_block_end$gas

    # debug message
    if (default(debug))
      glue::glue("processing data for '{gas_config}' ({start_pos}-{gas_data_block_end$pos})") |>
      log_message()

    # find gas configuration
    if (!gas_config %in% names(configs))
      glue::glue("could not find gas configuration for gas '{gas_config}', ",
                 "available: '{paste(names(configs), collapse = \"', '\")}'") |>
      stop(call. = FALSE)

    # find gas configuration masses
    masses <- configs[[gas_config]]$masses
    if (is.null(masses)) stop("could not identify measured ions for gas '", gas_config, "'", call. = FALSE)
    masses_columns <- str_c("v", masses, ".mV")

    # save voltage data
    ds$source <- ds$source |>
      capture_data_till_pattern("voltages", c("float", rep("double", length(masses))), data_end_re)
    voltages <- bind_rows(voltages,
                          ds$source$data$voltages |>
                            dplyr::as_tibble() |> rlang::set_names(c("time.s", masses_columns)))
  }

  # check for data
  if (nrow(voltages) == 0) stop("could not find raw voltage data", call. = FALSE)

  # add time point column
  ds$raw_data <-
    voltages |> arrange(.data$time.s) |>
    mutate(tp = 1:n()) |>
    select("tp", "time.s", everything())
 
  return(ds) 
}
