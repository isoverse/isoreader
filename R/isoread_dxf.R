# read isodat .dxf file
# @param ds the data structure to fill
isoread_dxf <- function(ds, ...) {

  # safety checks
  if(!is_isofile(ds) || !is(ds, "continuous_flow")) 
    stop("data structure must be a 'continuous_flow' isofile", call. = FALSE)
  
  # read binary file
  ds$binary <- read_binary_file(ds$file_info$file_path)
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_sequence_line_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_measurement_info, ds)
  }
  
  # process method info
  if (ds$read_options$method_info) {
    ds <- exec_func_with_error_catch(extract_primary_standard_information, ds)
    ds <- exec_func_with_error_catch(extract_secondary_standard_information, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data) {
    ds <- exec_func_with_error_catch(extract_dxf_raw_voltage_data, ds)
  }
  
  return(ds)

}


# extract voltage data in did file
extract_dxf_raw_voltage_data <- function(ds) {
  
  # move to beginning of intensity information (the larger block coming 
  # afterwards is not always present so not used as max pos here)
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify measured masses") %>%  
    move_to_C_block("CEvalDataItemTransferPart") 
  
  configs <- list()
  while(!is.null(find_next_pattern(ds$binary, re_text("Overwritten")))) {
    
    # find name of gas configuration
    ds$binary <- ds$binary %>% 
      move_to_next_pattern(re_text("Overwritten")) %>% 
      move_to_next_pattern(re_block("etx"), re_text("/"), re_block("fef-0")) %>% 
      move_to_next_pattern(re_block("fef-x"), re_block("text"), re_block("fef-0"), re_block("fef-x"), move_to_end = FALSE) %>% 
      move_to_next_pattern(re_block("fef-x"), max_gap = 0) %>% 
      capture_data("gas", "text", re_block("fef-0"), re_block("fef-x")) 
    
    # don't double process if same gas configuration listed twice
    if (ds$binary$data$gas %in% names(configs)) next
    configs[[ds$binary$data$gas]] <- c()
    
    # find all masses
    intensity_id <- 1
    while(!is.null(find_next_pattern(ds$binary, re_text(str_c("rIntensity", intensity_id))))) {
      ds$binary <- ds$binary %>% 
        move_to_next_pattern(re_text(str_c("rIntensity", intensity_id))) %>% 
        move_to_next_pattern(re_block("fef-x"), re_text("rIntensity "), max_gap = 0) %>% 
        capture_data("mass", "text", re_block("fef-x"), move_past_dots = TRUE)
      configs[[ds$binary$data$gas]] <- c(configs[[ds$binary$data$gas]], ds$binary$data$mass)
      intensity_id <- intensity_id + 1
    }
    
  }  
  
  # move to beginning of original data
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover raw voltages") %>% 
    move_to_C_block_range("CAllMoleculeWeights", "CMethod") %>% 
    move_to_next_C_block("CStringArray") %>% 
    move_to_next_pattern(re_text("OrigDataBlock"), re_null(4), re_block("stx"))
  
  # find all data sets
  data_start_re <- re_combine(
    re_block("fef-0"), re_null(4), re_block("x-000"), re_block("x-000"), re_direct(".."), 
    re_block("x-000"), re_direct(".."), re_null(2))
  voltages <- data_frame()
  while (!is.null(find_next_pattern(ds$binary, data_start_re))) {
    # move to beginning of data
    ds$binary <- ds$binary %>% move_to_next_pattern(data_start_re)
    
    # find gas configuration name
    gas_config <- ds$binary %>% 
      move_to_next_pattern(re_direct(".."), re_null(6), re_block("fef-0")) %>% 
      move_to_next_pattern(re_block("etx"), re_block("x-000"), re_block("x-000"), re_block("fef-x")) %>% 
      capture_data("gas", "text", re_block("fef-0"), re_null(4), re_direct(".."), re_block("etx"), re_text("/"),
                   re_block("fef-0"), data_bytes_max = 50) %>% 
                   { .$data$gas }
    
    # find gas configuration masses
    masses <- configs[[gas_config]]
    if (is.null(masses)) stop("could not identify measured ions for gas ", gas_config, call. = FALSE)
    masses_columns <- str_c("v", masses, ".mV")
    
    # save voltage data
    ds$binary <- ds$binary %>% 
      capture_data("voltages", c("float", rep("double", length(masses))),  
                   re_direct(".."), re_null(6), re_block("fef-0")) 
    voltages <- bind_rows(voltages, 
                          ds$binary$data$voltages %>% 
                            as_data_frame() %>% setNames(c("time.s", masses_columns)))
  }
  
  # add time point column
  ds$raw_data <-
    voltages %>% arrange(time.s) %>% 
    mutate(tp = 1:n()) %>% 
    select(tp, time.s, everything())
 
  return(ds) 
}
