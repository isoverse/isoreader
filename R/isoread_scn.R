# read isodat .scn file
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_scn <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_scan(ds)) 
    stop("data structure must be a 'scan' iso_file", call. = FALSE)
  
  # read binary file
  ds$binary <- get_ds_file_path(ds) %>% read_binary_file()
  
  # # process file info
  # if(ds$read_options$file_info) {
  #   ds <- exec_func_with_error_catch(extract_isodat_sequence_line_info, ds)
  #   ds <- exec_func_with_error_catch(extract_isodat_measurement_info, ds)
  #   ds <- exec_func_with_error_catch(extract_isodat_datetime, ds)
  #   ds <- exec_func_with_error_catch(extract_H3_factor_info, ds)
  #   ds <- exec_func_with_error_catch(extract_MS_integration_time_info, ds)
  # }
  # 
  # # process raw data
  # if (ds$read_option$raw_data) {
  #   ds <- exec_func_with_error_catch(extract_dxf_raw_voltage_data, ds)
  # }
  # 
  # # process method info
  # if (ds$read_options$method_info) {
  #   ds <- exec_func_with_error_catch(
  #     extract_isodat_reference_values, ds, 
  #     cap_at_fun = function(bin) cap_at_next_C_block(bin, "CResultArray"))
  #   ds <- exec_func_with_error_catch(extract_isodat_resistors, ds)
  # }
  # 
  # # process pre-evaluated data table
  # if (ds$read_options$vendor_data_table) {
  #   ds <- exec_func_with_error_catch(
  #     extract_isodat_continuous_flow_vendor_data_table, ds, 
  #     cap_at_fun = function(bin) cap_at_pos(bin, find_next_pattern(bin, re_text("DetectorDataBlock"))))
  # }
  # 
  return(ds)
  
}
