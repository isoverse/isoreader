# read isox files for their continuous flow data
# @param ds the iso_file data structure to fill
# @param custom reader options - none needed
iso_read_isox <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_file(ds) || !is(ds, "orbitrap")) 
    stop("data structure must be a 'orbitrap' iso_file", call. = FALSE)
  
  # process file info
  if(ds$read_options$file_info) {
    # TODO: process sequence info (from different file?)
  }
  
  # process raw data
  if (ds$read_option$raw_data) {
    # TODO: read from raw data file?
  }
  
  # process pre-evaluated data table
  if (ds$read_options$vendor_data_table) {
    ds <- exec_func_with_error_catch(extract_orbitrap_vendor_data_table, ds)
  }
  
  return(ds)
}

# extract data table from isox file
extract_orbitrap_vendor_data_table <- function(ds) {
  ds$vendor_data_table <- readr::read_tsv(get_ds_file_path(ds))
  return(ds)
}