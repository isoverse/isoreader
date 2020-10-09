# read isox files for their continuous flow data
# @param ds the iso_file data structure to fill
# @param custom reader options - none needed
iso_read_isox <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_file(ds) || !is(ds, "orbitrap")) 
    stop("data structure must be a 'orbitrap' iso_file", call. = FALSE)
  
  # process file info
  if(ds$read_options$file_info) {
    # TODO: process sequence info (from sequence file? or merge later?)
    ds$file_info$file_id <- stringr::str_remove(ds$file_info$file_id, "\\.isox$")
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
  
  # read function
  quiet_read_tsv <- purrr::quietly(readr::read_tsv)
  
  # data columns
  columns <- readr::cols_only(
    Compound = readr::col_character(),
    Isotopolog = readr::col_character(),
    `Number of Ions` = readr::col_double(),
    `Measured Mass:` = readr::col_double(),
    `Ret. Time:` = readr::col_double(),
    `Scan Number:` = readr::col_double(),
    `Abs. Intensity:` = readr::col_double(),
    `Peak Noise` = readr::col_double(),
    `FT Resolution:` = readr::col_double(),
    `Number of Packets:` = readr::col_double(),
    `IT [ms]:` = readr::col_double(),
    `TIC:` = readr::col_double(),
    `TIC*IT:` = readr::col_double(),
    `Rel. Intensity:` = readr::col_double(),
    `Peak Resolution` = readr::col_double(),
    `Peak Baseline` = readr::col_double(),
    `Deviation [ppm]:` = readr::col_double()
    #`Deviation [mmu]:` = readr::col_double(),  
    #`Elapsed scan time [ms]:` = readr::col_double(),
    #`Target:` = readr::col_logical(),
    #`RF [V]:` = readr::col_double(),
    #`Lockmass correction [ppm]:` = readr::col_logical(),
    #`Lockmass #1 [m/z]:` = readr::col_double(),
    #`Lockmass #2 [m/z]:` = readr::col_double(),
    #`Lockmass #3 [m/z]:` = readr::col_double(),
    #A = readr::col_double(),
    #B = readr::col_double(),
    #C = readr::col_double(),
    #Vt = readr::col_double(),
    #Mode = readr::col_double(),
    #`Peak Flags` = readr::col_character(),
  )
  
  # read vendor data table
  tsv_read <- quiet_read_tsv(get_ds_file_path(ds), col_types = columns)
  
  # send unexpected warnings
  if (any(warn <- !stringr::str_detect(tsv_read$warnings, "column names filled")))
    for (msg in tsv_read$warnings[warn]) warning(msg, immediate. = TRUE, call. = FALSE)
  
  # finalize column names
  vdt <- tsv_read$result
  cols <- intersect(names(columns$cols), names(vdt)) # uniform column order
  vdt <- vdt[cols]
  names(vdt) <- stringr::str_remove(names(vdt), ":$") # skip terminal colons
  vdt <- iso_make_units_implicit(vdt) # make units of type '[unit]' implicit
  ds$vendor_data_table <- vdt
  
  return(ds)
}