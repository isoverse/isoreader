# read isodat .caf files
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_caf <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_dual_inlet(ds)) 
    stop("data structure must be a 'dual_inlet' iso_file", call. = FALSE)
  
  # read binary file
  ds$binary <- get_ds_file_path(ds) %>% read_binary_file()
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_old_sequence_line_info, ds)
    # NOTE: measurement info (see did) does not seem to be stored in caf files
    ds <- exec_func_with_error_catch(extract_isodat_datetime, ds)
    # NOTE: the following extraction is not tested but should work (don't have a .caf file with H3 factor)
    ds <- exec_func_with_error_catch(extract_H3_factor_info, ds)
    ds <- exec_func_with_error_catch(extract_MS_integration_time_info, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data){
    ds <- exec_func_with_error_catch(extract_caf_raw_voltage_data, ds)
  }
  
  # process method info
  if (ds$read_options$method_info) {
    ds <- exec_func_with_error_catch(
      extract_isodat_reference_values, ds,
      function(bin) cap_at_pos(bin, find_next_pattern(bin, re_text("Administrator"))))
    ds <- exec_func_with_error_catch(extract_isodat_resistors, ds)
  }
  
  # process pre-evaluated data table
  if (ds$read_options$vendor_data_table)
    ds <- exec_func_with_error_catch(extract_caf_vendor_data_table, ds)
  
  return(ds)
}

# extract voltage data in caf file
extract_caf_raw_voltage_data <- function(ds) {

  # locate masses
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify measured masses") %>% 
    move_to_C_block_range("CResultData", "CEvalDataIntTransferPart") 
  
  # read all masses
  masses_re <- re_combine(re_block("x-000"), re_block("fef-x"), re_text("rIntensity"))
  masses <- 
    tibble(
      pos = find_next_patterns(ds$binary, masses_re) + masses_re$size,
      # capture cup and mass
      data = map(.data$pos, function(pos) {
        ds$binary %>%
          move_to_pos(pos) %>%  
          capture_data("cup", "text", re_block("fef-x"), data_bytes_max = 8, move_past_dots = TRUE) %>%
          move_to_next_pattern(re_text("rIntensity "), max_gap = 0L) %>% 
          capture_data("mass", "text", re_block("fef-x"), data_bytes_max = 8) %>% 
          { dplyr::as_tibble(.$data[c("cup", "mass")]) }
      })
    ) %>% 
    # unnest data
    unnest(.data$data) %>% 
    mutate(
      cup = as.integer(.data$cup),
      column = str_c("v", .data$mass, ".mV")
    )

  # locate voltage data
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot locate voltage data") %>% 
    move_to_C_block_range("CDualInletRawData", "CResultData") 
  
  # find binary positions for voltage standards and samples
  standard_block_start <- find_next_pattern(
    ds$binary, re_combine(re_text("Standard Block"), re_null(4), re_block("x-000")))
  sample_block_start <- find_next_pattern(
    ds$binary, re_combine(re_text("Sample Block"), re_null(4), re_block("x-000")))
  
  # safety checks
  if (is.null(standard_block_start) || is.null(sample_block_start) || 
      standard_block_start > sample_block_start) {
    op_error(ds$binary, "cannot find standard and sample voltage data blocks at expected positions")
  }
  
  # read voltage data
  ds$binary <- set_binary_file_error_prefix(ds$binary, "cannot process voltage data") 
  
  # right before this sequence there is a 4 byte sequence that could be a date, the last block is the # of masses
  read_blocks_re <- re_combine(re_null(4), re_block("etx"), re_block("x-000"))
  positions <- find_next_patterns(ds$binary, read_blocks_re)
  
  # function to capture voltages
  capture_voltages <- function(pos) {
    bin <- ds$binary %>% 
      move_to_pos(pos - 4) %>% 
      capture_n_data("n_masses", "integer", n = 1)
    
    # safety check
    if (bin$data$n_masses != nrow(masses)) {
      op_error(bin, glue("inconsistent number of voltage measurements encountered ({bin$data$n_masses}), expected {nrow(masses)}"))
    }
    
    bin <- bin %>% 
      capture_n_data("voltage", "double", n = nrow(masses), sensible = c(-1000, 100000))
    
    # return voltage data
    return(tibble(cup = 1:nrow(masses), voltage = bin$data$voltage))
  }
  
  # assemble voltages data frame
  voltages <- tibble(
    pos = positions + read_blocks_re$size,
    # note last read in the sample block is actually the "pre"-read of the standard
    type = ifelse(.data$pos < sample_block_start | .data$pos==max(.data$pos), "standard", "sample")
  ) %>% 
    group_by(.data$type) %>% 
    mutate(
      cycle = as.integer(ifelse(.data$type[1] == "standard" & .data$pos == max(.data$pos), 0L, 1L:n())),
      # capture voltages
      voltages = map(.data$pos, capture_voltages)
    ) %>% ungroup() %>% 
    # unnest voltager data
    unnest(.data$voltages) %>% 
    # combine with cup/mass information
    left_join(select(masses, .data$cup, .data$column), by = "cup") 
  
  # safety check
  if (any(notok <- is.na(voltages$column))) {
    op_error(ds$binary, glue("inconsistent cup designations: {collapse(voltages$cup[notok], ', ')}"))
  }
  
  # voltages data frame
  ds$raw_data <- 
    voltages %>% 
    select(-.data$pos, -.data$cup) %>% 
    spread(.data$column, .data$voltage) %>% 
    arrange(desc(.data$type), .data$cycle)
  
  return(ds)
}

# extract data table
extract_caf_vendor_data_table <- function(ds) {
  
  # reset navigation
  ds$binary <- reset_binary_file_navigation(ds$binary)
  
  # get data table
  extracted_dt <- 
    ds %>% 
    extract_isodat_main_vendor_data_table(
      C_block = "CResultData", cap_at_fun = NULL,
      col_include = "^(d |AT |Nr\\.|Is Ref)", 
      skip_row_check = function(column, value) column == "Is Ref.?" && value == "1")
  vendor_dt <- extracted_dt$cell_values
  
  # safety check
  req_cols <- c("Nr.", "Is Ref.?")
  if (!all(ok <- req_cols %in% names(vendor_dt))) {
    glue("not all required columns found, missing: {collapse(req_cols[!ok], '. ')}") %>% 
      stop(call. = FALSE)
  }
  
  # divided row columns (some are in the first block, some in the second)
  second_block_cols <- vendor_dt[1,] %>% map_lgl(~is.na(.x))
  if (sum(second_block_cols) > 0) {
    # separate and merge the two blocks
    condition <- as.name(names(vendor_dt)[which(second_block_cols)[1]])
    first_block <- filter(vendor_dt, is.na(!!condition))[!second_block_cols]
    second_block_cols['Nr.'] <- TRUE
    second_block <- filter(vendor_dt, !is.na(!!condition))[second_block_cols]
    vendor_dt <- left_join(first_block, second_block, by = "Nr.") 
  }
  
  # assign data table
  ds$vendor_data_table <- vendor_dt %>% 
    arrange(!!as.name("Nr.")) %>% 
    mutate(cycle = as.integer(1:n())) %>% 
    select(-!!as.name("Nr."), -!!as.name("Is Ref.?")) %>% 
    select(!!as.name("cycle"), everything())
  
  # save information on the column units
  attr(ds$vendor_data_table, "units") <- 
    bind_rows(
      tibble(column = c("cycle"), units = ""),
      filter(extracted_dt$columns, .data$column %in% names(ds$vendor_data_table))[c("column", "units")]
    )
  
  # FIXME: do this directly
  ds$vendor_data_table <- convert_df_units_attr_to_implicit_units(ds$vendor_data_table)
  
  return(ds)
}