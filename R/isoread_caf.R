# read isodat .caf files
# @param ds the data structure to fill
isoread_caf <- function(ds, ...) {
  
  # safety checks
  if(!is_dual_inlet(ds)) 
    stop("data structure must be a 'dual_inlet' isofile", call. = FALSE)
  
  # read binary file
  ds$binary <- read_binary_file(ds$file_info$file_path)
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_old_sequence_line_info, ds)
    # NOTE: measurement info (see did) does not seem to be stored in caf files
    ds <- exec_func_with_error_catch(extract_isodat_datetime, ds)
    # NOTE: the following extraction is not tested but should work (don't have a .caf file with H3 factor)
    ds <- exec_func_with_error_catch(extract_H3_factor_info, ds)
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
  # global vars
  #pos <- cup <- colummn <- voltage <- type <- cycle <- NULL
  
  # locate masses
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify measured masses") %>% 
    move_to_C_block_range("CResultData", "CEvalDataIntTransferPart") 
  
  # read all masses
  masses_re <- re_combine(re_block("x-000"), re_block("fef-x"), re_text("rIntensity"))
  masses <- 
    data_frame(
      pos = find_next_patterns(ds$binary, masses_re) + masses_re$size,
      # capture cup and mass
      data = map(pos, function(pos) {
        ds$binary %>%
          move_to_pos(pos) %>%  
          capture_data("cup", "text", re_block("fef-x"), data_bytes_max = 8, move_past_dots = TRUE) %>%
          move_to_next_pattern(re_text("rIntensity "), max_gap = 0L) %>% 
          capture_data("mass", "text", re_block("fef-x"), data_bytes_max = 8) %>% 
          { as_data_frame(.$data[c("cup", "mass")]) }
      })
    ) %>% 
    # unnest data
    unnest(data) %>% 
    mutate(
      cup = as.integer(cup),
      column = str_c("v", mass, ".mV")
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
    return(data_frame(cup = 1:nrow(masses), voltage = bin$data$voltage))
  }
  
  # assemble voltages data frame
  voltages <- data_frame(
    pos = positions + read_blocks_re$size,
    # note last read in the sample block is actually the "pre"-read of the standard
    type = ifelse(pos < sample_block_start | pos==max(pos), "standard", "sample")
  ) %>% 
    group_by(type) %>% 
    mutate(
      cycle = ifelse(type[1] == "standard" & pos == max(pos), 0L, 1L:n()),
      # capture voltages
      voltages = map(pos, capture_voltages)
    ) %>% ungroup() %>% 
    # unnest voltager data
    unnest(voltages) %>% 
    # combine with cup/mass information
    left_join(select(masses, cup, column), by = "cup") 
  
  # safety check
  if (any(notok <- is.na(voltages$column))) {
    op_error(bin, glue("inconsistent cup designations: {collapse(voltages$cup[notok], ', ')}"))
  }
  
  # voltages data frame
  ds$raw_data <- 
    voltages %>% 
    select(-pos, -cup) %>% 
    spread(column, voltage) %>% 
    arrange(desc(type), cycle)
  
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
    mutate(cycle = 1:n()) %>% 
    select(-!!as.name("Nr."), -!!as.name("Is Ref.?")) %>% 
    select(!!as.name("cycle"), everything())
  
  # save information on the column units
  attr(ds$vendor_data_table, "units") <- 
    bind_rows(
      data_frame(column = c("cycle"), units = ""),
      filter(extracted_dt$columns, column %in% names(ds$vendor_data_table))[c("column", "units")]
    )
  return(ds)
}