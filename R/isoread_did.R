# read isodat .did file
# @param ds the data structure to fill
isoread_did <- function(ds, ...) {
  
  # safety checks
  if(!is_isofile(ds) || !is(ds, "dual_inlet")) 
    stop("data structure must be a 'dual_inlet' isofile", call. = FALSE)
  
  # read binary file
  ds$binary <- read_binary_file(ds$file_info$file_path)
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_sequence_line_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_measurement_info, ds)
    ds <- exec_func_with_error_catch(extract_standard_information, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data)
    ds <- exec_func_with_error_catch(extract_did_raw_voltage_data, ds)
  
  # process pre-evaluated data table
  if (ds$read_options$vendor_data_table)
    ds <- exec_func_with_error_catch(extract_vendor_data_table, ds)
  
  return(ds)
}

# extract reference standard information
extract_standard_information <- function(ds) {
  
  # CSecondaryStandardMethodPart <- fetch_keys(ds$binary, "CSecondaryStandardMethodPart", occurence = 1, fixed = TRUE, require = 1)
  # Instrument <- fetch_keys(ds$binary, "Instrument", occurence = 1, byte_min = CSecondaryStandardMethodPart$byte_end, fixed = TRUE, require = 1)
  # 
  # ds$binary <- move_to_pos(ds$binary, CSecondaryStandardMethodPart$byte_end + 14L)
  # 
  # #print(CSecondaryStandardMethodPart$byte_end)
  # #print(Instrument$byte_start)
  #   
  # standard_name <- get_unicode(ds$binary$raw[(CSecondaryStandardMethodPart$byte_end+14L):Instrument$byte_start])
  # 
  # 
  # CConfiguration <- fetch_keys(ds$binary, "CConfiguration", occurence = 1, fixed = TRUE, require = 1)
  # CPrimaryStandardMethodPart <- fetch_keys(ds$binary, "CSecondaryStandardMethodPart", occurence = 1, fixed = TRUE, require = 1)
  # 
  # NOTE: the following holds a number of keys that suggest information about tuning
  # (Trap, Emission, Extraction, Shield, etc.) but it is not obvious where the numerical information
  # these keys refer to is kept - perhaps not actually in the vicinity of the text indicators?
  # There is a large binary block right before CConfiguration that starts after the "Administrator" key
  #IsotopeMS <- fetch_keys(ds$binary, "Isotope MS/\\w+", byte_min = CConfiguration$byte_end, byte_max = CPrimaryStandardMethodPart$byte_start) 
  
  return(ds)
}

# extract vendor computed data table
extract_vendor_data_table <- function(ds) {
  
  # pre-evaluated data table
  CDualInletEvaluatedData <- fetch_keys(ds$binary, "CDualInletEvaluatedData", occurence = 1, fixed = TRUE, require = 1)
  GasIndices <- fetch_keys(ds$binary, "Gas Indices", occurence = 1, fixed = TRUE, require = 1) 
  data_table_keys <-
    fetch_keys(
      ds$binary, "^(d |AT).+$", byte_min = CDualInletEvaluatedData$byte_end, byte_max = GasIndices$byte_start, 
      require = "1+", error_prefix = "could not find vendor computed data table") 

  # number of cycles
  CDualInletRawData <- fetch_keys(ds$binary, "CDualInletRawData", occurence = 1, fixed = TRUE, require = 1)
  CTwoDoublesArrayData <- fetch_keys(ds$binary, "CTwoDoublesArrayData", occurence = 1, fixed = TRUE, require = 1)
  n_cycles <- fetch_keys(ds$binary, "^(Standard|Sample) \\d+$", byte_min = CDualInletRawData$byte_end, byte_max = CTwoDoublesArrayData$byte_start) %>% 
    { str_count(., "Sample \\d+") %>% sum() }
  
  if (length(n_cycles) == 0) 
    stop("cannot find number of measurements")
  
  # data gap intervals
  gap_to_data <- c("d " = 54, "AT" = 50)
    
  # assign vendor_data_table
  ds$vendor_data_table <-
    data_table_keys %>% 
    # remove trailing white spaces in the column names
    mutate(column = str_replace(value, "\\s*$", "")) %>% 
    group_by(column) %>% 
    # extract the column data
    do(with(., {
      # move to right place in binary
      ds$binary <- move_to_pos(ds$binary, byte_end + 1L)
      gap <- gap_to_data[str_sub(column, 1, 2)]
      if (is.na(gap)) stop("could not process vendor data table column: ", value, call. = FALSE)
      ds$binary <- skip_pos(ds$binary, gap)
      
      # parse data
      data <- parse_binary_data(ds$binary, "double", length = 2*n_cycles, 
                                sensible = c(-1e10, 1e10), error_prefix = "cannot extract vendor calculated values")
      
      # assemble data frame
      data_frame(
        cycle = data[c(TRUE, FALSE)] + 1L,
        value = data[c(FALSE, TRUE)]
      )
      
    })) %>% 
    ungroup() %>% 
    spread(column, value) 
  
  return(ds)
}

# extracts the sequence line information for isodat files
extract_isodat_sequence_line_info <- function(ds) {
  
  # find sequence line information
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot process sequence line info") %>% 
    move_to_C_block_range("CParsedEvaluationString", "CBinary") %>% 
    move_to_next_pattern(re_text("Sequence Line Information"))
  
  seq_line_info <- list()
  while(!is.null(find_next_pattern(ds$binary, re_null(4), re_or(re_block("vtab"), re_block("C-block"))))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(re_text("/"), re_block("fef-x")) %>% 
      capture_data("value", "text", re_or(re_block("fef-x"), re_block("nl")), re_block("text"), re_null(4), move_past_dots = FALSE) %>% 
      move_to_next_pattern(re_or(re_block("fef-x"), re_block("nl"))) %>% 
      capture_data("info", "text", re_null(4), re_or(re_block("vtab"), re_block("C-block")), move_past_dots = FALSE) %>% 
      move_to_next_pattern(re_null(4)) 
    if (!is.null(ds$binary$data$info))
      ds$file_info[[ds$binary$data$info]] <- ds$binary$data$value
  }
  
  return(ds)
}

# extracts the measurement information for isodat files
extract_isodat_measurement_info = function(ds) {
  
  # find measurement info
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot process measurement info") %>% 
    move_to_C_block_range("CMeasurmentInfos", "CMeasurmentErrors") %>% 
    move_to_next_C_block("CISLScriptMessageData")
  
  isl_info_msgs <- c()
  while(!is.null(find_next_pattern(ds$binary, re_text("CUserInfo")))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(re_block("00-x-000"), re_block("fef-x")) %>% 
      capture_data("info", "text", re_block("fef-x"), re_text("CUserInfo"), move_past_dots = TRUE) 
    isl_info_msgs <- c(isl_info_msgs, ds$binary$data$info)
  }
  
  # store all in one information set
  ds$file_info$measurement_info <- isl_info_msgs

  return(ds)
}

# extract voltage data in did file
extract_did_raw_voltage_data <- function(ds) {
  
  # mass information
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot identify measured masses") %>% 
    move_to_C_block("CBinary") %>%
    move_to_next_C_block_range("CTraceInfoEntry", "CPlotRange") 
  
  # read all masses
  masses <- c()
  while(!is.null(find_next_pattern(ds$binary, re_block("fef-x"), re_text("Mass ")))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(re_block("fef-x"), re_text("Mass ")) %>% 
      capture_data("mass", "text", re_or(re_block("fef-x"), re_block("C-block")), 
                   data_bytes_max = 8, move_past_dots = FALSE) 
    masses <- c(masses, ds$binary$data$mass)
  }
  
  # mass column formatting
  masses_columns <- str_c("v", masses, ".mV")
  
  # locate voltage data
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot locate voltage data") %>% 
    move_to_C_block_range("CDualInletRawData", "CTwoDoublesArrayData") %>% 
    move_to_next_C_block("CIntegrationUnitTransferPart") %>% 
    set_binary_file_error_prefix("cannot process voltage data") 
  
  # read voltage data for the standards
  voltages <- list()
  while(!is.null(find_next_pattern(ds$binary, re_text("/"), re_block("fef-x"), re_block("nl"), re_text("Standard ")))) {
    ds$binary <- ds$binary %>% 
      move_to_next_pattern(re_text("/"), re_block("fef-x"), re_block("nl"), re_text("Standard ")) %>% 
      capture_data("cycle", "text", re_block("00-x-000"), move_past_dots = TRUE) %>% 
      move_to_next_pattern(re_text("/"), re_block("fef-0"), re_block("fef-0"), re_null(2), re_block("00-x-000")) %>%
      move_to_next_pattern(re_block("00-x-000"), re_block("x-000")) %>%
      capture_data("voltage", "double", re_block("00-x-000"), sensible = c(-1000, 100000))
    if (length(ds$binary$data$voltage) != length(masses)) 
      stop("incorrect number of voltage measurements supplied for standard ", ds$binary$data$cycle, call. = FALSE)
    measurement <- list(
      c(list(
        type = "standard",
        cycle = as.integer(ds$binary$data$cycle) + 1
      ), setNames(as.list(ds$binary$data$voltage), masses_columns)))
    voltages <- c(voltages, measurement)
  }
  
  # read the voltage data for the samples
  while(!is.null(find_next_pattern(ds$binary, re_text("/"), re_block("fef-0"), re_block("fef-x"), re_text("Sample ")))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(re_text("/"), re_block("fef-0"), re_block("fef-x"), re_text("Sample ")) %>%
      capture_data("cycle", "text", re_null(2), re_block("00-x-000"), move_past_dots = TRUE) %>%
      move_to_next_pattern(re_text("/"), re_block("fef-0"), re_block("fef-0"), re_null(2), re_block("00-x-000")) %>%
      move_to_next_pattern(re_block("00-x-000"), re_block("x-000")) %>%
      capture_data("voltage", "double", re_block("00-x-000"), sensible = c(-1000, 100000))
    if (length(ds$binary$data$voltage) != length(masses)) 
      stop("incorrect number of voltage measurements supplied for sample ", ds$binary$data$cycle, call. = FALSE)
    measurement <- list(
      c(list(
        type = "sample",
        cycle = as.integer(ds$binary$data$cycle) + 1
      ), setNames(as.list(ds$binary$data$voltage), masses_columns)))
    voltages <- c(voltages, measurement)
  }
  
  # voltages data frame
  ds$raw_data <- bind_rows(voltages)
  return(ds)
}
