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
  }
  
  # process method info
  if (ds$read_options$method_info) {
    ds <- exec_func_with_error_catch(extract_primary_standard_information, ds)
    ds <- exec_func_with_error_catch(extract_secondary_standard_information, ds)
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
extract_primary_standard_information <- function(ds) {
  
  # find the different standard names
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover primary standard") %>% 
    move_to_C_block_range("CPrimaryStandardMethodPart", "CSecondaryStandardMethodPart")
  
  ref_names <- ref_pos <- c()
  while(!is.null(find_next_pattern(ds$binary, re_text("/"), re_block("fef-0"), re_block("fef-x"), re_block("text"), re_null(4), re_block("stx")))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(re_text("/"), re_block("fef-0"), re_block("fef-x")) %>%
      capture_data("reference", "text", re_null(4), re_block("stx")) 
    if (!is.null(ds$binary$data$reference)) {
      ref_names <- c(ref_names, ds$binary$data$reference)
      ref_pos <- c(ref_pos, ds$binary$pos)
    }
  }
  
  # find the different standard values
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover primary standard") %>% 
    move_to_C_block_range("CPrimaryStandardMethodPart", "CSecondaryStandardMethodPart")
  
  primary_stds <- list()
  standard_value_re <- re_combine(re_text(","), re_times(re_block("fef-0"), 2), re_null(4), re_block("x-000"), re_block("fef-x"))
  while(!is.null(find_next_pattern(ds$binary, standard_value_re))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(standard_value_re) %>%
      capture_data("code", "text", re_block("fef-x"), move_past_dots = TRUE) %>% 
      capture_data("ratio_name", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      capture_data("ratio_format", "text", re_block("fef-0"), re_block("fef-x"), move_past_dots = TRUE) %>% 
      capture_data("element", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      move_to_next_pattern(re_block("stx"), re_block("x-000")) %>% 
      capture_data("ratio_value", "double", re_null(2), re_block("x-000")) %>% 
      identity()
    ref_name <- ref_names[max(which(ds$binary$pos > ref_pos))]
    primary_stds <- c(primary_stds, list(c(reference = ref_name, ds$binary$data[c("element", "ratio_name", "ratio_value", "ratio_format")])))
  }
  
  # store information
  ds$method_info$primary_standards <- bind_rows(primary_stds)
  return(ds)
}

# extract secondary standard information
extract_secondary_standard_information <- function(ds) {
  # get secondary standard names
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover secondary standard names") %>% 
    move_to_C_block("CSecondaryStandardMethodPart")
  
  if (is.null(cap <- find_next_pattern(ds$binary, re_text("Isotope MS"))))
    stop("cannot find 'Isotope MS' section", call. = FALSE)
  ds$binary <- cap_at_pos(ds$binary, cap)
  
  refs <- list()
  pre_std_name_re <- re_combine(re_block("stx"), re_block("nl"), re_text("Instrument"), re_block("etx"), re_block("fef-x"))
  while(!is.null(find_next_pattern(ds$binary, pre_std_name_re))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(pre_std_name_re) %>%
      capture_data("name", "text", re_block("fef-x"), move_past_dots = TRUE) %>% 
      capture_data("molecule", "text", re_null(4), re_direct(".."), re_block("etx")) %>% 
      identity()
    refs <- c(refs, list(ds$binary$data[c("name", "molecule")]))
  }
  
  # get secondar standard values
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover secondary standard values") %>% 
    move_to_C_block("CSecondaryStandardMethodPart", reset_cap = FALSE) # move back to start, cap is still the same
  
  secondary_stds <- list()
  standard_value_re <- re_combine(re_text("/"), re_times(re_block("fef-0"), 2), re_null(4), re_block("x-000"), re_block("fef-x"))
  while(!is.null(find_next_pattern(ds$binary, standard_value_re))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(standard_value_re) %>%
      capture_data("code", "text", re_block("fef-x"), move_past_dots = TRUE) %>% 
      capture_data("delta_name", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      capture_data("delta_format", "text", re_block("fef-x"), move_past_dots = TRUE) %>% 
      capture_data("molecule", "text", re_block("fef-0"), re_block("fef-x"), move_past_dots = TRUE) %>%
      capture_data("delta_units", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      move_to_next_pattern(re_block("stx"), re_block("x-000")) %>% 
      capture_data("delta_value", "double", re_null(2), re_block("x-000"), move_past_dots = TRUE) %>% 
      move_to_next_pattern(re_block("stx"), re_block("fef-x"), max_gap = 0) %>% 
      capture_data("reference", "text", re_null(12), re_or(re_block("ee-85"), re_block("x-000"))) %>% 
      identity()
    secondary_stds <- c(secondary_stds, list(ds$binary$data[c("delta_name", "delta_value", "delta_units", "delta_format", "molecule", "reference")]))
  }
  
  # standards
  ds$method_info$secondary_standards <- 
    full_join(bind_rows(refs), bind_rows(secondary_stds), by = "molecule")
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
      move_to_next_pattern(re_block("x-000"), re_block("fef-x")) %>% 
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
      capture_data("cycle", "text", re_null(4), re_block("stx"), move_past_dots = TRUE) %>% 
      move_to_next_pattern(re_text("/"), re_block("fef-0"), re_block("fef-0"), re_null(4), re_block("stx")) %>%
      move_to_next_pattern(re_block("x-000"), re_block("x-000")) %>% 
      capture_data("voltage", "double", re_null(6),re_block("x-000"), sensible = c(-1000, 100000))
    
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
      capture_data("cycle", "text", re_null(4), re_block("stx"), move_past_dots = TRUE) %>% 
      move_to_next_pattern(re_text("/"), re_block("fef-0"), re_block("fef-0"), re_null(4), re_block("stx")) %>%
      move_to_next_pattern(re_block("x-000"), re_block("x-000")) %>% 
      capture_data("voltage", "double", re_null(6),re_block("x-000"), sensible = c(-1000, 100000))
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

# extract vendor computed data table
extract_vendor_data_table <- function(ds) {
  
  # find sequence line information
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot process vendor computed data table") %>% 
    move_to_C_block_range("CDualInletEvaluatedData", "CParsedEvaluationString")
  
  # read the data table
  vendor_dt <- list()
  while(!is.null(find_next_pattern(ds$binary, re_block("fef-0"), re_block("stx")))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(re_text("/"), re_block("fef-x")) %>% 
      # capture column type (typically Delta or AT%)
      capture_data("type", "text", re_or(re_block("nl"), re_block("fef-x")), re_block("text"), move_past_dots = FALSE) %>%
      move_to_next_pattern(re_or(re_block("nl"), re_block("fef-x")), max_gap = 0) %>% 
      # capture actual colum name
      capture_data("column", "text", re_null(4), re_block("stx"), move_past_dots = TRUE) %>%
      move_to_next_pattern(re_text("/"), re_block("fef-0"), re_block("fef-x"), re_block("text"), re_null(4), re_times(re_block("x-000"), 3)) %>%
      # capture column data
      capture_data("values", "double", re_block("fef-0"), re_block("stx"), sensible = c(-1e10, 1e10), move_past_dots = TRUE)
    
    if (length(ds$binary$data$values) %% 2 != 0)
      stop("odd number of data entries recovered", call. = FALSE)
    
    table_column <- list(
      list(
        cycle = ds$binary$data$values[c(TRUE, FALSE)] + 1L,
        value = ds$binary$data$values[c(FALSE, TRUE)]
      )) %>% setNames(str_replace(ds$binary$data$column, "\\s*$", "")) # remove trailing white spaces in column names
    vendor_dt <- c(vendor_dt, table_column)
  }
  
  # safety checks
  if (length(vendor_dt) == 0) stop("no vendor computed data found", call. = FALSE)
  cycles <- lapply(vendor_dt, `[[`, 1) 
  if (!all(sapply(cycles, identical, cycles[[1]])))
    stop("not all columns have the same number of cycles", call. = FALSE)
  
  # vendor table
  ds$vendor_data_table <- bind_cols(
    data_frame(cycle = vendor_dt[[1]][[1]]), 
    lapply(vendor_dt, `[[`, 2) %>% as_data_frame())
  return(ds)
}
