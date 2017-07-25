# read isodat .did file
# @param ds the data structure to fill
isoread_did <- function(ds, ...) {
  
  # safety checks
  if(!is_dual_inlet(ds)) 
    stop("data structure must be a 'dual_inlet' isofile", call. = FALSE)
  
  # read binary file
  ds$binary <- read_binary_file(ds$file_info$file_path)
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_sequence_line_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_measurement_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_datetime, ds)
  }
  
  # process method info
  if (ds$read_options$method_info) {
    ds <- exec_func_with_error_catch(extract_isodat_reference_values, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data)
    ds <- exec_func_with_error_catch(extract_did_raw_voltage_data, ds)
  
  # process pre-evaluated data table
  if (ds$read_options$vendor_data_table)
    ds <- exec_func_with_error_catch(extract_did_vendor_data_table, ds)
  
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
  
  # find binary positions for voltage standards and samples
  voltages <- list()
  standard_voltage_start_re <- re_combine(re_text("/"), re_block("fef-x"), re_block("fef-x"), re_text("Standard "))
  standard_positions <- find_next_patterns(ds$binary, standard_voltage_start_re)
  sample_voltage_start_re <- re_combine(re_text("/"), re_block("fef-0"), re_block("fef-x"), re_text("Sample "))
  sample_positions <- find_next_patterns(ds$binary, sample_voltage_start_re)
  
  # process standards
  for(pos in standard_positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + standard_voltage_start_re$size) %>% 
      capture_data("cycle", "text", re_null(4), re_block("stx"), move_past_dots = TRUE) %>% 
      move_to_next_pattern(re_text("/"), re_block("fef-0"), re_block("fef-0"), re_null(4), re_block("stx")) %>%
      move_to_next_pattern(re_block("x-000"), re_block("x-000")) %>% 
      capture_data("voltage", "double", re_null(6),re_block("x-000"), sensible = c(-1000, 100000))
    
    if (length(ds$binary$data$voltage) != length(masses)) 
      stop("incorrect number of voltage measurements supplied for standard ", ds$binary$data$cycle, call. = FALSE)
    measurement <- list(
      c(list(
        type = "standard",
        cycle = if (ds$binary$data$cycle == "Pre") 0 else as.integer(ds$binary$data$cycle) + 1
      ), setNames(as.list(ds$binary$data$voltage), masses_columns)))
    voltages <- c(voltages, measurement)
  }
  
  # process samples
  for(pos in sample_positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + sample_voltage_start_re$size) %>% 
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
  type <- cycle <- NULL
  ds$raw_data <- bind_rows(voltages) %>% arrange(desc(type), cycle)
  return(ds)
}

# extract vendor computed data table
extract_did_vendor_data_table <- function(ds) {
  
  # find vendor data table
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot process vendor computed data table") %>% 
    move_to_C_block_range("CDualInletEvaluatedData", "CParsedEvaluationString")
  
  # cap
  if (!is.null(pos <- find_next_pattern(ds$binary, re_text("Gas Indices")))) {
    ds$binary <- ds$binary %>% cap_at_pos(pos - 20)
  } else op_error(ds$binary, "cannot find data deliminter 'Gas Indices'")
  
  # find data positions
  column_header_re <- re_combine(re_block("etx"), re_text("/"), re_block("fef-x"), re_block("text"), # Delta or AT%
                                 re_block("fef-x"), re_block("text"), # actual column name
                                 re_null(4), re_block("stx"))
  column_data_re <- re_combine(re_text("/"), re_block("fef-0"), re_block("fef-x"), re_block("text"), re_null(4), 
                               re_block("x-000"), re_block("x-000"), re_block("x-000")) # data comes after this
  column_header_positions <- find_next_patterns(ds$binary, column_header_re)
  column_data_positions <- find_next_patterns(ds$binary, column_data_re)
  
  # safety checks
  if (length(column_header_positions) == 0) {
    op_error(ds$binary, "no column headers found")
  } else if (length(column_header_positions) != length(column_data_positions)) {
    op_error(ds$binary, sprintf("unequal number of column headers (%d) and data entries (%d) found", 
                             length(column_header_positions), length(column_data_positions)))
  } else if (!all(column_header_positions < column_data_positions)) {
    op_error(ds$binary, "found column headers not interspersed with data entries")
  }
  
  # read the data
  vendor_dt <- list()
  for (i in 1:length(column_header_positions)) {
    ds$binary <- ds$binary %>%
      move_to_pos(column_header_positions[i] + 10) %>% # skip initial <stx>/<fef-x> at the start of header
      # capture column type (typically Delta or AT%) # could skip this to speed up
      capture_data("type", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      # capture actual colum name
      capture_data("column", "text", re_null(4), re_block("stx")) %>%
      # capture column data
      move_to_pos(column_data_positions[i]) %>% 
      move_to_next_pattern(column_data_re, max_gap = 0) %>% # move to start of data
      capture_data("values", "double", re_block("fef-0"), re_block("stx"), sensible = c(-1e10, 1e10))
    
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
  attr(ds$vendor_data_table, "units") <- NA # units do not apply
  return(ds)
}
