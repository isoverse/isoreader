# read isodat .did file
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_did <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_dual_inlet(ds)) 
    stop("data structure must be a 'dual_inlet' iso_file", call. = FALSE)
  
  # read binary file
  ds$source <- get_ds_file_path(ds) |> read_binary_isodat_file()
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_sequence_line_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_measurement_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_datetime, ds)
    ds <- exec_func_with_error_catch(extract_MS_integration_time_info, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data)
    ds <- exec_func_with_error_catch(extract_did_raw_voltage_data, ds)
  
  # process method info
  if (ds$read_options$method_info) {
    ds <- exec_func_with_error_catch(extract_isodat_reference_values, ds) # NOTE: no cap for these
    ds <- exec_func_with_error_catch(extract_isodat_resistors, ds)
  }
  
  # process pre-evaluated data table
  if (ds$read_options$vendor_data_table)
    ds <- exec_func_with_error_catch(extract_did_vendor_data_table, ds)
  
  return(ds)
}

# extract voltage data in did file
extract_did_raw_voltage_data <- function(ds) {

  # mass information
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot identify measured masses") |> 
    move_to_C_block("CBinary") |>
    move_to_next_C_block_range("CTraceInfoEntry", "CPlotRange") 
  
  # read all masses
  masses_re <- re_combine(re_text_x(), re_unicode("Mass "))
  masses_positions <- find_next_patterns(ds$source, masses_re)
  masses <- map_chr(masses_positions, function(pos) {
    ds$source |>
      move_to_pos(pos + masses_re$size) |>  
      capture_data_till_pattern("mass", "text", re_or(re_text_x(), re_block("C-block")), 
                   data_bytes_max = 8, move_past_dots = FALSE) |> 
      purrr::pluck("data", "mass")
  })
  
  # mass column formatting
  masses_columns <- str_c("v", masses, ".mV")
  
  # locate voltage data
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot locate voltage data") |> 
    move_to_C_block_range("CDualInletRawData", "CTwoDoublesArrayData") |> 
    move_to_next_C_block("CIntegrationUnitTransferPart") |> 
    set_binary_file_error_prefix("cannot process voltage data") 
  
  # find binary positions for voltage standards and samples
  voltages <- list()
  standard_voltage_start_re <- re_combine(re_unicode("/"), re_text_x(), re_text_x(), re_unicode("Standard "))
  standard_positions <- find_next_patterns(ds$source, standard_voltage_start_re)
  sample_voltage_start_re <- re_combine(re_unicode("/"), re_text_0(), re_text_x(), re_unicode("Sample "))
  sample_positions <- find_next_patterns(ds$source, sample_voltage_start_re)

  # function to capture voltages
  capture_voltages <- function(pos) {
    
    bin <- ds$source |> 
      move_to_pos(pos) |> 
      capture_data_till_pattern("cycle", "text", re_null(4), re_block("stx"), move_past_dots = TRUE) |> 
      move_to_next_pattern(re_unicode("/"), re_text_0(), re_text_0(), re_null(4), re_block("stx")) |>
      move_to_next_pattern(re_x_000(), re_x_000()) |> 
      capture_data_till_pattern("voltage", "double", re_null(6),re_x_000(), sensible = c(-1000, 100000))
    
    # safety check
    if (length(bin$data$voltage) != length(masses)) {
      iso_source_file_op_error(bin, glue("inconsistent number of voltage measurements encountered ({length(bin$data$voltage)}), expected {length(masses)}"))
    }

    # return voltage data
    return(tibble(cycle = bin$data$cycle, cup = 1:length(bin$data$voltage), voltage = bin$data$voltage))
  }
  
  # assemble voltages data frame
  voltages <- 
    bind_rows(
      tibble(pos = standard_positions + standard_voltage_start_re$size, type = "standard"),
      tibble(pos = sample_positions + sample_voltage_start_re$size, type = "sample")
    ) |> 
    mutate(
      voltages = map(.data$pos, capture_voltages)
    ) |> 
    unnest("voltages") |> 
    # join in the mass information
    left_join(tibble(cup = 1:length(masses), mass = masses_columns), by = "cup") |> 
    # spread out the volrages
    select(-"pos", -"cup") |> 
    spread(.data$mass, .data$voltage) |> 
    # update cycle
    mutate(cycle = as.integer(ifelse(.data$cycle == "Pre", -1L, .data$cycle)) + 1L)
  
  # voltages data frame
  ds$raw_data <- arrange(voltages, desc(.data$type), .data$cycle)
  return(ds)
}

# extract vendor computed data table
# @note this could potentially also use the more universal data table functions in iso_read_isodat
extract_did_vendor_data_table <- function(ds) {
  
  # find vendor data table
  ds$source <- ds$source |> 
    set_binary_file_error_prefix("cannot process vendor computed data table") |> 
    move_to_C_block_range("CDualInletEvaluatedData", "CParsedEvaluationString")
  
  # cap
  if (!is.null(pos <- find_next_pattern(ds$source, re_unicode("Gas Indices")))) {
    ds$source <- ds$source |> cap_at_pos(pos - 20)
  } else iso_source_file_op_error(ds$source, "cannot find data deliminter 'Gas Indices'")
  
  # find data positions
  column_header_re <- re_combine(re_block("etx"), re_unicode("/"), re_text_x(), re_block("text"), # Delta or AT%
                                 re_text_x(), re_block("text"), # actual column name
                                 re_null(4), re_block("stx"))
  column_data_re <- re_combine(re_unicode("/"), re_text_0(), re_text_x(), re_block("text"), re_null(4), 
                               re_x_000(), re_x_000()) # data comes after this
  column_header_positions <- find_next_patterns(ds$source, column_header_re)
  column_data_positions <- find_next_patterns(ds$source, column_data_re)
  
  # safety checks
  if (length(column_header_positions) == 0) {
    iso_source_file_op_error(ds$source, "no column headers found")
  } else if (length(column_header_positions) != length(column_data_positions)) {
    iso_source_file_op_error(ds$source, sprintf("unequal number of column headers (%d) and data entries (%d) found", 
                             length(column_header_positions), length(column_data_positions)))
  } else if (!all(column_header_positions < column_data_positions)) {
    iso_source_file_op_error(ds$source, "found column headers not interspersed with data entries")
  }
  
  # read the data
  vendor_dt <- list()
  for (i in 1:length(column_header_positions)) {
    ds$source <- ds$source |>
      move_to_pos(column_header_positions[i] + 10) |> # skip initial <stx>/<fef-x> at the start of header
      # capture column type (typically Delta or AT%) # could skip this to speed up
      capture_data_till_pattern("type", "text", re_text_x(), move_past_dots = TRUE) |>
      # capture actual colum name
      capture_data_till_pattern("column", "text", re_null(4), re_block("stx")) |>
      # capture column data
      move_to_pos(column_data_positions[i]) |> 
      move_to_next_pattern(column_data_re, max_gap = 0) |> # move to start of data
      capture_n_data("n_values", "integer", n = 1, sensible = c(1, 1000)) |>  # NOTE: this assumes more than 1000 cycles are unrealistic in dual inlet
      capture_data_till_pattern("values", "double", re_text_0(), re_block("stx"), sensible = c(-1e10, 1e10))
    
    # safety check
    if (length(ds$source$data$values) != 2 * ds$source$data$n_values)
      glue::glue("inconsistent number of data entries recovered ({length(ds$source$data$values)}) - ",
                 "expected {2 * ds$source$data$n_values} values from {ds$source$data$n_values} cycles") |> 
      stop(call. = FALSE)
    
    table_column <- list(
      list(
        cycle = as.integer(ds$source$data$values[c(TRUE, FALSE)] + 1L),
        value = ds$source$data$values[c(FALSE, TRUE)]
      )) |> rlang::set_names(str_replace(ds$source$data$column, "\\s*$", "")) # remove trailing white spaces in column names
    vendor_dt <- c(vendor_dt, table_column)
  }
  
  # safety checks
  if (length(vendor_dt) == 0) stop("no vendor computed data found", call. = FALSE)
  cycles <- lapply(vendor_dt, `[[`, 1) 
  if (!all(sapply(cycles, identical, cycles[[1]])))
    stop("not all columns have the same number of cycles", call. = FALSE)
  
  # vendor table
  ds$vendor_data_table <- bind_cols(
    tibble(cycle = vendor_dt[[1]][[1]]), 
    lapply(vendor_dt, `[[`, 2) |> dplyr::as_tibble())
  attr(ds$vendor_data_table, "units") <- NULL # units do not apply
  return(ds)
}
