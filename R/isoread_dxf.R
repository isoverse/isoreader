# read isodat .dxf file
# @param ds the data structure to fill
isoread_dxf <- function(ds, ...) {

  # safety checks
  if(!is_continuous_flow(ds)) 
    stop("data structure must be a 'continuous_flow' isofile", call. = FALSE)
  
  # read binary file
  ds$binary <- read_binary_file(ds$file_info$file_path)
  
  # process file info
  if(ds$read_options$file_info) {
    ds <- exec_func_with_error_catch(extract_isodat_sequence_line_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_measurement_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_datetime, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data) {
    ds <- exec_func_with_error_catch(extract_dxf_raw_voltage_data, ds)
  }
  
  # process method info
  if (ds$read_options$method_info) {
    ds <- exec_func_with_error_catch(extract_isodat_reference_values, ds, function(bin) cap_at_next_C_block(bin, "CResultArray"))
    ds <- exec_func_with_error_catch(extract_isodat_resistors, ds)
  }
  
  # process pre-evaluated data table
  if (ds$read_options$vendor_data_table)
    ds <- exec_func_with_error_catch(extract_dxf_vendor_data_table, ds)
  
  return(ds)

}


# extract voltage data in dxf file
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
  data_start_re <- re_combine(re_block("fef-0"), re_null(4), re_block("x-000"), re_block("x-000"), re_direct("..", size = 2), re_block("x-000"))
  data_end_re <- re_combine(re_direct(".{4}"), re_null(4), re_block("fef-0"), re_block("stx"))
  gas_config_re <- re_combine(re_block("fef-x"), re_block("text"), re_block("fef-0"))
  voltages <- data_frame()
  positions <- find_next_patterns(ds$binary, data_start_re)
  for (pos in positions) {
    # move to beginning of data
    ds$binary <- ds$binary %>% move_to_pos(pos + data_start_re$size + 4L) # 4 byte gap before data
    
    # find gas configuration name
    gas_config <- ds$binary %>% 
      move_to_next_pattern(data_end_re) %>% 
      move_to_next_pattern(gas_config_re, move_to_end = FALSE, max_gap = 20) %>% 
      skip_pos(4) %>% # skip the fef-x at the beginning
      capture_data("gas", "text", re_block("fef-0"), data_bytes_max = 50) %>% { .$data$gas }
    
    # find gas configuration masses
    masses <- configs[[gas_config]]
    if (is.null(masses)) stop("could not identify measured ions for gas ", gas_config, call. = FALSE)
    masses_columns <- str_c("v", masses, ".mV")
    
    # save voltage data
    ds$binary <- ds$binary %>% 
      capture_data("voltages", c("float", rep("double", length(masses))), data_end_re)
    voltages <- bind_rows(voltages, 
                          ds$binary$data$voltages %>% 
                            as_data_frame() %>% setNames(c("time.s", masses_columns)))
  }
  
  # check for data
  if (nrow(voltages) == 0) {
    if (setting("debug")) { # debug
      message("DEBUG: found gas configurations: ")
      print(configs)
    }
    stop("could not find raw voltage data", call. = FALSE)
  }
  
  # add time point column
  tp <- time.s <- NULL # global vars
  ds$raw_data <-
    voltages %>% arrange(time.s) %>% 
    mutate(tp = 1:n()) %>% 
    select(tp, time.s, everything())
 
  return(ds) 
}

# extract vendor computed data table
extract_dxf_vendor_data_table <- function(ds) {
  # find vendor data table
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot process vendor identified peaks") %>% 
    move_to_C_block("CGCPeakList")
  
  if (is.null(cap_pos <- find_next_pattern(ds$binary, re_text("DetectorDataBlock")))) {
    op_error(ds$binary, "cannot find DetectorDataBlock")
  }
  ds$binary <- ds$binary %>% cap_at_pos(cap_pos)
  
  ### basic peak info
  # find basic peak information (Rts, amplitude, bg) - this information is stored separatedly from the rest of the table
  rt_pre_re <- re_combine(re_null(19), re_direct("(\\x00|[\x01-\x1f])\\x00{3}", size = 4), re_block("x-000"))
  rt_re <- re_combine(rt_pre_re, re_direct("..\\x00{2}"), re_block("etx"))
  positions <- find_next_patterns(ds$binary, rt_re)
  rts <- list()
  for (pos in positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + rt_pre_re$size) %>% 
      capture_n_data("mass", "integer", 1) %>% 
      move_to_next_pattern(re_block("etx"), max_gap = 0) %>% 
      capture_n_data("peak", "integer", 1) %>% 
      skip_pos(4) %>% # random xx00 follows
      capture_n_data("start", "double", 1) %>%
      capture_n_data("bg", "double", 1) %>% 
      skip_pos(4) %>% 
      capture_n_data("rt", "double", 1) %>% 
      capture_n_data("amp", "double", 1) %>% 
      skip_pos(4) %>% 
      capture_n_data("end", "double", 1) 
    # NOTE: after this there is another unknown value similar to 'bg', then the rIntensity, then the Intensity (but we get those from the data table too)
    rts <- c(rts, list(c(ini = pos, pos = ds$binary$pos, ds$binary$data[c("mass", "peak", "start", "rt", "end", "amp", "bg")])))
  }
  
  # NOTE: the retention time is only ALMOST repeated each time, if there are significant 
  # chromatographic shifts (as is alwayst he case for H2), these will in fact NOT quite be
  # identical. Isodat seems to report only the major ion (first ion here) so we are doing the same
  rts_df <- bind_rows(rts) 
  
  # retention times
  peak <- start <- rt <- end <- amp <- Ampl <- bg <- BGD <- NULL
  peaks <- rts_df %>% 
    select(peak, Start = start, Rt = rt, End = end) %>% 
    distinct(peak, .keep_all = TRUE) %>% 
    # add in amplitudes
    left_join(
      rts_df %>% 
        select(peak, Ampl = mass, amp) %>% 
        spread(Ampl, amp, sep = " "),
      by = "peak"
    ) %>% 
    # add in backgrounds
    left_join (
      rts_df %>% 
        select(peak, BGD = mass, bg) %>% 
        spread(BGD, bg, sep = " "),
      by = "peak"
    ) %>% 
    rename(`Nr.` = peak)
  
  ### rest of data table
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot process vendor data table") %>% 
    move_to_C_block("CGCPeakList", reset_cap = FALSE) # important to NOT reset position cap
  
  # find columns and row data for the whole data table
  columns <- list()
  rows <- list()
  rows_i <- 0
  pre_column_re <- re_combine(re_text("/"), re_block("fef-0"), re_block("fef-0"), re_null(4), re_block("x-000"), re_block("fef-x"))
  positions <- find_next_patterns(ds$binary, pre_column_re)
  for(pos in positions) {
    # get column id
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + pre_column_re$size) %>% 
      move_to_next_pattern(re_block("fef-x")) %>% # skip ID column since it is not unique in peak jumping files
      capture_data("column", "raw", re_block("fef-x"), move_past_dots = TRUE, ignore_trailing_zeros = FALSE) %>% 
      capture_data("format", "text", re_block("fef-x"), move_past_dots = TRUE) # retrieve format (!not always the same)
    
    # skip data columns without formatting infromation right away
    if(ds$binary$data$format %in% c("", " ")) next # skip 
    
    # check for columns starting with delta symbol, replace with d instead of delta symbol
    if (identical(ds$binary$data$column[1:2], as.raw(c(180, 03)))) 
      ds$binary$data$column[1:2] <- as.raw(c(100, 00)) 
    col <- ds$binary$data$column <- parse_raw_data(ds$binary$data$column, "text")
    
    # store information about new column if not already stored
    if (!col %in% names(columns)) {
      ds$binary <- ds$binary %>%
        # skip what looks like it might be the gas configuration and an unknown piece of information
        move_to_next_pattern(re_block("text0"), re_block("fef-x"), re_block("text0"), re_block("fef-x")) %>% 
        capture_data("units", "text", re_block("fef-x"), move_past_dots = TRUE) # retrieve units
      
      # data format
      type <- 
      if (ds$binary$data$format == "%s") "text"
      else if (ds$binary$data$format %in% c("%u", "%d")) "integer"
      else if (str_detect(ds$binary$data$format, "\\%[0-9.]+f")) "double"
      else op_error(ds$binary, 
                    sprintf("could not data table column format '%s' for column '%s'",
                            ds$binary$data$format, col))
      
      # store
      new_col <- c(list(pos = ds$binary$pos, type = type), ds$binary$data[c("column", "format", "units")])
      columns[[col]] <- new_col
    } else if (ds$binary$data$format != columns[[col]]$format) {
      # double check formatting
      op_error(ds$binary, 
               sprintf("mismatched data column format for column '%s', found '%s' but expected '%s'",
                       col, ds$binary$data$format, columns[[col]]$format))
    }
    
    # new row
    if (col == names(columns)[1]) {
      rows_i <- rows_i + 1 
      rows[[rows_i]] <- list()
    }
    
    # capture data
    if (columns[[col]]$type == "text") {
      ds$binary <- 
        ds$binary %>% move_to_next_pattern(re_block("stx"), re_null(6), re_block("x-000"), re_block("x-000")) %>% 
        capture_data("value", "text", re_null(2), re_direct(".."), re_block("etx"))
    } else {
      ds$binary <- 
        ds$binary %>% move_to_next_pattern(re_block("stx"), re_block("x-000")) %>% 
        capture_data("value", columns[[col]]$type, re_null(2), re_block("x-000"))
      # sanity checks
      if (length(ds$binary$data$value) > 1) {
        op_error(ds$binary, sprintf("expected one value for cell '%s' but found %d", col, length(ds$binary$data$value)))
      } else if (ds$binary$data$value != 0 && (abs(ds$binary$data$value) < 1e-100 || abs(ds$binary$data$value) > 1e100)) {
        op_error(ds$binary, sprintf("found cell value '%s' for cell '%s' which is not a sensible numeric value", str_c(ds$binary$data$value), col))
      }
    }
    rows[[rows_i]][[col]] <- ds$binary$data$value
  }
  
  # data column information
  cols <- bind_rows(columns)
  
  # store vendor data table
  .check. <- NULL # global var
  data_table <- full_join(peaks, mutate(bind_rows(rows), .check. = TRUE), by = "Nr.")
  if (any(is.na(data_table$Start) || any(is.na(data_table$.check.)))) {
    ds <- register_warning(ds, details = "vendor data table has unexpected empty cells, process vendor table with care")
  }
  ds$vendor_data_table <- select(data_table, -.check.)
  
  # safe information on the column units
  attr(ds$vendor_data_table, "units") <- 
    bind_rows(
      select_(cols, .dots = c("column", "units")),
      data_frame(column = c("Start", "Rt", "End"), units = "[s]"),
      data_frame(column = peaks %>% select(starts_with("Ampl"), starts_with("BGD")) %>% names(), units = "[mV]")
    ) %>% 
    mutate(units = ifelse(units == " ", "", units))
  
  return(ds)
}
