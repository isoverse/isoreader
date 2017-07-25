# isodat file information common to multiple file types =====

# extract the datetime of the run
extract_isodat_datetime <- function(ds) {
  # find date time
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover run datetime") %>% 
    move_to_C_block("CTimeObject") %>%
    move_to_next_pattern(re_null(4), re_block("x-000")) %>% 
    capture_n_data("date", "integer", 1, sensible = c(0,1000*365*24*3600)) # 1000 years as sensible limit
  
  # store as POSIXct
  ds$file_info$file_datetime <- as.POSIXct(ds$binary$data$date, origin = "1970-01-01")
  return(ds)
}

# extract resistor information
extract_isodat_resistors <- function(ds) {
  # move to resistor information
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover resistors") %>% 
    move_to_C_block("CEvalIntegrationUnitHWInfo")
  
  # cap depends on dxf vs did
  if (is_continuous_flow(ds)) {
    ds$binary <- cap_at_next_C_block(ds$binary, "CConfiguration")
  } else if (is_dual_inlet(ds)) {
    ds$binary <- cap_at_next_C_block(ds$binary, "CGasConfiguration")
  }
  
  # find resistors
  R_pre_re <- re_combine(re_text("/"), re_block("fef-0"), re_block("fef-0"), re_null(4), 
                         re_block("x-000"), re_null(4), re_direct(".{3}\x40", size = 4))
  
  positions <- find_next_patterns(ds$binary, R_pre_re)
  resistors <- list()
  for (pos in positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + R_pre_re$size) %>% 
      capture_n_data("cup", "integer", n = 1) %>% 
      capture_n_data("R.Ohm", "double", n = 1) 
    resistors <- c(resistors, list(ds$binary$data[c("cup", "R.Ohm")]))
  }
  ds$method_info$resistors <- bind_rows(resistors) %>% 
    mutate(cup = cup+1)
  
  # if mass data is read, include the information in the resistors
  if (ds$read_options$raw_data) {
    mass_column_pattern <- "^[vi](\\d+)\\.(.*)$"
    masses <- ds$raw_data %>% 
      names() %>%
      str_subset(mass_column_pattern) %>%
      str_match(mass_column_pattern) %>%
      { .[,2] }
    if (length(masses) != nrow(ds$method_info$resistors)) {
      op_error(ds$binary, 
               sprintf("found inconsistent number of resistors (%d) compared to ion dataset (%d)",
                       nrow(ds$method_info$resistors), length(masses)))
    }
    ds$method_info$resistors <- ds$method_info$resistors %>% 
      mutate(mass = masses) %>% select(cup, mass, R.Ohm)
  }
  return(ds)
}

# extract the reference deltas and ratios for isodat files
extract_isodat_reference_values <- function(ds) {
  # get secondar standard values
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover references") %>% 
    move_to_C_block("CSecondaryStandardMethodPart", reset_cap = TRUE) 
  
  # cap at CResultArray for dxf files
  if (is(ds, "continuous_flow")) {
    ds$binary <- cap_at_next_C_block(ds$binary, "CResultArray")
  }
  
  # global vars
  delta_code <- delta_format <- ratio_code <- ratio_format <- NULL
  
  # find instrument reference names
  instrument_pre1 <- re_combine(re_block("etx"), re_or(re_text("/"), re_text(",")), re_block("fef-0"), re_block("fef-x"))
  instrument_pre2 <- re_combine(re_null(4), re_block("stx"), re_block("nl"), re_text("Instrument"))
  ref_names <- ref_configs <- ref_pos <- c()
  positions <- find_next_patterns(ds$binary, re_combine(instrument_pre1, re_block("text"), instrument_pre2))
  for (pos in positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos) %>% 
      move_to_next_pattern(instrument_pre1, max_gap = 0) %>% 
      capture_data("ref_name", "text", instrument_pre2, move_past_dots = TRUE) 
    
    instrument_post1 <- re_combine(re_block("etx"), re_block("fef-x"), re_text(ds$binary$data$ref_name), re_block("fef-x"))
    instrument_post2 <- re_combine(re_null(4), re_direct("[^\\x00]{2}"), re_block("etx"))
    
    # check for gas configuration name
    if(!is.null(pos <- find_next_pattern(ds$binary, re_combine(instrument_post1, re_block("text"), instrument_post2), max_gap = 0))) {
      ds$binary <- ds$binary %>% 
        move_to_pos(pos) %>% 
        move_to_next_pattern(instrument_post1, max_gap = 0) %>% 
        capture_data("config", "text", instrument_post2) 
    } else {
      ds$binary$data$config <- ""
    }
    
    # store information
    ref_names <- c(ref_names, ds$binary$data$ref_name)
    ref_configs <- c(ref_configs, ds$binary$data$config)
    ref_pos <- c(ref_pos, ds$binary$pos)
  }
  # references (not saved, just for troubleshooting and for delta+ratio assignments)
  refs <- data_frame(name = ref_names, config = ref_configs, pos = ref_pos)
  
  ### deltas
  # get reference delta values
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover reference delta values") %>% 
    move_to_C_block("CSecondaryStandardMethodPart", reset_cap = FALSE) 
  
  # find delta values
  delta_re <- re_combine(re_null(4), re_block("x-000"), re_block("fef-x"), re_text("Delta "))
  deltas <- list()
  
  positions <- find_next_patterns(ds$binary, delta_re)
  for (pos in positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + delta_re$size) %>% 
      capture_data("delta_code", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      capture_data("delta_name", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      capture_data("delta_format", "text", re_block("fef-x"), move_past_dots = TRUE) %>% 
      capture_data("gas", "text", re_block("fef-0"), re_block("fef-x"), move_past_dots = TRUE) %>%
      #capture_data("delta_units", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      move_to_next_pattern(re_block("stx"), re_block("x-000")) %>% 
      capture_data("delta_value", "double", re_null(2), re_block("x-000"), move_past_dots = TRUE) %>% 
      move_to_next_pattern(re_block("stx"), re_block("fef-x"), max_gap = 0) %>% 
      capture_data("reference", "text", re_null(12), re_direct("([^\\x00]{2})?"), re_block("x-000")) %>% 
      identity()
    deltas <- c(deltas, list(c(standard = ref_names[max(which(ds$binary$pos > ref_pos))],
                               #config = ref_configs[max(which(ds$binary$pos > ref_pos))], # not actually used, usually the same as the $gas
                               ds$binary$data[c("gas", "delta_code", "delta_name", "delta_value", "delta_format", "reference")])))
  }
  
  deltas <- bind_rows(deltas) %>% 
    select(-delta_code) %>%  # code is very isodat specific and not stored in final
    select(-delta_format) # format does not realy hold information that isn't contained in the values themselves
  
  ### ratios
  
  # get reference delta values
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover reference ratio values") %>% 
    move_to_C_block("CSecondaryStandardMethodPart", reset_cap = FALSE) 
  
  # find ratios
  ratio_re <- re_combine(re_null(4), re_block("x-000"), re_block("fef-x"), re_text("Ratio "))
  ratios <- list()
  
  positions <- find_next_patterns(ds$binary, ratio_re)
  for (pos in positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + delta_re$size) %>% 
      capture_data("ratio_code", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      capture_data("ratio_name", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      capture_data("ratio_format", "text", re_block("fef-0"), re_block("fef-x"), move_past_dots = TRUE) %>% 
      capture_data("element", "text", re_block("fef-x"), move_past_dots = TRUE) %>%
      move_to_next_pattern(re_block("stx"), re_block("x-000")) %>% 
      capture_data("ratio_value", "double", re_null(2), re_block("x-000")) %>% 
      identity()
    ratios <- c(ratios, list(c(reference = ref_names[max(which(ds$binary$pos > ref_pos))],
                               ds$binary$data[c("ratio_code", "element", "ratio_name", "ratio_value", "ratio_format")])))
  }
  
  ratios <- bind_rows(ratios) %>% unique() %>% 
    select(-ratio_code) %>%  # code is very isodat specific and not stored in final
    select(-ratio_format) # format does not realy hold information that isn't contained in the values themselves
  
  # store information
  ds$method_info$standards <- deltas
  ds$method_info$reference_ratios <- ratios
  
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
  # note: fef-x block seems to be used in .dxf, nl in .did
  re_end_of_info <- re_combine(re_null(4), re_or(re_combine(re_direct(".."), re_block("etx")), re_block("C-block")))
  while(!is.null(find_next_pattern(ds$binary, re_end_of_info))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(re_text("/"), re_block("fef-x")) %>% 
      capture_data("value", "text", re_or(re_block("fef-x"), re_block("nl")), re_block("text"), re_null(4), 
                   data_bytes_max = 500, move_past_dots = FALSE) %>% 
      move_to_next_pattern(re_or(re_block("fef-x"), re_block("nl"))) %>% 
      capture_data("info", "text", re_end_of_info, 
                   data_bytes_max = 500, move_past_dots = FALSE) %>% 
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
