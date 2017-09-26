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
  R_pre_re <- re_combine(re_or(re_text("/"), re_text("-"), size = 2), re_block("fef-0"), re_block("fef-0"), re_null(4), re_block("x-000"))
  R_post_re <- re_combine(re_block("x-000"))
  
  positions <- find_next_patterns(ds$binary, R_pre_re, re_direct(".{20}"), R_post_re)
  resistors <- list()
  for (pos in positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + R_pre_re$size) %>% 
      capture_n_data("mass", "double", n =  1) %>% 
      capture_n_data("cup", "integer", n = 1) %>% 
      capture_n_data("R.Ohm", "double", n = 1) 
    resistors <- c(resistors, list(ds$binary$data[c("cup", "R.Ohm", "mass")]))
  }
  ds$method_info$resistors <- bind_rows(resistors)
  if (nrow(ds$method_info$resistors) > 0) {
    ds$method_info$resistors$cup <- ds$method_info$resistors$cup + 1L # re-index from 1 instead of 0
    ds$method_info$resistors$mass <- as.character(ds$method_info$resistors$mass) # resistors as character
    resistor_masses <- ds$method_info$resistors$mass
  } else {
    resistor_masses <- c()
  }
  
  # if mass data is read, double check that it's the right number of resistors
  if (ds$read_options$raw_data && nrow(ds$raw_data) > 0) {
    mass_column_pattern <- "^[vi](\\d+)\\.(.*)$"
    masses <- ds$raw_data %>% 
      names() %>%
      str_subset(mass_column_pattern) %>%
      { if(length(.) == 0) return (NULL) else 
        str_match(., mass_column_pattern) %>%  { .[,2] }
      }
    
    if (!setequal(masses, resistor_masses)) {
      ds <- register_warning(
        ds, func = "extract_isodat_resistors",
        details = sprintf("masses found for resistors (%s) do not match those found for the raw data (%s)",
                          resistor_masses %>% { if (length(.) > 0) str_c(., collapse = ",") else "none" },
                          masses %>% { if (length(.) > 0) str_c(., collapse = ",") else "none" }))
    }
  }
  return(ds)
}

# extract the reference deltas and ratios for isodat files
extract_isodat_reference_values <- function(ds, cap_at_fun = NULL) {
  # get secondar standard values
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot recover references") %>% 
    move_to_C_block("CSecondaryStandardMethodPart", reset_cap = TRUE) 
  
  # run cap at function if provided
  if (!is.null(cap_at_fun)) {
    ds$binary <- cap_at_fun(ds$binary)
  }
  
  # global vars
  delta_code <- delta_format <- ratio_code <- ratio_format <- NULL
  
  # global vars
  delta_code <- delta_format <- ratio_code <- ratio_format <- NULL
  
  # find instrument reference names
  instrument_pre1 <- re_combine(re_block("etx"), re_or(re_text("/"), re_text(","), re_text("-")), re_block("fef-0"), re_block("fef-x")) ###
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
      move_to_next_pattern(re_block("x-000"), re_block("x-000")) %>%  
      capture_n_data("delta_value", "double", 1) %>%  
      move_to_next_pattern(re_block("stx"), re_block("fef-x")) %>% 
      capture_data("reference", "text", re_null(12), re_direct("([^\\x00]{2})?"), re_block("x-000")) 
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
      move_to_next_pattern(re_block("x-000"), re_block("x-000")) %>%  ###
      capture_n_data("ratio_value", "double", 1) 
    ratios <- c(ratios, list(c(reference = ref_names[max(which(ds$binary$pos > ref_pos))],
                               ds$binary$data[c("ratio_code", "element", "ratio_name", "ratio_value", "ratio_format")])))
  }
  
  ratios <- bind_rows(ratios) %>% unique() %>% 
    select(-ratio_code) %>%  # code is very isodat specific and not stored in final
    select(-ratio_format) # format does not realy hold information that isn't contained in the values themselves
  
  # store information
  ds$method_info$standards <- unique(deltas)
  ds$method_info$reference_ratios <- unique(ratios)
  
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
extract_isodat_measurement_info <- function(ds) {
  
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

# extract H3 factor info
extract_H3_factor_info <- function(ds) {
  # H3 factor (if available)
  if ("CH3FactorResult" %in% ds$binary$C_blocks$block) {
    # extract H3 factor value (note H3 stability is not present)
    ds$binary <- ds$binary %>% 
      set_binary_file_error_prefix("cannot extract H3 factor") %>% 
      move_to_C_block("CH3FactorResult") %>% 
      move_to_next_pattern(re_text("H3 Factor")) %>% 
      move_to_next_pattern(re_block("x-000")) %>% 
      capture_n_data("H3_factor", "double", 1)
    ds$file_info$`H3 Factor` <- as.character(ds$binary$data$H3_factor)
  }
  return(ds)
}


# extract vendor computed data table for continuous flow files
extract_isodat_cf_vendor_data_table <- function(ds, cap_at_fun = NULL) {
  # find vendor data table
  ds$binary <- ds$binary %>% 
    set_binary_file_error_prefix("cannot process vendor identified peaks") %>% 
    move_to_C_block("CGCPeakList")
  
  # run cap at function if provided
  if (!is.null(cap_at_fun)) {
    ds$binary <- cap_at_fun(ds$binary)
  }
  
  ### basic peak info
  # find basic peak information (Rts, amplitude, bg) - this information is stored separatedly from the rest of the table
  rt_pre_re <- re_combine(re_null(19), re_direct("(\\x00|[\x01-\x1f])\\x00{3}", size = 4), re_block("x-000"))
  rt_re <- re_combine(rt_pre_re, re_direct("..\\x00{2}"), re_block("x-000")) 
  positions <- find_next_patterns(ds$binary, rt_re)
  rts <- list()
  for (pos in positions) {
    ds$binary <- ds$binary %>% 
      move_to_pos(pos + rt_pre_re$size) %>% 
      capture_n_data("mass", "integer", 1) %>% 
      move_to_next_pattern(re_block("x-000"), max_gap = 0) %>% 
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
    move_to_C_block("CGCPeakList", reset_cap = FALSE) %>% # important to NOT reset position cap
    move_to_next_C_block("CEvalDataIntTransferPart")
  
  # find columns and row data for the whole data table
  columns <- list()
  rows <- list()
  rows_i <- 0
  pre_column_re <- re_combine(
    re_or(re_text("/"), re_text("-"), size = 2), 
    re_block("fef-0"), re_block("fef-0"), re_null(4), re_block("x-000"), re_block("fef-x")) 
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
        capture_data("units", "raw", re_block("fef-x"), move_past_dots = TRUE, ignore_trailing_zeros = FALSE) # retrieve units
      
      # check for permil symbol (which is non-ASCII)
      permil <- as.raw(c(48, 32))
      units <- ds$binary$data$units
      if (length(units) >= length(permil) && any(apply(embed(units,length(units)-length(permil)+1),2,identical,permil))) {
        ds$binary$data$units <- "[permil]"
      } else {
        ds$binary$data$units <- parse_raw_data(ds$binary$data$units, "text")
      }
      
      # standardize the units
      if (ds$binary$data$units == "[per mil]")
        ds$binary$data$units <- "[permil]"
    
      # data format
      type <- 
        if (ds$binary$data$format == "%s") { "text"
        } else if (ds$binary$data$format %in% c("%u", "%d")) { "integer"
        } else if (str_detect(ds$binary$data$format, "\\%[0-9.]+f")) { "double"
        } else { op_error(ds$binary, 
                          sprintf("could not data table column format '%s' for column '%s'",
                                  ds$binary$data$format, col)) 
        }
      
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
        ds$binary %>% move_to_next_pattern(re_block("x-000"), re_direct("\\x00{4,6}"), re_block("x-000"), re_block("x-000")) %>%
        capture_data("value", "text", re_null(2), re_direct(".."), re_block("etx"))
    } else {
      ds$binary <- 
        ds$binary %>% move_to_next_pattern(re_block("x-000"), re_block("x-000")) %>% 
        capture_data("value", columns[[col]]$type, re_block("x-000"),
                     data_bytes_min = 4) # read at least one number
      
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

