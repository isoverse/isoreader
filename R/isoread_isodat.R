# isodat file information common to multiple file types =====

# extract the datetime of the run
extract_isodat_datetime <- function(ds) {
  # find date time
  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot recover run datetime") %>%
    move_to_C_block("CTimeObject") %>%
    move_to_next_pattern(re_null(4), re_x_000()) %>%
    capture_n_data("date", "integer", 1, sensible = c(0,1000*365*24*3600)) # 1000 years as sensible limit

  # store as POSIXct (converting seconds from CTimeObject) - use system time zone
  ds$file_info$file_datetime <- as_datetime(ds$binary$data$date, tz = Sys.timezone())
  return(ds)
}

# extract resistor information
extract_isodat_resistors <- function(ds) {

  # move to resistor information
  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot recover resistors") %>%
    move_to_C_block("CEvalIntegrationUnitHWInfo")

  # cap depends on dxf vs did
  if (iso_is_continuous_flow(ds)) {
    ds$binary <- cap_at_next_C_block(ds$binary, "CConfiguration")
  } else if (iso_is_dual_inlet(ds)) {
    ds$binary <- cap_at_next_C_block(ds$binary, "CGasConfiguration")
  }

  # find resistors
  R_pre_re <- re_combine(re_or(re_unicode("/"), re_unicode("-"), re_unicode(","), re_unicode("."), size = 2),
                         re_text_0(), re_text_0(), re_null(4), re_x_000())
  R_post_re <- re_combine(re_x_000())

  positions <- find_next_patterns(ds$binary, R_pre_re, re_direct(".{20}", label = ".{20}"), R_post_re)
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

  # instrument reference name reg exps
  instrument_pre1 <- re_combine(re_block("etx"), re_or(re_unicode("/"), re_unicode(","), re_unicode("-"), re_unicode(".")), re_text_0(), re_text_x())
  instrument_pre2 <- re_combine(re_null(4), re_block("stx"), re_block("nl"), re_unicode("Instrument"))
  instrument_post2 <- re_combine(re_null(4), re_direct("[^\\x00]{2}", label = "[^00]{2}"), re_block("etx"))

  # capture reference names
  capture_ref_names <- function(pos) {
    bin <- ds$binary %>%
      move_to_pos(pos) %>%
      move_to_next_pattern(instrument_pre1, max_gap = 0) %>%
      capture_data_till_pattern("ref_name", "text", instrument_pre2, move_past_dots = TRUE)

    instrument_post1 <- re_combine(re_block("etx"), re_text_x(), re_unicode(bin$data$ref_name), re_text_x())

    # check for gas configuration name
    if(!is.null(pos <- find_next_pattern(bin, re_combine(instrument_post1, re_block("text"), instrument_post2), max_gap = 0))) {
      bin <- bin %>%
        move_to_pos(pos) %>%
        move_to_next_pattern(instrument_post1, max_gap = 0) %>%
        capture_data_till_pattern("config", "text", instrument_post2)
    } else {
      bin$data$config <- ""
    }

    # store information
    tibble(name = bin$data$ref_name, config = bin$data$config, pos = bin$pos)
  }

  # run refs capture
  start_pos <- find_next_patterns(
    ds$binary, re_combine(instrument_pre1, re_block("text"), instrument_pre2))
  if (length(start_pos) == 0) {
    stop("could not find reference names at position ", ds$binary$pos,
         ", no match for search ",
         instrument_pre1$label, "<NAME>", instrument_pre2$label, call. = FALSE)
  }
  refs <- tibble(
    start_pos = start_pos,
    data = map(.data$start_pos, capture_ref_names)
  ) %>% unnest("data")

  ### deltas
  # get reference delta values
  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot recover reference delta values") %>%
    move_to_C_block("CSecondaryStandardMethodPart", reset_cap = FALSE)

  # find delta values
  delta_re <- re_combine(re_null(4), re_x_000(), re_text_x(), re_unicode("Delta "))
  positions <- find_next_patterns(ds$binary, delta_re)

  # capture delta values
  capture_delta_values <- function(pos) {
    bin <- ds$binary %>%
      move_to_pos(pos) %>%
      capture_data_till_pattern("delta_code", "text", re_text_x(), move_past_dots = TRUE) %>%
      capture_data_till_pattern("delta_name", "text", re_text_x(), move_past_dots = TRUE) %>%
      capture_data_till_pattern("delta_format", "text", re_text_x(), move_past_dots = TRUE) %>%
      capture_data_till_pattern("gas", "text", re_text_0(), re_text_x(), move_past_dots = TRUE) %>%
      #capture_data_till_pattern("delta_units", "text", re_text_x(), move_past_dots = TRUE) %>%
      move_to_next_pattern(re_x_000(), re_x_000()) %>%
      capture_n_data("delta_value", "double", 1) %>%
      move_to_next_pattern(re_block("stx"), re_text_x()) %>%
      capture_data_till_pattern("reference", "text", re_null(12),
                   re_direct("([^\\x00]{2})?", label = "[^00]{2}"), re_x_000())

    # return as data frame
    dplyr::as_tibble(
      bin$data[c("gas", "delta_code", "delta_name", "delta_value", "delta_format", "reference")]
    ) %>% mutate(
      standard = refs$name[max(which(bin$pos > refs$pos))]
      #config = refs$config[max(which(bin$pos > refs$pos))], # not actually used, usually the same as the $gas
    )
  }

  # run delta capture
  deltas <- tibble(
    pos = positions + delta_re$size,
    data = map(.data$pos, capture_delta_values)
  ) %>% unnest("data") %>%
    # delta_code is very isodat specific and not stored in final, delta_format does not really hold additional information
    select("standard", "gas", "delta_name", "delta_value", "reference")


  ### ratios

  # get reference delta values
  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot recover reference ratio values") %>%
    move_to_C_block("CSecondaryStandardMethodPart", reset_cap = FALSE)

  # find ratios
  ratio_re <- re_combine(re_null(4), re_x_000(), re_text_x(), re_unicode("Ratio "))
  positions <- find_next_patterns(ds$binary, ratio_re)

  # capture ratio values
  capture_ratio_values <- function(pos) {
    bin <- ds$binary %>%
      move_to_pos(pos) %>%
      capture_data_till_pattern("ratio_code", "text", re_text_x(), move_past_dots = TRUE) %>%
      capture_data_till_pattern("ratio_name", "text", re_text_x(), move_past_dots = TRUE) %>%
      capture_data_till_pattern("ratio_format", "text", re_text_0(), re_text_x(), move_past_dots = TRUE) %>%
      capture_data_till_pattern("element", "text", re_text_x(), move_past_dots = TRUE) %>%
      move_to_next_pattern(re_x_000(), re_x_000()) %>%  ###
      capture_n_data("ratio_value", "double", 1)

    # return as data frame
    dplyr::as_tibble(bin$data[c("ratio_code", "element", "ratio_name", "ratio_value", "ratio_format")]) %>%
      mutate(
        reference = refs$name[max(which(bin$pos > refs$pos))]
      )
  }

  # run ratio capture
  if (length(positions) > 0) {
    ratios <- tibble(
      pos = positions + ratio_re$size,
      data = map(.data$pos, capture_ratio_values)
    ) %>%
      unnest("data") %>%
      select("reference", "element", "ratio_name", "ratio_value")
  } else {
    # no ratios defined
    ratios <- tibble(reference = character(0), element = character(0),
                         ratio_name = character(0), ratio_value = numeric(0))
  }

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
    move_to_next_pattern(re_unicode("Sequence Line Information"))

  seq_line_info <- list()
  re_end_of_info <- re_combine(
    re_null(4),
    re_or(re_combine(re_not_null(2), re_block("etx")),
          re_block("C-block")))
  caps <- find_next_patterns(ds$binary, re_end_of_info)
  if (length(caps) == 0) stop("could not any data", call. = FALSE)
  positions <- c(ds$binary$pos, head(caps, -1))
  # note: fef-x block seems to be used in .dxf, nl in .did
  re_val_var_break <- re_or(re_text_x(), re_block("nl"))
  re_val_var_break$size <- 4

  # loop through all
  for (i in 1:length(positions)) {
    # capture value
    ds$binary <- ds$binary %>%
      move_to_pos(positions[i], reset_cap = TRUE) %>%
      cap_at_pos(caps[i]) %>%
      move_to_next_pattern(re_or(re_unicode("/"), re_unicode(".")), re_text_x()) %>%
      capture_data_till_pattern("value", "text", re_val_var_break, data_bytes_max = 500, move_past_dots = TRUE)

    # capture info name
    info_length <- (ds$binary$max_pos - ds$binary$pos)/2
    if (info_length %% 1 > 0)
      stop("length of sequence info for value '", ds$binary$data$value, "' is not an integer (", info_length, ")", call. = FALSE)
    ds$binary <- ds$binary %>%
      capture_n_data("info", "text", (ds$binary$max_pos - ds$binary$pos)/2)

    # store info
    if (!is.null(ds$binary$data$info))
      ds$file_info[[ds$binary$data$info]] <- ds$binary$data$value
  }

  return(ds)
}

# extract sequence line info from old isodat files (.cf and .caf)
extract_isodat_old_sequence_line_info <- function(ds) {

  # find sequence line information
  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot process sequence line info") %>%
    move_to_C_block("CSequenceLineInformationGridStorage") %>%
    move_to_next_pattern(re_direct("\xff{12}", label = "ff{12}"))

  # block delimiter
  cap_pos <- find_next_pattern(
    ds$binary,
    re_direct("\x86{3}\\x00\x96{3}\\x00\xCB{3}\\x00\xB2{3}\\x00\xD7{3}\\x00\xDD{3}\\x00",
              label = "86{3}0096{3}00cb{3}00b2{3}00d7{3}00dd{3}00")
  )
  if (!is.null(cap_pos)) {
    ds$binary <- ds$binary %>% cap_at_pos(cap_pos)
  } else iso_source_file_op_error(ds$binary, "cannot find binary delimiter for end of Sequence Information")

  # first line marker
  line_re <- re_combine(
    re_x_000(), re_direct(".{2,8}", label = ".{2,8}"),
    re_text_x(), re_unicode("Line"))
  ds$binary <- ds$binary %>%
    move_to_next_pattern(line_re, move_to_end = FALSE) %>%
    capture_n_data("info_marker", "raw", 4)

  # regular expressions
  re_entry_start <- re_control(ds$binary$data$info_marker)
  label_pre_re <- re_combine(re_direct(".{2,8}", size = 8, label = ".{2,8}"), re_text_x())
  # NOTE: all of these seem to be valid end blocks for text segements in this part of the file, any way to make this simpler?
  label_post_re <- re_or(
    re_combine(re_null(7), re_direct("\xff\\x00{3}", label = "ff00{3}")),
    re_combine(re_x_000(), re_text_x()),
    re_combine(re_null(4), re_text_x()),
    re_combine(re_null(4), re_x_000()),
    re_combine(re_null(4), re_direct("\xff{3}\\x00", size = 4, label = "ff{3}00")))

  # extract information
  positions <- find_next_patterns(ds$binary, re_entry_start)
  label <- value <- NULL # global vars
  labels <- list(list(label = "Line", label_marker = NA_character_))
  values <- list()
  reached_values <- FALSE
  for (pos in positions) {
    ds$binary <- ds$binary %>% move_to_pos(pos + re_entry_start$size)
    if (!is.null(find_next_pattern(ds$binary, label_pre_re, max_gap = 0))) {
      ds$binary <- ds$binary %>%
        move_to_next_pattern(label_pre_re) %>%
        { move_to_pos(., .$pos - 1) } %>%
        capture_n_data("marker", "raw", 1) %>%
        capture_data_till_pattern("text", "text", label_post_re)
      text <- ds$binary$data$text
      marker <- as.character(ds$binary$data$marker)
    } else {
      text <- marker <- NA_character_
    }

    # check for values
    if (!reached_values && marker %in% c("01", "02")) {
      # NOTE: is there any better way to detect where the value starts??
      reached_values <- TRUE
    }

    # add to values/labels
    if (is.null(text)) text <- NA_character_

    if (reached_values)
      values <- c(values, list(list(value = text, value_marker = marker)))
    else
      labels <- c(labels, list(list(label = text, label_marker = marker)))
  }

  if (length(values) != length(labels)) {
    if(default("debug")) {
      log_message("labels:\n", bind_rows(labels), prefix = "DEBUG: ")
      log_message("values:\n", bind_rows(values), prefix = "DEBUG: ")
    }
    stop(sprintf("unequal number of file info labels (%d) and file info values (%d)",
                 length(labels), length(values)), call. = FALSE)
  }

  # store file info
  file_info <- left_join(
    bind_rows(labels) %>% mutate(n = 1:n()),
    bind_rows(values) %>% mutate(n = 1:n()),
    by = "n"
  )

  # transfer to
  if (nrow(file_info) > 0) {
    ds$file_info <-
      dplyr::mutate(
        ds$file_info,
        !!!rlang::set_names(file_info$value, file_info$label)
      )
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
  while(!is.null(find_next_pattern(ds$binary, re_unicode("CUserInfo")))) {
    ds$binary <- ds$binary %>%
      move_to_next_pattern(re_x_000(), re_text_x()) %>%
      capture_data_till_pattern("info", "text", re_text_x(), re_unicode("CUserInfo"), move_past_dots = TRUE)
    isl_info_msgs <- c(isl_info_msgs, ds$binary$data$info)
  }

  # store all in one information set
  ds$file_info$measurement_info <- list(isl_info_msgs)

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
      move_to_next_pattern(re_unicode("H3 Factor")) %>%
      move_to_next_pattern(re_x_000()) %>%
      capture_n_data("H3_factor", "double", 1)
    # this is a text field for compatibility with other file formats
    ds$file_info$`H3 Factor` <- as.character(ds$binary$data$H3_factor)
  }
  return(ds)
}

# extract MS integration time
extract_MS_integration_time_info <- function(ds) {

  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot extract MS integration time") %>%
    move_to_C_block("CActionPeakCenter", move_to_end = FALSE) %>%
    skip_pos(-5) %>%
    capture_n_data("ms_integration_time", "integer", 1, sensible = c(0L, 3600000L))

  # store ms integration time (should this be text for compatibility with other formats?)
  ds$file_info$MS_integration_time.s <- ds$binary$data$ms_integration_time/1000

  return(ds)
}

# extract vendor computed data table for continuous flow files
extract_isodat_continuous_flow_vendor_data_table <- function(ds, cap_at_fun = NULL) {

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
  rt_pre_re <- re_combine(
    re_null(18),
    re_direct("(\\x00|[\x01-\x1f])\\x00{3}", size = 4, label = "00|[01-1f]00{3}"),
    re_x_000())
  rt_re <- re_combine(rt_pre_re, re_direct("..\\x00{2}", label = "..00{2}"), re_x_000())
  positions <- find_next_patterns(ds$binary, rt_re)
  rts <- list()
  for (pos in positions) {
    ds$binary <- ds$binary %>%
      move_to_pos(pos + rt_pre_re$size) %>%
      capture_n_data("mass", "integer", 1) %>%
      move_to_next_pattern(re_x_000(), max_gap = 0) %>%
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
    # FIXME: is this maybe the more accurate background value? main vendor data table seems to not always pull the right one
    rts <- c(rts, list(c(ini = pos, pos = ds$binary$pos, ds$binary$data[c("mass", "peak", "start", "rt", "end", "amp", "bg")])))
  }

  # NOTE: the retention time is only ALMOST repeated each time, if there are significant
  # chromatographic shifts (as is alwayst he case for H2), these will in fact NOT quite be
  # identical. Isodat seems to report only the major ion (first ion here) so we are doing the same
  rts_df <- dplyr::bind_rows(rts)
  if (nrow(rts_df) == 0 || !"mass" %in% names(rts_df) || !any(rts_df$mass > 0)) {
    return(ds) # no vendor data table entries found
  }

  # filter out false matches
  rts_df <- dplyr::filter(rts_df, .data$mass > 0)
  # retention times
  peaks <- rts_df %>%
    select("peak", Start = "start", Rt = "rt", End = "end") %>%
    distinct(.data$peak, .keep_all = TRUE) %>%
    # add in amplitudes
    left_join(
      rts_df %>%
        select("peak", Ampl = "mass", "amp") %>%
        spread(.data$Ampl, .data$amp, sep = " "),
      by = "peak"
    ) %>%
    # add in backgrounds
    left_join (
      rts_df %>%
        select("peak", BGD = "mass", "bg") %>%
        spread(.data$BGD, .data$bg, sep = " "),
      by = "peak"
    ) %>%
    rename(`Nr.` = "peak")

  ### rest of data table
  extracted_dt <- extract_isodat_main_vendor_data_table_fast(ds, C_block = "CGCPeakList", cap_at_fun = cap_at_fun)
  if (nrow(extracted_dt$cell_values) == 0L) {
    stop("could not find any vendor table data", call. = FALSE)
  }

  # store vendor data table
  data_table <- full_join(peaks, mutate(extracted_dt$cell_values, .check = TRUE), by = "Nr.")
  if (any(is.na(data_table$Start) | any(is.na(data_table$.check)))) {
    ds <- register_warning(ds, details = "vendor data table has unexpected empty cells, process vendor table with care")
  }
  ds$vendor_data_table <- select(data_table, -".check")

  # safe information on the column units
  attr(ds$vendor_data_table, "units") <-
    bind_rows(
      dplyr::select(extracted_dt$columns, "column", units = "column_units"),
      tibble::tibble(column = c("Start", "Rt", "End"), units = "[s]"),
      tibble::tibble(column = peaks %>% select(starts_with("Ampl"), starts_with("BGD")) %>% names(), units = "[mV]")
    ) %>%
    mutate(units = ifelse(units == " ", "", units))

  # FIXME: do this directly
  ds$vendor_data_table <- convert_df_units_attr_to_implicit_units(ds$vendor_data_table)

  return(ds)
}


# extract the main (recurring portion of the vendor data table)
# @param C_block which C_block to start at
# @param col_include regexp to decide which columns to include (all by default)
# @param skip_row_check function to check whether to skip a row based on the most recent column and column value
# @note: doing this in a for loop actually turned out faster than with apply, especially with the skip_row parameter
extract_isodat_main_vendor_data_table <- function(ds, C_block, cap_at_fun = NULL, col_include = "*",
                                                  skip_row_check = function(column, value) FALSE) {

  # main data table
  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot process vendor data table") %>%
    move_to_C_block(C_block, reset_cap = FALSE) %>% # important to NOT reset position cap
    move_to_next_C_block("CEvalDataIntTransferPart")

  # run cap at function if provided
  if (!is.null(cap_at_fun)) {
    ds$binary <- cap_at_fun(ds$binary)
  }

  # find columns and row data for the whole data table
  pre_column_re <- re_combine(
    re_or(re_unicode("/"), re_unicode("-"), re_unicode(","), re_unicode("."), size = 2),
    re_text_0(), re_text_0(), re_null(4), re_x_000(), re_text_x())
  positions <- find_next_patterns(ds$binary, pre_column_re)

  # collection variables
  columns <- list()
  rows <- list()
  rows_i <- 0
  skip_row <- FALSE

  for(pos in positions) {
    # get column name
    ds$binary <- ds$binary %>%
      move_to_pos(pos + pre_column_re$size) %>%
      # skip ID column since it is not unique in peak jumping files
      move_to_next_pattern(re_text_x())

    # capture column
    ds$binary <- ds$binary %>%
      capture_data_till_pattern("column", "raw", re_text_x(), move_past_dots = TRUE, ignore_trailing_zeros = FALSE)

    # check for columns starting with delta symbol, replace with d instead of delta symbol
    if (identical(ds$binary$data$column[1:2], as.raw(c(180, 03))))
      ds$binary$data$column[1:2] <- as.raw(c(100, 00))
    col <- parse_raw_data(ds$binary$data$column, "text")

    # skip columns that don't fit the include criteria right away
    if (!grepl(col_include, col)) next #skip

    # check for new row
    if (length(columns) == 0 || col == names(columns)[1]) {
      rows_i <- rows_i + 1
      rows[[rows_i]] <- list()
      skip_row <- FALSE
    }

    # check whether still in row skip
    if (skip_row) next # skip

    # check if have a proper units next
    if (is.null(find_next_pattern(ds$binary, re_block("text"), re_text_x(), max_gap = 0))) {
      # this is something else, not a proper units block
      next # skip
    }

    # get column formatting
    ds$binary <- ds$binary %>%
      # retrieve format (!not always the same)
      capture_data_till_pattern("format", "text", re_text_x(), move_past_dots = TRUE)

    # skip data columns without propre formatting infromation right away
    if(ds$binary$data$format %in% c("", " ") || nchar(ds$binary$data$format) > 4) {
      next # skip
    }

    # store information about new column if not already stored
    if (!col %in% names(columns)) {
      ds$binary <- ds$binary %>%
        # skip what looks like it might be the gas configuration and an unknown piece of information
        move_to_next_pattern(re_block("text0"), re_text_x(), re_block("text0"), re_text_x()) %>%
        capture_data_till_pattern("units", "raw", re_text_x(), move_past_dots = TRUE, ignore_trailing_zeros = FALSE) # retrieve units

      # process isodat units
      ds$binary$data$units <- process_isodat_units(ds$binary$data$units)

      # data format
      type <-
        if (ds$binary$data$format == "%s") { "text"
        } else if (ds$binary$data$format %in% c("%u", "%d")) { "integer"
        } else if (str_detect(ds$binary$data$format, "\\%[0-9.]*f")) { "double"
        } else { iso_source_file_op_error(ds$binary,
                          sprintf("could not process data table column format '%s' for column '%s'",
                                  ds$binary$data$format, col))
        }

      # store
      new_col <- c(list(pos = ds$binary$pos, type = type, column = col), ds$binary$data[c("format", "units")])
      columns[[col]] <- new_col
    } else if (ds$binary$data$format != columns[[col]]$format) {
      # double check formatting
      iso_source_file_op_error(ds$binary,
               sprintf("mismatched data column format for column '%s', found '%s' but expected '%s'",
                       col, ds$binary$data$format, columns[[col]]$format))
    }

    # capture data
    if (columns[[col]]$type == "text") {
      ds$binary <-
        ds$binary %>% move_to_next_pattern(
          re_x_000(), re_direct("\\x00{4,6}", label = "00{4,6}"),
          re_x_000(), re_x_000()) %>%
        capture_data_till_pattern("value", "text", re_null(2), re_direct("..", label = ".."), re_block("etx"))
    } else {
      ds$binary <-
        ds$binary %>% move_to_next_pattern(re_x_000(), re_x_000()) %>%
        capture_data_till_pattern("value", columns[[col]]$type, re_x_000(),
                     data_bytes_min = 4) # read at least one number

      # sanity checks
      if (is.nan(ds$binary$data$value)) {
        ds$binary$data$value <- NA # safety to catch things that are not valid numbers at all
      } else if (length(ds$binary$data$value) > 1) {
        iso_source_file_op_error(ds$binary, sprintf("expected one value for cell '%s' but found %d", col, length(ds$binary$data$value)))
      } else if (ds$binary$data$value != 0 && (abs(ds$binary$data$value) < 1e-100 || abs(ds$binary$data$value) > 1e100)) {
        iso_source_file_op_error(ds$binary, sprintf("found cell value '%s' for cell '%s' which is not a sensible numeric value", str_c(ds$binary$data$value), col))
      }
    }

    # check whether row should be skipped
    skip_row <- do.call(skip_row_check, args = list(col, ds$binary$data$value))
    if (skip_row) {
      rows[[rows_i]] <- NULL
    } else {
      rows[[rows_i]][[col]] <- ds$binary$data$value
    }
  }

  cols <- bind_rows(columns)
  cells <- bind_rows(rows)

  return(list(columns = cols, cell_values = cells))
}

# process isodat raw units into text units
# replaces permil symbol with "permil" for ASCII compatibility
# standardizes "per mil" to "permil"
process_isodat_units <- function(raw_units) {

  # raw vectors
  raw_permil <- as.raw(c(0x30, 0x20))
  text_permil <- as.raw(c(0x70, 0x00, 0x65, 0x00, 0x72, 0x00, 0x6d, 0x00, 0x69, 0x00, 0x6c, 0x00))

  if (length(raw_units) > 1) {
    # replace permil symbol (which is non-ASCII)
    raw_units <-
      tibble(
        pos1 = raw_units,
        pos2 = c(raw_units[-1], as.raw(0x00)),
        idx = 1:length(.data$pos1)
      ) %>%
      mutate(
        is_permil = purrr::map2_lgl(.data$pos1, .data$pos2, ~ identical(c(.x, .y), raw_permil)),
        pos1 = purrr::map2(.data$pos1, .data$is_permil, ~ {
          if(.y) text_permil
          else .x
        })
      ) %>%
      filter(
        !.data$idx %in% (.data$idx[.data$is_permil] + 1L)
      ) %>%
      dplyr::pull(.data$pos1) %>%
      unlist()
  }

  # convert to text units
  text_units <- parse_raw_data(raw_units, "text")

  # standardize the units
  text_units <- stringr::str_replace_all(text_units, "per mil", "permil")

  return(text_units)
}

# NEW approach - now used for all files with vendor data table except .caf files =====

# alternative implementation that's much faster
# @param col_include optional regexp to decide which columns to include
# @note: does not support the newer skip row parameters - does it need to or can that be done with a filter afterwards?
# @note: to debug, run extract_isodat_main_vendor_data_table_columns(iso_file, pos = ?, max = ?)
# with the positions in the error messages
extract_isodat_main_vendor_data_table_fast <- function(ds, C_block, cap_at_fun = NULL, col_include = NULL) {

  # main data table
  ds$binary <- ds$binary %>%
    set_binary_file_error_prefix("cannot process vendor data table") %>%
    move_to_control_block(block = C_block, reset_cap = TRUE) %>%
    move_to_next_control_block(block = "CEvalDataIntTransferPart")

  # run cap at function if provided
  if (!is.null(cap_at_fun)) {
    ds$binary <- cap_at_fun(ds$binary)
  }

  columns <- extract_isodat_main_vendor_data_table_columns(ds, col_include = col_include)

  # safety check: to make sure all columns have the same format specification
  if (!all(ok <- columns$n_formats == 1)) {
    formats <- map_chr(columns$data[!ok], ~collapse(unique(.x$format), ", "))
    problems <- glue("column {columns$column[!ok]} has multiple formats '{formats}'")
    iso_source_file_op_error(ds$binary, glue("mismatched data column formats:\n{collapse(problems, '\n')}"))
  }

  # safety check: to make sure all formats are resolved
  if (!all(ok <- !is.na(columns$type))) {
    problems <- glue("column {columns$column[!ok]} has unknown format '{columns$column_format[!ok]}'")
    iso_source_file_op_error(ds$binary, glue("unknown column formats:\n{collapse(problems, '\n')}"))
  }

  cell_values <- extract_isodat_main_vendor_data_table_values(ds, columns)

  return(list(columns = columns, cell_values = cell_values))
}

# extract the main (recurring) portion of the vendor data table
extract_isodat_main_vendor_data_table_columns <- function(ds, pos = ds$binary$pos, max = ds$binary$max_pos, col_include = NULL) {

  # error message
  ds$binary <- ds$binary %>%
    set_pos_and_cap(pos = pos, max = max) %>%
    set_binary_file_error_prefix("cannot retrieve vendor data table columns")

  # find columns and row data for the whole data table
  pre_column_re <- re_combine(
    re_or(re_unicode("/"), re_unicode("-"), re_unicode(","), re_unicode("."), size = 2),
    re_text_0(), re_text_0(), re_null(4), re_x_000(), re_text_x())
  positions <- find_next_patterns(ds$binary, pre_column_re) + pre_column_re$size - 4L

  if (length(positions) == 0) {
    iso_source_file_op_error(ds$binary, "vendor data column pattern yielded no matches, missing data table?")
  }

  # make sure to skip Raw Data to Overwritten block
  overwritten_block <- integer(0)
  raw_data_block <- fetch_block_idx(ds$binary, type = "text", block = "Raw Data", min_pos = ds$binary$pos, max_pos = ds$binary$max_pos, occurence = 1)
  if (length(raw_data_block) > 0) {
    overwritten_block <- fetch_block_idx(ds$binary, type = "text", block = "Overwritten", 
                                         min_block_idx = raw_data_block, max_pos = ds$binary$max_pos, occurence = 1)
  }
  if(length(raw_data_block) > 0 && length(overwritten_block) > 0) {
    filter_expr <- rlang::expr(block_idx < !!raw_data_block | block_idx > !!overwritten_block)
  } else {
    filter_expr <- rlang::expr(TRUE)
  }
  
  # column info
  cols_info <- dplyr::tibble(
    col = c("id", "column", "format", "gas_config?", "unclear1", "units", "ref_frame"),
    idx = 1:length(col)
  )

  # pull out all the columns
  columns <-
    ds$binary %>%
    fetch_block_entry(filter = !!filter_expr, type = "text", min_pos = positions[1], max_pos = ds$binary$max_pos) %>%
    dplyr::mutate(group = purrr::map_int(.data$start, ~sum(.x >= positions))) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(
      idx = 1:n(),
      continue_pos = max(.data$end[.data$idx <= nrow(cols_info)])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(cols_info, by = "idx") %>%
    tidyr::pivot_wider(id_cols = c("group", "continue_pos"), 
                       names_from = "col", values_from = "block") %>%
    # skip entries that don't have formats
    filter(!is.na(format), nchar(format) > 1) %>%
    # row numbers
    dplyr::mutate(row = cumsum(.data$column == .data$column[1])) %>%
    # remove duplicates
    dplyr::group_by(column, row) %>%
    dplyr::summarize(
      group = .data$group[1],
      continue_pos = .data$continue_pos[1],
      id = .data$id[1],
      format = .data$format[1],
      `gas_config?` = .data$`gas_config?`[1],
      units = .data$units[1],
      ref_frame = .data$units[1],
      .groups = "drop"
    ) %>%
    dplyr::arrange(group) %>%
    # nest by column and expand column details
    tidyr::nest(data = c(-"column")) %>%
    dplyr::mutate(
      n_formats = purrr::map_int(.data$data, ~length(unique(.x$format))),
      column_format = purrr::map_chr(.data$data, ~.x$format[1]),
      column_units = purrr::map_chr(.data$data, ~.x$units[1]),
      type =
        dplyr::case_when(
          .data$column_format == "%s" ~ "text",
          .data$column_format %in% c("%u", "%d") ~ "integer",
          str_detect(.data$column_format, "\\%[0-9.]*f") ~ "double",
          TRUE ~ NA_character_
        )
    ) %>%
    # naming adjustments
    dplyr::mutate(
      # avoid issues with delta symbol on different OS
      column = stringr::str_replace(column, fixed("\U03B4"), "d"),
      # and rename per mil and \U2030 to permil
      column_units = 
        stringr::str_replace(column_units, fixed("per mil"), "permil") %>%
        stringr::str_replace(fixed("\U2030"), "permil")
    )


  # apply col_include parameter if provided
  if (!is.null(col_include))  columns <- filter(columns, str_detect(column, !!col_include))

  return(columns)
}

# extract the cell values
# extract the main (recurring) portion of the vendor data table
extract_isodat_main_vendor_data_table_values <- function(ds, columns) {

  # global vars
  data <- column <- continue_pos <- type <- NULL

  # capture cell value
  # NOTES: adds about 1s to a 300 cell table)
  capture_table_cell_value <- function(pos, type) {
    # capture data
    bin <- ds$binary %>% move_to_pos(pos)
    if (type == "text") {
      bin <-
        bin %>% move_to_next_pattern(
          re_x_000(), re_direct("\\x00{4,6}", label = "00{4,6}"),
          re_x_000(), re_x_000()) %>%
        capture_data_till_pattern("value", "text", re_null(2), re_direct("..", label = ".."), re_x_000())
    } else {
      bin <-
        bin %>% move_to_next_pattern(re_x_000(), re_x_000()) %>%
        capture_data_till_pattern("value", type, re_x_000(),
                     data_bytes_min = 4) # read at least one number

      # sanity checks
      if (length(bin$data$value) > 1) {
        iso_source_file_op_error(ds$binary, sprintf("expected one cell value but found %d", length(bin$data$value)))
      } else if (!is.nan(bin$data$value) && bin$data$value != 0 && (abs(bin$data$value) < 1e-100 || abs(bin$data$value) > 1e100)) {
        iso_source_file_op_error(ds$binary, sprintf("found cell value '%s' which is not a sensible numeric value", str_c(ds$binary$data$value)))
      }
    }
    return(bin$data$value)
  }

  # get cell values
  columns %>%
    filter(!is.na(type)) %>%
    unnest("data") %>%
    select("column", "continue_pos", "type", "row") %>%
    nest(data = c(-row)) %>%
    mutate(
      data = map(data, function(row) {
        # note: have to do it this way (by row instead of in long format) to keep correct data types for each column
        mapply(function(x,y,z) list(capture_table_cell_value(y, z)), row$column, row$continue_pos, row$type)
      })
    ) %>%
    { bind_rows(.$data) }
}
