
# read nu .txt file
# @param ds the data structure to fill
# @param custom reader options - specify list(nu_masses = c()) to provide masses for the channels
iso_read_nu <- function(ds, options = list()) {
  
  # global vars
  header <- data <- NULL
  
  # safety checks
  if(!iso_is_dual_inlet(ds)) 
    stop("data structure must be a 'dual_inlet' iso_file", call. = FALSE)
  
  # read data file
  ds$source <- exec_func_with_error_catch(read_nu_data_file, get_ds_file_path(ds))
  
  # all parsers ==========
  # parsers are processed in order
  parsers <- 
    list(
      # sample failed parser
      sample_failed = create_nu_simple_file_info_parser("Sample Failed", "chr", n_req = "?"),
      # sample failed checkpoint
      sample_failed_check = 
        create_nu_checkpoint(
          id = "Sample Failed", 
          check_quo = quo(
            if (col_in_df(ds$file_info, "Sample Failed"))
              stop(ds$file_info$`Sample Failed`, call. = FALSE)
          ))
      
    )
  
  # file info parsers =====
  if(ds$read_options$file_info) {
    
    parsers <- c(parsers, list(
      # sample name
      sample_name = create_nu_simple_file_info_parser("Sample Name", "chr"),
      # sample weight
      sample_weight = create_nu_simple_file_info_parser("Sample Weight", "dbl"),
      # method file name
      method_file_name = create_nu_simple_file_info_parser("Method File Name", "chr"),
      # file time
      file_time = # try to get datetime from the UTC FileTime first
        create_nu_parser(
          type = "file_info", id = "file_datetime", n_req = 1, #n_req = "?",
          check_quo = quo(!col_in_df(ds$file_info, "Sample Failed")),
          pattern = "\"UTC FileTimeLow\",(-?\\d+),\" UTC FileTimeHigh\",(-?\\d+)",
          parse_quo = quo({
            brackets <- stringr::str_match(header, parser$pattern)
            windows_time <- as.integer(brackets[,2]) + 2^32 * as.integer(brackets[,3])
            unix_time <- windows_time / 10000000 - 11644473600
            lubridate::as_datetime(unix_time)
          })
        )
      # consider implementing alternative datetime retrieval (turn n_req = "?" on for regular file_Time)
      # file_time_string_tz = create_nu_parser(....),
      # file_time_string = 
      #   create_nu_parser(
      #     type = "file_info", id = "file_datetime",
      #     pattern = "\"Started analysis at....\"",
      #     check_quo = quo(is.na(ds$file_info$file_datetime)), 
      #     parse_quo = quo()
      #   )
    ))
  }
  
  # data parsers =========
  if (ds$read_option$raw_data) {
    parsers <- c(parsers, list(
      # (only run if sample failed not relevant)
      # number of blocks & channels
      num_blocks = 
        create_nu_parser(
          type = "temp", id = c("n_blocks", "n_channels"), 
          pattern = make_capture_pattern("Num Blocks", "int"),
          check_quo = quo(!col_in_df(ds$file_info, "Sample Failed")),
          parse_quo = quo(
            list(
              as.integer(extract_substring(header, parser$pattern, capture_bracket = 1)),
              sum(stringr::str_trim(data[[1]]) == "-1")
            )
          )
        ),
      
      # data blocks
      data_blocks =
        create_nu_parser(
          type = "data", id = c("bgrd_data", "raw_data"),
          pattern = make_capture_pattern("Sample Name is ", "chr"),
          check_quo = quo(!col_in_df(ds$file_info, "Sample Failed")),
          parse_quo = quo(
            exec_func_with_error_catch(parse_nu_data, data[[1]], ds$temp$n_blocks, ds$temp$n_channels, options$nu_masses)
          )
        )
    ))
  }
  
  # run parsers ======

  for (parser in parsers) {
    ds <- exec_func_with_error_catch(
      process_nu_parser, ds, parser, options,
      msg_prefix = str_c(str_c(parser$id, collapse = ", "), ": "))
  }
  
  # check on raw data masses vs. channels
  if (!is.null(ds$raw_data) && nrow(ds$raw_data) > 0) {
    if (any(n_channels <- stringr::str_detect(names(ds$raw_data), "^[iIvV]C(\\d+)"))) {
      # only have channel information, provide warning
      ds <- ds %>% register_warning(
        glue::glue(
          "found {sum(n_channels)} channels but {length(options$nu_masses)} masses were specified ",
          "- the raw data will be reported in channels instead of masses. ",
          "To correct this problem, make sure to fully specify the nu_masses parameter during file read.")
      )
    }
  }
  
  return(ds)
}

# read entire Nu text file
# defines each line starting with a quoted string as a data block (groups blocks via group_lines function)
# @return data frame with the data blocks, header line and data (everything but the header line)
read_nu_data_file <- function(filepath) {
  
  if (!file.exists(filepath) || file.info(filepath)$isdir == TRUE)
    stop("file does not exist or is a directory: ", filepath, call. = TRUE)
  
  # read file data
  readLines(con = filepath) %>% 
    group_lines("^\\\"") %>% 
    return()
}

# parser functions =======

# create parser for nu file
# @param type data type = file_info, raw_data
# @param id field id for where to safe the data
# @param n_reqs how many matches are allowed/required for this parser, default for all that are not specified is exactly 1.
# Allowed values are a specific number 1,2,3,4, etc. "*" for any number, "?" for 0 or 1 and "+" for at least one.
# @param check_quo quoted expression to check whether to run the parser. Has to return a boolean. Evaluated in the local environment of process_nu_parser.
# @param parse_quo quoted expression to parse the resulting data. Return value will be stored in ds->type->id. Evaluated in the local environment of process_nu_parser.
create_nu_parser <- function(type, id, pattern, n_req = 1, check_quo = quo(TRUE), parse_quo = quo()) {
  stopifnot(type %in% c("temp", "file_info", "data"))
  stopifnot(!missing(parse_quo))
  list(
    type = type, id = id, pattern = pattern, n_req = n_req,
    check_quo = check_quo, parse_quo = parse_quo
  )
}

# create file info standard parser
create_nu_simple_file_info_parser <- function(id, type, n_req = 1) {
  create_nu_parser(
    type = "file_info", id = id, n_req = n_req,
    pattern = make_capture_pattern(id, type),
    parse_quo = extract_substring_bracket_quo(1))
}

# create checkpoint for nu file
create_nu_checkpoint <- function(id, check_quo = quo()) {
  stopifnot(!missing(check_quo))
  list(id = id, check_quo = check_quo)
}

# common capture pattern
make_capture_pattern <- function(id, type) {
  stopifnot(type %in% c("int", "dbl", "chr"))
  if (type == "chr")
    paste0("^\"", id, ":?\\s?\",\"([^\"]*)\"")
  else if (type == "int")
    paste0("^\"", id, ":?\\s?\",(\\d+)")
  else if (type == "dbl")
    paste0("^\"", id, ":?\\s?\",(\\d+\\.?\\d*)")
}

# frequently used quo to extract pattern bracket
extract_substring_bracket_quo <- function(capture_bracket) {
  # global vars
  header <- parser <- NULL
  # quo
  quo(extract_substring(header, parser$pattern, capture_bracket = !!capture_bracket))
}

# process a nu parser
process_nu_parser <- function(ds, parser, options = list()) {
  
  # checkpoint vs parser
  is_parser <- !is.null(parser$parse_quo)
  
  # run check function for both checkpoint and parser
  check <- rlang::eval_tidy(rlang::get_expr(parser$check_quo))
  if(!is.null(check) && !check) return(ds)
  
  # move on if check point complete
  if (!is_parser) return(ds)
  
  # find matches for parser
  matches <- which(str_detect(ds$source$header, parser$pattern))
  
  # n reqs test
  meets_n_req <- 
    if (rlang::is_integerish(parser$n_req) && length(matches) == as.integer(parser$n_req)) TRUE
  else if (parser$n_req == "+" && length(matches) > 0) TRUE
  else if (parser$n_req == "?" && length(matches) %in% c(0L, 1L)) TRUE
  else if (parser$n_req == "*") TRUE
  else FALSE
  
  if (!meets_n_req) {
    glue::glue("capture failed, parser expected {parser$n_req} value(s) but found {length(matches)}") %>% 
      stop(call. = FALSE)
  }
  
  # process
  if (length(matches) > 0) {
    header <- ds$source$header[matches]
    data <- ds$source$data[matches]
    #if (default(debug)) nu_data <<- data
    value <- rlang::eval_tidy(rlang::get_expr(parser$parse_quo))
    if (n_problems(value) > 0) {
      ds <- set_problems(ds, combined_problems(ds, value))
      value <- set_problems(value, NULL)
    }
    if (!is.list(value)) value <- list(value)
    if (parser$type == "data")
      ds[parser$id] <- value
    else 
      ds[[parser$type]][parser$id] <- value
  }
  
  return(ds)
}

# parse nu data
parse_nu_data <- function(data, n_blocks, n_channels, masses = c()) {

  # global vars
  group <- header <- is_ref <- is_sample <- is_zero <- block <- intensity <- background <- channel <- NULL
  
  if (is.null(n_blocks)) stop("number of blocks not known", call. = FALSE)
  if (is.null(n_channels)) stop("number of channels not known", call. = FALSE)
  zeros <- group_lines(data, fixed("Zero Data"))
  raw_data <- group_lines(data, fixed("Individual Data"))
  
  # sanity checks on block number
  if (nrow(raw_data) != n_blocks) {
    glue::glue("found {nrow(raw_data)} data blocks, expected {n_blocks}") %>% stop(call. = FALSE)
  }
  
  # read in zeros
  # Q: averages for zeros are stored in ZEROS_DATA block but individual aquisitions are stored in the first chunk of the first data block (the 'gas bla' part)
  
  # prepare raw data
  raw_data <- 
    raw_data %>% 
    select(block = group, data) %>% 
    # unpack the blocks
    mutate(
      n_channels = !!n_channels,
      n_cycles = map_int(data, ~str_subset(.x, fixed("No_C_O_Cycles")) %>% readr::parse_number() %>% as.integer()),
      cycle_length = map_int(data, ~str_subset(.x, fixed("Cycle_Length")) %>% readr::parse_number() %>% as.integer()),
      zero_length = map_int(data, ~str_subset(.x, fixed("Zero_Measurement_Length")) %>% readr::parse_number() %>% as.integer()),
      data = map(data, group_lines, "^\\s*Gas")
    ) %>% 
    # unpack the 'Gas...' data chunks
    unnest(data) %>% 
    mutate(
      is_ref = str_detect(header, "^\\s*Gas\\s+Ref"),
      is_sample = str_detect(header, "^\\s*Gas\\s+Sam"),
      is_zero = !is_ref & !is_sample & str_detect(header, "^\\s*Gas\\s+[^ ]+")
    ) 
  
  # process zero/background data
  zero_data <- exec_func_with_error_catch(parse_nu_zero_data, filter(raw_data, is_zero), masses)
  
  # process raw sample/standard data
  raw_data <- exec_func_with_error_catch(parse_nu_raw_data, filter(raw_data, is_ref | is_sample), masses)
  
  # check for problems (log & return empty dta frames)
  if (n_problems(zero_data) > 0 || n_problems(raw_data) > 0) {
    retval <- list(bgrd_data = tibble(), raw_data = tibble()) %>% 
      set_problems(combined_problems(zero_data, raw_data)) 
    return(retval)
  }
  
  # multiply zero block
  zero_blocks <- unique(zero_data$block)
  data_blocks <- unique(raw_data$block)
  if (length(zero_blocks) == 1) {
    # single zero block for all
    zero_data <- select(zero_data, -block) %>% 
      tidyr::crossing(select(raw_data, block) %>% unique) %>% 
      select(block, everything())
  } else if (!setequal(zero_blocks, data_blocks)) {
    glue::glue("found {length(zero_blocks)} zero blocks, expected {length(data_blocks)}") %>% 
      stop(call. = FALSE)
  }
  
  # subtract zeros from data
  raw_data <-
    raw_data %>% 
    left_join(rename(zero_data, background = intensity), by = c("block", "channel")) %>% 
    mutate(intensity = intensity - background) %>% 
    select(-background)

  # spread data
  zero_data <- tidyr::spread(zero_data, channel, intensity)
  raw_data <- tidyr::spread(raw_data, channel, intensity)
  
  # return
  return(list(bgrd_data = zero_data, raw_data = raw_data))
}

# parse background data
parse_nu_zero_data <- function(raw_data, masses = c()) {
  
  # globals
  block <- data <- n_channels <- zero_length <- NULL
  
  # process raw data for zeros
  df <- 
    raw_data %>% 
    group_by(block) %>% 
    mutate(data = map(data, ~tibble(channel = seq_along(.x), intensities = stringr::str_split(.x, "\\s+")))) %>% 
    select(block, n_channels, zero_length, data)

  # safety checks
  check_channels(df)
  
  # safety check on cycle length and first value
  df_channels <- unnest(df, data)
  check_cycle_length(df_channels, "zero_length")
  
  # calculate intensities
  df_intensities <- calculate_intensities(df_channels, c("block", "channel"), masses)
  
  return(df_intensities)
}

# parse raw data
parse_nu_raw_data <- function(raw_data, masses = c()) {
  
  # global vars
  block <- is_ref <- data <- n_channels <- n_cycles <- cycle_length <- cycle <- type <- NULL
  
  # process raw data
  df <- 
    raw_data %>% 
    # determine cycles
    group_by(block, is_ref) %>% 
    mutate(cycle = 1:n() - is_ref) %>% ungroup() %>% 
    # convert string data to numeric data
    mutate(
      type = ifelse(is_ref, "standard", "sample"),
      data = map(data, ~tibble(channel = seq_along(.x), intensities = stringr::str_split(.x, "\\s+")))
    ) %>% 
    select(block, n_channels, n_cycles, cycle_length, data, cycle, type)
  
  # safety check on cycles
  cycles_count <- 
    dplyr::count(df, block, n_cycles, type) %>% 
    mutate(
      n = n - (type == "standard"), # the inital REF cycle add 1 to the "standard"
      check = n == n_cycles) 
  if (!all(cycles_count$check)) {
    glue::glue(
      "found data for {str_c(unique(cycles_count$n), collapse = ', ')} cycles, ",
      "expected {cycles_count$n_cycles[1]}") %>% 
      stop(call. = FALSE)
  }
  
  # safety checks
  check_channels(df)
  
  # safety check on cycle length and first value
  df_channels <- unnest(df, data)
  check_cycle_length(df_channels, "cycle_length")
  
  # calculate intensities
  df_intensities <- calculate_intensities(df_channels, c("block", "cycle", "type", "channel"), masses)
  
  return(df_intensities)
}

# safety check on channels
check_channels <- function(df) {
  
  # global vars
  data <- channel_n <- n_channels <- NULL
  
  channels_count <- 
    mutate(df, channel_n = map_int(data, nrow)) %>%
    mutate(check = channel_n == n_channels)
  if (!all(channels_count$check)) {
    glue::glue(
      "found data for {str_c(unique(channels_count$channel_n), collapse = ', ')} channels, ",
      "expected {channels_count$n_channels[1]}") %>% 
      stop(call. = FALSE)
  }
}

# safety check on cycle length
check_cycle_length <- function(df_channels, length_column) {
  
  # global vars
  intensities <- intensities_n <- first_value_0 <- NULL
  
  intensities_check <- df_channels %>% 
    mutate(
      intensities_n = map_int(intensities, length),
      check = intensities_n == !!sym(length_column) + 1L,
      first_value_0 = map_lgl(intensities, ~.x[1] == "0.000000E+00")
    )
  
  # checks
  if (!all(intensities_check$check)) {
    glue::glue(
      "found {str_c(unique(intensities_check$intensities_n - 1L), collapse = ', ')} measurements, ",
      "expected {intensities_check[[length_column]][1]}") %>% 
      stop(call. = FALSE)
  }
  if (!all(intensities_check$first_value_0)) {
    wrong_first_value <- filter(intensities_check, !first_value_0)$intensities %>% map_chr(~.x[1]) %>% unique()
    glue::glue(
      "found {str_c(wrong_first_value, collapse = ', ')} as first value(s), ",
      "expected 0.000000E+00") %>% 
      stop(call. = FALSE)
  }
}

# calculate intensities
calculate_intensities <- function(df_channels, grouping, masses = c()) {

  # calculate raw data intensities
  df_intensities <- df_channels %>% 
    unnest(.data$intensities) %>% 
    mutate(intensities = as.numeric(.data$intensities)) %>% 
    group_by(!!!map(grouping, sym)) %>% 
    summarize(intensity = mean(.data$intensities[-1])) %>% 
    ungroup() 
  
  # convert channels to masses
  n_channels <- length(unique(df_intensities$channel))
  if (length(masses) == n_channels) {
    # got the right number
    masses <- 
      tibble(
        channel = 1L:n_channels,
        mass = paste0("i", unname(!!masses), ".A")
      )
    df_intensities <- df_intensities %>% 
      left_join(masses, by = "channel") %>% 
      select(-.data$channel) %>% 
      rename(channel = .data$mass)
    
  } else {
    # don't have the right number
    df_intensities <- df_intensities %>% 
      mutate(channel = sprintf("iC%d.A", .data$channel))
  }
  
  return(df_intensities)
}

# utility functions ======

# utility function to group text lines by the occurence of the grouping regular expression
group_lines <- function(lines, group_regexp) {
  # global vars
  group <- line <- start <- end <- NULL
  
  stopifnot(is(lines, "character"))
  tibble(
    lines = lines,
    line = seq_along(lines),
    group = cumsum(str_detect(lines, group_regexp))
  ) %>% 
    filter(group > 0) %>% 
    group_by(group) %>% 
    summarize(
      start = min(line),
      end = max(line),
      header = lines[1],
      data = if(start == end) list(character(0)) else list(lines[-1])
    )
}