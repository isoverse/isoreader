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
    ds <- exec_func_with_error_catch(extract_isodat_measurement_info, ds)
    ds <- exec_func_with_error_catch(extract_isodat_sequence_line_info, ds)
  }
  
  # process raw data
  if (ds$read_option$raw_data)
    ds <- exec_func_with_error_catch(extract_did_raw_voltage_data, ds)
  
  # process pre-evaluated data table
  if (ds$read_options$vendor_data_table)
    ds <- exec_func_with_error_catch(extract_vendor_data_table, ds)
  
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
  
  SeqLineInfo <- fetch_keys(ds$binary, "Sequence Line Information", occurence = 1, fixed = TRUE, require = 1)
  VisInfo <- fetch_keys(ds$binary, "Visualisation Informations", occurence = 1, fixed = TRUE, require = 1)
  
  # parse sequence lin info
  rawtable <- ds$binary$raw[SeqLineInfo$byte_end:VisInfo$byte_start]
  if (length(rawtable) < 10)
    stop("this file does not seem to have a data block for the sequence line information", call. = FALSE)
  
  dividers <- grepRaw("\xff\xfe\xff", rawtable, all=TRUE)
  if (length(dividers) == 0) 
    stop("this file does not seem to have the expected hex code sequence FF FE FF as dividers in the sequence line information", call. = FALSE)
  
  for (i in 2:length(dividers)) {
    x <- get_unicode(rawtable[(dividers[i-1]+4):dividers[i]])
    if (i %% 2 == 1) ds$file_info[[x]] <- value # store key / value pair in data list
    else value <- x # keep value for key (which comes AFTER its value)
  }
  
  return(ds)
}

# extracts the measurement information for isodat files
extract_isodat_measurement_info = function(ds) {
  
  CMeasurmentInfos <- fetch_keys(ds$binary, "CMeasurmentInfos", occurence = 1, fixed = TRUE, require = 1)
  CMeasurmentErrors <- fetch_keys(ds$binary, "CMeasurmentErrors", occurence = 1, fixed = TRUE, require = 1)
  
  # parse measurement info table
  rawtable <- ds$binary$raw[CMeasurmentInfos$byte_end:CMeasurmentErrors$byte_start]
  dividers <- c(grepRaw("\xff\xfe\xff", rawtable, all=TRUE), length(rawtable))
  if (length(dividers) == 0) 
     stop("this file does not seem to have the expected hex code sequence FF FE FF as dividers in the grid info", call. = FALSE)
  
  # go through info table
  value <- NA_character_
  for (i in 2:length(dividers)) {
    x <- get_unicode(rawtable[(dividers[i-1]+4):dividers[i]])
    if (x == "CUserInfo") {
      id <- paste0("Info_", sub("^(\\w+).*$", "\\1", value))
      if (!is.null(ds$file_info[[id]]))
        ds$file_info[[id]] <- c(ds$file_info[[id]], value) # append value with first word as ID
      else
        ds$file_info[[id]] <- value # store new value with first word as ID
    }
    else value <- x # keep value
  }
  
  return(ds)
}

# extract voltage data in did file
extract_did_raw_voltage_data <- function(ds) {
  # find masses
  CTraceInfo <- fetch_keys(ds$binary, "CTraceInfo", occurence = 1, fixed = TRUE, require = 1)
  CPlotRange <- fetch_keys(ds$binary, "CPlotRange", occurence = 1, fixed = TRUE, require = 1)
  masses <- fetch_keys(
    ds$binary, "Mass \\d+", byte_min = CTraceInfo$byte_end, byte_max = CPlotRange$byte_start, 
    require = "1+", error_prefix = "cannot find mass names") %>% 
    mutate(
      # column names
      column = str_replace(value, "Mass (\\d+)", "v\\1.mV")
    )
  
  # read voltage data
  read_voltage_data <- function(byte_start, byte_end) {
    ds$binary <- move_to_pos(ds$binary, byte_end + 1L)
    
    # intensity block skip
    has_intensity_block <- 
      fetch_keys(ds$binary, "CIntensityData", fixed = TRUE, byte_min = byte_start, byte_max = byte_end + 64L) %>% { nrow(.) > 0}
    ds$binary <- skip_pos(ds$binary, if (has_intensity_block) 82 else 64)
    
    # retrieve data
    parse_binary_data(ds$binary, "double", length = nrow(masses), 
                      sensible = c(-1000, 100000), error_prefix = "cannot extract voltages") %>% 
      as.list() %>% 
      setNames(masses$column) %>% 
      as_data_frame()
  }
  
  # extract cycle information (Standard vs. Sample) and retrieve raw data
  CDualInletRawData <- fetch_keys(ds$binary, "CDualInletRawData", occurence = 1, fixed = TRUE, require = 1)
  CTwoDoublesArrayData <- fetch_keys(ds$binary, "CTwoDoublesArrayData", occurence = 1, fixed = TRUE, require = 1)
  ds$raw_data <- fetch_keys(
    ds$binary, "^(Standard|Sample) \\d+$", byte_min = CDualInletRawData$byte_end, byte_max = CTwoDoublesArrayData$byte_start, 
    require = "1+", error_prefix = "cannot find measurement type") %>% 
    mutate(
      type = str_match(value, "^(Standard|Sample) (\\d+)$") %>% {str_to_lower(.[,2])},
      cycle.0idx = str_match(value, "^(Standard|Sample) (\\d+)$") %>% {.[,3]}, # 0 based index, adjust in next line
      cycle = ifelse(cycle.0idx == "Pre", 0, suppressWarnings(as.integer(cycle.0idx)) + 1L)
    ) %>% 
    # read volutage data for each analysis and cycle
    group_by(type, cycle) %>% 
    do({
      read_voltage_data(.$byte_start, .$byte_end)
    }) %>% 
    ungroup()
  return(ds)
}
