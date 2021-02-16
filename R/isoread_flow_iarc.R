# read ionos .iarc archives for their continuous flow data
# @param ds the iso_file data structure to fill
# @param custom reader options - none needed
iso_read_flow_iarc <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_file(ds) || !is(ds, "continuous_flow")) 
    stop("data structure must be a 'continuous_flow' iso_file", call. = FALSE)
  
  # check for availability of xml2
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "'xml2' package is required to read .iarc, please run: install.packages('xml2')",
      call. = FALSE
    )
    return(invisible(iso_files))
  }
  
  # check for availability of rhdf5
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop(
      "'rhdf5' package is required to read .iarc, please run: install.packages('BiocManager'); BiocManager::install('rhdf5')",
      call. = FALSE
    )
    return(invisible(iso_files))
  }
  
  # unzipping iarc archive ====
  folder_name <- ds$file_info$file_path %>% basename() %>% { str_replace(., fixed(get_file_ext(.)), "") }
  folder_path <- file.path(tempdir(), folder_name)
  if (!file.exists(folder_path)) {
    if (!default("quiet")) log_message("unpacking isoprime archive file...", prefix = "      ")
    unzip(get_ds_file_path(ds), exdir = folder_path)
  }
  
  # info file ====
  info_file <- list.files(folder_path, pattern = "^Info$", full.names = T)
  if (length(info_file) != 1) {
    stop("no Info xml file found in iarc", call. = FALSE)
  }
  processing_lists <- exec_func_with_error_catch(process_iarc_info_xml, info_file)
  col_check(c("DefinitionUniqueIdentifier"), processing_lists, 
            msg = "iarc Info processing list information insufficient")
  
  # methods files ====
  method_files <- list.files(folder_path, pattern = "^Method", full.names = T)
  if (length(method_files) == 0) {
    log_warning("found no Method xml file(s) in iarc, proceeding without method information")
  }
  method_params <- exec_func_with_error_catch(process_iarc_methods_xml, method_files) 
  
  # tasks files ====
  task_files <- list.files(folder_path, pattern = "^Task", full.names = T)
  if (length(task_files) == 0) {
    stop("no Task xml file(s) found in iarc", call. = FALSE)
  }
  tasks <- exec_func_with_error_catch(process_iarc_tasks_xml, task_files, method_params) 
  col_check(c("GlobalIdentifier", "Name", "Id", "ProcessingListTypeIdentifier"), 
            map(tasks, "info") %>% bind_rows(), msg = "iarc tasks' information insufficient")
  
  # processing lists / gas configuration ====
  all_processing_lists <- 
    tasks %>% map("info") %>% bind_rows() %>% 
    group_by(.data$ProcessingListTypeIdentifier) %>% 
    summarize(samples = n()) %>% 
    ungroup() %>% 
    full_join(processing_lists, by = c("ProcessingListTypeIdentifier" = "DefinitionUniqueIdentifier"))
  
  # safety check on processing lists (make sure all processing lists defined in tasks have a ProcessingListId)
  if (any(is.na(all_processing_lists$ProcessingListId))) {
    sprintf("mismatch between processing lists in tasks ('%s') and in iarc info ('%s')",
            all_processing_lists$ProcessingListTypeIdentifier[is.na(all_processing_lists$samples)] %>% str_c(collapse = "', '"),
            all_processing_lists$ProcessingListTypeIdentifier[is.na(all_processing_lists$ProcessingListId)] %>% str_c(collapse = "', '")
    ) %>% stop(call. = FALSE)
  }
  
  # get gas configurations
  used_processing_lists <- filter(all_processing_lists, !is.na(.data$samples))
  gas_configs <- mapply(
    process_iarc_processing_xml, 
    used_processing_lists$ProcessingListTypeIdentifier, 
    file.path(folder_path, str_c("ProcessingList_", used_processing_lists$ProcessingListId)))
  
  # read sample/task data ====
  iso_files <- exec_func_with_error_catch(process_iarc_samples, ds, tasks, gas_configs, folder_path)
  
  # propagate problems =====
  iarc_problems <- combined_problems(processing_lists, method_params, tasks, gas_configs)
  for (i in 1:length(iso_files)) {
    # add general iarc problems to individual files
    iso_files[[i]] <- set_problems(
      iso_files[[i]], bind_rows(iarc_problems, get_problems(iso_files[[i]])))
  }
  
  # turn into iso_files list and return
  return(iso_as_file_list(iso_files))
}

# process iarc samples
process_iarc_samples <- function(iso_file_template, tasks, gas_configs, folder_path) {
  # function to generate sample id
  generate_task_sample_id <- function(task) {
    str_c(task$info$Id, "_", task$info$Name)
  }
  
  # sort task to process in order 
  #FIXME: sorting no longer works because purrr removed the function
  #not sorting at all right now but revisit this
  #tasks <- tasks %>% sort_by(generate_task_sample_id)
  
  # loop through and process info and data
  sapply(tasks, function(task) {
    # prepare iso_file object
    iso_file <- iso_file_template %>% 
      # set file path parameters
      set_ds_file_path(
        file_root = iso_file_template$file_info$file_root,
        file_path = iso_file_template$file_info$file_path, 
        file_id = generate_task_sample_id(task), 
        file_subpath = task$filename)
    
    # processing info
    if (!default("quiet")) {
      sprintf("processing sample '%s' (IRMS data '%s')",
              generate_task_sample_id(task), 
              task$data_files %>% 
                dplyr::filter(!!sym("TypeIdentifier") == "Acquire") %>% 
                { .$DataFile } %>% 
                { if(length(.) > 0) str_c(., collapse = "', '") else "" }
              #task$info$GlobalIdentifier
              ) %>% 
        log_message(prefix = "      ")
    }
    
    # process task info
    if(iso_file$read_options$file_info)
      iso_file <- exec_func_with_error_catch(process_iarc_sample_info, iso_file, task)
    
    # process task data
    if (iso_file$read_option$raw_data)
      iso_file <- exec_func_with_error_catch(process_iarc_sample_data, iso_file, task, 
                                            gas_configs, folder_path)
      
    return(list(iso_file))
  })
}

# process iarc task info
# @param iso_file task
process_iarc_sample_info <- function(iso_file, task) {
  iso_file$file_info <- c(iso_file$file_info, as.list(task$info))
  if (!is.null(iso_file$file_info$AcquisitionStartDate)) {
    # use AcquisitionStartDate as file_dattime (OS = with fractional seconds)
    # Problem: what is the time-zone? assuming UTC for now
    iso_file$file_info$file_datetime <- parse_datetime(iso_file$file_info$AcquisitionStartDate, format = "%Y-%m-%dT%H:%M:%OS")
  }
  return(iso_file)
}

# process iarc task data
# @param temp_dir the temporary directory where the files are unzipped
# @param gas_configs the gas configurations
process_iarc_sample_data <- function(iso_file, task, gas_configs, folder_path) {
  
  # aquire = IRMS data
  irms_data <- task$data_files %>% dplyr::filter(!!sym("TypeIdentifier") == "Acquire") 
  if (nrow(irms_data) == 0) stop("no IRMS acquisitions associated with this sample", call. = FALSE)
  
  # check for gas configurations
  if (!task$info$ProcessingListTypeIdentifier %in% names(gas_configs)) 
    stop("no gas configurations specified for sample processing list ", task$info$ProcessingListTypeIdentifier, call. = FALSE)
  gas_config <- gas_configs[[task$info$ProcessingListTypeIdentifier]]
  
  # read data
  dt_format <- "%Y-%m-%dT%H:%M:%OS" # with fractional seconds - Q: what is the time-zone
  for (i in 1:nrow(irms_data)) {
    iso_file <- with(irms_data[i,], {
      filepath <- file.path(folder_path, DataFile)
      run_time.s <- difftime(parse_datetime(AcquireEndDate, format = dt_format), parse_datetime(AcquireStartDate, format = dt_format), units = "s") %>% as.numeric()
      read_irms_data_file(iso_file, filepath, gas_config, run_time.s, data_units = "nA", data_scaling = 1e-9)
    })
  }
  
  return(iso_file)
}

# read irms data file and convert the scan to column format tp, time.s, iXX.[data_units] based on gas configuration
# will also add H3 factor if part of the gas configuration
# @param iso_file
read_irms_data_file <- function(iso_file, filepath, gas_config, run_time.s, data_units = "nA", data_scaling = 1e-9) {
  if (!"DataSet" %in% rhdf5::h5ls(filepath)$name)
    stop("expected DataSet attribute not present in HDF5 data file", call. = FALSE)
  
  # attributes (NOTE: not sure what to do with the $Tuning information (usually not filled))
  dataset_attributes <- rhdf5::h5readAttributes(filepath, "DataSet")
  if (!dataset_attributes$Species %in% names(gas_config$species))
    stop("gas configuration for species ", dataset_attributes$Species, " not specified", call. = FALSE)
  config <- gas_config$species[[dataset_attributes$Species]]
  
  # read irms data and determine which beams are used
  irms_data <- rhdf5::h5read(filepath, "DataSet") %>% dplyr::as_tibble()
  rhdf5::H5close() # garbage collect
  
  if (!"Scan" %in% names(irms_data)) 
    stop("Scan column missing from data file ", basename(filepath), call. = FALSE)
  
  data_channels <- irms_data %>% names() %>% str_subset("^Beam")
  config_channels <- config$channels %>% filter(.data$channel %in% data_channels)
  
  # safety check for channels (if no channels defined in the config at all, we've got problems)
  if ( nrow(config_channels) == 0)
    stop("no channel information in gas configuration for ", data_channels %>% str_c(collapse = ", "), call. = FALSE)
  
  # proceed only with the channels that are config defined
  irms_data <- irms_data[c("Scan", config_channels$channel)]
  
  multiple <- config_channels %>% group_by(.data$channel) %>% summarize(n = n(), masses = str_c(.data$mass, collapse = ", "))
  if (any(multiple$n > 1)) {
    stop("cannot process beam channels, some channels assigned to more than one mass: ",
         multiple %>% filter(n > 1) %>% mutate(label = paste0(.data$channel, ": ", .data$masses)) %>%
         { .$label } %>% str_c(collapse = "; "), call. = FALSE)
  }
  
  # h3 factor
  if (!is.null(config$H3_factor))
    iso_file$file_info$H3_factor <- config$H3_factor
  
  # rename channels
  rename_dots <- config_channels %>% { rlang::set_names(.$channel, str_c("i", .$mass, ".", data_units)) }
  irms_data <- irms_data %>% dplyr::rename(!!!rename_dots)
  
  # scale currents
  scale_data <- function(x) x / data_scaling
  irms_data <- irms_data %>% mutate_at(vars(starts_with("i")), scale_data)
  
  # scale time
  dt <- run_time.s / nrow(irms_data)
  irms_data <- irms_data %>% 
    rename(tp = .data$Scan) %>% 
    mutate(tp = as.integer(.data$tp), time.s = dt * .data$tp) %>% 
    select(.data$tp, .data$time.s, everything())
  
  # store mass data
  if (nrow(iso_file$raw_data) > 0) {
    existing <- iso_file$raw_data %>% select(starts_with("i")) %>% names()
    if ( any(dups <- existing %in% names(irms_data)) )
      stop("same ions reported in multiple data files, cannot reconcile duplicate data: ", 
           str_c(existing[dups], collapse = ", "), call. = FALSE)
  } else {
    iso_file$raw_data <- irms_data
  }
  
  return(iso_file)
}
