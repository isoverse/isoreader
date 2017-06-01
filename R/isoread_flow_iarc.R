# read ionos .iarc archieves for their continuous flow data
# @param ds the isofile data structure to fill
isoread_flow_iarc <- function(ds) {
  
  if(!is(ds, "isofile") || !is(ds, "continuous_flow")) 
    stop("data structure must have class 'isofile' and 'continuous_flow'", call. = FALSE)
  col_check(c("file_info", "mass_data"), ds)
  col_check(c("file_id", "file_path", "file_subpath"), ds$file_info)
  
  # global variables for NSE
  sample <- NULL
  
  # unzipping iarc archieve ====
  folder_name <- ds$file_info$file_path %>% basename() %>% { str_replace(., fixed(get_file_ext(.)), "") }
  folder_path <- file.path(tempdir(), folder_name)
  if (!file.exists(folder_path)) {
    if (!setting("quiet")) message("      unpacking isoprime archieve file...")
    unzip(ds$file_info$file_path, exdir = folder_path)
  }
  
  # info file ====
  info_file <- list.files(folder_path, pattern = "^Info$", full.names = T)
  if (length(info_file) != 1) {
    stop("no Info xml file found in iarc", call. = FALSE)
  }
  processing_lists <- process_iarc_info_xml(info_file) 
  col_check(c("DefinitionUniqueIdentifier"), processing_lists, 
            msg = "iarc Info processing list information insufficient")
  
  # methods files ====
  method_files <- list.files(folder_path, pattern = "^Method", full.names = T)
  if (length(method_files) == 0) {
    stop("no Method xml file(s) found in iarc", call. = FALSE)
  }
  method_params <- process_iarc_methods_xml(method_files)
  
  # tasks files ====
  task_files <- list.files(folder_path, pattern = "^Task", full.names = T)
  if (length(task_files) == 0) {
    stop("no Task xml file(s) found in iarc", call. = FALSE)
  }
  tasks <- process_iarc_tasks_xml(task_files, method_params) 
  col_check(c("GlobalIdentifier", "Name", "Id", "ProcessingListTypeIdentifier"), 
            map(tasks, "info") %>% bind_rows(), msg = "iarc tasks' information insufficient")
  
  # processing lists / gas configuration ====
  all_processing_lists <- 
    tasks %>% map("info") %>% bind_rows() %>% 
    group_by_(.dots = "ProcessingListTypeIdentifier") %>% 
    summarise(samples=n()) %>% 
    full_join(processing_lists, by = c("ProcessingListTypeIdentifier" = "DefinitionUniqueIdentifier"))
  
  # safety check on processing lists (make sure all processing lists defined in tasks have a ProcessingListId)
  if (any(is.na(all_processing_lists$ProcessingListId))) {
    sprintf("mismatch between processing lists in tasks ('%s') and in iarc info ('%s')",
            with(all_processing_lists, ProcessingListTypeIdentifier[is.na(samples)]) %>% str_c(collapse = "', '"),
            with(all_processing_lists, ProcessingListTypeIdentifier[is.na(ProcessingListId)]) %>% str_c(collapse = "', '")) %>% 
      stop(call. = FALSE)
  }
  
  # get gas configurations
  used_processing_lists <- filter(all_processing_lists, !is.na(samples))
  gas_configs <- mapply(
    process_iarc_processing_xml, 
    used_processing_lists$ProcessingListTypeIdentifier, 
    file.path(folder_path, str_c("ProcessingList_", used_processing_lists$ProcessingListId)))
  
  # read sample/task data ====
  isofiles <- process_iarc_samples(ds, tasks, gas_configs, folder_path)
  class(isofiles) <- c("isofiles", class(isofiles))
  
  # propagate problems =====
  iarc_problems <- combined_problems(
    processing_lists, method_params, tasks, gas_configs)
  
  # compile all problems 
  all_problems <- data_frame()
  for (i in 1:length(isofiles)) {
    isofile_problems <- get_problems(isofiles[[i]]) %>% 
      mutate(file_id = isofiles[[i]]$file_info$file_id) 
    all_problems <- bind_rows(all_problems, isofile_problems)
    # add general iarc problems to individual files
    isofiles[[i]] <- set_problems(
      isofiles[[i]], bind_rows(iarc_problems, get_problems(isofiles[[i]])))
  }
  
  all_problems <- bind_rows(
    mutate(iarc_problems, file_id = basename(ds$file_info$file_path)),
    all_problems
  )
  
  # final isofiles
  isofiles %>% set_problems(all_problems)
}

# process iarc samples
process_iarc_samples <- function(isofile_template, tasks, gas_configs, folder_path) {
  # function to generate sample id
  generate_task_sample_id <- function(task) {
    str_c(task$info$Id, "_", task$info$Name)
  }
  
  # sort task to process in order
  tasks <- tasks %>% sort_by(generate_task_sample_id)
  
  # loop through and process info and data
  sapply(tasks, function(task) {
    # prepare isofile object
    isofile <- isofile_template %>% 
      # set file path parameters
      set_ds_file_path(
        file_path = isofile_template$file_info$file_path, 
        file_id = generate_task_sample_id(task), 
        file_subpath = task$filename)
    
    # processing info
    if (!setting("quiet")) {
      sprintf("      processing sample '%s' (IRMS data %s)",
              generate_task_sample_id(task), 
              task$data_files %>% 
                filter_(.dots = list(~TypeIdentifier == "Acquire")) %>% 
                {.$DataFile} %>% { if(length(.) > 0) str_c(., collapse = "', '") else "" }
              #task$info$GlobalIdentifier
              ) %>% 
        message()
    }
    
    # process task info
    isofile <- exec_func_with_error_catch(process_iarc_sample_info, isofile, task)
    
    # process task data
    isofile <- exec_func_with_error_catch(process_iarc_sample_data, isofile, task, 
                                          gas_configs, folder_path)
    
    return(list(isofile) %>% setNames(isofile$file_info$file_id))
  })
}

# process iarc task info
# @param isofile task
process_iarc_sample_info <- function(isofile, task) {
  isofile$file_info <- c(isofile$file_info, as.list(task$info))
  return(isofile)
}

# process iarc task data
# @param temp_dir the temporary directory where the files are unzipped
# @param gas_configs the gas configurations
process_iarc_sample_data <- function(isofile, task, gas_configs, folder_path) {
  # aquire = IRMS data
  irms_data <- task$data_files %>% filter_(.dots = list(~TypeIdentifier == "Acquire")) 
  if (nrow(irms_data) == 0) stop("no IRMS acquisitions associated with this sample", call. = FALSE)
  
  # check for gas configurations
  if (!task$info$ProcessingListTypeIdentifier %in% names(gas_configs)) 
    stop("no gas configurations specified for sample processing list ", task$info$ProcessingListTypeIdentifier, call. = FALSE)
  gas_config <- gas_configs[[task$info$ProcessingListTypeIdentifier]]
  
  # read data
  dt_format <- "%Y-%m-%dT%H:%M:%OS" # with fractional seconds
  for (i in 1:nrow(irms_data)) {
    isofile <- with(irms_data[i,], {
      filepath <- file.path(folder_path, DataFile)
      run_time.s <- interval(strptime(AcquireStartDate, dt_format), strptime(AcquireEndDate, dt_format) ) / duration(1, "s")
      read_irms_data_file(isofile, filepath, gas_config, run_time.s, data_units = "nA")
    })
  }
  
  return(isofile)
}

# read irms data file and convert the scan to column format tp, time.s, iXX.[data_units] based on gas configuration
# will also add H3 factor if part of the gas configuration
# @param isofile
read_irms_data_file <- function(isofile, filepath, gas_config, run_time.s, data_units = "nA") {
  if (!"DataSet" %in% h5ls(filepath)$name)
    stop("expected DataSet attribute not present in HDF5 data file", call. = FALSE)
  
  # global variables to allow NSE
  channel <- mass <- masses <- Scan <- tp <- time.s <- NULL
  
  # attributes (NOTE: not sure what to do with the $Tuning information (usually not filled))
  dataset_attributes <- h5readAttributes(filepath, "DataSet")
  if (!dataset_attributes$Species %in% names(gas_config$species))
    stop("gas configuration for species ", dataset_attributes$Species, " not specified", call. = FALSE)
  config <- gas_config$species[[dataset_attributes$Species]]
  
  # read irms data and determine which beams are used
  irms_data <- h5read(filepath, "DataSet") %>% as_data_frame()
  H5close() # garbage collect
  data_channels <- irms_data %>% names() %>% str_subset("^Beam")
  config_channels <- config$channels %>% filter(channel %in% data_channels)
  
  # safety check for channels
  if ( length(missing <- setdiff(data_channels, config_channels$channel)) > 0)
    stop("no channel information in gas configuration for ", data_channels %>% str_c(collapse = ", "), call. = FALSE)
  
  multiple <- config_channels %>% group_by(channel) %>% summarize(n = n(), masses = str_c(mass, collapse = ", "))
  if (any(multiple$n > 1)) {
    stop("cannot process beam channels, some channels assigned to more than one mass: ",
         multiple %>% filter(n > 1) %>% mutate(label = str_c(channel, ": ", masses)) %>%
         { .$label } %>% str_c(collapse = "; "), call. = FALSE)
  }
  
  # h3 factor
  if (!is.null(config$H3_factor))
    isofile$file_info$H3_factor <- config$H3_factor
  
  # rename channels
  rename_dots <- config_channels %>% { setNames(.$channel, str_c("i", .$mass, ".", data_units)) }
  irms_data <- irms_data %>% rename_(.dots = rename_dots)
  
  # scale currents
  data_scaling <- 1/get_si_prefix_scaling(data_units, "A")
  scale_data <- function(x) x*data_scaling
  irms_data <- irms_data %>% mutate_at(vars(starts_with("i")), scale_data)
  
  # scale time
  dt <- run_time.s / nrow(irms_data)
  irms_data <- irms_data %>% 
    rename(tp = Scan) %>% 
    mutate(time.s = dt * tp) %>% 
    select(tp, time.s, everything())
  
  # store mass data
  if (nrow(isofile$mass_data) > 0) {
    existing <- isofile$mass_data %>% select(starts_with("i")) %>% names()
    if ( any(dups <- existing %in% names(irms_data)) )
      stop("same ions reported in multiple data files, cannot reconcile duplicate data: ", 
           str_c(existing[dups], collapse = ", "), call. = FALSE)
  } else {
    isofile$mass_data <- irms_data
  }
  
  return(isofile)
}
