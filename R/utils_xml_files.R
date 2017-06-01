# XML file utils =============

# maps nodes' children as text
map_xml_children <- function(nodes, select = NULL) {
  nodes %>% 
    map_df(function(node) {
      as_list(node) %>% 
        # if select is specific, only take the children specific
      { if(is.null(select)) . else .[select[select %in% names(.)]] } %>% 
        # map all as text ignoring everything that does not have exactly 1 value
        map_chr(function(x) if (length(x) == 1) x[[1]] else NA_character_) %>% 
        # convert to data frame
        as.list() %>% as_data_frame()
    })
}

# retrieve Identifier/Value pairs from 'container' type children of current node
xml_fetch_container_value <- function(xml, ids, container = "PersistedPropertyBagProperty") {
  sapply(ids, function(id) {
    xml %>% xml_find_all(str_c(".//", container, "[Identifier[.='", id, "']]")) %>% 
      xml_child("Value") %>% xml_text() %>% list()
  })
}

# XML iarc xml file processing ========

# process iarc info xml file
process_iarc_info_xml <- function(filepath) {
  info_xml <- read_xml(filepath, encoding = "UTF-8")
  info_version <- info_xml %>% xml_child("Version") %>% xml_text()
  
  # retrieve processing lists information
  processing_lists <- 
    info_xml %>% xml_child("ProcessingLists") %>% 
    xml_children() %>% 
    map_xml_children() 
  
  # version safety check
  supported_versions <- c("2")
  if (!info_version %in% supported_versions) {
    processing_lists <- 
      register_warning(
        processing_lists,
        str_c("iarc info xml lists Version ", info_version, 
              " which has not been tested and may cause unexpected processing issues"))
  }
  
  # processing lists safety check
  if (nrow(processing_lists) == 0) {
    stop("no processing lists in info xml", call. = FALSE)
  }
  
  # information
  if (!setting("quiet")) {
    sprintf("      found %d processing list(s) in .iarc: '%s'", 
            nrow(processing_lists), 
            str_c("ProcessingList_", processing_lists$ProcessingListId, collapse = "', '")) %>% 
      message()
  }
  
  return(processing_lists)
}

# process iarc methods xml files
process_iarc_methods_xml <- function(filepaths) {
  
  method_params <- 
    filepaths %>% 
    lapply(function(methods_file) {
      method_xml <- read_xml(methods_file, encoding = "UTF-8")
      # id
      method_id <- method_xml %>% xml_child("Id") %>% xml_text()
      # method parameters
      method_xml %>% 
        xml_find_all(".//SerialisedFlowParameter") %>% 
        map_xml_children() %>% 
        mutate(MethodId = method_id,
               MethodFile = basename(methods_file))
    }) %>% 
    bind_rows()
  
  # info
  if (!setting("quiet")) {
    method_files <- method_params$MethodFile %>% unique()
    sprintf("      found %d method(s) in .iarc: '%s'", 
            method_files %>% length(), 
            str_c(method_files, collapse = "', '")) %>% 
      message()
  }
  
  return(method_params)
}

# process iarc tasks xml files
process_iarc_tasks_xml <- function(filepaths, method_parameters) {
  
  # global variables for NSE
  #ID <- GlobalIdentifier <- DisplayName <- Value <- NULL
  
  process_iarc_task_xml <- function(task_file) {
    # read file
    task_xml <- read_xml(task_file, encoding = "UTF-8")  
    
    # retrieve general task info
    task_info <- 
      c("GlobalIdentifier", "Name", "Id", 
        "AcquisitionStartDate", "AcquisitionEndDate", # not sure these are useful
        "CompletionState", "MethodId", "ProcessingListTypeIdentifier") %>% 
      sapply(function(child) task_xml %>% xml_child(child) %>% xml_text() %>% list())
    
    # retrieve task values based on methods information
    task_values <-
      task_xml %>% 
      xml_find_all(".//SerialisableTaskValue") %>% 
      map_xml_children() %>% 
      # link with parameters defined in methods
      mutate_(
        .dots = list(
          MethodId = ~task_info[["MethodId"]],
          GlobalIdentifier = ~task_info[["GlobalIdentifier"]]
        )) %>% 
      full_join(method_parameters, by = c("MethodId" = "MethodId", "ParameterIdentifier" = "Id"))
    
    # @NOTE: TypeIdentifier in the method_parameters holds the data type but even for numbers it seems to always be "String", currently not processed further (i.e. not turned into a different data type)
    
    # retrieve task data (where the real information is recorded)
    task_data <- 
      task_xml %>% 
      xml_find_all(".//SerialisableDataSet") %>% 
      map_xml_children(select = c("Id", "AcquireDataStatus", "AcquireStartDate", "AcquireEndDate", "TypeIdentifier")) %>% 
      mutate_(
        .dots = list(
          GlobalIdentifier = ~task_info[["GlobalIdentifier"]],
          DataFile = ~str_c(Id, ".hdf5")
        )) %>% 
      { .[names(.) != "Id"] }
    
    # return
    list(
      filename = basename(task_file),
      # combine task info with task values 
      info = 
        task_info %>% as_data_frame() %>% 
        left_join(
          # wide format for task values
          task_values %>% select_("GlobalIdentifier", "DisplayName", "Value") %>% 
            spread_("DisplayName", "Value"), 
          by = "GlobalIdentifier"),
      # task data
      data_files = task_data
    )
  }
  
  # for all task files, run the processing function
  tasks <- filepaths %>% lapply(process_iarc_task_xml)
  
  if (!setting("quiet")) {
    sprintf("      found %d sample(s) in .iarc", length(tasks)) %>% 
      message()
  }
  
  # combine info and data_files across tasks
  return(tasks)
}

# process iarc tasks xml files
process_iarc_processing_xml <- function(processing_list_id, filepath) {
  if (!file.exists(filepath)) stop("invalid processing list file path: ", filepath, call. = FALSE)
  if (!setting("quiet")) {
    sprintf("      searching processing list '%s' for gas configurations...", basename(filepath)) %>% 
      message()
  }
  
  # global variables for NSE
  Label <- NumeratorBeamChannel <- numerator_mass <- DenominatorBeamChannel <- denominator_mass <- NULL
  
  # read file
  xml <- read_xml(filepath, encoding = "UTF-8")
  global_id <- xml %>% xml_child("DefinitionUniqueIdentifier") %>% xml_text()
  
  # safety check
  if (global_id != processing_list_id) {
    sprintf("mismatch between Info processing list ID ('%s') and processing list file id ('%s')",
            processing_list_id, global_id, call. = FALSE)
  }
  
  ## helper functions ##
  # find the species
  xml_find_species <- function(node) {
    # potentially useful(?): DetectionBeamChannel
    node %>% xml_child("SerialisedPropertyBagProperties") %>%
      xml_fetch_container_value("Species") %>% { .$Species }
  }
  
  # find the channel masses from the beam ratio definitions
  xml_find_channel_masses <- function(node) {
    # find the beam ratio definitions
    ratio_defs <-
      node %>% xml_child("SerialisedChildPropertyBags") %>% 
      xml_find_all(".//SerialisablePropertyBag[Identifier[.='{42D28191-A6E9-4B7B-8C3D-0F0037624F7D}']]") %>%  
      map(xml_fetch_container_value, c("NumeratorBeamChannel", "DenominatorBeamChannel", "Label")) %>%
      bind_rows() 
    if (nrow(ratio_defs) == 0) return (data_frame(channel = character(), mass = character()))  
    
    # derive channel defintions
    channel_defs <-
      ratio_defs %>% 
      # find masses from label
      mutate(
        numerator_mass = str_match(Label, "^(\\d+)/") %>% {.[,2]},
        denominator_mass = str_match(Label, "/(\\d+)$") %>% {.[,2]}
      ) %>% 
      # channel to mass matches
      {
        bind_rows(
          select(., channel=NumeratorBeamChannel, mass=numerator_mass),
          select(., channel=DenominatorBeamChannel, mass=denominator_mass)
        )
      } %>% 
      unique()
    return(channel_defs)
  }
  
  # find the H3 factor
  xml_find_H3_factor <- function(node) {
    H3_factor <- 
      node %>% xml_child("SerialisedPropertyBagProperties") %>%
      xml_fetch_container_value(c("ApplyH3CorrectionFactor", "H3CorrectionFactor"))
    if (!is.na(H3_factor$ApplyH3CorrectionFactor) && H3_factor$ApplyH3CorrectionFactor == "True") 
      return(as.numeric(H3_factor$H3CorrectionFactor))
    else return(NULL)
  }
  
  # process channel configurations
  species_config <- xml %>% 
    xml_find_all("//SerialisablePropertyBag[Identifier[.='10DC1602-5ED4-4D62-BAB0-2693E3FBC3AF']]") %>% 
    sapply(function(node) {
      species <- xml_find_species(node)
      if (is.null(species) || is.na(species)) # no species definition found
        return(list())
      
      config <- list(channels = xml_find_channel_masses(node))
      if (!is.null(H3_factor <- xml_find_H3_factor(node))) config$H3_factor <- H3_factor
      
      config %>% list() %>% setNames(species)
    })
  
  # info
  if (!setting("quiet")) {
    sprintf("      found configurations for '%s'", 
            species_config %>% names() %>% str_c(collapse = "', '")) %>% 
      message()
  }
  
  # debug
  if (setting("debug")) {
    message("DEBUG: species configurations: ")
    print(species_config)
  }
  
  list(list(species = species_config))
}
