# XML file utils =============

# maps nodes' children as text
map_xml_children <- function(nodes, select = NULL) {
  nodes %>% 
    map_df(function(node) {
      xml2::as_list(node) %>% 
        # if select is specific, only take the children specific
      { if(is.null(select)) . else .[select[select %in% names(.)]] } %>% 
        # map all as text ignoring everything that does not have exactly 1 value
        map_chr(function(x) if (length(x) == 1) x[[1]] else NA_character_) %>% 
        # convert to data frame
        as.list() %>% dplyr::as_tibble()
    })
}

# retrieve Identifier/Value pairs from 'container' type children of current node
xml_fetch_container_value <- function(xml, ids, container = "PersistedPropertyBagProperty") {
  sapply(ids, function(id) {
    xml %>% xml2::xml_find_all(str_c(".//", container, "[Identifier[.='", id, "']]")) %>% 
      xml2::xml_child("Value") %>% xml2::xml_text() %>% list()
  })
}

# XML iarc xml file processing ========

# process iarc info xml file
process_iarc_info_xml <- function(filepath) {
  info_xml <- xml2::read_xml(filepath, encoding = "UTF-8")
  info_version <- info_xml %>% xml2::xml_child("Version") %>% xml2::xml_text()
  
  # retrieve processing lists information
  processing_lists <- 
    info_xml %>% xml2::xml_child("ProcessingLists") %>% 
    xml2::xml_children() %>% 
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
  if (!default("quiet")) {
    sprintf("found %d processing list(s) in .iarc: '%s'", 
            nrow(processing_lists), 
            str_c("ProcessingList_", processing_lists$ProcessingListId, collapse = "', '")) %>% 
      log_message(prefix = "      ")
  }
  
  return(processing_lists)
}

# process iarc methods xml files
process_iarc_methods_xml <- function(filepaths) {
  
  if (length(filepaths) == 0) return(tibble())
  
  method_params <- 
    filepaths %>% 
    lapply(function(methods_file) {
      method_xml <- xml2::read_xml(methods_file, encoding = "UTF-8")
      # id
      method_id <- method_xml %>% xml2::xml_child("Id") %>% xml2::xml_text()
      # method parameters
      method_xml %>% 
        xml2::xml_find_all(".//SerialisedFlowParameter") %>% 
        map_xml_children() %>% 
        mutate(MethodId = method_id,
               MethodFile = basename(methods_file))
    }) %>% 
    bind_rows()
  
  # info
  if (!default("quiet")) {
    method_files <- method_params$MethodFile %>% unique()
    sprintf("found %d method(s) in .iarc: '%s'", 
            method_files %>% length(), 
            str_c(method_files, collapse = "', '")) %>% 
      log_message(prefix = "      ")
  }
  
  return(method_params)
}

# process iarc tasks xml files
process_iarc_tasks_xml <- function(filepaths, method_parameters) {
  
  # global vars
  Id <- NULL
  
  process_iarc_task_xml <- function(task_file) {
    # read file
    task_xml <- xml2::read_xml(task_file, encoding = "UTF-8")  
    
    # retrieve general task info
    task_info <- 
      c("GlobalIdentifier", "Name", "Id", 
        "AcquisitionStartDate", "AcquisitionEndDate", # not sure these are useful
        "CompletionState", "MethodId", "ProcessingListTypeIdentifier") %>% 
      sapply(function(child) task_xml %>% xml2::xml_child(child) %>% xml2::xml_text() %>% list())
    
    # retrieve task values based on methods information (if there is any)
    if (nrow(method_parameters) > 0) {
      task_values <-
        task_xml %>% 
        xml2::xml_find_all(".//SerialisableTaskValue") %>% 
        map_xml_children() %>% 
        # link with parameters defined in methods
        mutate(
          MethodId = task_info[["MethodId"]],
          GlobalIdentifier = task_info[["GlobalIdentifier"]]
        ) %>% 
        left_join(method_parameters, by = c("MethodId" = "MethodId", "ParameterIdentifier" = "Id"))
    } else {
      task_values <- tibble()
    }
    
    # @NOTE: TypeIdentifier in the method_parameters holds the data type but even for numbers it seems to always be "String", currently not processed further (i.e. not turned into a different data type)
    
    # retrieve task data (where the real information is recorded)
    task_data <- 
      task_xml %>% 
      xml2::xml_find_all(".//SerialisableDataSet") %>% 
      map_xml_children(
        select = c("Id", "AcquireDataStatus", "AcquireStartDate", "AcquireEndDate", "TypeIdentifier")) %>% 
      mutate(
        GlobalIdentifier = task_info[["GlobalIdentifier"]],
        DataFile = str_c(Id, ".hdf5")
      ) %>% 
      select(-Id)
    
    # prepare return
    Value <- NULL # global variables
    list(
      filename = basename(task_file),
      # combine task info with task values 
      info = 
        task_info %>% dplyr::as_tibble() %>% 
        {
          if (nrow(task_values) > 0) {
            left_join(., 
              # wide format for task values
              task_values %>% select("GlobalIdentifier", "DisplayName", "Value") %>% 
                group_by(!!sym("GlobalIdentifier"), !!sym("DisplayName")) %>% 
                summarize(Value = str_c(Value, collapse = ", ")) %>% # make sure multiple values are collapsed properly
                ungroup() %>% 
                spread("DisplayName", "Value"), 
              by = "GlobalIdentifier")
          } else .
        },
      # task data
      data_files = task_data
    )
  }
  
  # for all task files, run the processing function
  tasks <- filepaths %>% lapply(process_iarc_task_xml)
  
  if (!default("quiet")) {
    sprintf("found %d sample(s) in .iarc", length(tasks)) %>% 
      log_message(prefix = "      ")
  }
  
  # combine info and data_files across tasks
  return(tasks)
}

# process iarc tasks xml files
process_iarc_processing_xml <- function(processing_list_id, filepath) {
  if (!file.exists(filepath)) stop("invalid processing list file path: ", filepath, call. = FALSE)
  if (!default("quiet")) {
    sprintf("searching processing list '%s' for gas configurations...", basename(filepath)) %>% 
      log_message(prefix = "      ")
  }
  
  # global variables for NSE
  Label <- NumeratorBeamChannel <- numerator_mass <- DenominatorBeamChannel <- denominator_mass <- NULL
  
  # read file
  xml <- xml2::read_xml(filepath, encoding = "UTF-8")
  global_id <- xml %>% xml2::xml_child("DefinitionUniqueIdentifier") %>% xml2::xml_text()
  
  # safety check
  if (global_id != processing_list_id) {
    sprintf("mismatch between Info processing list ID ('%s') and processing list file id ('%s')",
            processing_list_id, global_id) %>% stop(call. = FALSE)
  }
  
  ## helper functions ##
  # find the species
  xml_find_species <- function(node) {
    # potentially useful(?): DetectionBeamChannel
    node %>% xml2::xml_child("SerialisedPropertyBagProperties") %>%
      xml_fetch_container_value("Species") %>% { .$Species }
  }
  
  # find the channel masses from the beam ratio definitions
  xml_find_channel_masses <- function(node) {
    # find the beam ratio definitions
    ratio_defs <-
      node %>% xml2::xml_child("SerialisedChildPropertyBags") %>% 
      xml2::xml_find_all(".//SerialisablePropertyBag[Identifier[.='{42D28191-A6E9-4B7B-8C3D-0F0037624F7D}']]") %>%  
      map(xml_fetch_container_value, c("NumeratorBeamChannel", "DenominatorBeamChannel", "Label")) %>%
      bind_rows() 
    if (nrow(ratio_defs) == 0) return (tibble(channel = character(), mass = character()))  
    
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
      node %>% xml2::xml_child("SerialisedPropertyBagProperties") %>%
      xml_fetch_container_value(c("ApplyH3CorrectionFactor", "H3CorrectionFactor"))
    if (!is.na(H3_factor$ApplyH3CorrectionFactor) && H3_factor$ApplyH3CorrectionFactor == "True") 
      return(as.numeric(H3_factor$H3CorrectionFactor))
    else return(NULL)
  }
  
  # process channel configurations
  species_config <- xml %>% 
    xml2::xml_find_all("//SerialisablePropertyBag[Identifier[.='10DC1602-5ED4-4D62-BAB0-2693E3FBC3AF']]") %>% 
    sapply(function(node) {
      species <- xml_find_species(node)
      if (is.null(species) || is.na(species)) # no species definition found
        return(list())
      
      config <- list(channels = xml_find_channel_masses(node))
      if (!is.null(H3_factor <- xml_find_H3_factor(node))) config$H3_factor <- H3_factor
      
      config %>% list() %>% rlang::set_names(species)
    })
  
  # info
  if (!default("quiet")) {
    sprintf("found configurations for '%s'", 
            species_config %>% names() %>% str_c(collapse = "', '")) %>% 
      log_message(prefix = "      ")
  }
  
  # debug
  if (default("debug")) {
    log_message("species configurations:\n", species_config, prefix = "DEBUG: ")
  }
  
  list(list(species = species_config))
}

