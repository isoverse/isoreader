# File info and data retrieval functions 
# @note: should make the aggregation functions type safe?
# @note: need an "iso_get_data()" funtion that nests everything together
# @note: simplify the get_file_id/path/subpath/datetime function or cut them entirely? it's all possible with the select functions anyways...

# Specific file info calls ======

# Get file information
# 
# Retrieve basic file information form an individual isotope file (iso_file) object. All of these can also be recoverd for an entire set of files using \code{\link{iso_get_file_info}} and specifiying which info to recover, for example, \code{include = c("file_id", "file_path", "file_datetime")}
# 
# @details \code{get_file_id()}: retrieve the file ID (this is typially the file name)
# @param iso_file an iso_file to retrieve basic file information from
# @rdname file_info
# @aliases get_file_info
# @family data retrieval functions
# @export
get_file_id <- function(iso_file) {
  check_iso_file_param(iso_file)
  return(iso_file$file_info$file_id)
}

# @details \code{get_file_path()}: retrieve the file path (this is the path to the file in case of single file formats such as .dxf or .did and the path to the archieve file in case of collective file formats such as .iarc)
# @rdname file_info
get_file_path <- function(iso_file) {
  check_iso_file_param(iso_file)
  return(iso_file$file_info$file_path)
}

# @details \code{get_file_subpath()}: retrieve the file subpath (this only exists for collective file formats such as .iarc and is the name of the metadata file inside the .iarc archive). Returns NA for iso_file without subpath.
# @rdname file_info
get_file_subpath <- function(iso_file) {
  check_iso_file_param(iso_file)
  return(iso_file$file_info$file_subpath)
}

# @details \code{get_file_datetime()}: retrieve the run date and time in \code{\link[base]{POSIXct}} format
# @rdname file_info
get_file_datetime <- function(iso_file) {
  check_iso_file_param(iso_file)
  return(iso_file$file_info$file_datetime)
}

# internal convenience function
check_iso_file_param <- function(iso_file) {
  if(missing(iso_file)) stop("no iso_file provided to retrieve file information from", call. = FALSE)
  if(!iso_is_file(iso_file)) stop("can only retrieve file information from an iso_file object, not from '", class(iso_file)[1], "'", call. = FALSE)
}


# Data summary information =====

#' Get data summary
#' 
#' Summarize the data information from one or multiple iso files. 
#' @inheritParams iso_read_files
#' @param iso_files single iso file or collection of iso_file objects
#' @return a \code{\link[tibble]{data_frame}} that summarizes the data in the \code{iso_files}
#' @export
iso_get_data_summary <- function(iso_files, quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  if (!quiet) {
    glue("Info: aggregating data summary from {length(iso_files)} data file(s)") %>% 
      message()
  }
  
  # @note speed this up by vectorizing the get info functions more efficiently
  lapply(iso_files, function(iso_file) {
    data_frame(file_id = iso_file$file_info$file_id,
               raw_data = get_raw_data_info(iso_file),
               file_info = get_file_info_info(iso_file),
               method_info = get_method_info_info(iso_file),
               vendor_data_table = get_vendor_data_table_info(iso_file),
               file_path = sprintf("%s%s", iso_file$file_info$file_path, 
                                   iso_file$file_info$file_subpath %>% { if(!is.na(.)) str_c("|", .) else "" })
    )
  }) %>% bind_rows()
}

# summary of raw data info
get_raw_data_info <- function(x) {
  stopifnot(iso_is_file(x))
  if (x$read_options$raw_data) {
    cols <- names(x$raw_data) %>% str_subset("^[iIvV](\\d+)\\.") 
    if (length(cols) == 0) return("no ions")
    cols <- cols %>% str_match("^[iIvV](\\d+)\\.") %>% {.[,2] } %>% sort()
    rows <- 
      if (iso_is_dual_inlet(x)) glue("{floor(nrow(x$raw_data)/2)} cycles")
      else if (iso_is_continuous_flow(x)) glue("{nrow(x$raw_data)} time points")
      else glue("{nrow(x$raw_data} rows")
    glue("{rows}, {length(cols)} ions ({collapse(cols, ',')})") %>% 
      as.character()
  } else {
    "raw data not read"
  }
}

# summary of file info
get_file_info_info <- function(x) {
  stopifnot(iso_is_file(x))
  if (x$read_options$file_info) {
    glue("{length(x$file_info)} entries") %>% 
      as.character()
  } else {
    "file info not read"
  }
}

# summary of method info
# @note: this needs manual update depending on method information (to keep things compact in summary)
get_method_info_info <- function(x) {
  stopifnot(iso_is_file(x))
  if (x$read_options$method_info) {
    method_info <- c()
    if (!is.null(x$method_info$standards)) method_info <- c(method_info, "standards")
    if (!is.null(x$method_info$resistors)) method_info <- c(method_info, "resistors")
    if (!is_empty(method_info)) 
      glue("{collapse(method_info, ', ')}") %>% as.character()
    else
      "no method info"
  } else {
    "method info not read"
  }
}

# summary of vendor data table
get_vendor_data_table_info <- function(x) {
  stopifnot(iso_is_file(x))
  if (x$read_options$vendor_data_table) {
    if (nrow(x$vendor_data_table) > 0 || ncol(x$vendor_data_table) > 0)
      glue("{nrow(x$vendor_data_table)} rows, {ncol(x$vendor_data_table)} columns") %>% as.character()
    else
      "no vendor data table"
  } else {
    "vendor data table not read"
  }
}


# Specific data aggregation calls =====

#' Aggregate file info
#'
#' Combine file information from multiple iso_files. By default all information is included but specific items can be specified using the \code{include} parameter. The file id is always included. File information beyond \code{file_id} and \code{file_path} is only available if the iso_files were read with parameter \code{read_file_info=TRUE}.
#'
#' @inheritParams iso_get_raw_data
#' @param select which columns to select - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax. Includes all columns by default. File id is always included no matter what selection parameters. 
#' @family data retrieval functions
#' @note File info entries with multiple values are concatenated for this aggregation function. To get access to a specific multi-value file info entry, access using \code{iso_file$file_info[['INFO_NAME']]} on the iso_file object directly.
#' @export
iso_get_file_info <- function(iso_files, select = everything(), quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  if (!quiet) sprintf("Info: aggregating file info from %d data file(s)", length(iso_files)) %>% message()
  check_read_options(iso_files, "file_info")
  
  # retrieve info
  file_info <- lapply(iso_files, function(iso_file) {
    lapply(iso_file$file_info, function(entry) {
      if (length(entry) > 1) str_c(entry, collapse = "; ") else entry
    })  %>% as_data_frame()
  }) %>% bind_rows()
  
    # safety check (probably not necessary because of iso_file combination checks but consequences would be too problematic not to check)
  if (any(duplicated(file_info$file_id))) {
    stop("duplicate file ids are not permitted as they can lead to unexpected consequences in data processing", call. = FALSE)
  }
  
  # get include information
  select_cols <- get_column_names(file_info, select = enquo(select), n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  
  return(file_info[select_cols])
}

#' Aggregate raw data
#' 
#' Aggregate the raw ion data from the provided iso_files. Can aggregate either in a wide table (for easy overview) or a gathered long table (for plotting and further data processing). The raw data is only available if the iso_files were read with parameter \code{read_raw_data=TRUE}.
#' 
#' @inheritParams iso_read_files
#' @param iso_files collection of iso_file objects
#' @param gather whether to gather data into long format after aggregation (e.g. for plotting)
#' @param include_file_info if provided, will include the requested file information (see \code{\link{iso_get_file_info}}) with the raw data. 
#' Use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax.
#' @family data retrieval functions
#' @export
iso_get_raw_data <- function(iso_files, gather = FALSE, include_file_info = NULL, quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) { 
    sprintf("Info: aggregating raw data from %d data file(s)%s", length(iso_files),
            get_info_message_concat(include_file_info_quo, prefix = ", including file info ")) %>% message()
  }
  check_read_options(iso_files, "raw_data")
  
  data <- 
    lapply(iso_files, function(iso_file) {
      data <- as_data_frame(iso_file$raw_data) %>% 
        mutate(file_id = iso_file$file_info$file_id) %>% 
        select(file_id, everything())
      return(data)
    }) %>% bind_rows()
  
  # check for rows
  if (nrow(data) == 0) return(data)
  
  # if gathering
  if (gather) {
    column <- value <- extra_parens <- category <- NULL # global vars
    masses_ratios_re <- "^([vir])(\\d+/?\\d*)(\\.(.+))?$"
    data <- data %>% 
      # gather all masses and ratios
      gather(column, value, matches(masses_ratios_re)) %>% 
      # extract unit information
      extract(column, into = c("category", "data", "extra_parens", "units"), regex = masses_ratios_re) %>% 
      select(-extra_parens) %>% 
      # remove unknown data
      filter(!is.na(value)) %>% 
      # assign category
      mutate(category = ifelse(category == "r", "ratio", "mass"))
  } 
  
  # if file info
  if (!quo_is_null(include_file_info_quo)) {
    info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  return(data)
}


#' Aggregate standards from methods info
#'
#' Aggregates the isotopic standard information recovered from the provided iso_files. Can aggregate just the standards' delta values or combine the delta values with the recovered ratios (if any). Use paramter \code{with_ratios} to exclude/include the ratios. This information is only available if the iso_files were read with parameter \code{read_method_info=TRUE}.
#'
#' @inheritParams iso_get_raw_data
#' @param with_ratios whether to include ratios or just standard delta values
#' @family data retrieval functions
#' @export
iso_get_standards_info <- function(iso_files, with_ratios = FALSE, include_file_info = NULL, quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) { 
    sprintf("Info: aggregating standards info from %d data file(s)%s", length(iso_files),
            get_info_message_concat(include_file_info_quo, prefix = ", including file info ")) %>% message()
  }
  
  check_read_options(iso_files, "method_info")
  
  # aggregate standards info
  data <- lapply(iso_files, function(iso_file) {
    if(with_ratios) {
      stds <- left_join(
        iso_file$method_info$standards,
        iso_file$method_info$reference_ratios,
        by = "reference")
    } else {
      stds <- iso_file$method_info$standards
    }
    
    # check if there is any data
    if(is.null(stds) || nrow(stds) == 0) return(data_frame())
    
    # return with file_id included
    stds %>% 
      mutate(file_id = iso_file$file_info$file_id) %>% 
      select(file_id, everything())
  }) %>% bind_rows()
  
  # if file info
  if (!quo_is_null(include_file_info_quo)) {
    info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  return(data)
}

#' Aggregate resistors from methods info
#'
#' Aggregates the resistor information recovered from the provided iso_files. This information is only available if the iso_files were read with parameter \code{read_method_info=TRUE} and only linked to specific masses if the iso_files were additionally read with parametr \code{read_raw_data=TRUE}.
#'
#' @inheritParams iso_get_raw_data
#' @family data retrieval functions
#' @export
iso_get_resistors_info  <- function(iso_files, include_file_info = NULL, quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) { 
    sprintf("Info: aggregating resistors info from %d data file(s)%s", length(iso_files),
            get_info_message_concat(include_file_info_quo, prefix = ", including file info ")) %>% message()
  }
  
  check_read_options(iso_files, "method_info")
  
  # aggregate standards info
  data <- lapply(iso_files, function(iso_file) {
    Rs <- iso_file$method_info$resistors
    
    # check if there is any data
    if(is.null(Rs) || nrow(Rs) == 0) return(data_frame())
    
    # return with file_id included
    Rs %>% 
      mutate(file_id = iso_file$file_info$file_id) %>% 
      select(file_id, everything())
  }) %>% bind_rows()
  
  # if file info
  if (!quo_is_null(include_file_info_quo)) {
    info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  return(data)
}

#' Aggregate vendor computed table data
#' 
#' Aggregate data from the vendor-computed data table. This information is only available if the iso_files were read with parameter \code{read_vendor_data_table=TRUE}.
#' 
#' @inheritParams iso_get_raw_data
#' @inheritParams iso_get_file_info
#' @param with_units whether to include units in the column headers (if there are any) or not (default is FALSE)
#' @family data retrieval functions
#' @export
iso_get_vendor_data_table <- function(iso_files, with_units = FALSE, select = everything(), include_file_info = NULL, 
                                        quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) { 
    sprintf("Info: aggregating vendor data table %s from %d data file(s)%s", 
            if (with_units) "with units" else "without units",
            length(iso_files),
            get_info_message_concat(include_file_info_quo, prefix = ", including file info ")) %>% message()
  }
  check_read_options(iso_files, "vendor_data_table")
  
  # check for missing with units
  if (with_units && (no_units <- sum(sapply(iso_files, function(iso_file) is.null(attr(iso_file$vendor_data_table, "units"))))) > 0) {
    sprintf("%d/%d files do not have unit information for their vendor data table and will have missing units",
            no_units, length(iso_files)) %>% warning(call. = FALSE, immediate. = TRUE)
  }
  
  # get vendor data
  column <- units <- NULL # global vars
  vendor_data_table <- lapply(iso_files, function(iso_file) {
    df <- iso_file$vendor_data_table
    
    # see if there is any data at all
    if (nrow(df) == 0) return(df)
    
    # use units 
    if (with_units && !is.null(attr(df, "units")) && !is.na(attr(df, "units")))  {
      cols_with_units <- attr(df, "units")[c("column", "units")] %>% 
        mutate(units = ifelse(!is.na(units) & nchar(units) > 0, str_c(column, " ", units), column)) %>% 
        deframe()
      names(df) <- cols_with_units[names(df)]
    }
    
    # include file id
    df %>% 
      mutate(file_id = iso_file$file_info$file_id) %>% 
      dplyr::select(file_id, everything())
  }) %>% bind_rows()
  
  # check for any rows
  if (nrow(vendor_data_table) == 0) return(vendor_data_table)
  
  # get select information
  # ASDFSD=====
  
  # get include information
  select_cols <- get_column_names(vendor_data_table, select = enquo(select), n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  vendor_data_table <- vendor_data_table[select_cols]
  
  # include file info
  if (!quo_is_null(include_file_info_quo)) {
    info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE)
    vendor_data_table <- right_join(info, vendor_data_table, by = "file_id")
  }
  
  return(vendor_data_table)
}

# check if read options are compatible
check_read_options <- function(iso_files, option) {
  iso_files <- iso_as_file_list(iso_files)
  option_values <- map(iso_files, "read_options") %>% map_lgl(option)
  if (!all(option_values)) {
    warning(sum(!option_values), "/", length(iso_files), 
            " files were read without extracting the ", str_replace_all(option, "_", " "), 
            " (parameter '", str_c("read_", option), 
            "=FALSE') and will have missing values",
            call. = FALSE, immediate. = TRUE)
  }
}
