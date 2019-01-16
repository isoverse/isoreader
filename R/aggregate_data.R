# File info and data retrieval functions 
# @note: should make the aggregation functions type safe?
# @note: need an "iso_get_nested_data()" funtion that nests everything together for people who would like to use that
# @note: --> maybe provide the nest and unnest data functions from isoprocessor?
# @note: --> maybe allow plotting and exporting functions to also work with nested data sets? 
#            that would allow people to manipulate them easily to change certain naming fields and then go plotting?
# @note: consider supporting the nested data sets in the plot functions
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
  
  if (length(iso_files) == 0) return(data_frame())
  
  # aggregate all the info
  data_frame(
    file_id = names(iso_files),
    file_path_ = map_chr(iso_files, ~.x$file_info$file_path),
    file_subpath = map_chr(iso_files, ~.x$file_info$file_subpath)
  ) %>%
    left_join(get_raw_data_info(iso_files), by = "file_id") %>%
    left_join(get_file_info_info(iso_files), by = "file_id") %>%
    left_join(get_method_info_info(iso_files), by = "file_id") %>%
    left_join(get_vendor_data_table_info(iso_files), by = "file_id") %>%
    mutate(file_path = ifelse(!is.na(file_subpath), glue("{file_path_}|{file_subpath}"), file_path_)) %>%
    select(-file_path_, -file_subpath)
}

# summary of raw data info
get_raw_data_info <- function(iso_files) {
  
  # make sure to convert to file list
  iso_files <- iso_as_file_list(iso_files)
  
  # make sure to not process empty list
  if (length(iso_files) == 0)
    return(data_frame(file_id = character(), raw_data = character()))
  raw_data_not_read <- "raw data not read"
  
  # retrieve the raw data info
  raw_data_sum <- 
    data_frame(
      file_id = names(iso_files),
      read_raw_data = map_lgl(iso_files, ~.x$read_options$raw_data),
      all_ions = map(iso_files, ~names(.x$raw_data) %>% str_subset("^[iIvV](\\d+)\\.")),
      n_ions = map_int(all_ions, length),
      ions = map2_chr(all_ions, n_ions, ~if(.y > 0) { collapse(.x, sep = ", ") } else {""}) %>% 
        str_replace_all("[^0-9,]", "")
    )
  
  if (iso_is_continuous_flow(iso_files[[1]])) {
    raw_data_sum <- raw_data_sum %>% 
      mutate(
        n_tps = map_int(iso_files, ~nrow(.x$raw_data)),
        label = ifelse(read_raw_data, glue("{n_tps} time points, {n_ions} ions ({ions})"), "raw data not read")
      )
  } else if (iso_is_dual_inlet(iso_files[[1]])) {
    raw_data_sum <- raw_data_sum %>% 
      mutate(
        n_cycles = map_int(iso_files, ~as.integer(floor(nrow(.x$raw_data)/2))),
        label = ifelse(read_raw_data, glue("{n_cycles} cycles, {n_ions} ions ({ions})"), "raw data not read")
      )
  } else {
    # should not get here
    glue("cannot process '{class(iso_files[[1]])[1]}'") %>% stop(call. = FALSE)
  }
  
  return(select(raw_data_sum, file_id, raw_data = label))
}

# summary of file info
get_file_info_info <- function(iso_files) {
  # make sure to convert to file list
  iso_files <- iso_as_file_list(iso_files) %>% convert_file_info_to_data_frame()
  
  # make sure to not process empty list
  if (length(iso_files) == 0) {
    data_frame(file_id = character(), file_info = character())
  } else {
    # retrieve the raw data info
    data_frame(
      file_id = names(iso_files),
      read_file_info = map_lgl(iso_files, ~.x$read_options$file_info),
      file_info = ifelse(!read_file_info, "file info not read", paste(map_int(iso_files, ~ncol(.x$file_info)), "entries"))
    ) %>% select(file_id, file_info)
  }
}

# summary of method info
get_method_info_info <- function(iso_files) {
  
  # make sure to convert to file list
  iso_files <- iso_as_file_list(iso_files) 
  
  # make sure to not process empty list
  if (length(iso_files) == 0) {
    data_frame(file_id = character(), method_info = character())
  } else {
    # retrieve the raw data info
    data_frame(
      file_id = names(iso_files),
      read_method_info = map_lgl(iso_files, ~.x$read_options$method_info),
      has_standards = map_lgl(iso_files, ~!is.null(.x$method_info$standards)),
      has_resistors = map_lgl(iso_files, ~!is.null(.x$method_info$resistors)),
      method_info = case_when(
        !read_method_info ~ "method info not read",
        has_standards & has_resistors ~ "standards, resistors",
        has_standards ~ "standards",
        has_resistors ~ "resistors",
        TRUE ~ "no method info"
      ) 
    ) %>% select(file_id, method_info)
  }
  
}

# summary of vendor data table
get_vendor_data_table_info <- function(iso_files) {
  # make sure to convert to file list
  iso_files <- iso_as_file_list(iso_files) %>% convert_file_info_to_data_frame()
  
  # make sure to not process empty list
  if (length(iso_files) == 0) {
    data_frame(file_id = character(), vendor_data_table = character())
  } else {
    # retrieve the raw data info
    data_frame(
      file_id = names(iso_files),
      read_vendor_data_table = map_lgl(iso_files, ~.x$read_options$vendor_data_table),
      rows = map_int(iso_files, ~nrow(.x$vendor_data_table)),
      cols = map_int(iso_files, ~ncol(.x$vendor_data_table)),
      vendor_data_table = case_when(
        !read_vendor_data_table ~ "vendor data table not read",
        rows > 0 & cols > 0 ~ sprintf("%d rows, %d columns", rows, cols),
        TRUE ~ "no vendor data table"
      ) 
    ) %>% select(file_id, vendor_data_table)
  }
}

# Specific data aggregation calls =====

#' Aggregate all isofiles data
#' 
#' This function aggregates all isofiles data and returns it in a large data frame with nested columns for each type of information (file_info, raw_data, etc.). For targeted retrieval of specific data \code{\link{iso_get_raw_data}}, \code{\link{iso_get_file_info}}, \code{\link{iso_get_vendor_data_table}}, etc. are much faster and easier to work with. This function is primarily useful for downstream processing pipelines that want to carry all information along. To \code{\link[tidyr]{unnest}} any of the specific data types (e.g. \code{raw_data}), make sure to filter first for the files that have this data type available (e.g. \code{filter(has_raw_data)}). 
#' 
#' @inheritParams iso_get_raw_data
#' @inheritParams iso_get_standards_info
#' @inheritParams iso_get_vendor_data_table
#' @param include_raw_data which columns from the raw data to include use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default.
#' @param include_vendor_data_table which columns from the vendor data table to include - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default.
#' @return data_frame with file_ids, file_types and nested data frames for each data type (file_info, raw_data, vendor_data_table, etc.)
#' @family data retrieval functions
#' @export
iso_get_data <- function(iso_files, include_file_info = everything(), include_raw_data = everything(), include_vendor_data_table = everything(), 
                         gather = FALSE, with_units = FALSE, with_ratios = FALSE, quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  if (!quiet) sprintf("Info: aggregating all data from %d data file(s)", length(iso_files)) %>% message()
  
  # file class
  file_class <- 
    data_frame(
      file_id = names(iso_files),
      file_type = map_chr(iso_files, ~class(.x)[1])
    )
  
  # file info
  include_file_info_quo <- enquo(include_file_info)
  file_info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE) 
  if (ncol(file_info) > 1)
    file_info <- nest(file_info, -file_id, .key = file_info)
  else
    file_info <- data_frame(file_id = character(0), file_info = list(NULL))
  
  # raw data
  include_raw_data_quo <- enquo(include_raw_data)
  raw_data <- iso_get_raw_data(iso_files, select = !!include_raw_data_quo, gather = gather, quiet = TRUE)
  if (ncol(raw_data) > 1)
    raw_data <- nest(raw_data, -file_id, .key = raw_data)
  else
    raw_data <- data_frame(file_id = character(0), raw_data = list(NULL))
  
  # vendor data table
  include_vendor_data_table_quo <- enquo(include_vendor_data_table)
  dt <- iso_get_vendor_data_table(iso_files, with_units = with_units, select = !!include_vendor_data_table_quo, quiet = TRUE)
  if (ncol(dt) > 1)
    dt <- nest(dt, -file_id, .key = vendor_data_table)
  else
    dt <- data_frame(file_id = character(0), vendor_data_table = list(NULL))
  
  # methods_data - standards
  standards <- iso_get_standards_info(iso_files, with_ratios = with_ratios, quiet = TRUE)
  if (ncol(standards) > 1)
    standards <- nest(standards, -file_id, .key = standards)
  else
    standards <- data_frame(file_id = character(0), standards = list(NULL))
  
  # methods_data - resistors
  resistors <- iso_get_resistors_info(iso_files, quiet = TRUE)
  if (ncol(resistors) > 1)
    resistors <- nest(resistors, -file_id, .key = resistors)
  else
    resistors <- data_frame(file_id = character(0), resistors = list(NULL))
  
  # combine everything
  file_class %>% 
    left_join(file_info, by = "file_id") %>% 
    left_join(raw_data, by = "file_id") %>%
    left_join(dt, by = "file_id") %>% 
    left_join(standards, by = "file_id") %>% 
    left_join(resistors, by = "file_id") %>% 
    # info about what's missing
    mutate(
      has_file_info = !map_lgl(file_info, is.null),
      has_raw_data = !map_lgl(raw_data, is.null),
      has_vendor_data_table = !map_lgl(vendor_data_table, is.null),
      has_standards = !map_lgl(standards, is.null),
      has_resistors = !map_lgl(resistors, is.null)
    )
}

#' Aggregate file info
#'
#' Combine file information from multiple iso_files. By default all information is included but specific items can be specified using the \code{include} parameter. The file id is always included. File information beyond \code{file_id} and \code{file_path} is only available if the iso_files were read with parameter \code{read_file_info=TRUE}.
#'
#' @inheritParams iso_get_raw_data
#' @param select which columns to select - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default. File id is always included no matter which selection parameters. 
#' @family data retrieval functions
#' @note File info entries with multiple values are concatenated for this aggregation function. To get access to a specific multi-value file info entry, access using \code{iso_file$file_info[['INFO_NAME']]} on the iso_file object directly.
#' @export
iso_get_file_info <- function(iso_files, select = everything(), quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  if (!quiet) sprintf("Info: aggregating file info from %d data file(s)", length(iso_files)) %>% message()
  check_read_options(iso_files, "file_info")
  
  # @note: for safety, could be deprecated in later version because it is also done at the end of every isoread
  iso_files <- convert_file_info_to_data_frame(iso_files) 
  
  # retrieve info
  file_info <- iso_files %>% 
    # retrieve file info (turn into list columns)
    map(~.x$file_info) %>% 
    # combine in data frame
    bind_rows()
  
  # nothing returned
  if (nrow(file_info) == 0) return(file_info)
  
  # figure out which columns are selected
  select_cols <- get_column_names(file_info, select = enquo(select), n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  
  # return
  file_info %>% 
    # focus on selected columns only (also takes care of the rename)
    dplyr::select(!!!select_cols) %>% 
    # unnest aggregated data frame
    unnest_aggregated_data_frame()
}

# note: consider providing a separate iso_gather_raw_data method that works just on the raw data table and could be used in other contexts

#' Aggregate raw data
#' 
#' Aggregate the raw ion data from the provided iso_files. Can aggregate either in a wide table (for easy overview) or a gathered long table (for plotting and further data processing). The raw data is only available if the iso_files were read with parameter \code{read_raw_data=TRUE}.
#' 
#' @inheritParams iso_read_files
#' @param iso_files collection of iso_file objects
#' @param select which raw data columns to select - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax. By default, all columns are selected.
#' @family data retrieval functions
#' @param gather whether to gather raw data into long format (e.g. for ease of use in plotting). Not that the \code{select} parameter applies to the data columns BEFORE gathering.
#' @param include_file_info which file information to include (see \code{\link{iso_get_file_info}}). Use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns.
#' @family data retrieval functions
#' @export
iso_get_raw_data <- function(iso_files, select = everything(), gather = FALSE, include_file_info = NULL, quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  select_quo <- enquo(select)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) { 
    glue(
      "Info: aggregating raw data from {length(iso_files)} data file(s)",
      "{get_info_message_concat(select_quo, prefix = ', selecting data columns ', empty = 'everything()')}",
      "{get_info_message_concat(include_file_info_quo, prefix = ', including file info ')}") %>% message()
  }
  check_read_options(iso_files, "raw_data")
  
  # check whether there are any
  if (length(iso_files) == 0) return(data_frame())
  
  # fetch data
  data <-
    # fetch data
    data_frame(
      file_id = names(iso_files),
      raw_data = map(iso_files, ~.x$raw_data)
    ) %>% 
    # make sure to include only existing raw data
    filter(!map_lgl(raw_data, is.null)) %>% 
    # unnest
    unnest(raw_data)
  
  # check for rows
  if (nrow(data) == 0) return(data)
  
  # selecting columns
  select_cols <- get_column_names(data, select = select_quo, n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  data <- data %>% 
    # focus on selected columns only (also takes care of the rename)
    dplyr::select(!!!select_cols) 
  
  # if gathering
  if (gather) {
    column <- value <- extra_parens <- category <- NULL # global vars
    masses_ratios_re <- "^([vir])(\\d+/?\\d*)(\\.(.+))?$"
    data <- data %>% 
      # gather all masses and ratios
      gather(column, value, matches(masses_ratios_re)) %>% 
      # extract unit information
      extract(column, into = c("category", "data", "extra_parens", "units"), regex = masses_ratios_re) %>% 
      dplyr::select(-extra_parens) %>% 
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


#' Aggregate background data
#' 
#' Aggregate the background data from the provided iso_files. Can aggregate either in a wide table (for easy overview) or a gathered long table (for plotting and further data processing). The background data is only available if the iso_files were read with parameter \code{read_raw_data=TRUE}.
#' 
#' @inheritParams iso_get_raw_data
#' @family data retrieval functions
#' @export
iso_get_bgrd_data <- function(iso_files, select = everything(), gather = FALSE, include_file_info = NULL, quiet = default(quiet)) {
  iso_files <- iso_as_file_list(iso_files)
  if (!all(map_lgl(iso_files, iso_is_dual_inlet))) stop("background data is only available in dual inlet data files", call. = FALSE)
  select_quo <- enquo(select)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) { 
    glue(
      "Info: aggregating background data from {length(iso_files)} data file(s)",
      "{get_info_message_concat(select_quo, prefix = ', selecting data columns ', empty = 'everything()')}",
      "{get_info_message_concat(include_file_info_quo, prefix = ', including file info ')}") %>% message()
  }
  check_read_options(iso_files, "raw_data")
  
  # check whether there are any
  if (length(iso_files) == 0) return(data_frame())
  
  # fetch data
  data <-
    # fetch data
    data_frame(
      file_id = names(iso_files),
      bgrd_data = map(iso_files, ~.x$bgrd_data)
    ) %>% 
    # make sure to include only existing raw data
    filter(!map_lgl(bgrd_data, is.null)) %>% 
    # unnest
    unnest(bgrd_data)
  
  # check for rows
  if (nrow(data) == 0) return(data)
  
  # selecting columns
  select_cols <- get_column_names(data, select = select_quo, n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  data <- data %>% 
    # focus on selected columns only (also takes care of the rename)
    dplyr::select(!!!select_cols) 
  
  # if gathering
  if (gather) {
    column <- value <- extra_parens <- category <- NULL # global vars
    masses_ratios_re <- "^([vir])(\\d+/?\\d*)(\\.(.+))?$"
    data <- data %>% 
      # gather all masses and ratios
      gather(column, value, matches(masses_ratios_re)) %>% 
      # extract unit information
      extract(column, into = c("category", "data", "extra_parens", "units"), regex = masses_ratios_re) %>% 
      dplyr::select(-extra_parens) %>% 
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
  
  # check whether there are any
  if (length(iso_files) == 0) return(data_frame())
  
  # fetch data
  data <-
    # fetch data
    data_frame(
      file_id = names(iso_files),
      standards = map(iso_files, ~.x$method_info$standard),
      ref_ratios = map(iso_files, ~.x$method_info$reference_ratios)
    ) 
  
  # check for rows
  if (nrow(data) == 0) return(data)
  
  # merge info
  standards <- data %>% select(file_id, standards) %>% filter(!map_lgl(standards, is.null)) %>% unnest(standards) 
  if (with_ratios) {
    ref_ratios <- data %>% select(file_id, ref_ratios) %>% filter(!map_lgl(ref_ratios, is.null)) %>% unnest(ref_ratios) 
    data <- left_join(standards, ref_ratios, by = c("file_id", "reference"))
  } else {
    data <- standards
  }
  
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
  
  # check whether there are any files
  if (length(iso_files) == 0) return(data_frame())
  
  # fetch data
  data <-
    # fetch data
    data_frame(
      file_id = names(iso_files),
      resistors = map(iso_files, ~.x$method_info$resistors)
    ) %>% 
    # make sure to include only existing raw data
    filter(!map_lgl(resistors, is.null)) %>% 
    # unnest
    unnest(resistors)

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
#' @param with_units whether to include units in the column headers (if there are any) or not (default is FALSE). Note that this can slow down the function significantely. 
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
  
  # check whether there are any files
  if (length(iso_files) == 0) return(data_frame())
  
  # get vendor data
  column <- units <- NULL # global vars
  
  # fetch data
  vendor_data_table <-
    # fetch data
    data_frame(
      file_id = names(iso_files),
      dt = map(iso_files, ~.x$vendor_data_table)
    ) %>% 
    # make sure to include only existing raw data
    filter(map_lgl(dt, ~!is.null(.x) & nrow(.x) > 0)) 
  
  # check for any rows
  if (nrow(vendor_data_table) == 0) return(vendor_data_table)
  
  # add units if requested
  if (with_units) {
  
    dt_units <- 
      # fetch units
      data_frame(
        file_id = names(iso_files),
        has_units = map_lgl(iso_files, ~attr(.x$vendor_data_table, "units") %>% { !is.null(.) & !identical(., NA) }),
        dt_units = map2(iso_files, has_units, ~{
          if (.y) { 
            attr(.x$vendor_data_table, "units") # if units provided, use them
          } else { 
            # if no units provided
            data_frame(column = names(.x$vendor_data_table), units = NA_character_) 
          }
        })
      ) %>% 
      # assemble units
      unnest(dt_units) %>% 
      mutate(units = ifelse(!is.na(units) & nchar(units) > 0, str_c(column, " ", units), column)) %>% 
      dplyr::select(file_id, has_units, column, units) %>% 
      nest(-file_id, -has_units, .key = dt_units)
  
    # join with vendor data table
    vendor_data_table <-
      vendor_data_table %>%
      left_join(dt_units, by = "file_id") %>% 
      # take care of renaming with units
      mutate(
        dt_units = map(dt_units, deframe),
        dt = map2(dt, dt_units, ~{ names(.x) <- .y[names(.x)]; .x })
      )
    
    # warning about missing units
    if (!all(vendor_data_table$has_units)) {
      glue("{sum(!vendor_data_table$has_units)} of {length(iso_files)} files do not have ",
           "unit information for their vendor data table and will have missing units") %>% 
        warning(call. = FALSE, immediate. = TRUE)
    }
    
  }
  
  # unnest
  vendor_data_table <- dplyr::select(vendor_data_table, file_id, dt) %>% unnest(dt)

  # get include information
  select_cols <- get_column_names(vendor_data_table, select = enquo(select), n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  
  # focus on selected columns only (also takes care of the rename)
  vendor_data_table <- dplyr::select(vendor_data_table, !!!select_cols) 
  
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


# Data Aggregation helpers ==========

# helper function to convert file_path into rooted path for unrooted legacy files
convert_file_path_to_rooted <- function(iso_files, root = ".", ...) {
  
  stopifnot(iso_is_file_list(iso_files))
  
  # the ones needing updating
  needs_conversion <- map_lgl(iso_files, ~is.null(.x$file_info[["file_root"]]) || is.na(.x$file_info$file_root))
  
  if (any(needs_conversion)) {
    
    # get paths
    paths <- 
      map_chr(iso_files[needs_conversion], ~.x$file_info$file_path) %>% 
      iso_root_paths(root = root, check_existence = FALSE)
    
    # prepare file info updates
    file_info_update <- with(paths, map2(root, path, ~list(file_info = list(file_root = .x, file_path = .y))))
    names(file_info_update) <- names(iso_files[needs_conversion])
    
    # make sure to keep format
    iso_files <- as.list(iso_files) %>% 
      modifyList(file_info_update) %>% 
      iso_as_file_list(...)
  }
  
  return(iso_files)
}

# helper function to turn file info from list to data frame for easier/faster aggregation
convert_file_info_to_data_frame <- function(iso_files, ...) {
  
  stopifnot(iso_is_file_list(iso_files))
  
  # the ones needing updating
  needs_conversion <- map_lgl(iso_files, ~!is.data.frame(.x$file_info))
  
  if (any(needs_conversion)) {
    
    # standard fields
    standard_fields <- names(make_iso_file_data_structure()$file_info)
    
    # make sure to keep format
    iso_files <- as.list(iso_files)
    
    # convert only the ones that need conversion
    iso_files[needs_conversion] <-
      iso_files[needs_conversion] %>% 
      map(
        ~{ 
          # don't convert standard fields, just custom ones generated by indiviudal file readers
          is_standard_field <- names(.x$file_info) %in% standard_fields
          .x$file_info[!is_standard_field] <- map(.x$file_info[!is_standard_field], list)
          .x$file_info <- as_data_frame(.x$file_info)
          .x  
        }
      )
    
    iso_files <- iso_as_file_list(iso_files, ...)
  }
  
  return(iso_files)
}

# helper function to unnest aggregated columns that have single or no values and the same data types
unnest_aggregated_data_frame <- function(df) {
  
  # safety
  stopifnot(is.data.frame(df))
  if (nrow(df) == 0) return(df)
  
  # NA defaults
  NA_defaults <- list(character = NA_character_, numeric = NA_real_, integer = NA_integer_, logical = NA)
  
  # get information about the data frame columns
  cols <- 
    data_frame(
      column = names(df),
      id = 1:length(column),
      lengths = map(column, ~map_int(df[[.x]], length)),
      min_length = map_int(lengths, ~min(.x)),
      max_length = map_int(lengths, ~max(.x)),
      is_missing = map(lengths, ~.x == 0),
      has_missing = min_length == 0,
      main_class = map2(column, is_missing, ~map_chr(df[[.x]], ~class(.x)[1])[!.y]),
      has_identical_class = map_lgl(main_class, ~all(.x==.x[1])),
      identical_class = ifelse(has_identical_class, map_chr(main_class, ~.x[1]), NA_character_),
      unnest_single_value = max_length == 1 & has_identical_class,
      renest_missing_value = min_length == 0 & max_length > 1 & has_identical_class & identical_class %in% names(NA_defaults)
    )
  
  # FIXME: add warning messages about inconsistent data columns with multiple data types!
  if (any(!cols$has_identical_class)) {
    glue("encountered different value types within the same column(s), they cannot be automatically unnested: ",
         "'{collapse(filter(cols, !has_identical_class)$column, sep = \"', '\")}'") %>% 
      warning(immediate. = TRUE, call. = FALSE)
  }
  
  # unnest data
  for (i in 1:nrow(cols)) {
    
    if (cols$unnest_single_value[i]) {
      # unnest single values
      if (cols$identical_class[i] == "character")
        df <- mutate(df, !!cols$column[i] := 
                          map2_chr(!!sym(cols$column[i]), cols$is_missing[[i]], 
                                   ~if(.y) { NA_character_ } else {.x[1]}))
      else if(cols$identical_class[i] == "numeric")
        df <- mutate(df, !!cols$column[i] := 
                          map2_dbl(!!sym(cols$column[i]), cols$is_missing[[i]], 
                                   ~if(.y) { NA_real_ } else {.x[1]}))
      else if (cols$identical_class[i] == "logical")
        df <- mutate(df, !!cols$column[i] := 
                       map2_lgl(!!sym(cols$column[i]), cols$is_missing[[i]], 
                                ~if(.y) { NA } else {.x[1]}))
      else if (cols$identical_class[i] == "integer")
        df <- mutate(df, !!cols$column[i] := 
                          map2_int(!!sym(cols$column[i]), cols$is_missing[[i]], 
                                   ~if(.y) { NA_integer_ } else {.x[1]}))
      else if (cols$identical_class[i] == "POSIXct") 
        df <- mutate(df, !!cols$column[i] := 
                          map2_int(!!sym(cols$column[i]), cols$is_missing[[i]], 
                                   ~if (.y) { NA_integer_ } else { as.integer(.x[1])}) %>% 
                          as_datetime(tz = Sys.timezone()))
      else {
        glue("cannot unnest file info column {cols$column[i]}, encountered unusual class {cols$identical_class[i]}") %>% 
          warning(immediate. = TRUE, call. = FALSE)
      }
    } else if (cols$renest_missing_value[i]) {
      # replace NA values in columns that have too many values for unnesting (so unnesting is easy for the user)
      df <- mutate(df, !!cols$column[i] := 
                        map2(!!sym(cols$column[i]), cols$is_missing[[i]], 
                             ~if(.y) { NA_defaults[[cols$identical_class[i]]] } else {.x}))
    }

  }
  
  return(df)
}

# helper function to concatenate list columns for export file formats that cannot handle the embedded data
collapse_list_columns <- function(df, sep = ", ") {
  collapse_function <- function(x) collapse(x, sep = sep)
  df %>% 
    mutate_if(.predicate = is.list, .funs = map_chr, collapse_function)
}
