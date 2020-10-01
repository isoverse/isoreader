# General helper functions ======

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
#' @return a \code{\link[tibble]{tibble}} that summarizes the data in the \code{iso_files}
#' @export
iso_get_data_summary <- function(iso_files, quiet = default(quiet)) {

  # global vars
  file_subpath <- file_path_ <- NULL

  iso_files <- iso_as_file_list(iso_files)

  if (!quiet) {
    glue("Info: aggregating data summary from {length(iso_files)} data file(s)") %>%
      message()
  }

  if (length(iso_files) == 0) return(tibble())

  # aggregate all the info
  tibble(
    file_id = names(iso_files),
    file_path_ = map_chr(
      iso_files,
      ~if (col_in_df(.x$file_info, "file_path")) { .x$file_info$file_path } else { NA_character_ }),
    file_subpath = map_chr(
      iso_files,
      ~if (col_in_df(.x$file_info, "file_subpath")) { .x$file_info$file_subpath } else { NA_character_ })
  ) %>%
    left_join(get_raw_data_info(iso_files), by = "file_id") %>%
    left_join(get_file_info_info(iso_files), by = "file_id") %>%
    left_join(get_method_info_info(iso_files), by = "file_id") %>%
    {
      # scan files don't have vendor data table
      if (!iso_is_scan(iso_files))
        left_join(., get_vendor_data_table_info(iso_files), by = "file_id")
      else .
    } %>%
    mutate(file_path = ifelse(!is.na(file_subpath), glue("{file_path_}|{file_subpath}"), file_path_)) %>%
    select(-file_path_, -file_subpath)
}

# summary of raw data info
get_raw_data_info <- function(iso_files) {

  # make sure to convert to file list
  iso_files <- iso_as_file_list(iso_files)

  # make sure to not process empty list
  if (length(iso_files) == 0)
    return(tibble(file_id = character(), raw_data = character()))
  raw_data_not_read <- "raw data not read"

  # retrieve the raw data info
  raw_data_sum <-
    tibble(
      file_id = names(iso_files),
      read_raw_data = map_lgl(iso_files, ~.x$read_options$raw_data),
      all_ions = map(iso_files, ~names(.x$raw_data) %>% str_subset("^[iIvV]C?(\\d+)\\.")),
      n_ions = map_int(.data$all_ions, length),
      full_ions = map2_chr(.data$all_ions, .data$n_ions, ~if(.y > 0) { collapse(.x, sep = ", ") } else {""}),
      ions = .data$full_ions %>% str_replace_all("[^0-9,]", "")
    )

  if (iso_is_continuous_flow(iso_files)) {
    raw_data_sum <- raw_data_sum %>%
      mutate(
        n_tps = map_int(iso_files, ~nrow(.x$raw_data)),
        label = case_when(
          read_raw_data & stringr::str_detect(.data$full_ions, "[iIvV]C") ~ glue("{n_tps} time points, {n_ions} channels ({ions})"),
          read_raw_data ~ glue("{n_tps} time points, {n_ions} ions ({ions})"),
          TRUE ~ "raw data not read"
        )
      )
  } else if (iso_is_dual_inlet(iso_files)) {
    raw_data_sum <- raw_data_sum %>%
      mutate(
        n_cycles = map_int(iso_files, ~as.integer(floor(nrow(.x$raw_data)/2))),
        label = case_when(
          read_raw_data & stringr::str_detect(full_ions, "[iIvV]C") ~ glue("{n_cycles} cycles, {n_ions} channels ({ions})"),
          read_raw_data ~ glue("{n_cycles} cycles, {n_ions} ions ({ions})"),
          TRUE ~ "raw data not read"
        )
      )
  } else if (iso_is_scan(iso_files)) {
    raw_data_sum <- raw_data_sum %>%
      mutate(
        n_tps = map_int(iso_files, ~nrow(.x$raw_data)),
        label = case_when(
          read_raw_data & stringr::str_detect(full_ions, "[iIvV]C") ~ glue("{n_tps} measurements, {n_ions} channels ({ions})"),
          read_raw_data ~ glue("{n_tps} measurements, {n_ions} ions ({ions})"),
          TRUE ~ "raw data not read"
        )
      )
  } else if (iso_is_file(iso_files[[1]])) {
    # can only get here using make_iso_file_data_structure
    stop("make_iso_file_data_structure should never be called directly", call. = FALSE)
  } else {
    # should not get here
    glue("cannot process '{class(iso_files[[1]])[1]}' in get_raw_data_info") %>% stop(call. = FALSE)
  }

  return(dplyr::select(raw_data_sum, .data$file_id, raw_data = .data$label))
}

# summary of file info
get_file_info_info <- function(iso_files) {

  # make sure to convert to file list
  iso_files <- iso_as_file_list(iso_files)

  # make sure to not process empty list
  if (length(iso_files) == 0) {
    tibble(file_id = character(), file_info = character())
  } else {
    # retrieve the raw data info
    tibble(
      file_id = names(iso_files),
      read_file_info = map_lgl(iso_files, ~.x$read_options$file_info),
      file_info = ifelse(!.data$read_file_info, "file info not read", paste(map_int(iso_files, ~length(.x$file_info)), "entries"))
    ) %>% select(.data$file_id, .data$file_info)
  }
}

# summary of method info
get_method_info_info <- function(iso_files) {

  # make sure to convert to file list
  iso_files <- iso_as_file_list(iso_files)

  # make sure to not process empty list
  if (length(iso_files) == 0) {
    tibble(file_id = character(), method_info = character())
  } else {
    # retrieve the raw data info
    tibble(
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
    ) %>% select(.data$file_id, .data$method_info)
  }

}

# summary of vendor data table
get_vendor_data_table_info <- function(iso_files) {
  # make sure to convert to file list
  iso_files <- iso_as_file_list(iso_files)

  # make sure to not process empty list
  if (length(iso_files) == 0) {
    tibble(file_id = character(), vendor_data_table = character())
  } else {
    # retrieve the raw data info
    tibble(
      file_id = names(iso_files),
      read_vendor_data_table = map_lgl(iso_files, ~.x$read_options$vendor_data_table),
      rows = map_int(iso_files, ~nrow(.x$vendor_data_table)),
      cols = map_int(iso_files, ~ncol(.x$vendor_data_table)),
      vendor_data_table = case_when(
        !read_vendor_data_table ~ "vendor data table not read",
        .data$rows > 0 & .data$cols > 0 ~ sprintf("%d rows, %d columns", .data$rows, .data$cols),
        TRUE ~ "no vendor data table"
      )
    ) %>% select(.data$file_id, .data$vendor_data_table)
  }
}

# Specific data aggregation calls =====

#' DEPRECATED
#'
#' Please use \link{iso_get_all_data} instead.
#' @param ... forwarded to \link{iso_get_all_data}
#'
#' @export
iso_get_data <- function(...) {
  warning("'iso_get_data()' is deprecated in favor of the more descriptive 'iso_get_all_data()'. Please use 'iso_get_all_data()' directly to avoid this warning.", immediate. = TRUE, call. = FALSE)
  iso_get_all_data(...)
}

#' Aggregate all isofiles data
#'
#' This function aggregates all isofiles data and returns it in a large data frame with nested columns for each type of information (file_info, raw_data, etc.). For targeted retrieval of specific data \code{\link{iso_get_raw_data}}, \code{\link{iso_get_file_info}}, \code{\link{iso_get_vendor_data_table}}, etc. are much faster and easier to work with. This function is primarily useful for downstream processing pipelines that want to carry all information along. To \code{\link[tidyr:nest]{unnest}} any of the specific data types (e.g. \code{raw_data}), make sure to filter first for the files that have this data type available (e.g. \code{filter(has_raw_data)}). Exclude specific types of information by setting its \code{include...} parameter to \code{NULL} (Note: for historical reasons, setting it to \code{FALSE} will also include the information).
#'
#' @inheritParams iso_get_raw_data
#' @inheritParams iso_get_standards
#' @inheritParams iso_get_vendor_data_table
#' @param include_raw_data which columns from the raw data to include. Use \code{c(...)} to select multiple columns, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default. Set to NULL to include no raw data.
#' @param include_standards which columns from the standards info to include. Use \code{c(...)} to select multiple columns, supports all \link[dplyr]{select} syntax including renaming columns. By default, everything is included (both standards and ratios). To omit the ratios, change to \code{select = file_id:reference}. Set to NULL to include no standards info.
#' @param include_resistors which columns from the resistors info to include. Use \code{c(...)} to select multiple columns, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default. Set to NULL to include no resistors info.
#' @param include_vendor_data_table which columns from the vendor data table to include. Use \code{c(...)} to select multiple columns, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default. Set parameter \code{with_explicit_units = TRUE} to make column units explicit (keep in mind that this will require specific \code{include_vendor_data_table} column selections to reflect the column names including the units). Set to NULL to include no vendor data table.
#' @param include_problems which columns from problems to include. Use \code{c(...)} to select multiple columns, supports all \link[dplyr]{select} syntax including renaming columns. Includes none of the read problems by default. Set to \code{include_problems = everything()} to include all columns.
#' @return data_frame with file_ids, file_types and nested data frames for each data type (file_info, raw_data, vendor_data_table, etc.)
#' @family data retrieval functions
#' @export
iso_get_all_data <- function(
  iso_files,
  include_file_info = everything(), include_raw_data = everything(),
  include_standards = everything(), include_resistors = everything(),
  include_vendor_data_table = everything(),
  include_problems = NULL,
  gather = FALSE, with_explicit_units = with_units,
  with_units = FALSE, with_ratios = NULL, quiet = default(quiet)) {

  # info
  iso_files <- iso_as_file_list(iso_files)
  if (!quiet) sprintf("Info: aggregating all data from %d data file(s)", length(iso_files)) %>% message()

  # deprecated parameter
  if (!missing(with_ratios)) {
    warning("the 'with_ratios' parameter is deprecated, please use the column selection parameter 'include_standards' to explicitly include or exclude ratio columns", immediate. = TRUE, call. = FALSE)
  }

  # is di or cf?
  di_or_cf <- iso_is_continuous_flow(iso_files) || iso_is_dual_inlet(iso_files)

  # select expressions
  include_file_info_exp <- rlang::enexpr(include_file_info)
  include_file_info <- !rlang::as_label(include_file_info_exp) %in% c("NULL", "FALSE")
  include_raw_data_exp <- rlang::enexpr(include_raw_data)
  include_raw_data <- !rlang::as_label(include_raw_data_exp) %in% c("NULL", "FALSE")
  include_standards_exp <- rlang::enexpr(include_standards)
  include_standards <- di_or_cf && !rlang::as_label(include_standards_exp) %in% c("NULL", "FALSE")
  include_resistors_exp <- rlang::enexpr(include_resistors)
  include_resistors <- !rlang::as_label(include_resistors_exp) %in% c("NULL", "FALSE")
  include_vendor_data_table_exp <- rlang::enexpr(include_vendor_data_table)
  include_vendor_data_table <- di_or_cf && !rlang::as_label(include_vendor_data_table_exp) %in% c("NULL", "FALSE")
  include_problems_exp <- rlang::enexpr(include_problems)
  include_problems <- !rlang::as_label(include_problems_exp) %in% c("NULL", "FALSE")

  # file class
  file_class <-
    tibble(
      file_id = names(iso_files),
      file_type = map_chr(iso_files, ~class(.x)[1]) %>% unname()
    )

  # all file data
  # note that this uses the iso_get_... functions to have some built in error
  # checking although a straight up map(~.x$...) would be faster

  # data merge function
  merge_with_file_class <- function(new_df, col_name) {
    nested_df <- nest(new_df[c(),], !!col_name := c(-.data$file_id))
    if (ncol(new_df) > 1)
      nested_df <- nest(new_df, !!col_name := c(-.data$file_id))
    nested_df <- bind_rows(nested_df, tibble(file_id = setdiff(file_class$file_id, nested_df$file_id), !!col_name := list(tibble())))
    left_join(file_class, nested_df, by = "file_id")
  }

  # file info
  if (include_file_info) {
    file_class <- iso_get_file_info(iso_files, select = !!include_file_info_exp, quiet = TRUE) %>%
      merge_with_file_class("file_info")
  }

  # raw data
  if (include_raw_data) {
    file_class <- iso_get_raw_data(iso_files, select = !!include_raw_data_exp, gather = gather, quiet = TRUE) %>%
      merge_with_file_class("raw_data")
  }

  # standards
  if (include_standards) {
    file_class <- iso_get_standards(iso_files, select = !!include_standards_exp, quiet = TRUE) %>%
      merge_with_file_class("standards")
  }

  # resistors
  if (include_resistors) {
    file_class <- iso_get_resistors(iso_files, select = !!include_resistors_exp, quiet = TRUE) %>%
      merge_with_file_class("resistors")
  }

  # vendor data table (only cflow and dual inlet)
  if (include_vendor_data_table) {
    file_class <- iso_get_vendor_data_table(
      iso_files,
      with_explicit_units = with_explicit_units,
      select = !!include_vendor_data_table_exp, quiet = TRUE) %>%
      merge_with_file_class("vendor_data_table")
  }

  # problems
  if (include_problems) {
    file_class <- iso_get_problems(iso_files, select = !!include_problems_exp) %>%
      merge_with_file_class("problems")
  }

  return(file_class)
}

#' Aggregate file info
#'
#' Combine file information from multiple iso_files. By default all information is included but specific columns can be targeted using the \code{select} parameter to select and/or rename columns. File information beyond \code{file_id}, \code{file_root}, \code{file_path}, \code{file_datetime} and \code{file_size} (in bytes) is only available if the \code{iso_files} were read with parameter \code{read_file_info=TRUE}.
#'
#' @inheritParams iso_get_raw_data
#' @inheritParams iso_select_file_info
#' @param select which columns to select - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns. File id is always included and cannot be renamed.
#' @param simplify if set to TRUE (the default), nested value columns in the file info will be unnested as long as they are compatible across file types. Note that file info entries with multiple values still remain nested multi-value (=list) columns even with \code{simplify=TRUE}. These can be unnested using \link[tidyr:nest]{unnest}.
#' @family data retrieval functions
#' @note this function used to allow selecting/renaming different file_info_columns in different files to the same column. This was a significant speed impediment and only covered very rare use cases. It is still available in the related function \code{\link{iso_select_file_info}} with a special flag but is no longer the default and not encouraged for use in the frequently called \code{iso_get_file_info}.
#' @export
iso_get_file_info <- function(iso_files, select = everything(), file_specific = FALSE, simplify = TRUE, quiet = default(quiet)) {

  # make sure it's an iso file list
  iso_files <- iso_as_file_list(iso_files)
  select_exp <- rlang::enexpr(select)

  if (!quiet) {
    glue::glue(
      "Info: aggregating file info from {length(iso_files)} data file(s)",
      "{get_info_message_concat(select_exp, prefix = ', selecting info columns ', empty = 'everything()')}") %>%
      message()
  }
  check_read_options(iso_files, "file_info")

  # retrieve info
  file_info <- iso_files %>%
    {
      if (rlang::as_label(select_exp) != "everything()")
        # select columns
        iso_select_file_info(., !!select_exp, file_specific = file_specific, quiet = TRUE)
      else . # much faster (if selecting everything)
    } %>%
    # retrieve file info
    map(~.x$file_info) %>%
    # combine in data frame (use safe bind to make sure different data column
    # types of the same name don't trip up the combination)
    safe_bind_rows()

  # check if empty
  if(nrow(file_info) == 0) return(tibble(file_id = character(0)))

  # simplify by disaggregated columns
  if (simplify)
    # unnest aggregated columns
    file_info <- unnest_aggregated_data_frame(file_info)

  return(file_info)
}

# note: consider providing a separate iso_gather_raw_data method that works just on the raw data table and could be used in other contexts

#' Aggregate raw data
#'
#' Aggregate the raw ion data from the provided iso_files. Can aggregate either in a wide table (for easy overview) or a gathered long table (for plotting and further data processing). The raw data is only available if the iso_files were read with parameter \code{read_raw_data=TRUE}.
#'
#' @inheritParams iso_read_files
#' @param iso_files collection of iso_file objects
#' @param select which data columns to select - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax. By default, all columns are selected.
#' @family data retrieval functions
#' @param gather whether to gather raw data into long format (e.g. for ease of use in plotting). Not that the \code{select} parameter applies to the data columns BEFORE gathering.
#' @param include_file_info which file information to include (see \code{\link{iso_get_file_info}}). Use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns.
#' @family data retrieval functions
#' @export
iso_get_raw_data <- function(iso_files, select = everything(), gather = FALSE, include_file_info = NULL, quiet = default(quiet)) {

  # global
  raw_data <- NULL

  iso_files <- iso_as_file_list(iso_files)
  select_exp <- rlang::enexpr(select)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) {
    glue::glue(
      "Info: aggregating raw data from {length(iso_files)} data file(s)",
      "{get_info_message_concat(select_exp, prefix = ', selecting data columns ', empty = 'everything()')}",
      "{get_info_message_concat(include_file_info_quo, prefix = ', including file info ')}") %>% message()
  }
  check_read_options(iso_files, "raw_data")

  # check whether there are any
  if (length(iso_files) == 0) return(tibble())

  # fetch data
  data <-
    # fetch data
    tibble(
      file_id = names(iso_files),
      raw_data = map(iso_files, ~.x$raw_data)
    ) %>%
    # make sure to include only existing raw data
    filter(!map_lgl(raw_data, is.null)) %>%
    # unnest
    unnest(raw_data)

  # check for rows
  if (nrow(data) == 0) return(dplyr::select(data, .data$file_id))

  # selecting columns
  select_cols <- get_column_names(data, select = select_exp, n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols)
    select_cols <- c("file_id", select_cols) # file id always included
  data <- data %>%
    # focus on selected columns only (also takes care of the rename)
    dplyr::select(!!!select_cols)

  # if gathering
  if (gather) {
    data_cols_re <- "^([^0-9]+)(\\d+/?\\d*)(\\.(.+))?$"
    gather_cols <- stringr::str_subset(names(data), data_cols_re)
    data <- data %>%
      # gather all masses and ratios
      tidyr::pivot_longer(gather_cols, names_to = "column", values_to = "value", values_drop_na = TRUE) %>%
      # extract unit information
      extract(.data$column, into = c("prefix", "data", "extra_parens", "units"), regex = data_cols_re) %>%
      dplyr::select(-.data$extra_parens) %>%
      mutate(
        # units cleanup
        units = ifelse(is.na(units) | nchar(units) == 0, NA_character_, units),
        # assign category
        category = case_when(
          .data$prefix %in% c("i", "v") ~ "mass",
          .data$prefix %in% c("iC", "vC") ~ "channel",
          .data$prefix == "r" ~ "ratio",
          .data$prefix == "d" ~ "delta",
          TRUE ~ "other"),
        data = case_when(
          .data$category %in% c("mass", "channel", "ratio") ~ .data$data,
          .data$category == "delta" ~ paste0("d", .data$data),
          TRUE ~ paste0(.data$prefix, .data$data)
        )
      ) %>%
      dplyr::select(-.data$prefix)
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

  # global vars
  bgrd_data <- NULL

  iso_files <- iso_as_file_list(iso_files)
  if (!all(map_lgl(iso_files, iso_is_dual_inlet))) stop("background data is only available in dual inlet data files", call. = FALSE)
  select_exp <- rlang::enexpr(select)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) {
    glue(
      "Info: aggregating background data from {length(iso_files)} data file(s)",
      "{get_info_message_concat(select_exp, prefix = ', selecting data columns ', empty = 'everything()')}",
      "{get_info_message_concat(include_file_info_quo, prefix = ', including file info ')}") %>% message()
  }
  check_read_options(iso_files, "raw_data")

  # check whether there are any
  if (length(iso_files) == 0) return(tibble())

  # fetch data
  data <-
    # fetch data
    tibble(
      file_id = names(iso_files),
      bgrd_data = map(iso_files, ~.x$bgrd_data)
    ) %>%
    # make sure to include only existing raw data
    filter(!map_lgl(bgrd_data, is.null)) %>%
    # unnest
    unnest(bgrd_data)

  # check for rows
  if (nrow(data) == 0) return(dplyr::select(data, .data$file_id))

  # selecting columns
  select_cols <- get_column_names(data, select = select_exp, n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
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
      extract(.data$column, into = c("category", "data", "extra_parens", "units"), regex = masses_ratios_re) %>%
      dplyr::select(-.data$extra_parens) %>%
      # remove unknown data
      filter(!is.na(.data$value)) %>%
      # assign category
      mutate(category = ifelse(.data$category == "r", "ratio", "mass"))
  }

  # if file info
  if (!quo_is_null(include_file_info_quo)) {
    info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  return(data)
}

#' DEPRECATED
#'
#' Please use \link{iso_get_standards} instead.
#' @param ... forwarded to \link{iso_get_standards}
#'
#' @export
iso_get_standards_info <- function(...) {
  warning("'iso_get_standards_info()' is deprecated in favor of the simpler 'iso_get_standards()'. Please use 'iso_get_standards()' directly to avoid this warning.", immediate. = TRUE, call. = FALSE)
  iso_get_standards(...)
}

#' Aggregate standards from methods info
#'
#' Aggregates the isotopic standard information recovered from the provided iso_files. Can aggregate just the standards' delta values or combine the delta values with the recovered ratios (if any). Use parameter \code{select} to exclude/include the ratios. All standards info is only available if the iso_files were read with parameter \code{read_method_info=TRUE}.
#'
#' @inheritParams iso_get_raw_data
#' @param select which data columns to select - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax. By default, everything is included (both standards and ratios). To omit the ratios, change to \code{select = file_id:reference}.
#' @param with_ratios deprecated, please use the \code{select} parameter to explicitly include or exclude ratio columns
#' @family data retrieval functions
#' @export
iso_get_standards <- function(iso_files, select = everything(), include_file_info = NULL, with_ratios = NULL, quiet = default(quiet)) {

  iso_files <- iso_as_file_list(iso_files)

  # safety checks
  if (iso_is_scan(iso_files))
    stop("scan files don't have standards information", call. = FALSE)
  else if (!iso_is_continuous_flow(iso_files) && !iso_is_dual_inlet(iso_files))
    stop("only dual inlet and continuous flow files can have standards information", call. = FALSE)

  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) {
    sprintf("Info: aggregating standards info from %d data file(s)%s", length(iso_files),
            get_info_message_concat(include_file_info_quo, prefix = ", including file info ")) %>% message()
  }

  # deprecated parameter
  if (!missing(with_ratios)) {
    warning("the 'with_ratios' parameter is deprecated, please use the column selection parameter 'select' to explicitly include or exclude ratio columns", immediate. = TRUE, call. = FALSE)
  }

  check_read_options(iso_files, "method_info")

  # check whether there are any
  if (length(iso_files) == 0) return(tibble())

  # fetch data
  data <-
    # fetch data
    tibble(
      file_id = names(iso_files),
      standards = map(iso_files, ~.x$method_info$standard),
      ref_ratios = map(iso_files, ~.x$method_info$reference_ratios)
    )

  # check for rows
  if (nrow(data) == 0) return(dplyr::select(data, .data$file_id))

  # merge info
  standards <- data %>%
    dplyr::select(.data$file_id, standards) %>%
    dplyr::filter(!map_lgl(.data$standards, is.null)) %>% unnest(.data$standards)
  ref_ratios <- data %>% dplyr::select(.data$file_id, .data$ref_ratios) %>%
      dplyr::filter(!map_lgl(.data$ref_ratios, is.null)) %>%
      tidyr::unnest(.data$ref_ratios)
  if ("reference" %in% names(ref_ratios))
    data <- dplyr::left_join(standards, ref_ratios, by = c("file_id", "reference"))
  else
    data <- standards

  # select columns (only warn if it's not the default and cols don't exist)
  select_exp <- rlang::enexpr(select)
  warn <- rlang::as_label(select_exp) != rlang::as_label(formals(iso_get_standards)$select)
  select_cols <- get_column_names(data, select = select_exp, n_reqs = list(select = "*"), cols_must_exist = FALSE, warn = warn)$select
  if (!"file_id" %in% select_cols)
    select_cols <- c("file_id", select_cols) # file info always included

  # focus on selected columns only (also takes care of the rename)
  data <- dplyr::select(data, !!!select_cols) %>% unique()

  # if file info
  if (!quo_is_null(include_file_info_quo)) {
    info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  return(data)
}


#' DEPRECATED
#'
#' Please use \link{iso_get_resistors} instead.
#'
#' @param ... forwarded to \link{iso_get_resistors}
#'
#' @export
iso_get_resistors_info <- function(...) {
  warning("'iso_get_resistors_info()' is deprecated in favor of the simpler 'iso_get_resistors()'. Please use 'iso_get_resistors()' directly to avoid this warning.", immediate. = TRUE, call. = FALSE)
  iso_get_resistors(...)
}

#' Aggregate resistors from methods info
#'
#' Aggregates the resistor information recovered from the provided iso_files. This information is only available if the iso_files were read with parameter \code{read_method_info=TRUE} and only linked to specific masses if the iso_files were additionally read with parameter \code{read_raw_data=TRUE}.
#'
#' @inheritParams iso_get_raw_data
#' @family data retrieval functions
#' @export
iso_get_resistors  <- function(iso_files, select = everything(), include_file_info = NULL, quiet = default(quiet)) {

  # global vars
  resistors <- NULL

  iso_files <- iso_as_file_list(iso_files)
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) {
    sprintf("Info: aggregating resistors info from %d data file(s)%s", length(iso_files),
            get_info_message_concat(include_file_info_quo, prefix = ", including file info ")) %>% message()
  }

  check_read_options(iso_files, "method_info")

  # check whether there are any files
  if (length(iso_files) == 0) return(tibble())

  # fetch data
  data <-
    # fetch data
    tibble(
      file_id = names(iso_files),
      resistors = map(iso_files, ~.x$method_info$resistors)
    ) %>%
    # make sure to include only existing raw data
    dplyr::filter(!map_lgl(resistors, is.null)) %>%
    # unnest
    tidyr::unnest(resistors)

  # check for rows
  if (nrow(data) == 0) return(dplyr::select(data, .data$file_id))

  # select columns
  select_cols <- get_column_names(data, select = enquo(select), n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols)
    select_cols <- c("file_id", select_cols) # file info always included

  # focus on selected columns only (also takes care of the rename)
  data <- dplyr::select(data, !!!select_cols)

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
#' @param with_units this parameter has been DEPRECATED with the introduction of unit-data types (see \code{\link{iso_double_with_units}}) and will be removed in future versions of isoreader. Please use \code{with_explicit_units} instead if you really want columns to have units explicitly in the column name. Alternatively, consider working with the new implicit unit system and convert vendor data tables as needed with \code{\link{iso_make_units_explicit}} and \code{\link{iso_make_units_implicit}}.
#' @param with_explicit_units whether to include units in the column headers of the returned data frame instead of the column data types (see \code{\link{iso_double_with_units}}). Note that any \code{select} conditions have to refer to the column names including the full units.
#' @family data retrieval functions
#' @export
iso_get_vendor_data_table <- function(
  iso_files, with_units = FALSE,
  select = everything(), include_file_info = NULL,
  with_explicit_units = with_units,
  quiet = default(quiet)) {

  # globals
  dt <- has_units <- NULL
  iso_files <- iso_as_file_list(iso_files)

  # safety checks
  if (iso_is_scan(iso_files))
    stop("scan files don't have vendor data tables", call. = FALSE)
  else if (!iso_is_continuous_flow(iso_files) && !iso_is_dual_inlet(iso_files))
    stop("only dual inlet and continuous flow files can have vendor data tables", call. = FALSE)

  # process
  include_file_info_quo <- enquo(include_file_info)
  if (!quiet) {
    sprintf("Info: aggregating vendor data table%s from %d data file(s)%s",
            if (with_explicit_units) " with explicit units" else "",
            length(iso_files),
            get_info_message_concat(include_file_info_quo, prefix = ", including file info ")) %>% message()
  }
  check_read_options(iso_files, "vendor_data_table")

  # units
  if (!missing(with_units)) {
    warning(
      "The 'use_units' parameter has been DEPRECATED with the introduction of unit-data types (see ?iso_double_with_units) and will be removed in future versions of isoreader. Please use parameter 'with_explicit_units' instead if you really want columns to have units explicitly in the column name. Alternatively, simply remove all units with ?iso_strip_units or consider working with the new implicit unit system and convert vendor data tables as needed with ?iso_make_units_explicit",
      call. = FALSE, immediate. = TRUE)
  }

  # check whether there are any files
  if (length(iso_files) == 0) return(tibble())

  # get vendor data
  column <- units <- NULL # global vars

  # fetch data
  vendor_data_table <-
    # fetch data
    tibble(
      file_id = names(iso_files),
      dt = map(iso_files, ~.x$vendor_data_table)
    ) %>%
    # make sure to include only existing data
    filter(map_lgl(dt, ~!is.null(.x) & nrow(.x) > 0))

  # check for any rows
  if (nrow(vendor_data_table) == 0) return(dplyr::select(vendor_data_table, .data$file_id))

  # make units explicit if wanted
  if (with_explicit_units) {
    vendor_data_table <- vendor_data_table %>%
      mutate(dt = map(dt, iso_make_units_explicit))
  }

  # unnest
  vendor_data_table <- dplyr::select(vendor_data_table, .data$file_id, .data$dt) %>% unnest(.data$dt)

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


# Binding disparate data frames ========

# safely bind rows by converting all columns to list columns if they aren't already
# works both with lists and data frames (or mixed)
# this is typically followed by unnest_aggreated_data_frame
safe_bind_rows <- function(df_list, exclude = names(make_iso_file_data_structure()$file_info)) {
  vctrs::vec_rbind(!!!map(df_list, ensure_data_frame_list_columns, exclude = exclude))
}

# make sure that data frame columns are list columns (except those listed as exclude)
# this is to allow for safely binding rows of data frames with unpredictable data types in the same columns
# @param x data frame or list
# @param exclude names of columns to leave the way they are
# @note - could this use the new tidyr::chop more effectively?
ensure_data_frame_list_columns <- function(x, exclude = names(make_iso_file_data_structure()$file_info)) {
  # make sure all columns are ready
  cols_to_list <- names(x)[!map_lgl(x, is.list)] %>% setdiff(exclude)
  if(length(cols_to_list) > 0) {
    func <- if (is.data.frame(x)) as.list else list
    x[cols_to_list] <- map(x[cols_to_list], func)
  }
  # convert list to tibble
  if (!is.data.frame(x)) x <- dplyr::as_tibble(x)
  return(x)
}

# helper function to unnest aggregated columns that have single or no values and the same data types
# note: this would be nice to do with the new tidyr::unnest() but it does not handle some of the
# the contingencies the right way (multiple values, NAs, etc.) so staying with the original appraoch
# which is still decently fast
# NOTE: consider implementing with vctrs::vec_ptype_common if it's faster that way
unnest_aggregated_data_frame <- function(df) {

  # global vars
  column <- min_length <- main_class <- has_identical_class <- max_length <- identical_class <- is_missing <- NULL

  # safety
  stopifnot(is.data.frame(df))
  if (nrow(df) == 0) return(df)

  # NA defaults
  NA_defaults <- list(character = NA_character_, numeric = NA_real_, integer = NA_integer_, logical = NA)

  # get information about the data frame columns
  cols <-
    tibble(
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

  # warning message about inconsistent data columns with multiple data types
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
                       # have to switch to int b/c there is no map2_datetime yet
                       map2_int(!!sym(cols$column[i]), cols$is_missing[[i]],
                                # NA_integer_ is okay here because of the as_datetime wrapper afterwards
                                ~if (.y) { NA_integer_ } else { as.integer(.x[1]) }) %>%
                       as_datetime(tz = Sys.timezone()))
      else if (cols$identical_class[i] == "iso_double_with_units")
        df <- mutate(df, !!cols$column[i] :=
                       do.call(
                         vctrs::vec_c,
                         map2(!!sym(cols$column[i]), cols$is_missing[[i]],
                              ~if(.y) { NA } else {.x[1]})
                       )
        )
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
