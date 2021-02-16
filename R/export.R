## Export functions =======

#' Export data to Excel
#'
#' This function exports the passed in iso_files to Excel. The different kinds of data (raw data, file info, methods info, etc.) are exported to separate tabs within the excel file. Use the various \code{include_...} parameters to specify what information to include. Note that in rare instances where vectorized data columns exist in the file information (e.g. measurement_info), they are concatenated with ', ' in the excel export. Note that the openxlsx package required for this export is not installed automatically as part of isoreader. Please install it manually if missing using \code{install.packages("openxlsx")}.
#'
#' @inheritParams iso_save
#' @inheritParams iso_get_all_data
#' @param include_method_info deprecated in favor of the more specific include_standards and include_resistors
#' @family export functions
#' @return returns the iso_files object invisibly for use in pipelines
#' @export
iso_export_to_excel <- function(
  iso_files, filepath,
  include_file_info = everything(), include_raw_data = everything(),
  include_standards = !!enexpr(include_method_info), include_resistors = !!enquo(include_method_info),
  include_vendor_data_table = everything(), include_problems = everything(),
  with_explicit_units = FALSE,
  include_method_info = everything(),
  with_ratios = NULL,
  quiet = default(quiet)) {

  # check for availability
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning(
      "The 'openxlsx' package is required to export to this file format, please install it using the following command: install.packages(\"openxlsx\")",
      immediate. = TRUE, call. = FALSE
    )
    return(invisible(iso_files))
  }
  
  # safety checks
  if(!iso_is_object(iso_files)) stop("can only export iso files or lists of iso files", call. = FALSE)
  export_iso_files <- iso_as_file_list(iso_files)
  filepath <- get_excel_export_filepath(export_iso_files, filepath)

  # info message
  if (!quiet) {
    sprintf("Info: exporting data from %d iso_files into Excel '%s'", length(export_iso_files),
            str_replace(filepath, "^\\.(/|\\\\)", "")) %>% message()
  }

  # include method info message
  if (!missing(include_method_info)) {
    warning("the 'include_method_info' parameter was deprecated in favor of the more specific 'include_resistors' and 'include_standards' parameters. Please use those directly instead in the future.", immediate. = TRUE, call. = FALSE)
  }

  # deprecated parameter
  if (!missing(with_ratios)) {
    warning("the 'with_ratios' parameter is deprecated, please use the column selection parameter 'include_standards' to explicitly include or exclude ratio columns", immediate. = TRUE, call. = FALSE)
  }

  # get all data
  all_data <- iso_get_all_data(
    export_iso_files,
    include_file_info = !!enexpr(include_file_info),
    include_raw_data = !!enexpr(include_raw_data),
    include_standards = !!enexpr(include_standards),
    include_resistors = !!enexpr(include_resistors),
    include_vendor_data_table = !!enexpr(include_vendor_data_table),
    include_problems = !!enexpr(include_problems),
    with_explicit_units = with_explicit_units,
    quiet = FALSE
  )

  # make excel workbook
  wb <- openxlsx::createWorkbook()

  # file info
  if ("file_info" %in% names(all_data)) {
    # note: collapse_list_columns takes care of nested vectors, they get concatenated with ', '
    file_info <-
      all_data %>% select(.data$file_id, .data$file_info) %>%
      unnest(.data$file_info) %>%
      collapse_list_columns()
    add_excel_sheet(wb, "file info", file_info)
  }

  # raw data
  if ("raw_data" %in% names(all_data)) {
    raw_data <- all_data %>% select(.data$file_id, .data$raw_data) %>% unnest(.data$raw_data)
    add_excel_sheet(wb, "raw data", raw_data)
  }

  # standards
  if ("standards" %in% names(all_data)) {
    standards <- all_data %>% select(.data$file_id, standards) %>% unnest(standards)
    add_excel_sheet(wb, "standards", standards)
  }

  # resistors
  if ("resistors" %in% names(all_data)) {
    resistors <- all_data %>% select(.data$file_id, .data$resistors) %>% unnest(.data$resistors)
    add_excel_sheet(wb, "resistors", resistors)
  }

  # vendor data table
  if ("vendor_data_table" %in% names(all_data)) {
    vendor_data <- all_data %>% select(.data$file_id, .data$vendor_data_table) %>%
      unnest(.data$vendor_data_table) %>% iso_strip_units()
    add_excel_sheet(wb, "vendor data table", vendor_data)
  }

  # problems
  if ("problems" %in% names(all_data)) {
    problems <- all_data %>% select(.data$file_id, .data$problems) %>% unnest(.data$problems)
    add_excel_sheet(wb, "problems", problems)
  }
  openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)

  return(invisible(iso_files))
}

# add an excel sheet to a workbook
# @param ... the data frames
# @param dbl_digits how many digits to export for dbls
# @param col_max_width maximum column width
add_excel_sheet <- function(wb, sheet_name, ..., dbl_digits = 2, col_max_width = 75) {

  # sheet
  openxlsx::addWorksheet(wb, sheet_name)
  hs <- openxlsx::createStyle(textDecoration = "bold") # header style

  # data
  sheet_data_sets <- list(...)
  start_row <- 1L
  for (sheet_data in sheet_data_sets) {
    if (ncol(sheet_data) > 0) {
      openxlsx::writeData(wb, sheet_name, sheet_data, startRow = start_row, headerStyle = hs)
      int_cols <- which(purrr::map_lgl(sheet_data, is.integer))
      dbl_cols <- setdiff(which(purrr::map_lgl(sheet_data, is.numeric)), int_cols)
      if (dbl_digits < 1) {
        int_cols <- c(int_cols, dbl_cols)
        dbl_cols <- integer()
      }
      # integer column formatting
      if (length(int_cols) > 0) {
        openxlsx::addStyle(
          wb, sheet_name, style = openxlsx::createStyle(numFmt = "0"),
          rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
          cols = int_cols, gridExpand = TRUE)
      }
      # double column formatting
      if (length(dbl_cols) > 0) {
        dbl_format <- paste0("0.", paste(rep("0", dbl_digits), collapse = ""))
        openxlsx::addStyle(
          wb, sheet_name, style = openxlsx::createStyle(numFmt = dbl_format),
          rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
          cols = dbl_cols, gridExpand = TRUE)
      }
      # new start row
      start_row <- start_row + nrow(sheet_data) + 2L
    }
  }

  # calculate header widths
  header_widths <-
    sheet_data_sets %>%
    # account for bold width
    purrr::map(~nchar(names(.x)))
  max_n_cols <- purrr::map_int(header_widths, length) %>% max()

  # calculate data widths
  if (max_n_cols > 0) {
    calculate_data_width <- function(x) {
      if (is.integer(x)) x <- sprintf("%d", x)
      else if (is.numeric(x)) x <- sprintf(paste0("%.", dbl_digits, "f"), x)
      else x <- as.character(x)
      return(max(c(0, nchar(x)), na.rm = TRUE))
    }
    data_widths <-
      sheet_data_sets %>%
      purrr::map(
        ~dplyr::summarise_all(.x, list(calculate_data_width)) %>%
          unlist(use.names = FALSE)
      )
    max_widths <- purrr::map2(header_widths, data_widths , ~{
      widths <- if (is.null(.y)) .x else pmax(.x, .y, 0)
      widths <- pmin(col_max_width, widths)
      c(widths, rep(0L, times = max_n_cols - length(widths)))
    })
    col_widths <- do.call(pmax, args = max_widths)
    openxlsx::setColWidths(wb, sheet_name, cols = 1:length(col_widths), widths = col_widths)
  }

}

#' Export to feather
#'
#' This function exports the passed in iso_files to the Python and R shared feather file format. The different kinds of data (raw data, file info, methods info, etc.) are exported to separate feather files that are saved with the provided \code{filepath_prefix} as prefix. All are only exported if the corresponding \code{include_} parameter is set to \code{TRUE} and only for data types for which this type of data is available and was read (see \code{\link{iso_read_dual_inlet}}, \code{\link{iso_read_continuous_flow}} for details on read parameters). Note that in rare instances where vectorized data columns exist in the file information (e.g. measurement_info), they are concatenated with ', ' in feather output. Note that the feather package required for this export is not installed automatically as part of isoreader. Please install it manually if missing using \code{install.packages("feather")}.
#'
#' @inheritParams iso_save
#' @inheritParams iso_export_to_excel
#' @param filepath_prefix what to use as the prefix for the feather file names (e.g. name of the data collection or current date)
#' @family export functions
#' @return returns the iso_files object invisibly for use in pipelines
#' @export
iso_export_to_feather <- function(
  iso_files, filepath_prefix,
  include_file_info = everything(), include_raw_data = everything(),
  include_standards = !!enexpr(include_method_info), include_resistors = !!enquo(include_method_info),
  include_vendor_data_table = everything(), include_problems = everything(),
  with_explicit_units = FALSE,
  include_method_info = everything(),
  quiet = default(quiet)) {

  # check for availability
  if (!requireNamespace("feather", quietly = TRUE)) {
    warning(
      "The 'feather' package is required to export to this file format, please install it using the following command: install.packages(\"feather\")",
      immediate. = TRUE, call. = FALSE
    )
    return(invisible(iso_files))
  }
  
  # safety checks
  if(!iso_is_object(iso_files)) stop("can only export iso files or lists of iso files", call. = FALSE)
  export_iso_files <- iso_as_file_list(iso_files)
  filepaths <- get_feather_export_filepaths(export_iso_files, filepath_prefix)

  # include method info message
  if (!missing(include_method_info)) {
    warning("the 'include_method_info' parameter was deprecated in favor of the more specific 'include_resistors' and 'include_standards' parameters. Please use those directly instead in the future.", immediate. = TRUE, call. = FALSE)
  }

  # info
  if (!quiet) {
    sprintf("Info: exporting data from %d iso_files into %s files at '%s'", length(iso_as_file_list(iso_files)),
            filepaths[['ext']], str_replace(filepaths[['base']], "^\\.(/|\\\\)", "")) %>% message()
  }

  # get all data
  all_data <- iso_get_all_data(
    export_iso_files,
    include_file_info = !!enexpr(include_file_info),
    include_raw_data = !!enexpr(include_raw_data),
    include_standards = !!enexpr(include_standards),
    include_resistors = !!enexpr(include_resistors),
    include_vendor_data_table = !!enexpr(include_vendor_data_table),
    include_problems = !!enexpr(include_problems),
    with_explicit_units = with_explicit_units,
    quiet = FALSE
  )

  # create feather files in temporary dir
  # file info
  if ("file_info" %in% names(all_data)) {
    # note: collapse_list_columns takes care of nested vectors, they get concatenated with ', '
    all_data %>% select(.data$file_id, .data$file_info) %>%
      unnest(.data$file_info) %>%
      collapse_list_columns() %>%
      feather::write_feather(filepaths[['file_info']])
  }

  # raw data
  if ("raw_data" %in% names(all_data)) {
    all_data %>% select(.data$file_id, .data$raw_data) %>% unnest(.data$raw_data) %>%
      feather::write_feather(filepaths[['raw_data']])
  }

  # standards
  if ("standards" %in% names(all_data)) {
   all_data %>% select(.data$file_id, .data$standards) %>% unnest(.data$standards) %>%
      feather::write_feather(filepaths[['method_info_standards']])
  }

  # resistors
  if ("resistors" %in% names(all_data)) {
    all_data %>% select(.data$file_id, .data$resistors) %>% unnest(.data$resistors) %>%
      feather::write_feather(filepaths[['method_info_resistors']])
  }

  # vendor data table
  if ("vendor_data_table" %in% names(all_data)) {
    all_data %>% select(.data$file_id, .data$vendor_data_table) %>%
      unnest(.data$vendor_data_table) %>% iso_strip_units() %>%
      feather::write_feather(filepaths[['vendor_data_table']])
  }

  # problems
  if ("problems" %in% names(all_data)) {
   all_data %>% select(.data$file_id, .data$problems) %>% unnest(.data$problems) %>%
      feather::write_feather(filepaths[['problems']])
  }

  return(invisible(iso_files))
}


# utility functions ====

# convenience function for export file paths (extension checks and addition)
get_export_filepath <- function(filepath, ext) {
  # file name and folder
  if (missing(filepath)) stop("no filepath provided", call. = FALSE)
  filename <- basename(filepath)
  folder <- dirname(filepath)
  if (!file.exists(folder)) stop("the folder '", folder, "' does not exist", call. = FALSE)
  if (!is.null(ext))
    filename <- filename %>% str_replace(fixed(ext), "") %>% str_c(ext) # to make sure correct extension
  return(file.path(folder, filename))
}

# excel export filephat
get_excel_export_filepath <- function(iso_files, filepath) {
  if (iso_is_continuous_flow(iso_files))
    ext <- ".cf.xlsx"
  else if (iso_is_dual_inlet(iso_files))
    ext <- ".di.xlsx"
  else if (iso_is_scan(iso_files))
    ext <- ".scan.xlsx"
  else
    stop("Excel export of this type of iso_files not yet supported", call. = FALSE)
  return(get_export_filepath(filepath, ext))
}

# feather export filepath
get_feather_export_filepaths <- function(iso_files, filepath) {
  if (iso_is_continuous_flow(iso_files))
    ext <- ".cf.feather"
  else if (iso_is_dual_inlet(iso_files))
    ext <- ".di.feather"
  else if (iso_is_scan(iso_files))
    ext <- ".scan.feather"
  else
    stop("Feather export of this type of iso_files not yet supported", call. = FALSE)

  filepath <- get_export_filepath(filepath, NULL)
  return(
    c(
      base = filepath,
      ext = ext,
      raw_data = str_c(filepath, "_raw_data", ext),
      file_info = str_c(filepath, "_file_info", ext),
      method_info_standards = str_c(filepath, "_standards", ext),
      method_info_resistors = str_c(filepath, "_resistors", ext),
      vendor_data_table = str_c(filepath, "_vendor_data_table", ext),
      problems = str_c(filepath, "_problems", ext)
    )
  )
}
