# file types & reader =================

#' Register file readers
#' 
#' Register file extensions and reader functions for different data files. Isoreader automatically registers all built-in file readers so this function is usually only needed when registering additional readers provided for testing purposes from outside of the isoreader package.
#' 
#' @details \code{iso_register_dual_inlet_file_reader}: use this function to register file readers for dual inlet files.
#' 
#' @rdname file_readers
#' @param extension the file extension (e.g. \code{.dxf}) of the data file. Must be unique otherwise different files can not automatically be matched with the appropriate file reader based on their extension.
#' @param func the name of the function that should be used a filter reader. All file reader functions must accept a data structure argument as the first argument and return the same data structure with added data.
#' @param description what is this file type about?
#' @param cacheable whether this file type is cacheable. If \code{TRUE} (the default), user requests to cache the file will be honored. If \code{FALSE}, this file type will never be cached no matter what the user requests.
#' @param overwrite whether to overwrite an existing file reader for the same extension
#' @family file_types
#' @export
iso_register_dual_inlet_file_reader <- function(extension, func, description = NA_character_, cacheable = TRUE, overwrite = FALSE) {
  register_file_reader("dual inlet", "iso_read_dual_inlet", extension, func, description, cacheable, overwrite)
}

#' @details \code{iso_register_continuous_flow_file_reader}: use this function to register file readers for continuous flow files.
#' @rdname file_readers
#' @family file_types
iso_register_continuous_flow_file_reader <- function(extension, func, description = NA_character_, cacheable = TRUE, overwrite = FALSE) {
  register_file_reader("continuous flow", "iso_read_continuous_flow", extension, func, description, cacheable, overwrite)
}

register_file_reader <- function(type, call, extension, func, description, cacheable, overwrite) {

  if (!is.character(func))
    stop("please provide the function name rather than the function itself to register it",
         call. = FALSE)
  
  frs <- default("file_readers", allow_null = TRUE)
  new_fr <-
    dplyr::data_frame(
      type = type, call = call, extension = extension,
      func = func, cacheable = cacheable, description = description
    )
  
  if (!is.null(frs) && extension %in% frs$extension) {
    if (identical(new_fr, dplyr::filter(frs, extension == !!extension))) {
      # already exists and is identical, nothing more to do
      return(frs)
    }
    
    if (!overwrite) {
      # already exists but don't overwrite --> error
      glue::glue(
        "file reader for extension '{extension}' already exists, specify overwrite = TRUE to replace the existing file reader"
      ) %>%
      stop(call. = FALSE)
    } 
    
    # already exists and will be overwritten
    glue::glue("file reader for extension '{extension}' already exists and will be overwritten") %>%
      warning(immediate. = TRUE, call. = FALSE)
    frs <- dplyr::filter(frs, extension != !!extension)
  }
  set_default("file_readers", dplyr::bind_rows(frs, new_fr))
  default("file_readers")
}

#' Supported file types
#' 
#' Get an overview of all the file types currently supported by the isoreader package. To register additional file readers, use the \code{\link{iso_register_dual_inlet_file_reader}} and \code{\link{iso_register_continuous_flow_file_reader}} functions.
#' 
#' @family file_types
#' @export
iso_get_supported_file_types <- function() {
  dplyr::select(default("file_readers"), extension, description, type, call)
}

get_supported_di_files <- function() {
  dplyr::filter(default("file_readers"), type == "dual inlet")
}

get_supported_cf_files <- function() {
  dplyr::filter(default("file_readers"), type == "continuous flow")
}

# file reading ===========

#' Read isotope data file
#' 
#' This function from the original isoread package is deprecated, please use \link{iso_read_dual_inlet}, \link{iso_read_continuous_flow} and \link{iso_read_scan} instead.
#'
#' @param ... original isoread parameters
#' @export
isoread <- function(...) {
  stop(
    "Deprecated, use iso_read_dual_inlet(), iso_read_continuous_flow() or iso_read_scan() instead.",
    call. = FALSE)
}

#' Load dual inlet data
#' 
#' @inheritParams iso_read_files
#' @param ... one or multiple file/folder paths. All files must have a supported file extension. All folders are expanded and searched for files with supported file extensions (which are then included in the read).
#' @param read_raw_data whether to read the raw mass/ion data from the file
#' @param read_file_info whether to read auxiliary file information (file id, sequence information, etc.)
#' @param read_method_info whether to read methods information (standards, processing info)
#' @param read_vendor_data_table whether to read the vendor computed data table
#' @family isoread functions for different types of IRMS data
#' @export
iso_read_dual_inlet <- function(
  ..., 
  read_raw_data = default(read_raw_data), read_file_info = default(read_file_info), 
  read_method_info = default(read_method_info), read_vendor_data_table = default(read_vendor_data_table),
  discard_duplicates = TRUE, parallel = FALSE, cache = default(cache), read_cache = default(cache), quiet = default(quiet)) {
  
  # process data
  iso_read_files(
    unlist(list(...), use.names = FALSE),
    supported_extensions = get_supported_di_files(),
    data_structure = make_di_data_structure(),
    read_raw_data = read_raw_data,
    read_file_info = read_file_info,
    read_method_info = read_method_info,
    read_vendor_data_table = read_vendor_data_table,
    discard_duplicates = discard_duplicates,
    parallel = parallel,
    cache = cache,
    read_cache = read_cache,
    quiet = quiet
  )
}

#' Load continuous flow data
#' 
#' @inheritParams iso_read_dual_inlet
#' @family isoread functions for different types of IRMS data
#' @export
iso_read_continuous_flow <- function(
  ..., 
  read_raw_data = default(read_raw_data), read_file_info = default(read_file_info), 
  read_method_info = default(read_method_info), read_vendor_data_table = default(read_vendor_data_table), 
  discard_duplicates = TRUE, parallel = FALSE, cache = default(cache), read_cache = default(cache), quiet = default(quiet)) {
  
  # process data
  iso_read_files(
    unlist(list(...), use.names = FALSE),
    supported_extensions = get_supported_cf_files(),
    data_structure = make_cf_data_structure(),
    read_raw_data = read_raw_data,
    read_file_info = read_file_info,
    read_method_info = read_method_info,
    read_vendor_data_table = read_vendor_data_table,
    discard_duplicates = discard_duplicates,
    parallel = parallel,
    cache = cache,
    read_cache = read_cache,
    quiet = quiet
  )
}

#' Core function to read isotope data files
#' 
#' This function takes care of extracting basic information about iso_files, dealing with problems and making sure only valid fire formats are processed. 
#' This function is not typicaly called directly but indirectly by calling \link{iso_read_dual_inlet}, \link{iso_read_continuous_flow} and \link{iso_read_scan}. 
#' It is made available outside the package because it can be very useful for testing new file readers.
#' 
#' @param paths one or multiple file/folder paths. All files must have a supported file extension. All folders are expanded and searched for files with supported file extensions (which are then included in the read).
#' @param supported_extensions data frame with supported extensions and corresponding reader functions (columns 'extension', 'func', 'cacheable')
#' @param data_structure the basic data structure for the type of iso_file
#' @inheritParams iso_as_file_list
#' @param parallel whether to process in parallel based on the number of available CPU cores. This may yield performance increases for files that are slow to parse such as continuous flow isodat files but usually provides little benefit for efficient data formats such as reading from R Data Archives.
#' @param quiet whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages. Set parameter to overwrite global defaults for this function or set global defaults with calls to \link[=iso_info_messages]{iso_turn_info_message_on} and \link[=iso_info_messages]{iso_turn_info_message_off}
#' @param cache whether to cache iso_files. Note that previously exported R Data Archives (di.rda, cf.rda) are never cached since they are already essentially in cached form.
#' @param read_cache whether to reload from cache if a cached version exists. Note that it will only read from cache if the file was previously read with the exact same isoreader version and read options and has not been modified since.
#' @param ... read options to be stored in the data structure as read options
#' @return single iso_file object (if single file) or list of iso_files (iso_file_list)
iso_read_files <- function(paths, supported_extensions, data_structure, ..., discard_duplicates = TRUE, parallel = FALSE, cache = default(cache), read_cache = default(cache), quiet = default(quiet)) {

  # start timer
  start_time <- Sys.time()
  
  # set quiet for the current and sub-calls and reset back to previous setting on exit
  on_exit_quiet <- update_quiet(quiet)
  on.exit(on_exit_quiet(), add = TRUE)
  
  # parallel processing
  if (parallel) {
    threads <- availableCores()
    oplan <- plan(multiprocess)
  } else {
    threads <- 1
    oplan <- plan(sequential)
  }
  on.exit(plan(oplan), add = TRUE)
  
  # supplied data checks
  col_check(c("extension", "func", "cacheable"), supported_extensions)
  if(!is(data_structure, "iso_file")) stop("data structure must include class 'iso_file'", call. = FALSE)
  col_check(c("file_info"), data_structure)
  
  # global vars
  filepath <- ext <- NULL
  
  # read options update in data structure
  data_structure <- update_read_options(data_structure, ...)
  
  # expand & safety check paths (will error if non-suppored file types are included or same filename occurs multiple times)
  if (missing(paths) || is.null(paths) || is.na(paths)) stop("file path(s) required", call. = FALSE)
  filepaths <- expand_file_paths(paths, supported_extensions$extension)
  
  # overview
  if (!quiet) {
    glue::glue(
      "Info: preparing to read {length(filepaths)} data file(s)",
      if (parallel) { " in parallel using {threads} available cores..." } 
      else {"..."}) %>% 
      message()
  }
  
  # check if there are any
  if (length(filepaths) == 0) 
    return(iso_as_file_list(list()))
  
  # generate read files overview
  files <- 
    data_frame(
      filepath = filepaths,
      cachepath = generate_cache_filepaths(filepath, data_structure$read_options),
      file_n = 1:length(filepaths),
      batch = file_n %/% threads
    ) %>% 
    # merge in supported extensions with reader and cacheable info
    match_to_supported_file_types(supported_extensions) 
    
  # safety check on reader functions
  req_readers <- unique(files$func)
  in_workspace <- map_lgl(req_readers, exists, mode = "function")
  in_isoreader_ns <- map_lgl(req_readers, exists, mode = "function", where = asNamespace("isoreader"))
  if ( any(missing <- !in_workspace && !in_isoreader_ns) ) {
    stop("required reader function(s) does not seem to exist: ", 
         str_c(req_readers[missing], collapse = ", "), call. = FALSE)
  }
  
  # read function
  read_iso_file <- function(filepath, cachepath, ext, reader_fun, cacheable, file_n) {
    
    # prepare iso_file object
    iso_file <- set_ds_file_path(data_structure, filepath)
    
    # read/write cache?
    read_from_cache <- read_cache && cacheable && file.exists(cachepath)
    write_to_cache <- cache && cacheable
    
    # user info
    if (!quiet) {
      if (read_from_cache) { 
        glue("Info: reading file {file_n}/{nrow(files)} '{filepath}' from cache") %>%
        message(appendLF = threads > 1)
      } else {
        glue(
          "Info: reading{if (write_to_cache) ' and caching' else ''} ",
          "file {file_n}/{nrow(files)} '{filepath}' with '{ext}' reader") %>% 
        message(appendLF = threads > 1)
      }
      if (threads == 1) message("...", appendLF = FALSE)
    }
    
    
    # evaluate read file event quosure if it exists
    read_file_event <- getOption("isoreader.read_file_event")
    if (!is.null(read_file_event) && is_quosure(read_file_event) && !quo_is_null(read_file_event)) {
      eval_tidy(get_expr(read_file_event))
    }
    
    # run as future
    future(
      globals = list(
        iso_file = iso_file, reader_fun = reader_fun, cachepath = cachepath,
        read_from_cache = read_from_cache, write_to_cache = write_to_cache),
      expr = {
      
      if (read_from_cache) {
        # read from cache
        iso_file <- load_cached_iso_file(cachepath)
      } else {
        # read from original file
        iso_file <- exec_func_with_error_catch(reader_fun, iso_file)
        
        # cleanup any binary content depending on debug setting
        if (!default(debug)) iso_file$binary <- NULL
        
        # store in cached file
        if (write_to_cache) cache_iso_file(iso_file, cachepath)
      }
      
      return(iso_file)
    })
  }

  # finish function
  finish_iso_file <- function(filepath, file_n) {
    if (!quiet) message(".", file_n, "\U2713", appendLF = FALSE)
    # evaluate finish file event quosure if it exists
    finish_file_event <- getOption("isoreader.finish_file_event")
    if (!is.null(finish_file_event) && is_quosure(finish_file_event) && !quo_is_null(finish_file_event)) {
      eval_tidy(get_expr(finish_file_event))
    }
  }
  
  # generate futures
  iso_files <- list()
  for (i in unique(files$batch)) {
    
    batch <- filter(files, batch == i)
    finished <- rep(FALSE, nrow(batch))
    futures <- 
      with(batch,
           mapply(
             read_iso_file,
             filepath = filepath,
             cachepath = cachepath,
             ext = extension,
             reader_fun = func,
             cacheable = cacheable,
             file_n = file_n
           )
      )

    # query futures status
    while (TRUE) {
      is_finished <- purrr::map_lgl(futures, resolved)
      newly_finished <- which(finished != is_finished)
      if (length(newly_finished) > 0) {
        with(batch, purrr::walk2(filepath[newly_finished], file_n[newly_finished], finish_iso_file))
        finished <- is_finished
      }
      if (all(finished)) break
      # progress
      if (!quiet) message(".", appendLF = FALSE)
      Sys.sleep(0.1) 
    }
    if (!quiet) message() # newline
    
    # retrieve values from futures
    iso_files <- c(iso_files, lapply(futures, value))
  }
  
  # finish time
  end_time <- Sys.time()
  if (!quiet) {
    sprintf(
      "Info: finished reading %s files in %.2f %s",
      nrow(files), as.numeric(end_time - start_time), 
      attr(end_time - start_time, "units")) %>% 
    message()
  }
  
  # turn into iso_file list
  iso_files <- iso_as_file_list(iso_files, discard_duplicates = discard_duplicates) 

  # convert file_info to data frame in isofiles for faster access
  # @note: this is not quite ideal because it basically casts iso_as_file_list twice if there are any files that have non-data frame file_info
  iso_files <- convert_file_info_to_data_frame(iso_files, discard_duplicates = discard_duplicates)
  
  # report problems
  if (!default(quiet) && iso_has_problems(iso_files)) {
    message(sprintf("Info: encountered %.0f problems in total", n_problems(iso_files)))
    print(problems(iso_files))
    cat("\n")
  }
  
  # return single or file or list
  if (length(iso_files) == 1) return (iso_files[[1]])
  return(iso_files)
}

#' Re-read iso_files
#' 
#' Sometimes it is useful to reload isotope files from their original data files (e.g. after upgrading to a newer version of the isoreader package). 
#' The functions described below are intended to make this very easy. 
#' However, it is only possible for iso_file objects whose file paths still point to the original raw data files.
#' 
#' @details \code{iso_reread_files} will re-read all the original data files for the passed in \code{iso_files} object. Returns the reread iso_file objects.
#' @inheritParams iso_get_raw_data
#' @param ... additional read parameters that should be used for re-reading the iso_files, see \code{\link{iso_read_dual_inlet}} and \code{\link{iso_read_continuous_flow}} for details
#' @param stop_if_missing whether to stop re-reading if any of the original data files are missing (if FALSE, will warn about the missing files adding a warning to them, but also re-read those that do exist)
#' @note re-reading files with their original read parameters is not yet supported
#' @export
iso_reread_files <- function(iso_files, ..., stop_if_missing = FALSE, quiet = default(quiet)) {
  
  # checks
  if(!iso_is_object(iso_files)) stop("can only re-read iso_files", call. = FALSE)
  single_file <- iso_is_file(iso_files) # to make sure return is the same as supplied
  iso_files <- iso_as_file_list(iso_files)
  
  # reread
  filepaths <- get_reread_filepaths(iso_files)
  files_exist <- filepaths %>% map_lgl(file.exists)
  
  # overview
  if (!default(quiet)) {
    message("Info: re-reading ", length(filepaths), " data file(s)...")
  }
  
  # safety check for non existent data files
  if (!all(files_exist)) {
    msg <- sprintf("%d file(s) do no longer exist at the referenced location and can not be re-read:\n - %s\n",
                   sum(!files_exist), str_c(filepaths[!files_exist], collapse = "\n - "))
    if (stop_if_missing) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE, immediate. = TRUE)
      iso_files[!files_exist] <- map(iso_files[!files_exist], register_warning, func = "iso_reread_files", 
                                     details = "file does not exist at its original location and can not be re-read",
                                     warn = FALSE)
      
    }
  }
  
  # reread files
  # @FIXME: if no read parameters are supplied, re-read with the original ones
  if (any(files_exist)) {
    args <- c(list(paths = filepaths[files_exist]), list(...))
    if (iso_is_continuous_flow(iso_files)) {
      # read continuous flow
      new_iso_files <- iso_as_file_list(do.call(iso_read_continuous_flow, args = args))
    } else if (iso_is_dual_inlet(iso_files)) {
      # read dual inlet
      new_iso_files <- iso_as_file_list(do.call(iso_read_dual_inlet, args = args))
    } else {
      stop("re-reading iso_files objects of type ", class(iso_files[[1]])[1], " is not yet supported", call. = FALSE)
    }
    
    # replace the ones that were re-read (and add new files in case there were any e.g. from updated iarc archives)
    overlap_ids <- names(iso_files)[names(iso_files) %in% names(new_iso_files)]
    new_ids <- names(new_iso_files)[!names(new_iso_files) %in% names(iso_files)]
    for(id in overlap_ids) iso_files[[id]] <- new_iso_files[[id]]
    if (length(new_ids) > 0) iso_files <- c(iso_files, new_iso_files[new_ids])
  }
  
  # return single (if passed in as single) 
  if (single_file && length(iso_files) == 1) return (iso_files[[1]])
  return(iso_files)
}

#' @details \code{iso_reread_archive} is a convenience function for refreshing saved iso_file collections. It will load a specific iso_files R Data Archive (\code{rda_filepath}), re-read all the data from the original data files and save the collection back to the same rda file. The iso_files are returned invisibly.
#' @rdname iso_reread_files
#' @param rda_filepaths the path(s) to the iso_files R data archive(s) to re-read (can be a single file or vector of files)
#' @export
iso_reread_archive <- function(rda_filepaths, ..., stop_if_missing = FALSE, quiet = default(quiet)) {
  
  extensions <- iso_get_supported_file_types() %>% dplyr::filter(stringr::str_detect(extension, "rda"))
  file_types <- data_frame(filepath = rda_filepaths) %>% match_to_supported_file_types(extensions)
  
  if (any(missing <- !file.exists(rda_filepaths))) 
    stop("file(s) do not exist: ", str_c(rda_filepaths[missing], collapse = ", "), call. = FALSE)
  
  reread_archive <- function(filepath, call) {
    if(!quiet) message("Info: loading R Data Archive ", basename(filepath), "...")
    suppressWarnings(do.call(call, args = list(paths = filepath, quiet=TRUE))) %>% 
      iso_reread_files(..., stop_if_missing = stop_if_missing, quiet=quiet) %>% 
      iso_export_to_rda(filepath = filepath, quiet=quiet)
    return(TRUE)
  }
  
  # note: cannot combine these in case some of them are dual inlet while others are continuous flow
  with(file_types, mapply(reread_archive, filepath, call))
  invisible(NULL)
}


