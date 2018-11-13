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
#' @param env the environment where to find the function, by default this will be determined automatically and will throw an error if there is any ambiguity (e.g. the same function name in multiple packages) in which case it should be set manually
#' @family file_types
#' @export
iso_register_dual_inlet_file_reader <- function(
  extension, func, description = NA_character_, cacheable = TRUE, overwrite = FALSE, env = find_func(func)) {
  register_file_reader("dual inlet", "iso_read_dual_inlet", extension, func, description, cacheable, overwrite, env)
}

#' @details \code{iso_register_continuous_flow_file_reader}: use this function to register file readers for continuous flow files.
#' @rdname file_readers
#' @family file_types
iso_register_continuous_flow_file_reader <- function(
  extension, func, description = NA_character_, cacheable = TRUE, overwrite = FALSE, env = find_func(func)) {
  register_file_reader("continuous flow", "iso_read_continuous_flow", extension, func, description, cacheable, overwrite, env)
}

register_file_reader <- function(type, call, extension, func, description, cacheable, overwrite, env) {

  if (!is.character(func))
    stop("please provide the function name rather than the function itself to register it",
         call. = FALSE)
  
  if (length(env) == 0)
    stop("could not find function '", func, "' in any environment - please make sure that it is defined", 
         call. = FALSE)
  
  if (length(env) > 1)
    glue::glue("function '{func}' exists in more than one environment ",
               "({paste(env, collapse = ', ')})", 
               ", please specify parameter 'env' to clarify") %>% 
    stop(call. = FALSE)
  
  frs <- default("file_readers", allow_null = TRUE)
  
  new_fr <-
    dplyr::data_frame(
      type = type, call = call, extension = extension,
      func = func, cacheable = cacheable, description = description,
      env = env
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

# convenience function to find packages where function is located
find_func <- function(func) {
  if (!is.character(func)) stop("please provide the function name rather than the function itself", call. = FALSE)
  findFunction(func) %>% map_chr(environmentName) %>% str_replace("^package:", "") %>% { .[!str_detect(., "^imports:")] } %>% unique()
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
    cores <- future::availableCores()
    oplan <- plan(future::multisession) #plan(future::multiprocess)
    on.exit(plan(oplan), add = TRUE)
  } 
  
  # supplied data checks
  col_check(c("extension", "func", "cacheable"), supported_extensions)
  if(!is(data_structure, "iso_file")) stop("data structure must include class 'iso_file'", call. = FALSE)
  col_check(c("file_info"), data_structure)
  
  # global vars
  filepath <- ext <- NULL
  
  # read options update in data structure
  data_structure <- update_read_options(data_structure, ...)
  
  # expand & safety check paths (will warn if non-supported file types are included or same filename occurs multiple times)
  if (missing(paths) || is.null(paths) || is.na(paths)) stop("file path(s) required", call. = FALSE)
  filepaths <- expand_file_paths(paths, supported_extensions$extension)
  
  # check if there are any
  if (length(filepaths) == 0) 
    return(iso_as_file_list(list()))
  
  # initialize progress bar
  pb <- progress::progress_bar$new(
    format = sprintf("Progress: [:bar] :current/%d (:percent) :elapsed", length(filepaths)),
    clear = FALSE, show_after = 0, total = length(filepaths))
  pb$tick(0)
  
  # overview
  if (!default(quiet)) {
    glue::glue(
      "Info: preparing to read {length(filepaths)} data file(s)",
      if (parallel) { ", setting up {min(cores, length(filepaths))} parallel processes..." } 
      else {"..."}) %>% 
      pb$message()
  }
  
  # generate read files overview
  files <- 
    data_frame(
      filepath = filepaths,
      file_n = 1:length(filepaths),
      files_n = length(filepaths),
      cachepath = generate_cache_filepaths(filepath, data_structure$read_options),
      process = if(!parallel) NA_integer_ else ((file_n - 1) %% cores) + 1L
    ) %>% 
    # merge in supported extensions with reader and cacheable info
    match_to_supported_file_types(supported_extensions) %>% 
    # make cache read/write decisions
    mutate(
      read_from_cache = read_cache & cacheable & file.exists(cachepath),
      write_to_cache = cache & cacheable
    )
    
  # safety check on reader functions
  req_readers <- unique(files$func)
  in_workspace <- map_lgl(req_readers, exists, mode = "function")
  in_isoreader_ns <- map_lgl(req_readers, exists, mode = "function", where = asNamespace("isoreader"))
  if ( any(missing <- !in_workspace && !in_isoreader_ns) ) {
    stop("required reader function(s) does not seem to exist: ", 
         str_c(req_readers[missing], collapse = ", "), call. = FALSE)
  }
  
  # set up log files (for updates while parallel processing)
  log_files <- NULL
  if (parallel) {
    log_files <- tempfile() %>% { list(started = paste0(., "_started"), finished = paste0(., "_finished")) }
    cat("0", file = log_files$started)
    cat("0", file = log_files$finished)
  }
  
  # setup up processes
  processes <- 
    files %>% 
    mutate(process_nest = process) %>% 
    nest(-process_nest) %>% 
    mutate(
      result = map(
        data,
        ~ create_read_process(
          parallel = parallel, data_structure = data_structure,
          files = .x, progress_bar = pb, log_files = log_files
        )
      )
    )

  # evaluate result for sequential vs. parallel processing
  if (!parallel) {
    # sequential
    iso_files <- processes$result %>% unlist(recursive = FALSE)
  } else {
    # parallel
    status <- list(started = rep(FALSE, nrow(files)), finished = rep(FALSE, nrow(files)))
    # check on processes and update progress + user info
    while (TRUE) {
      # update status
      status <- process_parallel_logs(pb, status, log_files, files)
      
      # processors report
      futures_finished <- purrr::map_lgl(processes$result, resolved)
      
      # done?
      if (all(futures_finished)) break
    }
    
    # finall call to wrap up logs
    process_parallel_logs(pb, status, log_files, files)
    
    # delete log files
    file.remove(log_files$started)
    file.remove(log_files$finished)
    
    # fetch results 
    iso_files <- processes$result %>% lapply(future::value) %>% 
      unlist(recursive = FALSE)
  }
  
  # terminate progress bar
  while (!pb$finished) pb$tick()
  
  # final user update
  if (!default(quiet)) {
    end_time <- Sys.time()
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

# wrapper function for creating a read procss
create_read_process <- function(parallel, data_structure, files, progress_bar = NULL, log_files = NULL) {
  if (parallel) {
    # find required global functions and packages from the used readers
    func_globals <- filter(files, env == "R_GlobalEnv")$func %>% { setNames(purrr::map(., ~rlang::eval_tidy(rlang::sym(.x))), .) }
    packages <- c("isoreader", filter(files, env != "R_GlobalEnv")$env) %>% unique()
    # parallel via futures 
    result <- 
      future::future(
        # @NOTE: this may require a way to set globals and packages for the call backs and alternative processing functions to work!
        # maybe just an option to turn globals = TRUE? maybe as part of iso_debug_mode?
        globals = c(func_globals, list(data_structure = data_structure, files = files, log_files = log_files, all_opts = get_all_options())),
        packages = packages,
        expr = {
          # reload same isoreader options
          require(isoreader)
          options(all_opts)
          # Note: explicit namespace call required to all parallel session processing
          isoreader:::map_read_mode(data_structure, files, isoreader:::read_parallel, log_files = log_files)
        })
  } else {
    # sequential (avoid futures package completely for safety)
    result <- map_read_mode(data_structure, files, read_sequential, progress_bar = progress_bar)
  }
  return(result)
}

# map sequential vs. parallel processing mode
map_read_mode <- function(data_structure, files, read_func, progress_bar = NULL, log_files = NULL) {
  with(
    files,
    mapply(
      read_func,
      # args from files
      process = process,
      filepath = filepath, 
      file_n = file_n, 
      files_n = files_n, 
      read_from_cache = read_from_cache, 
      write_to_cache = write_to_cache, 
      cachepath = cachepath, 
      ext = extension, 
      reader_fun = func,
      reader_fun_env = env,
      # single value args
      MoreArgs = list(
        ds = data_structure,
        pb = progress_bar, 
        log_files = log_files
      )
    ))
}

# sequential reading
read_sequential <- function(pb, ds, process, filepath, file_n, files_n, read_from_cache, write_to_cache, cachepath, ext, reader_fun, reader_fun_env, ...) {
  start_iso_file(
    pb = pb, process = process, filepath = filepath, file_n = file_n, 
    files_n = files_n, read_from_cache = read_from_cache, 
    write_to_cache = write_to_cache, ext = ext)
  isofile <- read_iso_file(
    ds = ds, filepath = filepath, read_from_cache = read_from_cache, 
    write_to_cache = write_to_cache, cachepath = cachepath, 
    reader_fun = reader_fun, reader_fun_env = reader_fun_env)
  finish_iso_file(
    pb = pb, process = process, filepath = filepath, file_n = file_n, 
    files_n = files_n)
  return(list(isofile))
}

# parallel processing
read_parallel <- function(log_files, ds, process, filepath, file_n, files_n, read_from_cache, write_to_cache, cachepath, ext, reader_fun, reader_fun_env, ...) {
  cat(paste0(",", file_n), file = log_files$started, append = TRUE)
  isofile <- read_iso_file(
    ds = ds, filepath = filepath, read_from_cache = read_from_cache, 
    write_to_cache = write_to_cache, cachepath = cachepath, 
    reader_fun = reader_fun, reader_fun_env = reader_fun_env)
  cat(paste0(",", file_n), file = log_files$finished, append = TRUE)
  return(list(isofile))
}

# process parallel logs
process_parallel_logs <- function(progress_bar, status, log_files, files) {
  # started files
  newly_started <- readr::read_file(log_files$started) %>% stringr::str_split(fixed(",")) %>% { .[[1]][-1] } %>% as.numeric()
  if (any(!status$started[newly_started])) {
    new_idxs <- newly_started[!status$started[newly_started]]
    status$started[newly_started] <- TRUE
    files[new_idxs,] %>% 
      with(mapply(start_iso_file, process = process, filepath = filepath, file_n = file_n, 
                  files_n = files_n, read_from_cache = read_from_cache, 
                  write_to_cache = write_to_cache, ext = extension, MoreArgs = list(pb = progress_bar)))
  }
  
  # finished files
  newly_finished <- readr::read_file(log_files$finished) %>% stringr::str_split(fixed(",")) %>% { .[[1]][-1] } %>% as.numeric()
  if (any(!status$finished[newly_finished])) {
    new_idxs <- newly_finished[!status$finished[newly_finished]]
    status$finished[newly_finished] <- TRUE
    files[new_idxs,] %>% 
      with(mapply(finish_iso_file, process = process, filepath = filepath, 
                  file_n = file_n, files_n = files_n, MoreArgs = list(pb = progress_bar))) 
  } 
  
  return(status)
}

# progress / user info at start of file (extra parameters included in case needed by read_file_event)
start_iso_file <- function(pb, process, filepath, file_n, files_n, read_from_cache, write_to_cache, ext) {
  
  # progress update
  if (!default(quiet)) {
    if (read_from_cache) { 
      msg <- glue(
        #"Info: reading file {file_n}/{files_n} '{filepath}' from cache",
        "Info: reading file '{filepath}' from cache",
        if(!is.na(process)) {" on process {process}..."} else {"..."})
    } else {
      msg <- glue(
        "Info: reading{if (write_to_cache) ' and caching' else ''} ",
        #"file {file_n}/{files_n} '{filepath}' with '{ext}' reader",
        "file '{filepath}' with '{ext}' reader",
        if(!is.na(process)) {" on process {process}..."} else {"..."}) 
    }
    pb$message(msg)
  }
  
  # evaluate read file event quosure if it exists
  read_file_event <- getOption("isoreader.read_file_event")
  if (!is.null(read_file_event) && is_quosure(read_file_event) && !quo_is_null(read_file_event)) {
    eval_tidy(get_expr(read_file_event))
  }
}

# read function
read_iso_file <- function(ds, filepath, read_from_cache, write_to_cache, cachepath, reader_fun, reader_fun_env) {
  
  # prepare iso_file object
  iso_file <- set_ds_file_path(ds, filepath)
  
  if (read_from_cache) {
    # read from cache
    iso_file <- load_cached_iso_file(cachepath)
  } else {
    # read from original file
    env <- if (reader_fun_env == "R_GlobalEnv") .GlobalEnv else asNamespace(reader_fun_env)
    iso_file <- exec_func_with_error_catch(reader_fun, iso_file, env = env)
    
    # cleanup any binary content depending on debug setting
    if (!default(debug)) iso_file$binary <- NULL
    
    # store in cached file
    if (write_to_cache) cache_iso_file(iso_file, cachepath)
  }
  
  return(iso_file)
}

# progress / user info at end of file (extra parameters in case needed by finish_file_event)
finish_iso_file <- function(pb, process, filepath, file_n, files_n) {
  # evaluate finish file event quosure if it exists
  finish_file_event <- getOption("isoreader.finish_file_event")
  if (!is.null(finish_file_event) && is_quosure(finish_file_event) && !quo_is_null(finish_file_event)) {
    eval_tidy(get_expr(finish_file_event))
  }
  
  # progress bar update (do this even if not quiet)
  if(!pb$finished) pb$tick()
}

# file re-reading =========

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


