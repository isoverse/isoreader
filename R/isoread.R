# file types & reader =================

#' Register file readers
#' 
#' Register file extensions and reader functions for different data files. Isoreader automatically registers all built-in file readers so this function is usually only needed when registering additional readers provided for testing purposes from outside of the isoreader package. Note that file extensions are case-insensitive, i.e. a reader for \code{.ext} will also recognize \code{.Ext} and \code{.EXT}
#' 
#' @details \code{iso_register_dual_inlet_file_reader}: use this function to register file readers for dual inlet files.
#' 
#' @rdname file_readers
#' @param extension the file extension (e.g. \code{.dxf}) of the data file. Must be unique otherwise different files can not automatically be matched with the appropriate file reader based on their extension.
#' @param func the name of the function that should be used a filter reader. All file reader functions must accept a data structure argument as the first argument and return the same data structure with added data.
#' @param description what is this file type about?
#' @param software what is the software program that creates this filetype?
#' @param cacheable whether this file type is cacheable. If \code{TRUE} (the default), user requests to cache the file will be honored. If \code{FALSE}, this file type will never be cached no matter what the user requests.
#' @param overwrite whether to overwrite an existing file reader for the same extension
#' @param env the environment where to find the function, by default this will be determined automatically and will throw an error if there is any ambiguity (e.g. the same function name in multiple packages) in which case it should be set manually
#' @family file_types
#' @export
iso_register_dual_inlet_file_reader <- function(
  extension, func, description = NA_character_, software = NA_character_, cacheable = TRUE, overwrite = FALSE, env = find_func(func)) {
  register_file_reader("dual inlet", "iso_read_dual_inlet", extension, func, description, software, cacheable, overwrite, env)
}

#' @details \code{iso_register_continuous_flow_file_reader}: use this function to register file readers for continuous flow files.
#' @rdname file_readers
#' @family file_types
#' @export
iso_register_continuous_flow_file_reader <- function(
  extension, func, description = NA_character_, software = NA_character_, cacheable = TRUE, overwrite = FALSE, env = find_func(func)) {
  register_file_reader("continuous flow", "iso_read_continuous_flow", extension, func, description, software, cacheable, overwrite, env)
}

#' @details \code{iso_register_scan_file_reader}: use this function to register file readers for scan files.
#' @rdname file_readers
#' @family file_types
#' @export
iso_register_scan_file_reader <- function(
  extension, func, description = NA_character_, software = NA_character_, cacheable = TRUE, overwrite = FALSE, env = find_func(func)) {
  register_file_reader("scan", "iso_read_scan", extension, func, description, software, cacheable, overwrite, env)
}

register_file_reader <- function(type, call, extension, func, description, software, cacheable, overwrite, env) {

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
      software = software, env = env
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
  methods::findFunction(func) %>% map_chr(environmentName) %>% str_replace("^package:", "") %>% { .[!str_detect(., "^imports:")] } %>% unique()
}

#' Supported file types
#' 
#' Get an overview of all the file types currently supported by the isoreader package. To register additional file readers, use the \code{\link{iso_register_dual_inlet_file_reader}} and \code{\link{iso_register_continuous_flow_file_reader}} functions.
#' 
#' @family file_types
#' @export
iso_get_supported_file_types <- function() {
  dplyr::select(default("file_readers"), "extension", "software", "description", "type", "call")
}

get_supported_di_files <- function() {
  dplyr::filter(default("file_readers"), !!sym("type") == "dual inlet")
}

get_supported_cf_files <- function() {
  dplyr::filter(default("file_readers"), !!sym("type") == "continuous flow")
}

get_supported_scan_files <- function() {
  dplyr::filter(default("file_readers"), !!sym("type") == "scan")
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
#' @param nu_masses list of masses (e.g. \code{c("46","45","44")}) to map the collector channels (interpreted in order, i.e. the first channel will be linked to the first mass, the second channel to the second mass, etc.). This parameter is only used for reading Nu data files.
#' @family isoread functions for different types of IRMS data
#' @export
iso_read_dual_inlet <- function(
  ..., 
  root = ".",
  read_raw_data = default(read_raw_data), read_file_info = default(read_file_info), 
  read_method_info = default(read_method_info), read_vendor_data_table = default(read_vendor_data_table),
  nu_masses = c(),
  discard_duplicates = TRUE, parallel = FALSE, parallel_plan = future::multiprocess, 
  cache = default(cache), cache_files_with_errors = TRUE, read_cache = default(cache), quiet = default(quiet)) {
  
  # process data
  iso_read_files(
    unlist(list(...), use.names = FALSE),
    root = root,
    supported_extensions = get_supported_di_files(),
    data_structure = make_di_data_structure(),
    read_options = c(
      read_raw_data = read_raw_data,
      read_file_info = read_file_info,
      read_method_info = read_method_info,
      read_vendor_data_table = read_vendor_data_table
    ),
    reader_options = list(nu_masses = nu_masses),
    discard_duplicates = discard_duplicates,
    parallel = parallel,
    parallel_plan = parallel_plan,
    cache = cache,
    cache_files_with_errors = cache_files_with_errors, 
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
  root = ".",
  read_raw_data = default(read_raw_data), read_file_info = default(read_file_info), 
  read_method_info = default(read_method_info), read_vendor_data_table = default(read_vendor_data_table), 
  discard_duplicates = TRUE, parallel = FALSE, parallel_plan = future::multiprocess,
  cache = default(cache), cache_files_with_errors = TRUE, read_cache = default(cache), quiet = default(quiet)) {
  
  # process data
  iso_read_files(
    unlist(list(...), use.names = FALSE),
    root = root,
    supported_extensions = get_supported_cf_files(),
    data_structure = make_cf_data_structure(),
    read_options = c(
      read_raw_data = read_raw_data,
      read_file_info = read_file_info,
      read_method_info = read_method_info,
      read_vendor_data_table = read_vendor_data_table
    ),
    reader_options = list(),
    discard_duplicates = discard_duplicates,
    parallel = parallel,
    parallel_plan = parallel_plan,
    cache = cache,
    cache_files_with_errors = cache_files_with_errors,
    read_cache = read_cache,
    quiet = quiet
  )
}

#' Load scan data
#' 
#' @inheritParams iso_read_dual_inlet
#' @family isoread functions for different types of IRMS data
#' @export
iso_read_scan <- function(
  ..., 
  root = ".",
  read_raw_data = default(read_raw_data), read_file_info = default(read_file_info), read_method_info = default(read_method_info),
  discard_duplicates = TRUE, parallel = FALSE, parallel_plan = future::multiprocess,
  cache = default(cache), cache_files_with_errors = TRUE, read_cache = default(cache), quiet = default(quiet)) {
  
  # process data
  iso_read_files(
    unlist(list(...), use.names = FALSE),
    root = root,
    supported_extensions = get_supported_scan_files(),
    data_structure = make_scan_data_structure(),
    read_options = c(
      read_raw_data = read_raw_data,
      read_file_info = read_file_info,
      read_method_info = read_method_info
    ),
    reader_options = list(),
    discard_duplicates = discard_duplicates,
    parallel = parallel,
    parallel_plan = parallel_plan,
    cache = cache,
    cache_files_with_errors = cache_files_with_errors,
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
#' @param paths one or multiple file/folder paths. All files must have a supported file extension. All folders are expanded and searched for files with supported file extensions (which are then included in the read). Paths can be absolute paths or relative to the provided file \code{root} (which is the current working directory by default). For absolute paths, a common root directory will be guessed using \link{iso_find_absolute_path_roots}. The root portion of paths will never be displayed in info messages.
#' @inheritParams iso_expand_paths
#' @param supported_extensions data frame with supported extensions and corresponding reader functions (columns 'extension', 'func', 'cacheable')
#' @param data_structure the basic data structure for the type of iso_file
#' @inheritParams iso_as_file_list
#' @param parallel whether to process in parallel based on the number of available CPU cores. This may yield performance increases for files that are slow to parse such as continuous flow isodat files but usually provides little benefit for efficient data formats such as reading from R Data Archives.
#' @param parallel_plan which parallel processing strategy to use, see \link[future]{plan}, typically \code{future::multiprocess} (the default, uses multicore if supported by the operating system, otherwise multisession), \code{future::multisession} or \code{future::multicore}.
#' @param quiet whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages. Set parameter to overwrite global defaults for this function or set global defaults with calls to \link[=iso_info_messages]{iso_turn_info_message_on} and \link[=iso_info_messages]{iso_turn_info_message_off}
#' @param cache whether to cache iso_files. Note that previously exported R Data Archives (di.rda, cf.rda) are never cached since they are already essentially in cached form.
#' @param cache_files_with_errors whether to cache files that had errors during reading
#' @param read_cache whether to reload from cache if a cached version exists. Note that it will only read from cache if the file was previously read with the exact same isoreader version and read options and has not been modified since.
#' @param read_options vector of read options to be stored in the data structure (e.g. \code{c(read_vendor_data_table = FALSE)}). The \code{read_} prefix is optional.
#' @param reader_options list of paramters to be passed on to the reader
#' @return single iso_file object (if single file) or list of iso_files (iso_file_list)
iso_read_files <- function(paths, root, supported_extensions, data_structure, 
                           read_options = c(), reader_options = list(), discard_duplicates = TRUE, 
                           parallel = FALSE, parallel_plan = future::multiprocess, 
                           cache = default(cache), cache_files_with_errors = TRUE, read_cache = default(cache), 
                           quiet = default(quiet)) {

  # global
  path <- file_n <- cacheable <- cachepath <- process <- data <- idx <- NULL
  
  # start timer
  start_time <- Sys.time()
  
  # set quiet for the current and sub-calls and reset back to previous setting on exit
  on_exit_quiet <- update_quiet(quiet)
  on.exit(on_exit_quiet(), add = TRUE)
  
  # parallel processing
  if (parallel) {
    cores <- future::availableCores()
    oplan <- plan(parallel_plan)
    on.exit(plan(oplan), add = TRUE)
  } 
  
  # supplied data checks
  col_check(c("extension", "func", "cacheable"), supported_extensions)
  if(!is(data_structure, "iso_file")) stop("data structure must include class 'iso_file'", call. = FALSE)
  col_check(c("file_info"), data_structure)
  
  # read options update in data structure
  data_structure <- update_read_options(data_structure, read_options)
  
  # expand & safety check paths (will warn if non-supported file types are included or same filename occurs multiple times)
  if (missing(paths) || is.null(paths) || all(is.na(paths))) stop("file path(s) required, none provided", call. = FALSE)
  filepaths <- iso_expand_paths(paths, extensions = supported_extensions$extension, root = root)
  
  # check if there are any
  if (nrow(filepaths) == 0) 
    return(iso_as_file_list(list()))
  
  # find roots for absolute paths
  filepaths <- iso_find_absolute_path_roots(filepaths$path, filepaths$root)
  
  # initialize progress bar
  pb <- progress::progress_bar$new(
    format = sprintf("Progress: [:bar] :current/%d (:percent) :elapsed", nrow(filepaths)),
    clear = FALSE, show_after = 0, total = nrow(filepaths))
  set_temp("progress_bar", pb)
  pb$tick(0)
  
  # overview
  if (!default(quiet)) {
    glue::glue(
      "preparing to read {nrow(filepaths)} data files",
      if (cache && !cache_files_with_errors) { " (all will be cached unless they have errors)" }
      else if (cache && cache_files_with_errors) { " (all will be cached)" } else {""},
      if (parallel) { ", setting up {min(cores, nrow(filepaths))} parallel processes..." } 
      else {"..."}) %>% 
      log_message()
  }

  # generate read files overview
  files <- 
    filepaths %>% 
    mutate(
      file_n = 1:n(),
      files_n = n(),
      cachepath = generate_cache_filepaths(file.path(root, path), data_structure$read_options),
      process = if(!parallel) NA_integer_ else ((file_n - 1) %% cores) + 1L,
      reader_options = list(!!reader_options)
    ) %>% 
    # merge in supported extensions with reader and cacheable info
    match_to_supported_file_types(supported_extensions) %>% 
    # make cache read/write decisions
    mutate(
      read_from_cache = read_cache & cacheable & file.exists(cachepath),
      write_to_cache = cache & cacheable,
      write_to_cache_if_errors = cache_files_with_errors
    )
  
  # safety check on reader functions
  req_readers <- unique(files$func)
  in_workspace <- map_lgl(req_readers, exists, mode = "function")
  in_isoreader_ns <- map_lgl(req_readers, exists, mode = "function", where = asNamespace("isoreader"))
  if ( any(missing <- !in_workspace & !in_isoreader_ns) ) {
    stop("required reader function(s) does not seem to exist: ", 
         str_c(req_readers[missing], collapse = ", "), call. = FALSE)
  }
  
  # set up log files for parallel processing
  if (parallel) setup_parallel_logs()
  
  # setup up processes
  set_temp("parallel_process", NA_integer_) # mark the main process
  processes <- 
    files %>% 
    nest(data = c(-process)) %>% 
    mutate(
      result = purrr::map2(
        process,
        data,
        ~ create_read_process(
          process = .x, data_structure = data_structure, files = .y
        )
      )
    )

  # evaluate result for sequential vs. parallel processing
  if (!parallel) {
    # sequential
    iso_files <- processes$result %>% unlist(recursive = FALSE)
  } else {
    # parallel
    monitor_parallel_logs(processes)
    cleanup_parallel_logs()
    iso_files <- processes$result %>% lapply(future::value) %>% 
      unlist(recursive = FALSE)
  }
  
  # terminate progress bar
  while (!pb$finished) pb$tick()
  
  # final user update
  if (!default(quiet)) {
    end_time <- Sys.time()
    sprintf(
      "finished reading %s files in %.2f %s",
      nrow(files), as.numeric(end_time - start_time), 
      attr(end_time - start_time, "units")) %>% 
    log_message()
  }

  # turn into iso_file list
  iso_files <- iso_as_file_list(iso_files, discard_duplicates = discard_duplicates) 

  # bring files into the correct order after potential parallel processing jumble
  indices <- 
    tibble(path = purrr::map_chr(iso_files, ~.x$file_info$file_path), idx = 1:length(path)) %>% 
    dplyr::left_join(files, by = "path") %>% 
    dplyr::arrange(file_n) %>% 
    dplyr::pull(idx)
  iso_files <- iso_files[indices]
  
  # convert file_path_to_rooted for old files
  # @note: should be possible to deprecate in a future version since all paths will be rooted
  if (length(root) != 1) root <- "." # if there are multiple, default back to working directory in case of ambiguity
  iso_files <- convert_file_path_to_rooted(iso_files, root = root)
  
  # report problems
  if (!default(quiet) && iso_has_problems(iso_files)) {
    sprintf("encountered %.0f problems in total", n_problems(iso_files)) %>% log_message()
    print(problems(iso_files))
    cat("\n")
  }
  
  # return single or file or list
  if (length(iso_files) == 1) return (iso_files[[1]])
  return(iso_files)
}

# wrapper function for creating a read procss
# @param process if NA --> set up process in the current session, if integer --> set up parallel process
create_read_process <- function(process, data_structure, files) {
  
  # global vars
  root <- path <- file_n <- files_n <- read_from_cache <- write_to_cache <- write_to_cache_if_errors <- cachepath <- extension <- func <- reader_options <- env <- reader_fun_env <- all_opts <- NULL
  
  # specify relevant files columns to match read_iso_file parameters
  files <- files %>% 
    select(
      root, path, file_n, files_n, read_from_cache, write_to_cache, write_to_cache_if_errors, cachepath, ext = extension, 
      reader_fun = func, reader_options = reader_options, reader_fun_env = env
    )
  
  # parallel
  if (!is.na(process)) {
    # find required global functions and packages from the used readers
    func_globals <- filter(files, reader_fun_env == "R_GlobalEnv")$reader_fun %>% 
      unique() %>% { setNames(purrr::map(., ~rlang::eval_tidy(rlang::sym(.x))), .) }
    packages <- c("isoreader", "purrr", filter(files, reader_fun_env != "R_GlobalEnv")$reader_fun_env) %>% unique()
    log_file <- get_temp("parallel_log_file")
    progress_file <- get_temp("parallel_progress_file")
    # parallel via futures 
    result <- 
      future::future(
        globals = c(func_globals, list(
          process = process, data_structure = data_structure, files = files, 
          log_file = log_file, progress_file = progress_file, all_opts = get_all_options())),
        packages = packages,
        expr = {
          # require namespace if running in a separate session during parallel processing
          base::requireNamespace("isoprocessor", quietly = TRUE)
          # reload isoreader options
          options(all_opts)
          # set isoreader temp options for parallel processing
          set_temp("parallel_process", process)
          set_temp("parallel_log_file", log_file)
          set_temp("parallel_progress_file", progress_file)
          # process isofiles
          purrr::pmap(files, read_iso_file, ds = data_structure)
        })
  } else {
    # sequential (don't use future package)
    result <-  purrr::pmap(files, read_iso_file, ds = data_structure)
  }
  return(result)
}

#' Read individual iso file
#' 
#' Low level read function for an individual iso file. Usually not called directly but available for methods development.
#' @inheritParams iso_read_files
#' @param ds the basic data structure for the type of iso_file
#' @param path file path
#' @param file_n numer of processsed file for info messages
#' @param files_n total number of files for info messages
#' @param read_from_cache whether to read from cache
#' @param write_to_cache whether to write to cache
#' @param write_to_cache_if_errors whether to write to cache even if errors are encountered
#' @param cachepath path for the cache file
#' @param ext file extension
#' @param reader_fun file reader function
#' @param reader_fun_env where to find the reader function
#' 
#' @export
read_iso_file <- function(ds, root, path, file_n, files_n, read_from_cache, write_to_cache, write_to_cache_if_errors, cachepath, ext, reader_fun, reader_options, reader_fun_env) {
  
  # prepare iso_file object
  iso_file <- set_ds_file_path(ds, root, path)
  
  # progress update
  if (!default(quiet)) {
    if (read_from_cache) { 
      msg <- glue("reading file '{path}' from cache...")
    } else {
      msg <- glue("reading file '{path}' with '{ext}' reader")
    }
    log_message(msg)
  }
  
  # evaluate read file event quosure if it exists
  read_file_event <- getOption("isoreader.read_file_event")
  if (!is.null(read_file_event) && is_quosure(read_file_event) && !quo_is_null(read_file_event)) {
    eval_tidy(get_expr(read_file_event))
  }
  
  if (read_from_cache) {
    # read from cache
    iso_file <- load_cached_iso_file(cachepath)
    iso_file <- ensure_iso_file_backwards_compatibility(iso_file)
  } else {
    # read from original file
    env <- if (reader_fun_env == "R_GlobalEnv") .GlobalEnv else asNamespace(reader_fun_env)
    iso_file <- exec_func_with_error_catch(reader_fun, iso_file, options = reader_options, env = env)
    iso_file <- ensure_iso_file_backwards_compatibility(iso_file)
  
    # cleanup any binary and source content depending on debug setting
    if (!default(debug)) {
      iso_file$binary <- NULL # @FIXME: binary should be renamed to source throughout
      iso_file$source <- NULL
      iso_file$temp <- NULL
    }
    
    # store in cached file
    if (write_to_cache && (n_problems(iso_file) == 0 || write_to_cache_if_errors)) 
      cache_iso_file(iso_file, cachepath)
  }
  
  # evaluate finish file event quosure if it exists
  finish_file_event <- getOption("isoreader.finish_file_event")
  if (!is.null(finish_file_event) && is_quosure(finish_file_event) && !quo_is_null(finish_file_event)) {
    eval_tidy(get_expr(finish_file_event))
  }
  
  # marke progress for progress bar
  log_progress()
  
  return(iso_file)
}

# ensure backwards compatibility for read isofiles
# all operations that are needed for backwards compatibility
ensure_iso_file_backwards_compatibility <- function(iso_file) {
  # standard fields
  standard_fields <- names(make_iso_file_data_structure()$file_info)
  
  # convert file info columns to list columns (and ensure it's data frame format)
  # convert data frame units attribute to implicit double with units
  if (iso_is_file_list(iso_file)) {
    iso_file <- map(iso_file, ~{
      .x$file_info <- ensure_data_frame_list_columns(.x$file_info, exclude = standard_fields);
      .x$vendor_data_table <- convert_df_units_attr_to_implicit_units(.x$vendor_data_table)
      .x
    }) %>% iso_as_file_list()
  } else {
    iso_file$file_info <- ensure_data_frame_list_columns(iso_file$file_info, exclude = standard_fields)
    iso_file$vendor_data_table <- convert_df_units_attr_to_implicit_units(iso_file$vendor_data_table)
  }
  
  return(iso_file)
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
    log_message("re-reading ", length(filepaths), " data file(s)...")
  }
  
  # safety check for non existent data files
  if (!all(files_exist)) {
    msg <- sprintf("%d file(s) do no longer exist at the referenced location and can not be re-read:\n - %s\n",
                   sum(!files_exist), str_c(filepaths[!files_exist], collapse = "\n - "))
    if (stop_if_missing) {
      stop(msg, call. = FALSE)
    } else {
      log_warning(msg)
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

#' @details \code{iso_reread_storage} is a convenience function for refreshing saved iso_file collections (see \code{\link{iso_save}}). It will load a specific iso_files R Data Storage file (\code{rds_filepath}), re-read all the data from the original data files and save the collection back to the same rds file. The filepaths are returned invisibly.
#' 
#' Note that this function will also reread the older .rda Archive files but store them in the newer .rds format after re-reading.
#' 
#' @rdname iso_reread_files
#' @param rds_filepaths the path(s) to the iso_files R data archive(s) to re-read (can be a single file or vector of files)
#' @export
iso_reread_storage <- function(rds_filepaths, ..., stop_if_missing = FALSE, quiet = default(quiet)) {
  
  # global
  extension <- NULL
  
  extensions <- iso_get_supported_file_types() %>% dplyr::filter(stringr::str_detect(extension, "rd[sa]"))
  file_types <- data_frame(path = rds_filepaths) %>% match_to_supported_file_types(extensions)
  
  if (any(missing <- !file.exists(rds_filepaths))) 
    stop("file(s) do not exist: ", str_c(rds_filepaths[missing], collapse = ", "), call. = FALSE)
  
  reread_storage <- function(filepath, call) {
    if(!quiet) log_message("loading R Data Storage ", basename(filepath), "...")
    new_filepath <- str_replace(filepath, "\\.rd[sa]$", ".rds")
    do.call(call, args = list(paths = filepath, quiet=TRUE)) %>% 
      iso_reread_files(..., stop_if_missing = stop_if_missing, quiet=quiet) %>% 
      iso_save(filepath = new_filepath, quiet=quiet)
    return(new_filepath)
  }
  
  # note: cannot combine these in case some of them are dual inlet while others are continuous flow
  rds_filepaths <- with(file_types, purrr::map2_chr(path, call, reread_storage))
  return(invisible(rds_filepaths))
}

#' @details \code{iso_reread_archive} is deprecated in favour of \code{iso_reread_storage}
#' @rdname iso_reread_files
#' @export
iso_reread_archive <- function(...) {
  log_warning("'iso_reread_archive' is deprecated and will call 'iso_reread_storage'. Please use 'iso_reread_storage' directly to avoid this warning.")
  invisible(iso_reread_storage(...))
}

