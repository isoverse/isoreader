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
#' @param software what is the software program that creates this file type?
#' @param cacheable whether this file type is cacheable. If \code{TRUE} (the default), user requests to cache the file will be honored. If \code{FALSE}, this file type will never be cached no matter what the user requests.
#' @param post_read_check whether isoreader should conduct a data integrity check after reading the file. Should always be \code{TRUE} unless there is independent data integrity checking already taking place inside the reader.
#' @param overwrite whether to overwrite an existing file reader for the same extension
#' @param env the environment where to find the function, by default this will be determined automatically and will throw an error if there is any ambiguity (e.g. the same function name in multiple packages) in which case it should be set manually
#' @family file_types
#' @export
iso_register_dual_inlet_file_reader <- function(
  extension, func, description = NA_character_, software = NA_character_, cacheable = TRUE, post_read_check = TRUE, overwrite = FALSE, env = find_func(func)) {
  register_file_reader("dual inlet", "iso_read_dual_inlet", extension, func, description, software, cacheable, post_read_check, overwrite, env)
}

#' @details \code{iso_register_continuous_flow_file_reader}: use this function to register file readers for continuous flow files.
#' @rdname file_readers
#' @family file_types
#' @export
iso_register_continuous_flow_file_reader <- function(
  extension, func, description = NA_character_, software = NA_character_, cacheable = TRUE, post_read_check = TRUE, overwrite = FALSE, env = find_func(func)) {
  register_file_reader("continuous flow", "iso_read_continuous_flow", extension, func, description, software, cacheable, post_read_check, overwrite, env)
}

#' @details \code{iso_register_scan_file_reader}: use this function to register file readers for scan files.
#' @rdname file_readers
#' @family file_types
#' @export
iso_register_scan_file_reader <- function(
  extension, func, description = NA_character_, software = NA_character_, cacheable = TRUE, post_read_check = TRUE, overwrite = FALSE, env = find_func(func)) {
  register_file_reader("scan", "iso_read_scan", extension, func, description, software, cacheable, post_read_check, overwrite, env)
}

register_file_reader <- function(type, call, extension, func, description, software, cacheable, post_read_check, overwrite, env) {

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
    tibble::tibble(
      type = type, call = call, extension = extension,
      func = func, cacheable = cacheable,
      post_read_check = post_read_check,
      description = description,
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
  default("file_readers") %>% 
    dplyr::select(.data$type, .data$extension, .data$software, .data$description, .data$call) %>% 
    dplyr::arrange(.data$type, .data$extension)
}

get_supported_di_files <- function() {
  dplyr::filter(default("file_readers"), .data$type == "dual inlet") %>%
    dplyr::arrange(.data$extension)
}

get_supported_cf_files <- function() {
  dplyr::filter(default("file_readers"), .data$type == "continuous flow") %>%
    dplyr::arrange(.data$extension)
}

get_supported_scan_files <- function() {
  dplyr::filter(default("file_readers"), .data$type == "scan") %>%
    dplyr::arrange(.data$extension)
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
  discard_duplicates = TRUE,
  parallel = FALSE, parallel_plan = future::multisession, parallel_cores = future::availableCores(),
  cache = default(cache), read_cache = default(cache), reread_outdated_cache = FALSE,
  quiet = default(quiet), cache_files_with_errors = TRUE) {

  # cache files with errors deprecation warning
  if (!missing(cache_files_with_errors)) {
    warning(
      "the 'cache_file_with_errors' parameter is deprecated. Please use iso_reread_problem_files() instead to selectively re-read all files in a collection of iso files that had been previously read with errors or warnings.",
      immediate. = TRUE, call. = FALSE
    )
  }

  # process data
  iso_read_files(
    unlist_paths(list(...)),
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
    parallel_cores = parallel_cores,
    cache = cache,
    read_cache = read_cache,
    reread_outdated_cache = reread_outdated_cache,
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
  discard_duplicates = TRUE,
  parallel = FALSE, parallel_plan = future::multisession, parallel_cores = future::availableCores(),
  cache = default(cache), read_cache = default(cache), reread_outdated_cache = FALSE,
  quiet = default(quiet), cache_files_with_errors = TRUE) {

  # cache files with errors deprecation warning
  if (!missing(cache_files_with_errors)) {
    warning(
      "the 'cache_file_with_errors' parameter is deprecated. Please use iso_reread_problem_files() instead to selectively re-read all files in a collection of iso files that had been previously read with errors or warnings.",
      immediate. = TRUE, call. = FALSE
    )
  }

  # process data
  iso_read_files(
    unlist_paths(list(...)),
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
    parallel_cores = parallel_cores,
    cache = cache,
    read_cache = read_cache,
    reread_outdated_cache = reread_outdated_cache,
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
  discard_duplicates = TRUE,
  parallel = FALSE, parallel_plan = future::multisession, parallel_cores = future::availableCores(),
  cache = default(cache), read_cache = default(cache), reread_outdated_cache = FALSE,
  quiet = default(quiet), cache_files_with_errors = TRUE) {

  # cache files with errors deprecation warning
  if (!missing(cache_files_with_errors)) {
    warning(
      "the 'cache_file_with_errors' parameter is deprecated. Please use iso_reread_problem_files() instead to selectively re-read all files in a collection of iso files that had been previously read with errors or warnings.",
      immediate. = TRUE, call. = FALSE
    )
  }

  # process data
  iso_read_files(
    unlist_paths(list(...)),
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
    parallel_cores = parallel_cores,
    cache = cache,
    read_cache = read_cache,
    reread_outdated_cache = reread_outdated_cache,
    quiet = quiet
  )
}


#' Core function to read isotope data files
#'
#' This function takes care of extracting basic information about iso_files, dealing with problems and making sure only valid fire formats are processed.
#' This function is not typically called directly but indirectly by calling \link{iso_read_dual_inlet}, \link{iso_read_continuous_flow} and \link{iso_read_scan}.
#' It is made available outside the package because it can be very useful for testing new file readers.
#'
#' @param paths one or multiple file/folder paths. All files must have a supported file extension. All folders are expanded and searched for files with supported file extensions (which are then included in the read). Paths can be absolute paths or relative to the provided file \code{root} (which is the current working directory by default). For absolute paths, a common root directory will be guessed using \link{iso_find_absolute_path_roots}. The root portion of paths will never be displayed in info messages.
#' @inheritParams iso_expand_paths
#' @param supported_extensions data frame with supported extensions and corresponding reader functions (columns 'extension', 'func', 'cacheable')
#' @param data_structure the basic data structure for the type of iso_file
#' @inheritParams iso_as_file_list
#' @param parallel whether to process in parallel based on the number of available CPU cores. This may yield performance increases for files that are slow to parse such as continuous flow isodat files but usually provides little benefit for efficient data formats such as reading from R Data Archives.
#' @param parallel_plan which parallel processing strategy to use, see \link[future]{plan}, typically \code{future::multisession} for compatibility with RStudio interactive mode. If supported by the operating system and running in detached mode (not interactively in RStudio) can also use \code{future::multicore}.
#' @param parallel_cores how many processor cores to use for parallel processing. By default the maximum available number of cores (\link[future]{availableCores}), which will allow maximal processing speed but may slow other programs running on your machine. Choose a smaller number if you want some processing resources to remain available for other processes. Will issue a warning if too many cores are requested and reset to the maximum available.
#' @param quiet whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages. Set parameter to overwrite global defaults for this function or set global defaults with calls to \link[=iso_info_messages]{iso_turn_info_messages_on} and \link[=iso_info_messages]{iso_turn_info_messages_off}
#' @param cache whether to cache iso_files. Note that R Data Storage files (.rds, see \link{iso_save}) are never cached since they are already essentially in cached form.
#' @param cache_files_with_errors deprecated. Please use \link{iso_reread_problem_files} instead to selectively re-read all files in a collection of iso files that had been previously read with errors or warnings.
#' @param read_cache whether to reload from cache if a cached version exists. Note that it will only read from cache if the raw data file has not been modified since. Files that have been modified on disc (e.g. edited in the vendor software) will always be read anew. To automatically reread cached files that were cached by an outdated version of the isoreader package, set the \code{reread_outdated_cache} flag.
#' @param reread_outdated_cache whether to re-read outdated cache files whenever they are encountered.
#' @param read_options vector of read options to be stored in the data structure (e.g. \code{c(read_vendor_data_table = FALSE)}). The \code{read_} prefix is optional.
#' @param reader_options list of parameters to be passed on to the reader
#' @return single iso_file object (if single file) or list of iso_files (iso_file_list)
iso_read_files <- function(paths, root, supported_extensions, data_structure,
                           read_options = c(), reader_options = list(), discard_duplicates = TRUE, cache_files_with_errors = TRUE,
                           parallel = FALSE, parallel_plan = future::multisession, parallel_cores = future::availableCores(),
                           cache = default(cache), read_cache = default(cache), reread_outdated_cache = FALSE,
                           quiet = default(quiet)) {

  # start timer
  start_time <- Sys.time()

  # set quiet for the current and sub-calls and reset back to previous setting on exit
  on_exit_quiet <- update_quiet(quiet)
  on.exit(on_exit_quiet(), add = TRUE)

  # parallel processing
  if (parallel) {
    available_cores <- future::availableCores()
    if (parallel_cores > available_cores) {
      glue::glue(
        "{parallel_cores} cores were requested for parallel processing ",
        "but only {available_cores} are available"
      ) %>% warning(immediate. = TRUE, call. = FALSE)
    }
    cores <- min(parallel_cores, available_cores)
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
  if (missing(paths) || is.null(paths) || all(is.na(paths))) {
    stop("file path(s) required, none provided", call. = FALSE)
  }
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
  if (!default("quiet")) {
    glue::glue(
      "preparing to read {nrow(filepaths)} data files",
      if (cache) { " (all will be cached)" } else {""},
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
      cachepath = generate_cache_filepaths(file.path(.data$root, .data$path)),
      old_cachepath = generate_old_cache_filepaths(file.path(.data$root, .data$path), data_structure$read_options),
      process = if(!parallel) NA_integer_ else ((.data$file_n - 1) %% cores) + 1L,
      reader_options = list(!!reader_options)
    ) %>%
    # merge in supported extensions with reader and cacheable info
    match_to_supported_file_types(supported_extensions) %>%
    # make cache read/write decisions
    mutate(
      read_from_cache = read_cache & .data$cacheable & file.exists(.data$cachepath),
      read_from_old_cache = read_cache & .data$cacheable & file.exists(.data$old_cachepath),
      reread_outdated_cache = !!reread_outdated_cache,
      write_to_cache = cache & .data$cacheable
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
    nest(data = c(-.data$process)) %>%
    mutate(
      result = purrr::map2(
        .data$process,
        .data$data,
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
  if (!default("quiet")) {
    end_time <- Sys.time()
    sprintf(
      "finished reading %s files in %.2f %s",
      nrow(files), as.numeric(end_time - start_time),
      attr(end_time - start_time, "units")) %>%
    log_message()
  }

  # overall problem messages / warnings
  all_probs <- iso_get_problems(iso_files)
  ## outdated files
  if (any(stringr::str_detect(
    all_probs$details,
    fixed("outdated version of the isoreader package")))) {
    glue::glue(
      "some files were read from outdated cache files. They were checked for compatibility and should work without issues. However, to avoid this warning and improve read spead, please call iso_reread_outdated_files() on your collection of iso files to refresh outdated cache files."
    ) %>%
      warning(immediate. = TRUE, call. = FALSE)
  }
  ## unix file creation date
  if (any(stringr::str_detect(
    all_probs$details,
    fixed("file creation date cannot be accessed on this Linux system")))) {
    glue::glue(
      "file creation date could not be accessed for all files because this information is not available on some Linux systems, reporting last modified time for file_datetime instead. To turn these warnings off, call iso_turn_datetime_warnings_off() and reread these files with iso_reread_all_files()."
    ) %>%
      warning(immediate. = TRUE, call. = FALSE)
  }

  # turn into iso_file list
  iso_files <- iso_as_file_list(iso_files, discard_duplicates = discard_duplicates)

  # bring files into the correct order after potential parallel processing jumble
  indices <-
    tibble(path = purrr::map_chr(iso_files, ~.x$file_info$file_path) %>% unname(), idx = 1:length(.data$path)) %>%
    dplyr::left_join(files, by = "path") %>%
    dplyr::arrange(.data$file_n) %>%
    dplyr::pull(.data$idx) %>%
    unique()
  iso_files <- iso_files[indices]

  # report problems
  warn_problems(iso_files)

  # return single or file or list
  if (length(iso_files) == 1) return (iso_files[[1]])
  return(iso_files)
}

# wrapper function for creating a read procss
# @param process if NA --> set up process in the current session, if integer --> set up parallel process
create_read_process <- function(process, data_structure, files) {

  # specify relevant files columns to match read_iso_file parameters
  files <- files %>%
    select(
      .data$root, .data$path, .data$file_n, .data$files_n,
      .data$read_from_cache, .data$read_from_old_cache, .data$reread_outdated_cache,
      .data$write_to_cache, .data$cachepath, .data$old_cachepath,
      .data$post_read_check, ext = .data$extension,
      reader_fun = .data$func, reader_options = .data$reader_options, reader_fun_env = .data$env
    )

  # parallel
  if (!is.na(process)) {
    # session options
    all_opts <- get_all_options()
    # find required global functions and packages from the used readers
    func_globals <- filter(files, .data$reader_fun_env == "R_GlobalEnv")$reader_fun %>%
      unique() %>% { rlang::set_names(purrr::map(., ~rlang::eval_tidy(rlang::sym(.x))), .) }
    packages <- c("isoreader", "purrr", filter(files, .data$reader_fun_env != "R_GlobalEnv")$reader_fun_env) %>% unique()
    log_file <- get_temp("parallel_log_file")
    progress_file <- get_temp("parallel_progress_file")
    # parallel via futures
    result <-
      future::future(
        globals = c(func_globals, list(
          process = process, data_structure = data_structure, files = files,
          log_file = log_file, progress_file = progress_file, all_opts = all_opts)),
        packages = packages,
        seed = NULL,
        expr = {
          # require namespace if running in a separate session during parallel processing
          base::requireNamespace("isoreader", quietly = TRUE)
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
#' @param file_n number of processed file for info messages
#' @param files_n total number of files for info messages
#' @param read_from_cache whether to read from cache
#' @param read_from_old_cache whether to read from old cache files (to be deprecated in isoreader 2.0)
#' @param reread_outdated_cache whether to reread outdated cache files
#' @param write_to_cache whether to write to cache
#' @param cachepath path for the cache file
#' @param old_cachepath path for the old cache files
#' @param post_read_check whether to run data integrity checks after a file read
#' @param ext file extension
#' @param reader_fun file reader function
#' @param reader_fun_env where to find the reader function
#'
#' @export
read_iso_file <- function(
  ds, root, path, file_n, files_n,
  read_from_cache, read_from_old_cache, reread_outdated_cache,
  write_to_cache, cachepath, old_cachepath,
  post_read_check, ext, reader_fun, reader_options, reader_fun_env) {

  # prepare iso_file object
  ds <- set_ds_file_path(ds, root, path)
  iso_file <- ds # default

  # cache
  has_cache <- read_from_cache || read_from_old_cache

  # progress update
  if (!default("quiet")) {
    if (has_cache) {
      msg <- glue("reading file '{path}' from cache...")
    } else {
      msg <- glue("reading file '{path}' with '{ext}' reader...")
    }
    log_message(msg)
  }

  # evaluate read file event quosure if it exists
  read_file_event <- getOption("isoreader.read_file_event")
  if (!is.null(read_file_event) && is_quosure(read_file_event) && !quo_is_null(read_file_event)) {
    eval_tidy(get_expr(read_file_event))
  }

  # whether to reread raw file
  reread_file <- FALSE
  outdated_message <- function(iso_file) {
    sprintf(
      "cache file created by an outdated version of the isoreader package (%s)",
      as.character(get_iso_object_versions(iso_file)[[1]])
    )
  }

  # read cache
  if (has_cache) {
    iso_file <-
      if (read_from_cache) load_cached_iso_file(cachepath)
      else if (read_from_old_cache) load_cached_iso_file(old_cachepath)

    # check if reader options match, if not: re-read
    # FIXME: implement this
    # pseudocode
    # if (!has_same_reader_options(iso_file, reader_options)) {
    #    reread_file <- TRUE
    # }

    # check cached file version
    if (iso_is_object(iso_file) && is_iso_object_outdated(iso_file)) {

      # re-read file
      if (reread_outdated_cache) {
        # reread file
        reread_file <- TRUE
        log_message(glue("cached file is outdated --> re-reading file '{path}' with '{ext}' reader..."))
      } else {
        # post info message, run compatibility checks and attach a warning
        # NOTE: does not lead to automatic re-read, better to let user do this explicitly with iso_reread_outdated_files
        log_message(
          glue("running compatibility checks for outdated cached file ",
               "created by isoreader version < {as.character(get_last_structure_update_version())}")
        )
        # warning and compatibility checks
        iso_file <-
          iso_file %>%
          # warning
          register_warning(
            details = outdated_message(iso_file),
            func = "read_iso_file",
            warn = FALSE
          ) %>%
          # compatibility
          ensure_iso_file_backwards_compatibility()
      }
    }
  }

  # read isofile
  if (!has_cache || reread_file) {
    # read from original file
    env <- if (reader_fun_env == "R_GlobalEnv") .GlobalEnv else asNamespace(reader_fun_env)
    ds <- set_ds_file_size(ds)
    iso_file <- exec_func_with_error_catch(reader_fun, ds, options = reader_options, env = env)

    # post read checks
    if (post_read_check) {
      iso_file <- run_post_read_check(iso_file)
    }

    # check version
    if (iso_is_object(iso_file) && is_iso_object_outdated(iso_file)) {

      outdated_files <- get_iso_object_outdated(iso_file)

      # post info message
      log_message(
        glue("running compatibility checks for outdated files ",
             "({sum(outdated_files)}/{length(outdated_files)}) ",
             "created by isoreader version < {as.character(get_last_structure_update_version())}")
      )

      # run compatibility checks
      iso_file <- ensure_iso_file_backwards_compatibility(iso_file)

      # attach warning
      if (iso_is_file(iso_file)) {
        # single file
        iso_file <-
          register_warning(
            iso_file,
            details = outdated_message(iso_file),
            func = "read_iso_file", warn = FALSE
          )
      } else {
        # multiple files
        iso_file[outdated_files] <- map(iso_file[outdated_files], ~{
          register_warning(
            .x,
            details = outdated_message(.x),
            func = "read_iso_file", warn = FALSE
          )
        })
      }

    }

    # cleanup any binary and source content depending on debug setting
    if (!default(debug)) {
      iso_file$binary <- NULL # @FIXME: binary should be renamed to source throughout
      iso_file$source <- NULL
      iso_file$temp <- NULL
    }

    # store in cached file
    if (write_to_cache)
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

# run post read data integrity checks
run_post_read_check <- function(iso_files) {
  # file info column check
  iso_files <- ensure_file_info_list_columns(iso_files)

  # what else should go in here for data integrity checks?
  return(iso_files)
}

# ensure file info list columns
# convert file info columns to list columns (and ensure it's data frame format)
ensure_file_info_list_columns <- function(iso_files) {

  # standard fields
  standard_fields <- names(make_iso_file_data_structure()$file_info)

  # check list vs. single file
  if (iso_is_file_list(iso_files)) {
    iso_files <- map(iso_files, ~{
      .x$file_info <- ensure_data_frame_list_columns(.x$file_info, exclude = standard_fields)
      .x
    }) %>% iso_as_file_list()
  } else {
    iso_files$file_info <- ensure_data_frame_list_columns(iso_files$file_info, exclude = standard_fields)
  }

  return(iso_files)
}

# ensure backwards compatibility for read isofiles
# all operations that are needed for backwards compatibility
ensure_iso_file_backwards_compatibility <- function(iso_files) {

  # file info column check
  iso_files <- ensure_file_info_list_columns(iso_files)

  # convert data frame units attribute to implicit double with units
  # check for file size parameter
  # check for proper file datetime column type
  ensure_compatibility <- function(iso_file) {
    if (!"file_root" %in% names(iso_file$file_info)) iso_file$file_info$file_root <- "."
    iso_file <- set_ds_file_size(iso_file)
    iso_file <- check_file_datetime(iso_file)
    iso_file$vendor_data_table <- convert_df_units_attr_to_implicit_units(iso_file$vendor_data_table)
    return(iso_file)
  }

  # check list vs. single file
  if (iso_is_file_list(iso_files)) {
    iso_files <- map(iso_files, ensure_compatibility) %>% iso_as_file_list()
  } else {
    iso_files <- ensure_compatibility(iso_files)
  }

  return(iso_files)
}

# check file_datetime (no longer allowed to be a NA_integer_, has to be datetime NA)
check_file_datetime <- function(iso_file) {
  if ("file_datetime" %in% names(iso_file$file_info) && is.na(iso_file$file_info$file_datetime[1]))
    iso_file$file_info$file_datetime <- lubridate::as_datetime(NA)
  return(iso_file)
}

# file re-reading =========

#' Re-read iso_files
#'
#' Actual multi-purpose file-reread function (not exported) that powers \link{iso_reread_files}.
#'
#' @inheritParams iso_reread_files
#' @param reread_only_changed_files whether to re-read only files that have since be changed on disc (i.e. have no valid cache file), default FALSE i.e. re-read ALL files
#' @param reread_only_outdated_files whether to re-read only files that were read by an outdated version of isoreader (default FALSE, i.e. re-read ALL files)
#' @param reread_files_without_problems whether to re-read files that had read in without problems the last time (default TRUE)
#' @param reread_files_with_errors whether to re-read files that had read in with errors the last time (default TRUE)
#' @param reread_files_with_warnings whether to re-read files that had read in with warnings the last time (default TRUE)
reread_iso_files <- function(
  iso_files, ..., stop_if_missing = FALSE,
  reread_only_changed_files = FALSE,
  reread_only_outdated_files = FALSE,
  reread_files_without_problems = TRUE,
  reread_files_with_errors = TRUE,
  reread_files_with_warnings = TRUE,
  quiet = default(quiet)) {

  # checks
  if(missing(iso_files) || !iso_is_object(iso_files)) stop("can only re-read iso_files, not objects of type ", class(iso_files)[1], call. = FALSE)
  single_file <- iso_is_file(iso_files) # to make sure return is the same as supplied
  iso_files <- iso_as_file_list(iso_files)

  # find file ids for reread
  all_files <- names(iso_files)
  old_files <- all_files[get_iso_object_outdated(iso_files)]
  trouble_files <- problems(iso_files)
  error_files <- dplyr::filter(trouble_files, .data$type == "error") %>% dplyr::pull(.data$file_id)
  warning_files <- dplyr::filter(trouble_files, .data$type == "warning") %>% dplyr::pull(.data$file_id)
  good_files <- setdiff(all_files, c(error_files, warning_files))
  reread_file_ids <- c()
  if (reread_files_without_problems) reread_file_ids <- c(reread_file_ids, good_files)
  if (reread_files_with_errors) reread_file_ids <- c(reread_file_ids, error_files)
  if (reread_files_with_warnings) reread_file_ids <- c(reread_file_ids, warning_files)
  if (reread_only_outdated_files) reread_file_ids <- intersect(reread_file_ids, old_files)
  reread_file_ids <- unique(reread_file_ids)

  # reread paths
  file_paths <-
    tibble(
      file_id = reread_file_ids,
      file_root = iso_files[reread_file_ids] %>% map_chr(get_ds_file_root) %>% as.character(),
      file_path = iso_files[reread_file_ids] %>% map_chr(get_ds_file_path, include_root = FALSE) %>% as.character(),
      file_exists = file.path(.data$file_root, .data$file_path) %>% map_lgl(file.exists)
    )

  # safety check for non existent data files
  if (!all(file_paths$file_exists)) {
    msg <-
      # 'unique' paths to account for IARC type multi-file re-reads
      file_paths %>% select(-.data$file_id) %>% filter(!.data$file_exists) %>% unique() %>%
      {
        sprintf(
          "%d file(s) do not exist at their referenced location and can not be re-read. Consider setting a new root directory with iso_set_file_root() first:\n - %s\n",
          length(.$file_exists), paste(sprintf("'%s' in root '%s'", .$file_path, .$file_root), collapse = "\n - "))
      }
    if (stop_if_missing) {
      stop(msg, call. = FALSE)
    } else {
      log_warning(msg)
      iso_files[filter(file_paths, !.data$file_exists)$file_id] <-
        map(
          iso_files[filter(file_paths, !.data$file_exists)$file_id],
          register_warning,
          func = "reread_iso_files",
          details = "file does not exist at its referenced location and can not be re-read",
          warn = FALSE
        )
    }
  }

  # finalize file paths
  file_paths <-
    file_paths %>%
    # don't re-read non-existent
    filter(.data$file_exists) %>%
    # check if has cache
    mutate(
      cachepath = generate_cache_filepaths(file.path(.data$file_root, .data$file_path)),
      has_cache = file.exists(.data$cachepath)
    )

  # check if only rereading changed files
  if (reread_only_changed_files) {
    file_paths <- filter(file_paths, !.data$has_cache)
  }

  # info message
  if (!default("quiet")) {
    changed <- if (reread_only_changed_files) "changed " else ""
    outdated <- if (reread_only_outdated_files) "outdated " else ""
    reread_sum <- ""
    if (reread_files_without_problems && reread_files_with_errors && !reread_files_with_warnings) {
      reread_sum <- " without warnings"
    } else if (reread_files_without_problems && !reread_files_with_errors && reread_files_with_warnings) {
      reread_sum <- " without errors"
    } else if (reread_files_without_problems && !reread_files_with_errors && !reread_files_with_warnings) {
      reread_sum <- " without warnings or errors"
    } else if (!reread_files_without_problems && reread_files_with_errors && reread_files_with_warnings) {
      reread_sum <- " with warnings or errors"
    } else if (!reread_files_without_problems && reread_files_with_errors && !reread_files_with_warnings) {
      reread_sum <- " with errors"
    } else if (!reread_files_without_problems && !reread_files_with_errors && reread_files_with_warnings) {
      reread_sum <- " with warnings"
    }
    sprintf("found %d %s%sdata file(s)%s, re-reading %d/%d%s",
            nrow(file_paths), changed, outdated, reread_sum, nrow(file_paths), length(all_files),
            if(nrow(file_paths) > 0) { ":" } else {"."}) %>%
    log_message()
  }

  # reread files
  if (nrow(file_paths) > 0) {
    # 'unique' paths to account for IARC type multi-file re-reads
    reread_file_paths <- file_paths %>% select(-.data$file_id) %>% unique()
    args <- c(list(
      paths = reread_file_paths$file_path, root = reread_file_paths$file_root,
      read_cache = reread_only_outdated_files,
      reread_outdated_cache = reread_only_outdated_files),
      list(...))
    if (iso_is_continuous_flow(iso_files)) {
      # read continuous flow
      new_iso_files <- iso_as_file_list(do.call(iso_read_continuous_flow, args = args))
    } else if (iso_is_dual_inlet(iso_files)) {
      # read dual inlet
      new_iso_files <- iso_as_file_list(do.call(iso_read_dual_inlet, args = args))
    } else if (iso_is_scan(iso_files)) {
      # read scan
      new_iso_files <- iso_as_file_list(do.call(iso_read_scan, args = args))
    } else {
      stop("re-reading iso_files objects of type ", class(iso_files[[1]])[1], " is not yet supported", call. = FALSE)
    }

    # replace the ones that were re-read, remove missing rereads, and add new files in case there were any
    # (both missing and new can happen e.g. from updated iarc archives)
    actual_reread_ids <- intersect(names(new_iso_files), file_paths$file_id)
    missing_reread_ids <- setdiff(file_paths$file_id, actual_reread_ids)
    new_ids <- setdiff(names(new_iso_files), file_paths$file_id)
    if (length(actual_reread_ids) > 0) iso_files[actual_reread_ids] <- new_iso_files[actual_reread_ids]
    if (length(missing_reread_ids) > 0) iso_files[missing_reread_ids] <- NULL
    if (length(new_ids) > 0) iso_files <- c(iso_files, new_iso_files[new_ids])
  }

  # return single (if passed in as single)
  if (single_file && length(iso_files) == 1) return (iso_files[[1]])
  return(iso_files)
}

#' Re-read iso_files
#'
#' Sometimes it is useful to reload isotope files from their original data files (e.g. after modifying raw data files in vendor software, or after upgrading to a newer version of the isoreader package that provides new functionality). The functions described below are intended to make this very easy. However, re-reading files from disc is only possible if file paths still point to the original raw data files. If they have moved, please use \code{\link{iso_set_file_root}} first to change the root directory of your \code{iso_files}.
#'
#' To re-read files that have been modified on disc, please use \code{iso_reread_changed_files()}. To re-read files because of an isoreader version upgrade, please use \code{iso_reread_outdated_files()}. To try re-reading files that previously had warnings and/or errors, please use \code{iso_reread_problem_files()}.
#'
#' @inheritParams iso_read_files
#' @param iso_files collection of iso_files
#' @param ... additional read parameters that should be used for re-reading the iso_files, see \code{\link{iso_read_dual_inlet}}, \code{\link{iso_read_continuous_flow}} and \code{\link{iso_read_scan}} for details (except \code{read_cache} which is always set to \code{FALSE} to force re-reads).
#' @param stop_if_missing whether to stop re-reading if any of the original data files are missing (if FALSE, will warn about the missing files adding a warning to them, but also re-read those that do exist)
#' @param reread_files_with_errors whether to re-read files that had read in with errors the last time (default TRUE)
#' @param reread_files_with_warnings whether to re-read files that had read in with warnings the last time (default TRUE)
#' @export
iso_reread_files <- function(iso_files, ...) {
  warning("iso_reread_files() has been renamed to the more specific iso_reread_changed_files() and will be removed in the future. Please call iso_reread_changed_files() directly.", immediate. = TRUE, call. = FALSE)
  iso_reread_changed_files(iso_files, ...)
}

#' @details \code{iso_reread_all_files} re-reads all files in the collection.
#'
#' @rdname iso_reread_files
#'@examples
#'# example for re-reading a saved isofile collection
#'iso_turn_reader_caching_off()
#'saved_files_path <- "saved_isofile.scan.rds"
#'
#'# create saved collection
#'iso_get_reader_examples_folder() %>% 
#'  iso_read_scan() %>%
#'  iso_save(saved_files_path)
#'  
#'# load collection
#'iso_read_scan(saved_files_path) %>%
#'  # reread outdated files (alternatively "_all_" or "_changed_")
#'  iso_reread_outdated_files() %>%
#'  # re-save collection to its original location
#'  iso_save(saved_files_path)
#'
#'# cleanup
#'unlink(saved_files_path)
#' @export
iso_reread_all_files <- function(
  iso_files, ..., stop_if_missing = FALSE,
  quiet = default(quiet)) {

  reread_iso_files(
    iso_files, ..., stop_if_missing = stop_if_missing,
    reread_only_changed_files = FALSE,
    reread_only_outdated_files = FALSE,
    reread_files_without_problems = TRUE,
    reread_files_with_errors = TRUE,
    reread_files_with_warnings = TRUE,
    quiet = quiet
  )

}

#' @details \code{iso_reread_changed_files} re-reads all files that have been modified (e.g. in the vendor software) since they were last read by isoreader.
#'
#' @rdname iso_reread_files
#' @export
iso_reread_changed_files <- function(
  iso_files, ..., stop_if_missing = FALSE,
  quiet = default(quiet)) {

  reread_iso_files(
    iso_files, ..., stop_if_missing = stop_if_missing,
    reread_only_changed_files = TRUE,
    reread_only_outdated_files = FALSE,
    reread_files_without_problems = TRUE,
    reread_files_with_errors = TRUE,
    reread_files_with_warnings = TRUE,
    quiet = quiet
  )

}

#' @details \code{iso_reread_outdated_files} re-reads all files that were read with an outdated version of isoreader.
#'
#' @rdname iso_reread_files
#' @export
iso_reread_outdated_files <- function(
  iso_files, ..., stop_if_missing = FALSE,
  quiet = default(quiet)) {

  reread_iso_files(
    iso_files, ..., stop_if_missing = stop_if_missing,
    reread_only_changed_files = FALSE,
    reread_only_outdated_files = TRUE,
    reread_files_without_problems = TRUE,
    reread_files_with_errors = TRUE,
    reread_files_with_warnings = TRUE,
    quiet = quiet
  )
}

#' @details \code{iso_reread_problem_files} re-reads all files that have had errors the last time they were read by isoreader (set \code{reread_files_with_warnings = TRUE} to also re-read those that have warnings).
#'
#' @rdname iso_reread_files
#' @export
iso_reread_problem_files <- function(
  iso_files, ..., stop_if_missing = FALSE,
  reread_files_with_errors = TRUE, reread_files_with_warnings = FALSE,
  quiet = default(quiet)) {

  reread_iso_files(
    iso_files, ..., stop_if_missing = stop_if_missing,
    reread_only_changed_files = FALSE,
    reread_only_outdated_files = FALSE,
    reread_files_without_problems = FALSE,
    reread_files_with_errors = reread_files_with_errors,
    reread_files_with_warnings = reread_files_with_warnings,
    quiet = quiet
  )
}

#' @details \code{iso_reread_storage} is deprecated.
#' @rdname iso_reread_files
#' @export
iso_reread_storage <- function(...) {
  stop("'iso_reread_storage()' has been deprecated in favour of using the more explicit iso_reread_..._files() functions.", call. = FALSE)
}

#' @details \code{iso_reread_archive} is deprecated.
#' @rdname iso_reread_files
#' @export
iso_reread_archive <- function(...) {
  stop("'iso_reread_archive()' has been deprecated in favour of using the more explicit iso_reread_..._files() functions.", call. = FALSE)
}


# caching ====

# generates the cash file paths for iso_files inclulding file name, the file size and last modified
# does NOT include: file path, isoreader version, file contents, read_options
generate_cache_filepaths <- function(filepaths) {

  # generate cache filepaths
  file.info(filepaths) %>%
    tibble::rownames_to_column(var = "filepath") %>%
    dplyr::mutate(
      cache_file = sprintf("%s_%s_%.0f.rds", basename(.data$filepath), format(.data$mtime, "%Y%m%d%H%M%S"), .data$size),
      cache_filepath = file.path(default("cache_dir"), .data$cache_file)
    ) %>%
    dplyr::pull(.data$cache_filepath)
}

# generates old cache file path
generate_old_cache_filepaths <- function(filepaths, read_options = list()) {

  calculate_unf_hash <- function(filepath, size, modified) {
    obj <- c(list(filepath, size, modified), read_options)
    unf(obj)$hash %>% str_c(collapse = "")
  }

  # old cached files versioning
  iso_v <-
    packageVersion("isoreader") %>% {
      if (.$major < 1) paste0(.$major, ".", .$minor)
      else paste0(.$major, ".0")
    }

  file_info <- file.info(filepaths) %>%
    dplyr::as_tibble() %>%
    rownames_to_column() %>%
    select(filepath = .data$rowname, size = .data$size, modified = .data$mtime) %>%
    mutate(
      hash = mapply(calculate_unf_hash, .data$filepath, .data$size, .data$modified),
      cache_file = sprintf("iso_file_v%s_%s_%s.rds", !!iso_v, basename(.data$filepath), .data$hash),
      cache_filepath = file.path(default("cache_dir"), .data$cache_file)
    )

  return(file_info$cache_filepath)
}

# Cache iso_file
cache_iso_file <- function(iso_file, cachepath) {
  if (!file.exists(default("cache_dir"))) dir.create(default("cache_dir"))
  readr::write_rds(iso_file, cachepath)
}

# Load cached iso_file
load_cached_iso_file <- function(filepath) {
  # safety check (should never be a problem)
  if (!file.exists(filepath)) stop("cached file does not exist: ", filepath, call. = FALSE)

  # load
  iso_file <- readr::read_rds(filepath)

  # make sure object in file was loaded properly
  if (!(iso_is_object(iso_file))) {
    sprintf("cached file '%s' does not contain iso_file(s)", basename(filepath)) %>% stop(call. = FALSE)
  }

  # return
  return(iso_file)
}

#' Cleanup cached files
#'
#' Removes all cached files.
#' @param all deprecated
#' @export
iso_cleanup_reader_cache <- function(all = FALSE) {

  if (!missing(all)) warning("the 'all' parameter is deprecated because this function now always deletes all cached files", call. = FALSE, immediate. = TRUE)

  files <- list.files(default("cache_dir"), pattern = "^iso_?file_.*\\.rds$", full.names = TRUE)
  file.remove(files)
  if (!default("quiet")) message("Info: removed all (", length(files), ") cached isoreader files.")
  invisible(NULL)
}
