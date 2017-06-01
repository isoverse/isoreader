#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# helper to make sure columns exist
col_check <- function(cols, data, fun = sys.call(-1), msg = "You may have to change the parameters in your function call") {
  if (!is.null(cols) && length(missing <- setdiff(cols, names(data))) > 0) 
    stop("column(s) not in data: '", str_c(missing, collapse = "', '"), 
         "'. ", msg, ". Function: ", fun, call. = FALSE)
}

#' Show supported file types
#' @export
show_supported_file_types <- function() {
  extension <- NULL
  description <- NULL
  sprintf(
    c("Isoreader supported file types",
      "Dual Inlet | 'isoread_dual_inlet()':",
      "   %s", 
      "Continuous flow | 'isoread_continuous_flow()':",
      "   %s") %>% str_c(collapse = "\n"),
    get_supported_di_files() %>% 
      mutate(label = str_c(".", extension, " = ", description)) %>% 
      { str_c(.$label, collapse = "\n   ") },
    get_supported_cf_files() %>% 
      mutate(label = str_c(".", extension, " = ", description)) %>% 
      { str_c(.$label, collapse = "\n   ") }
  ) %>% 
    cat()
}

# get file extension
get_file_ext <- function(filepath) {
  basename(filepath) %>% str_extract("\\.[^.]+$")
}

# get all files in supplied folders that fit the provided data types
# @param extensions regular extensions (without the dots), will be turned into regexp pattern
retrieve_file_paths <- function(paths, extensions = c()) {
  
  # existence check
  exist <- paths %>% sapply(file.exists)
  if (!all(exist)) 
    stop("files/folders do not exist:\n\t", paths[!exist] %>% str_c(collapse = "\n\t"), call. = FALSE)
  
  # extensions check
  if(length(extensions) == 0) stop("no extensions provided for retrieving file paths", call. = FALSE)
  isdir <- paths %>% lapply(file.info) %>% map_lgl("isdir")
  pattern <- extensions %>% str_c(collapse = "|") %>% { str_c("\\.(", ., ")$") }
  has_ext <- paths[!isdir] %>% str_detect(pattern)
  if (!all(has_ext))
    stop("some file(s) do not have one of the supported extensions (", 
         str_c(extensions, collapse = ", "), 
         "):\n\t", paths[!isdir][!has_ext] %>% str_c(collapse = "\n\t"), call. = FALSE)
  
  # retrieve all the files
  filepaths <- 
    mapply(function(path, isdir) {
      list(
        if (!isdir) return(path)
        else file.path(path, list.files(path, pattern = pattern, recursive = TRUE, include.dirs = FALSE))
      )
    }, paths, isdir, USE.NAMES = FALSE) %>% 
    # simplify
    unlist() %>% 
    # make sure all files unique
    unique()
  
  # double check that filenames are unique
  filenames <- basename(filepaths)
  if (anyDuplicated(filenames)) {
    dups <- duplicated(filenames) | duplicated(filenames, fromLast = T)
    stop("some files from different folders have identical file names:\n\t", 
         filepaths[dups] %>% str_c(collapse = "\n\t"), call. = FALSE)
  }
   
  return(filepaths)
}

# execute function with catch if not in debug mode
# @param func can be either function name or function call
# problems are reported in obj
exec_func_with_error_catch <- function(func, obj, ...) {
  if (is.character(func)) func_name <- func
  else func_name <- substitute(func) %>% deparse()
  if (default("debug")) {
    # debug mode, don't catch any errors
    obj <- do.call(func, args = c(list(obj), list(...)))
  } else {
    # regular mode, catch errors and report them as problems
    obj <- 
      tryCatch({
        do.call(func, args = c(list(obj), list(...)))
      }, error = function(e){
        return(register_error(obj, e$message, func = func_name))
      })
  }
  return(obj)
}

# find parent call regardless of if it's called by piping or traditional
# ignores tryCatch calls
# @param current_func the name of the function this is called from (character)
find_parent_call <- function(current_func) {
  calls <- sys.calls()
  calls <- sapply(calls, as.character)
  is_trycatch <- sapply(calls, function(x) any(str_detect(x, "tryCatch")))
  calls <- calls[!is_trycatch]
  has_func <- sapply(calls, function(x) any(str_detect(x, current_func))) %>% which()
  calls[[has_func[1] - 1]][1]
}
