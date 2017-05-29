#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# helper to make sure columns exist
col_check <- function(col, data, fun = sys.call(-1)) {
  if (!is.null(col) && length(missing <- setdiff(col, names(data))) > 0) 
    stop("column(s) not in data: '", str_c(col, collapse = "', '"), "'. You may have to change the parameters in your call to ", fun, call. = FALSE)
}

# get supported continuous flow file types and which functions they map to
get_supported_cf_files <- function() {
  tribble(
    ~extension, ~fun,        ~description,
    "cf",       isoread_cf,  "Isodat Continuous Flow file format (older)",
    "dxf",      isoread_dxf, "Isodat Continuous Flow file format (newer)"
  )
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