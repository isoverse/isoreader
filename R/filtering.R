#' Filter iso_files
#' 
#' Filter for specific iso_files using file info parameters, most commonlty file_id and file_datetime but others can be used as well if they exist.
#' @inheritParams iso_get_raw_data
#' @param ... filter conditions applied based on each file's file_info (see \code{\link{iso_get_file_info}})
#' @export 
iso_filter_files <- function(iso_files, ..., quiet = default(quiet)) {
  # safety checks
  if(!iso_is_object(iso_files)) stop("can only calculate ratios for iso files", call. = FALSE)
  iso_files <- iso_as_file_list(iso_files)
  
  file_info <- iso_get_file_info(iso_files, quiet = TRUE) %>% 
    filter(...)
  
  # information
  if (!quiet) {
    str_interp("Info: applying file filter, keeping $[d]{n} of $[d]{n_all} files", 
               list(n = nrow(file_info), n_all = length(iso_files))) %>% message()
  }
  
  # return filtered iso_files
  if (nrow(file_info) == 0) return(NULL)
  else iso_files[names(iso_files) %in% file_info$file_id]
}