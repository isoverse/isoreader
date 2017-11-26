#' Filter isofiles
#' 
#' Filter for specific isofiles using file info parameters, most commonlty file_id and file_datetime but others can be used as well if they exist.
#' @inheritParams iso_aggregate_raw_data
#' @param ... filter conditions applied based on each file's file_info (see \code{\link{iso_aggregate_file_info}})
#' @export 
iso_filter_files <- function(isofiles, ..., quiet = default(quiet)) {
  # safety checks
  if(!iso_is_object(isofiles)) stop("can only calculate ratios for iso files", call. = FALSE)
  isofiles <- iso_as_file_list(isofiles)
  
  file_info <- iso_aggregate_file_info(isofiles, quiet = TRUE) %>% 
    filter(...)
  
  # information
  if (!quiet) {
    str_interp("Info: applying file filter, keeping $[d]{n} of $[d]{n_all} files", 
               list(n = nrow(file_info), n_all = length(isofiles))) %>% message()
  }
  
  # return filtered isofiles
  if (nrow(file_info) == 0) return(NULL)
  else isofiles[names(isofiles) %in% file_info$file_id]
}