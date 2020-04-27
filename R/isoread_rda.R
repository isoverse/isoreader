# read cached isoreader files
# @param ds the data structure to fill
# @param custom reader options - none needed
iso_read_rda <- function(ds, options = list()) {
  
  # safety checks
  if(!iso_is_file(ds)) stop("data structure must be an iso_file", call. = FALSE)
  
  # general deprecation warning
  log_warning("R Data Archives (.rda) are deprecated in favor of R Data Storage (.rds) files, please re-save your isofiles using the new iso_save() function.")
  
  # load rda file
  if (exists("iso_files", inherits = FALSE)) rm("iso_files")
  if (exists("isofiles", inherits = FALSE)) rm("isofiles") # backwards compatibility
  load(get_ds_file_path(ds)) 
  
  # make sure object in file was loaded properly
  if (exists("isofiles", inherits = FALSE)) { 
    if (!is.list(isofiles)) isofiles <- as.list(isofiles)
    is_old_isofile <- map_lgl(isofiles, ~is(.x, "isofile"))
    isofiles[is_old_isofile] <- map(isofiles[is_old_isofile], function(isofile) {
      class(isofile) <- class(isofile) %>% { .[.!="isofile"] } %>% c("iso_file")
      return(isofile)
    })
    iso_files <- iso_as_file_list(isofiles)
  }
  if (!exists("iso_files", inherits = FALSE) || !(iso_is_object(iso_files))) 
    stop("R Data Archive did not contain iso_file data", call. = FALSE)
  iso_files <- iso_as_file_list(iso_files)
  
  # make sure all are the appropriate classes
  if (!all(ok <- lapply(iso_files, class) %>% sapply(identical, class(ds)))) 
    sprintf("Mismatched file types, expected '%s' but encountered '%s'", 
            str_c(class(ds)[1]), str_c(iso_files[!ok] %>% sapply(function(i) class(i)[1]) %>% unique(), collapse = ", ")) %>% 
    stop(call. = FALSE)
  
  # information
  if (!default("quiet")) {
    sprintf("loaded data for %d data files from R Data Archive - checking loaded files for content consistency...", length(iso_files)) %>% 
      log_message()
  }
  
  return(iso_files)
}
