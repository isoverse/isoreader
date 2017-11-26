# read cached isoreader files
# @param ds the data structure to fill
isoread_rda <- function(ds, ...) {
  
  # safety checks
  if(!iso_is_file(ds)) stop("data structure must be an iso_file", call. = FALSE)
  
  # load rda file
  if (exists("iso_files", inherits = FALSE)) rm("iso_files") 
  load(ds$file_info$file_path) 
  
  # make sure object in file was loaded properly
  if (!exists("iso_files", inherits = FALSE) || !(iso_is_object(iso_files))) 
    stop("R Data Archive did not contain iso_file data", call. = FALSE)
  iso_files <- iso_as_file_list(iso_files)
  
  # make sure all are the appropriate classes
  if (!all(ok <- lapply(iso_files, class) %>% sapply(identical, class(ds)))) 
    sprintf("Mismatched file types, expected '%s' but encountered '%s'", 
            str_c(class(ds)[1]), str_c(iso_files[!ok] %>% sapply(function(i) class(i)[1]) %>% unique(), collapse = ", ")) %>% 
    stop(call. = FALSE)
  
  # information
  if (!default(quiet)) {
    sprintf("Info: loaded data for %d data files from R Data Archive - checking loaded files for content consistency...", length(iso_files)) %>% message()
  }
  
  # check for version warning
  versions <- map(iso_files, `[[`, "version")
  ok_version <- map_lgl(versions, same_as_isoreader_version, packageVersion("isoreader"))
  if (any(!ok_version)) {
    messages <- sprintf("file created by a different version of the isoreader package (%s)", map_chr(versions[!ok_version], as.character))
    iso_files[!ok_version] <- map2(iso_files[!ok_version], messages, register_warning, func = "isoread_rda", warn = FALSE)
  }

  if (any(!ok_version)) {
    sprintf("%.0f of the %.0f data files stored in the R Data Archive ('%s') were created by a different version of the isoreader package. This may lead to processing problems.\nConsider re-reading the original data files using the 'iso_reread_files()' or 'iso_reread_archive()' function. ", sum(!ok_version), length(iso_files), ds$file_info$file_id) %>% 
    warning(call. = FALSE, immediate. = TRUE)
  }

  return(iso_files)
}