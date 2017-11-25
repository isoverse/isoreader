# read cached isoreader files
# @param ds the data structure to fill
isoread_rda <- function(ds, ...) {
  
  # safety checks
  if(!is_isofile(ds)) stop("data structure must be an isofile", call. = FALSE)
  
  # load rda file
  if (exists("isofiles", inherits = FALSE)) rm("isofiles") 
  load(ds$file_info$file_path) 
  
  # make sure object in file was loaded properly
  if (!exists("isofiles", inherits = FALSE) || !(is_iso_object(isofiles))) 
    stop("R Data Archive did not contain isofile data", call. = FALSE)
  isofiles <- as_isofile_list(isofiles)
  
  # make sure all are the appropriate classes
  if (!all(ok <- lapply(isofiles, class) %>% sapply(identical, class(ds)))) 
    sprintf("Mismatched file types, expected '%s' but encountered '%s'", 
            str_c(class(ds)[1]), str_c(isofiles[!ok] %>% sapply(function(i) class(i)[1]) %>% unique(), collapse = ", ")) %>% 
    stop(call. = FALSE)
  
  # information
  if (!default(quiet)) {
    sprintf("Info: loaded data for %d data files from R Data Archive - checking loaded files for content consistency...", length(isofiles)) %>% message()
  }
  
  # check for version warning
  versions <- map(isofiles, `[[`, "version")
  ok_version <- map_lgl(versions, same_as_isoreader_version, packageVersion("isoreader"))
  if (any(!ok_version)) {
    messages <- sprintf("file created by a different version of the isoreader package (%s)", map_chr(versions[!ok_version], as.character))
    isofiles[!ok_version] <- map2(isofiles[!ok_version], messages, register_warning, func = "isoread_rda", warn = FALSE)
  }

  if (any(!ok_version)) {
    sprintf("%.0f of the %.0f data files stored in the R Data Archive ('%s') were created by a different version of the isoreader package. This may lead to processing problems.\nConsider re-reading the original data files using the 'reread_isofiles()' or 'reread_isofiles_archive()' function. ", sum(!ok_version), length(isofiles), ds$file_info$file_id) %>% 
    warning(call. = FALSE, immediate. = TRUE)
  }

  return(isofiles)
}