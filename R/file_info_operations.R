# dplyr style functions that operate on file info

# select ==================

# rename ==================

#' Rename file info columns
#' 
#' Rename individual file info columns (\code{\link{iso_get_file_info}}). Works just like dplyr's \link[dplyr]{rename} except that it can rename different columns into the same name in different iso_files depending on what exists in each file. This is very useful when working with data from multiple instruments that may have the same information (e.g. sample name) stored in different columns. You can also use \link[dplyr]{rename} directly but it will not provide summary information on the operation.
#' 
#' @inheritParams iso_get_raw_data
#' @param ... dplyr-style \link[dplyr]{rename} conditions applied based on each file's file_info (see \code{\link{iso_get_file_info}})
#' @export 
iso_rename_file_info <- function(iso_files, ..., quiet = default(quiet)) {
  # safety checks
  if(!iso_is_object(iso_files)) stop("can only rename file info in iso files", call. = FALSE)
  # rename
  rename(iso_files, ..., quiet = quiet)
}

#' @export
rename.iso_file_list <- function(.data, ..., quiet = TRUE) {
  
  # variables for all files
  rename_quos <- quos(...)
  std_vars <- names(make_iso_file_data_structure()$file_info)
  
  # rename iso file function
  rename_iso_file <- function(isofile) {
    old_vars <- names(isofile$file_info)
    new_vars <- tidyselect::vars_rename(old_vars, !!!rename_quos, .strict = FALSE)
    chg_vars <- tibble(
      file = isofile$file_info$file_id,
      from  = as.character(new_vars),
      to = names(new_vars),
      changed = !to %in% old_vars
    )
    # make sure no core file is renamed
    if (any(prob <- std_vars %in% filter(chg_vars, changed)$from)) {
      glue::glue("trying to rename a core file info column ",
                 "({paste(std_vars[prob], collapse = ', ')}) ",
                 "may lead to unpredictable behaviour and is therefore not allowed, sorry") %>% 
        stop(call. = FALSE)
    }
    isofile$file_info <- dplyr:::select_impl(isofile$file_info, new_vars)
    return(list(isofile = isofile, chg_vars = chg_vars))
  }
  
  # rename
  isofiles_rename <- map(.data, rename_iso_file)
  
  # info summary
  no_chg_vars <- map_lgl(isofiles_rename, ~is.null(.x$chg_vars))
  all_chg_vars <- map(isofiles_rename, "chg_vars") %>% bind_rows()
  
  # check for duplicates
  reps <- all_chg_vars %>% group_by(file, to) %>% 
    summarize(n = n(), from = paste(from, collapse = "', '")) %>% 
    ungroup() %>% 
    filter(n > 1)
  if (nrow(reps)> 0) {
    labels <- reps %>% select(to, from) %>% unique() %>% 
      mutate(label = paste0(" - '", to, "' <= '", from, "'")) %>% 
      { paste(.$label, collapse = "\n") }
    glue::glue("the following column(s) would be assigned to the same name in at ",
               "least 1 file leading to an unresolvable naming conflict:\n{labels}") %>% 
      stop(call. = FALSE)
  }
  
  # summary information
  if (!quiet) {
    info <- all_chg_vars %>% filter(changed) %>% group_by(to, from) %>% tally() %>% 
      mutate(label = sprintf(" - '%s' to '%s' in %d files", from, to, n))
    total_n <- all_chg_vars %>% filter(changed) %>% select(from) %>% unique()
    glue::glue("Info: renaming {nrow(total_n)} file info column(s) wherever they exist across {length(.data)} isofile(s):\n",
               "{paste(info$label, collapse = '\n')}") %>% 
      message()
  }
  
  return(iso_as_file_list(map(isofiles_rename, "isofile")))
}

#' @export
rename.iso_file <- function(.data, ...) {
  rename(iso_as_file_list(.data), ...)[[1]]
}

# mutate ==================


# filter ==================

