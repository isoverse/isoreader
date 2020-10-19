# Get the column names for a set of parameters referencing columns in a data frame. Compatible with all dplyr type column selection criteria including both standard and non-standard evaluation. Throws errors if expressions cannot be evaluated (unless \code{cols_must_exist = FALSE}) or if an incorrect number of columns are identified for a given parameter. To ignore missing columns entirely, set \code{cols_must_exist = FALSE} and \code{warn = FALSE}.
# @param df the data frame
# @param ... named expressions with variable selection criteria (anything that tidyselect::eval_select supports)
# @param n_reqs named list to specify how many columns are allowed/required for each selection criterion, default for all that are not specified is 1.
# Allowed values are a specific number 1,2,3,4, etc. "*" for any number, "?" for 0 or 1 and "+" for at least one.
# @param type_reqs named list to specify what types certain columns must be, allowed: "list" (also includes "vctrs_list_of"), "numeric", "integer", "character", "logical"
# @param cols_must_exist - if TRUE, will throw an error if a column does not exist, otherwise just warning (unless warn = FALSE)
# @return list of column names for each entry (may contain multiple depending on selection conditions)
get_column_names <- function(df, ..., df_name = rlang::as_label(rlang::enexpr(df)), n_reqs = list(), type_reqs = list(), cols_must_exist = TRUE, warn = TRUE) {
  
  # df name and data frame test
  if (missing(df)) stop("no data frame supplied", call. = FALSE)
  df_name <- force(df_name)
  df <- force(df)
  if (!is.data.frame(df))
    sprintf("parameter '%s' is not a data frame", df_name) %>% stop(call. = FALSE)
  
  # get colum name expressions from ...
  cols_exps <- list(...) %>% 
    # make sure to evaluate calls to default
    resolve_defaults() %>% 
    # convert quos to expressions (to ensure evaluation inside the df data frame to avoid name conflicts)
    map(~{if (rlang::is_quosure(.x)) rlang::quo_get_expr(.x) else .x}) %>% 
    # naming
    { if(is.null(names(.))) rlang::set_names(., rep("", length(.))) else . }
  
  # find column positions
  pos_results <- map(cols_exps, safe_local_eval_select, data = df)
  ok <- map_lgl(pos_results, ~is.null(.x$error))
  
  # summarize if there were any errors
  if (!all(ok)) {
    params <-
      map2_chr(names(cols_exps)[!ok], cols_exps[!ok], function(var, val) {
        if (nchar(var) > 0 && var != rlang::as_label(val)) str_c(var, " = ", rlang::as_label(val))
        else rlang::as_label(val)
      }) %>% 
      collapse("', '", last = "' and '")
    errors <- map_chr(pos_results[!ok], ~stringr::str_replace(.x$error, "\n", " ")) %>% 
      paste(collapse = "\n- ")
    
    # check for unique names error
    if (any(stringr::str_detect(errors, "Names must be unique"))) {
      stop("as of isoreader 1.1.0, renamed columns must be unique by default, to allow for faster processing. To allow for the recoding of column names across different iso_files, please set 'file_specific = TRUE' (this is slower but more flexible).", call. = FALSE)
    }
    
    err_msg <- 
      if (sum(!ok) > 1) 
        glue("'{params}' refer to unknown columns in data frame '{df_name}':\n- {errors}") 
      else 
        glue("'{params}' refers to unknown column(s) in data frame '{df_name}':\n- {errors}") 
    if (cols_must_exist) { 
      # throw error
      stop(err_msg, call. = FALSE)
    } else {
      # just a warning and find the columns omitting those missing
      if (warn) warning(err_msg, immediate. = TRUE, call. = FALSE)
      pos_results <- map(cols_exps, safe_local_eval_select, data = df, strict = FALSE)
    }
  }
  
  # check on the number requirements for each column match
  cols <- map(pos_results, ~eval_select_pos_to_cols(.x$result, data = df))
  if (any(missing <- !names(n_reqs) %in% names(cols)))
    glue("column requirements for unknow parameter(s) provided: {collapse(names(n_reqs[missing]), ', ')}") %>%
    stop(call. = FALSE)
  
  ## reqs labels
  all_n_reqs <- rep(1, length(cols)) %>% as.list() %>% rlang::set_names(names(cols)) %>% modifyList(n_reqs) %>% { .[names(cols)] }
  n_req_types <- c("*" = "any number", "+" = "at least one", "?" = "none or one", "integer" = "the specified number")
  all_n_req_types <- map_chr(all_n_reqs, function(req) {
    if (is_integerish(req)) return("integer")
    else if (req %in% names(n_req_types)) return(req)
    else return(NA_character_)
  })
  if ( any(unknown <- map_lgl(all_n_req_types, is.na))) {
    n_req_unknown <- map_chr(all_n_reqs[unknown], as.character)
    glue("unknown number requirement specification(s): '{collapse(n_req_unknown, \"', '\")}'. Allowed are: {collapse(names(n_req_types), ', ')}") %>%
      stop(call. = FALSE)
  }
  
  ## n reqs test
  col_meets_n_reqs <- map2_lgl(cols, all_n_reqs, function(col, req) {
    if (is_integerish(req) && length(col) == as.integer(req)) return(TRUE)
    else if (req == "+" && length(col) > 0) return(TRUE)
    else if (req == "?" && length(col) %in% c(0L, 1L)) return(TRUE)
    else if (req == "*") return(TRUE)
    else return(FALSE)
  })
  
  ## report missing columns
  if (!all(col_meets_n_reqs)) {
    n_errors <-
      sprintf("'%s%s' refers to %d column(s) instead of %s (%s)",
              names(cols_exps)[!col_meets_n_reqs] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
              map_chr(cols_exps[!col_meets_n_reqs], rlang::as_label),
              map_int(cols[!col_meets_n_reqs], length),
              n_req_types[all_n_req_types[!col_meets_n_reqs]],
              map_chr(all_n_reqs[!col_meets_n_reqs], as.character)) %>%
      collapse("\n- ")
    glue("not all parameters refer to the correct number of columns in data frame '{df_name}':\n- {n_errors}") %>%
      stop(call. = FALSE)
  }
  
  # check on the type requirements for each column
  if (length(type_reqs) > 0) {
    
    # valid types
    types <- c(list = "nested (<list>)", vctrs_list_of = "nested (<list of>)",
               numeric = "numeric (<dbl>)", integer = "integer (<int>)", 
               character = "text (<chr>)", logical = "logical (<lgl>)")
    if (!all(ok <- unlist(type_reqs) %in% names(types))) {
      type_req_unknown <- unlist(type_reqs)[!ok]
      glue("unknown type requirement specification(s): '{collapse(type_req_unknown, \"', '\")}'. Allowed are: {collapse(names(types), ', ')}") %>%
        stop(call. = FALSE)
    }
    
    # find type requirement problems
    all_type_reqs <- rep(NA_character_, length(cols)) %>% as.list() %>% rlang::set_names(names(cols)) %>% modifyList(type_reqs) %>% { .[names(cols)] }
    all_df_types <- map_chr(df, ~class(.x)[1])
    col_meets_type_reqs <- map2_lgl(cols, all_type_reqs, function(col, req) {
      if (is.na(req)) return(TRUE)
      # support vctrs_list_of for nested list colulmns
      else if (req == "list" && all_df_types[col] == "vctrs_list_of") return (TRUE)
      else if (all(all_df_types[col] == req)) return(TRUE)
      else return(FALSE)
    })
    
    ## report type mismatches
    if (!all(col_meets_type_reqs)) {
      n_errors <-
        sprintf("'%s%s' refers to column(s) of type '%s' instead of '%s'",
                names(cols_exps)[!col_meets_type_reqs] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
                map_chr(cols_exps[!col_meets_type_reqs], rlang::as_label),
                map_chr(cols[!col_meets_type_reqs], ~collapse(all_df_types[.x], "/")),
                map_chr(all_type_reqs[!col_meets_type_reqs], ~types[.x])) %>%
        collapse("\n- ")
      glue("not all parameters refer to the correct column types in data frame '{df_name}':\n- {n_errors}") %>%
        stop(call. = FALSE)
    }
    
  }
  
  return(cols)
}

# convert positions to column names
# @param the positions
# @param df the data frame
eval_select_pos_to_cols <- function(pos, data) {
  # assign names based on position indices
  cols <- rlang::set_names(names(data)[pos], names(pos))
  # assign any missing names based on the values (this might be a fix for tidyselect bug)
  names(cols)[nchar(names(cols)) == 0] <- cols[nchar(names(cols)) == 0]
  return(cols)
}

# safe local evaluation of tidyselect::eval_select
# note: don't use purrr::safely as it invalidates the local scope!
# @return list(return = value, error = NULL) if successful or list(return = NULL, error = character) if failed
safe_local_eval_select <- function(expr, data, ...) {
  # try catch find positions
  pos <- tryCatch(tidyselect::eval_select(expr, data, env = as.environment(2), ...), error = conditionMessage)
  if (is.integer(pos)) 
    return(list(result = pos, error = NULL))
  else
    return(list(result = NULL, error = pos))
}
