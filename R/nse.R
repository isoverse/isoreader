# Get the column names for a set of parameters referencing columns in a data frame. Compatible with all dplyr type column selection
# criteria including both standard and non-standard evaluation. Throws errors if expressions cannot be evaluated or if an incorrect
# number of columns are identified for a given parameter.
# @param df the data frame
# @param ... named quoted variable selection criteria (anything that tidyselect::vars_select supports)
# @param n_reqs named list to specify how many columns are allowed/required for each selection criterion, default for all that are not specified is 1.
# Allowed values are a specific number 1,2,3,4, etc. "*" for any number, "?" for 0 or 1 and "+" for at least one.
# @param type_reqs named list to specify what types certain columns must be, allowed: "list", "numeric", "integer", "character"
# @param cols_must_exist - if TRUE, will throw an error if a column does not exist, otherwise just  warning
# @return list of column names for each entry (may contain multiple depending on selection conditions)
get_column_names <- function(df, ..., n_reqs = list(), type_reqs = list(), cols_must_exist = TRUE) {
  
  # df name and data frame test
  if (missing(df)) stop("no data frame supplied", call. = FALSE)
  df_name <- enquo(df) %>% quo_text()
  df <- enquo(df) %>% eval_tidy()
  if (!is.data.frame(df))
    glue("parameter {df_name} is not a data frame") %>% stop(call. = FALSE)
  
  # use a safe version of vars_select to get all the column names
  safe_vars_select <- safely(vars_select)
  cols_quos <- quos(!!!list(...)) %>% 
    # make sure to evaluate calls to default
    resolve_defaults() %>% 
    # make sure that the expressions are locally evaluated
    map(~quo(!!get_expr(.x)))
  cols_results <- map(cols_quos, ~safe_vars_select(names(df), !!!.x))
  ok <- map_lgl(cols_results, ~is.null(.x$error))
  
  # summarize if there were any errors
  if (!all(ok)) {
    params <-
      map2_chr(names(cols_quos)[!ok], cols_quos[!ok], function(var, val) {
        if (nchar(var) > 0 && var != quo_text(val)) str_c(var, " = ", quo_text(val))
        else quo_text(val)
      }) %>% 
      collapse("', '", last = "' and '")
    errors <- map_chr(cols_results[!ok], ~.x$error$message) %>% collapse("\n- ")
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
      warning(err_msg, immediate. = TRUE, call. = FALSE)
      cols_results <- map(cols_quos, ~safe_vars_select(names(df), !!!.x, .strict = FALSE))
    }
  }
  
  # check on the number requirements for each column match
  cols <- map(cols_results, ~(.x$result))
  if (any(missing <- !names(n_reqs) %in% names(cols)))
    glue("column requirements for unknow parameter(s) provided: {collapse(names(n_reqs[missing]), ', ')}") %>%
    stop(call. = FALSE)
  
  ## reqs labels
  all_n_reqs <- rep(1, length(cols)) %>% as.list() %>% setNames(names(cols)) %>% modifyList(n_reqs) %>% { .[names(cols)] }
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
              names(cols_quos)[!col_meets_n_reqs] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
              map_chr(cols_quos[!col_meets_n_reqs], quo_text),
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
    types <- c(list = "nested (<list>)", numeric = "numeric (<dbl>)", integer = "integer (<int>)", 
               character = "text (<chr>)", logical = "logical (<lgl>)")
    if (!all(ok <- unlist(type_reqs) %in% names(types))) {
      type_req_unknown <- unlist(type_reqs)[!ok]
      glue("unknown type requirement specification(s): '{collapse(type_req_unknown, \"', '\")}'. Allowed are: {collapse(names(types), ', ')}") %>%
        stop(call. = FALSE)
    }
    
    # find type requirement problems
    all_type_reqs <- rep(NA_character_, length(cols)) %>% as.list() %>% setNames(names(cols)) %>% modifyList(type_reqs) %>% { .[names(cols)] }
    all_df_types <- map_chr(df, ~class(.x)[1])
    col_meets_type_reqs <- map2_lgl(cols, all_type_reqs, function(col, req) {
      if (is.na(req)) return(TRUE)
      else if (all(all_df_types[col] == req)) return(TRUE)
      else return(FALSE)
    })
    
    ## report type mismatches
    if (!all(col_meets_type_reqs)) {
      n_errors <-
        sprintf("'%s%s' refers to column(s) of type '%s' instead of '%s'",
                names(cols_quos)[!col_meets_type_reqs] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
                map_chr(cols_quos[!col_meets_type_reqs], quo_text),
                map_chr(cols[!col_meets_type_reqs], ~collapse(all_df_types[.x], "/")),
                map_chr(all_type_reqs[!col_meets_type_reqs], ~types[.x])) %>%
        collapse("\n- ")
      glue("not all parameters refer to the correct column types in data frame '{df_name}':\n- {n_errors}") %>%
        stop(call. = FALSE)
    }
    
  }
  
  return(cols)
}

# resolve default cols in a list of quos
resolve_defaults <- function(quos) {
  resolve_default <- function(x) if (quo_is_lang(x) && lang_head(x) == sym("default")) eval_tidy(x) else x
  if (is_quosure(quos)) return(resolve_default(quos))
  else map(quos, resolve_default)
}

# Convert quo to text accounting for plain text and symbol quos
quos_to_text <- function(lquos, check_for_validity = TRUE, variable = "variable") {
  single_quo <- is_quosure(lquos)
  lquos <- quos(!!!lquos)
  are_text_quos <- map_lgl(lquos, ~is.character(quo_expr(.x)))
  are_symbol_quos <- map_lgl(lquos, quo_is_symbol)
  
  # check for validity
  if (check_for_validity && !all(ok <- are_text_quos | are_symbol_quos)) {
    params <-
      str_c(names(lquos)[!ok] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
            map_chr(lquos[!ok], quo_text)) %>%
      collapse("', '", last = "' and '")
    if (sum(!ok) > 1)
      glue("parameters '{params}' do not refer to valid {variable} names") %>% stop(call. = FALSE)
    else
      glue("parameter '{params}' does not refer to a valid {variable} name") %>% stop(call. = FALSE)
  }
  
  text_quos <-
    map2_chr(lquos, are_text_quos, function(lquo, is_text)
      if(is_text) quo_expr(lquo) else quo_text(lquo)) %>% 
    as.list()
  if (single_quo) return(text_quos[[1]])
  else return(text_quos)
}

