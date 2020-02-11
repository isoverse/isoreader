# Test quo expressions by confirming they can create a valid column in a mutate
# @note that global variables will be interpreted even in the context of the data frame
# ideally this will be only within the data frame
check_expressions <- function(df, ...) {
  
  # df name and data frame test
  if (missing(df)) stop("no data frame supplied", call. = FALSE)
  df_name <- enquo(df) %>% rlang::as_label()
  df <- enquo(df) %>% eval_tidy()
  if (!is.data.frame(df))
    glue("parameter {df_name} is not a data frame") %>% stop(call. = FALSE)
  
  # use a safe version of mutate to check all expresions
  # (use mutate instead of eval_tidy to make sure it's absolutely valid)
  safe_eval <- safely(mutate)
  expr_quos <- quos(!!!list(...)) %>% 
    # make sure to evaluate calls to default
    resolve_defaults() %>% 
    # make sure that the expressions are locally evaluated
    map(~quo(!!get_expr(.x)))
  expr_quos <- expr_quos[!map_lgl(expr_quos, quo_is_null)]
  expr_errors <- map(expr_quos, ~safe_eval(df, !!.x)$error)

  # check results
  ok <- map_lgl(expr_errors, ~is.null(.x))

  # summarize if there were any errors
  if (!all(ok)) {
    params <-
      map2_chr(names(expr_quos)[!ok], expr_quos[!ok], function(var, val) {
        if (nchar(var) > 0 && var != rlang::as_label(val)) str_c(var, " = ", rlang::as_label(val))
        else rlang::as_label(val)
      }) %>%
      collapse("', '", last = "' and '")
    errors <- map_chr(expr_errors[!ok], ~.x$message) %>% collapse("\n- ")
    err_msg <-
      if (sum(!ok) > 1)
        glue("'{params}' are invalid expressions in data frame '{df_name}':\n- {errors}")
      else
        glue("'{params}' is not a valid expression in data frame '{df_name}':\n- {errors}")
    stop(err_msg, call. = FALSE)
  }

  return(invisible(df))
}

# Get the column names for a set of parameters referencing columns in a data frame. Compatible with all dplyr type column selection
# criteria including both standard and non-standard evaluation. Throws errors if expressions cannot be evaluated or if an incorrect
# number of columns are identified for a given parameter.
# @param df the data frame
# @param ... named expressions with variable selection criteria (anything that tidyselect::eval_select supports)
# @param n_reqs named list to specify how many columns are allowed/required for each selection criterion, default for all that are not specified is 1.
# Allowed values are a specific number 1,2,3,4, etc. "*" for any number, "?" for 0 or 1 and "+" for at least one.
# @param type_reqs named list to specify what types certain columns must be, allowed: "list" (also includes "vctrs_list_of"), "numeric", "integer", "character", "logical"
# @param cols_must_exist - if TRUE, will throw an error if a column does not exist, otherwise just warning (unless warn = FALSE)
# @return list of column names for each entry (may contain multiple depending on selection conditions)
get_column_names <- function(df, ..., n_reqs = list(), type_reqs = list(), cols_must_exist = TRUE, warn = TRUE) {
  
  # df name and data frame test
  if (missing(df)) stop("no data frame supplied", call. = FALSE)
  df_name <- rlang::as_label(rlang::enexpr(df))
  df <- force(df)
  if (!is.data.frame(df))
    sprintf("parameter '%s' is not a data frame", df_name) %>% stop(call. = FALSE)
  
  # use a safe version of eval select to get all the column names
  # note: uses tidyselect:eval_select because vars_select is in questioning stage
  safe_vars_select <- function(col_exp, ...) {
    safe_pos <- safely(tidyselect::eval_select)(col_exp, data = df, ...)
    if (is.null(safe_pos$error)) {
      # assign names based on opsition indices
      safe_pos$result <- rlang::set_names(names(df)[safe_pos$result], names(safe_pos$result))
      # assign any missing names with the values
      names(safe_pos$result)[nchar(names(safe_pos$result)) == 0] <- 
        safe_pos$result[nchar(names(safe_pos$result)) == 0]
    }
    return(safe_pos)
  }
  
  # get colum name expressions from ...
  cols_exps <- list(...) %>% 
    # make sure to evaluate calls to default
    resolve_defaults() %>% 
    # convert quos to expressions (to ensure evaluation inside the df data frame to avoid name conflicts)
    map(~{if (rlang::is_quosure(.x)) rlang::quo_get_expr(.x) else .x}) %>% 
    # naming
    { if(is.null(names(.))) setNames(., rep("", length(.))) else . }
  
  cols_results <- map(cols_exps, safe_vars_select)
  ok <- map_lgl(cols_results, ~is.null(.x$error))
  
  # summarize if there were any errors
  if (!all(ok)) {
    params <-
      map2_chr(names(cols_exps)[!ok], cols_exps[!ok], function(var, val) {
        if (nchar(var) > 0 && var != rlang::as_label(val)) str_c(var, " = ", rlang::as_label(val))
        else rlang::as_label(val)
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
      if (warn) warning(err_msg, immediate. = TRUE, call. = FALSE)
      cols_results <- map(cols_exps, safe_vars_select, strict = FALSE)
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
    all_type_reqs <- rep(NA_character_, length(cols)) %>% as.list() %>% setNames(names(cols)) %>% modifyList(type_reqs) %>% { .[names(cols)] }
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
