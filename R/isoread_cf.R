# read isodat .cf file
# @param ds the data structure to fill
isoread_cf <- function(ds) {
  
  # safety checks
  if(!is_continuous_flow(ds)) 
    stop("data structure must be a 'continuous_flow' isofile", call. = FALSE)
  
  ds %>% register_error("cf read not yet implemented") %>% 
    return()
}
