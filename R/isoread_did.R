# read isodat .did file
# @param ds the data structure to fill
isoread_did <- function(ds, ...) {
  
  # safety checks
  if(!is(ds, "isofile") || !is(ds, "dual_inlet")) 
    stop("data structure must have class 'isofile' and 'dual_inlet'", call. = FALSE)
  col_check(c("file_info", "raw_data"), ds)
  col_check(c("file_id", "file_path"), ds$file_info)
  
  return(ds)  
}
