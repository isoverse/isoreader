# read ionos .iarc archieves for their continuous flow data
# @param ds the data structure to fill
isoread_flow_iarc <- function(ds) {
  
  if(!is(ds, "isofile") || !is(ds, "dual_inlet")) 
    stop("data structure must have class 'isofile' and 'dual_inlet'", call. = FALSE)
  col_check(c("file_info", "mass_data"), ds)
  col_check(c("file_name", "file_path"), ds$file_info)
  
  return(ds)  
}
