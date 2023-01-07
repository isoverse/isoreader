# Binary file utils =============

# Generate binary file structure object
template_binary_file_object <- function() {
  structure(
    list(
      raw = raw(),
      data = list(),
      pos = 1L, # current position within the file
      max_pos = NULL, # max position to consider during operations
      error_prefix = "" # what prefix to append to errors
    ),
    class = c("binary_file")
  )
}

# Read binary file
read_binary_file <- function(filepath, bfile = template_binary_file_object()) {
  
  if (!file.exists(filepath) || file.info(filepath)$isdir == TRUE)
    stop("file does not exist or is a directory: ", filepath, call. = TRUE)
  
  # read
  size <- file.info(filepath)$size
  con <- file(filepath, "rb")
  bfile$raw <- readBin(con, raw(), n = size)
  bfile$max_pos <- length(bfile$raw)
  close(con)
  
  return(bfile)
}

# get captured data from binary file
get_captured_data <- function(bfile, id) {
  return(bfile$data[[id]])
}

# reset binary file navigation
reset_binary_file_navigation <- function(bfile) {
  bfile$pos <- 1L
  bfile$max_pos <- length(bfile$raw)
  return(bfile)
}

# set an error prefix for the operations that fellow
set_binary_file_error_prefix <- function(bfile, prefix = "") {
  bfile$error_prefix <- if (nchar(prefix) >0) str_c(prefix, " - ") else ""
  return(bfile)
}

# check if it's a binary file
check_bfile <- function(bfile) {
  if (is.null(bfile) || !is(bfile, "binary_file")) 
    stop("the binary source file is no longer available, make sure to run iso_turn_debug_on() before reading a file to have access to the source", call. = FALSE)
}

#' Throw source file operation error with useful debugging information for the file type.
#' 
#' @param source_file_obj the source file object for which to print the operation error
#' @param msg the message to print
#' @param ... additional parameters depending on source file type
#' @export 
iso_source_file_op_error <- function(source_file_obj, msg, ...) {
  UseMethod("iso_source_file_op_error")
} 

#' @export
iso_source_file_op_error.default <- function(source_file_obj, msg, ...) {
  stop("this function is not defined for source file objects of type '", 
       class(source_file_obj)[1], "'", call. = FALSE)
}

#' @export
iso_source_file_op_error.binary_file <- function(source_file_obj, msg, ...) {
  stop(sprintf("%s%s (pos %.0f, max %.0f)", source_file_obj$error_prefix, 
               msg, source_file_obj$pos, source_file_obj$max_pos), call. = FALSE)
}

# Position Navigation ======

# skip nbyte number of bytes in the raw data stream
skip_pos <- function(bfile, nbyte) {
  move_to_pos(bfile, bfile$pos + nbyte)
}

# move to position
move_to_pos <- function(bfile, pos, reset_cap = FALSE) {
  check_bfile(bfile)
  if (reset_cap) bfile$max_pos <- length(bfile$raw)
  if (pos > bfile$max_pos) {
    iso_source_file_op_error(
      bfile, sprintf("cannot move to position %.0f as it exceeds position max set at %.0f", 
                     pos, bfile$max_pos))
  } 
  bfile$pos <- as.integer(pos)
  return(bfile)
}

# cap at position
cap_at_pos <- function(bfile, pos) {
  check_bfile(bfile)
  if(is.null(pos)) stop("cannot cap at position NULL", call. = FALSE)
  if (pos < bfile$pos) {
    iso_source_file_op_error(
      bfile, sprintf("cannot cap at position %.0f as it is smaller than current position %.0f", 
                     pos, bfile$pos))
  }
  bfile$max_pos <- as.integer(pos)
  return(bfile)
}

# set pos and cap
set_pos_and_cap <- function(bfile, pos, max) {
  move_to_pos(bfile, pos = pos, reset_cap = TRUE) %>%
    cap_at_pos(pos = max)
}

