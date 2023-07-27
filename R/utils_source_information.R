# Get Source File Structure ========

#' Get source file and structure
#'
#' If an iso file is read with the \link[=iso_turn_debug_on]{debug mode on}, the source data (e.g. binary file) is stored with the file. By default this is not the case because it makes file objects unnecessarily large. Use these functions to retrieve and explore source structures.
#' 
#' Typically these functions are used for debugging purposes only.
#' 
#' @param iso_file iso file object
#' @examples
#' isoreader:::iso_turn_debug_on()
#' iso_get_reader_example("dual_inlet_example.did") |>  
#'    iso_read_dual_inlet() |>
#'    iso_get_source_file_structure() |>
#'    iso_print_source_file_structure(length = 500)
#' \dontrun{
#' isoreader:::iso_turn_debug_on()
#' iso_get_reader_example("dual_inlet_example.did") |>  
#'    iso_read_dual_inlet() |>
#'    iso_get_source_file_structure() |>
#'    iso_print_source_file_structure(save_to_file = "structure.txt")
#' }
#' @export
iso_get_source_file_structure <- function(iso_file) {
  
  # checks
  stopifnot(
    "`iso_file` has to be an iso file object" = !missing(iso_file) && iso_is_file(iso_file),
    "the provided `iso_file` does not have any source information. If it should, make sure to turn debug mode on to preserve it during file read." = !is.null(iso_file$source)
  )
  
  # reset position
  if (is.list(iso_file$source) && !is.null(iso_file$source$pos))
    iso_file$source$pos <- 1L
  
  return(iso_file$source)
}

# Print Source File Structure ======

#' @rdname iso_get_source_file_structure
#' @param x the object for which to print the source file structure.
#' @param ... additional parameters depending on source file types
#' @param save_to_file whether to save the source file structure to a text file (provide file path, will overwrite anything already in the file!) in addition to printing it out
#' @export 
iso_print_source_file_structure <- function(x, ..., save_to_file = NULL) {
  UseMethod("iso_print_source_file_structure")
} 

#' @export
iso_print_source_file_structure.default <- function(x, ..., save_to_file = NULL) {
  stop("this function is not defined for objects of type '", 
       class(x)[1], "'", call. = FALSE)
}

#' @rdname iso_get_source_file_structure
#' @export
iso_print_source_file_structure.iso_file <- function(x, ..., save_to_file = NULL) {
  # FIXME: should be $source instead of $source!!
  check_bfile(x$source)
  iso_print_source_file_structure(x$source, ..., save_to_file = save_to_file)
}

#' @rdname iso_get_source_file_structure
#' @param start starting position in the binary file to print from (prints the first block that spans this range)
#' @param length length in the binary file to print to (by default \code{NULL}, which means print everything)
#' @param end until which point in the binary file to print to. If provided, overrides whatever is specified in \code{length}
#' @export
iso_print_source_file_structure.binary_isodat_file <- function(x, start = 1, length = NULL, end = start + length, ..., save_to_file = NULL) {
  if(rlang::is_empty(end)) end <- max(x$blocks$end)
  partial <- start > 1 | end < max(x$blocks$end, 1)
  if(end <= start) stop("'end' cannot be smaller than 'start'", call. = FALSE)
  x$blocks <- filter(x$blocks, start >= !!start | (start < !!start & end > !!start), start <= !!end)
  file_structure <- format_isodat_structure_blocks(x, ...)
  
  # save to file
  if (!is.null(save_to_file)) {
    sprintf("Writing binary isodat file structure to file '%s'... ", save_to_file) |> cat()
    cat(file_structure$block_formatted, sep = "", file = save_to_file)
    cat("complete.")
  } else {
    if (partial)
      sprintf(
        "# Textual representation of the partial structure (bytes %d - %d) of the isodat file.\n# Print more/less by specifying the 'start', 'length' or 'end' parameters.\n",
        min(file_structure$start), max(file_structure$end)) |> cat()
    else
      cat("# Textual representation of the complete structure of the isodat file\n")
    cat(file_structure$block_formatted, sep = "")
  }
}

#' @rdname iso_get_source_file_structure
#' @export
print.binary_isodat_file <- function(x, start = x$pos, length = 200, ...) {
  iso_print_source_file_structure(x, start = start, length = length, ...)
}

