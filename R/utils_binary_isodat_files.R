# Isodat binary file utils ======

# Generate binary isodat file structure
template_binary_isodat_file_object <- function() {
  bfile <- template_binary_file_object() 
  # structure blocks
  bfile$blocks <- tibble(
    block_idx = integer(0), start = integer(0), end = integer(0), 
    len = integer(0), data_len = integer(0),
    type = character(0), priority = integer(0), block = character(0)
  )
  # current navigation block index position
  bfile$current_nav_block_idx = 1L 
  # classes
  class(bfile) <- c("binary_isodat_file", class(bfile))
  return(bfile)
}

# Read binary isodat file
read_binary_isodat_file <- function(filepath) {
  
  # read file
  bfile <- read_binary_file(filepath, bfile = template_binary_isodat_file_object())
  
  # find structure blocks
  bfile$blocks <- find_isodat_structure_blocks(bfile)
  bfile$C_blocks <- dplyr::filter(bfile$blocks, .data$type == "C block")
  
  return(bfile)
}

#' @export
iso_source_file_op_error.binary_isodat_file <- function(source_file_obj, msg, ...) {
  stop(sprintf(
    "%s%s (nav block#%.0f '%s', pos %.0f, max %.0f)", 
    source_file_obj$error_prefix, msg, 
    source_file_obj$current_nav_block_idx,
    fetch_block_entry(source_file_obj, block_idx = source_file_obj$current_nav_block_idx)$block,
    source_file_obj$pos, source_file_obj$max_pos), call. = FALSE
  )
}

# Block Navigation =====

# set the current navigation block index for higher level navigation and informative error messages
set_current_nav_block_idx <- function(bfile, block_idx) {
  check_bfile(bfile)
  if (!length(block_idx) == 1L)
    iso_source_file_op_error(bfile, sprintf("cannot set block index '%s', more than 1 value", paste(block_idx, collapse = ", ")))
  if (!is.numeric(block_idx))
    iso_source_file_op_error(bfile, sprintf("cannot set block index '%s', not a number", block_idx))
  if (block_idx < 1)
    iso_source_file_op_error(bfile, sprintf("cannot set block index '%s', must be >=1", block_idx))
  if (block_idx > nrow(bfile$blocks))
    iso_source_file_op_error(bfile, sprintf("cannot set block index '%s', must be <= %d", block_idx, nrow(bfile$blocks)))
  bfile$current_nav_block_idx <- block_idx
  return(bfile)
}

# fetch block indices
# @param filter optional filtering expression for the blocks tibble
# @param type optional block type string(s), must match exactly, otherwise use filter param
# @param block optional block text string(s), must match exactly if block_regex_mach = FALSE and partly if block_regex_match = TRUE. For more complex filters, the the filter parameter.
# @param min_pos optional minimum position to search
# @param max_pos optional maximum position to search
# @param min_block_idx optional minimum block index to search
# @param max_block_idx optional maximum block index to search
# @param occurences optional filter on which occurence(s) to return
# @param require_n if provided will require exactly this number of results (after all filter including occurences is applied)
fetch_block_idx <- function(bfile, filter = NULL, type = NULL, block = NULL, block_regex_match = FALSE, min_pos = NULL, max_pos = NULL, min_block_idx = NULL, max_block_idx = NULL, occurence = NULL, require_n = NULL) {
  check_bfile(bfile)
  filter_quo <- rlang::enquo(filter)
  
  # get block ids
  block_idx <- 
    bfile$blocks |>
    if_y_then(!is.null(min_pos), true_func = dplyr::filter, .data$start >= !!min_pos) |>
    if_y_then(!is.null(max_pos), true_func = dplyr::filter, .data$end <= !!max_pos) |>
    if_y_then(!is.null(min_block_idx), true_func = dplyr::filter, .data$block_idx >= !!min_block_idx) |>
    if_y_then(!is.null(max_block_idx), true_func = dplyr::filter, .data$block_idx <= !!max_block_idx) |>
    if_y_then(!rlang::quo_is_null(filter_quo), true_func = dplyr::filter, !!filter_quo) |>
    if_y_then(!is.null(type), true_func = dplyr::filter, .data$type %in% !!type) |>
    if_y_then(!is.null(block) && !block_regex_match, true_func = dplyr::filter, .data$block %in% !!block) |>
    if_y_then(!is.null(block) && block_regex_match, true_func = dplyr::filter, stringr::str_detect(.data$block, !!block)) |>
    if_y_then(!is.null(occurence), true_func = dplyr::filter, dplyr::row_number() %in% !!occurence) |>
    dplyr::pull(block_idx)
  
  # check requirements
  if (!is.null(require_n) && length(block_idx) != require_n) {
    pos_text <- 
      if(!is.null(min_pos) && !is.null(max_pos)) 
        paste0("at position >= ", min_pos, " and <= ", max_pos) 
      else if (!is.null(min_pos))
        paste0("at position >= ", min_pos)
      else if (!is.null(max_pos))
        paste0("at position <= ", max_pos) 
      else ""
    idx_text <- 
      if(!is.null(min_block_idx) && !is.null(max_block_idx)) 
        paste0("at block index >= ", min_block_idx, " and <= ", max_block_idx)
      else if(!is.null(min_block_idx)) 
        paste0("at block index >= ", min_block_idx)
      else if(!is.null(max_block_idx)) 
        paste0("at block index <= ", max_block_idx) 
      else ""
    filter_text <- if (!rlang::quo_is_null(filter_quo)) sprintf("with '%s'", rlang::as_label(filter_quo)) else ""
    type_text <- if (!is.null(type)) sprintf("with type '%s'", paste(type, collapse = "' or '")) else ""
    block_text <- if(!is.null(block) && !block_regex_match) sprintf(" '%s'", paste(block, collapse = "' or '")) 
    else if (!is.null(block) && block_regex_match) sprintf(" '*%s*'", paste(block, collapse = "' or '"))
    else ""
    occurence_text <- if(!is.null(occurence)) sprintf("occurence %.0f of ", occurence) else ""
    search_text <- c(type_text, filter_text, pos_text, idx_text)
    err_msg <- sprintf("could not find %sblock%s %s", occurence_text, block_text, 
                       paste(search_text[nchar(search_text) > 0], collapse = " and "))
    iso_source_file_op_error(bfile, err_msg)
  }  
  
  return(block_idx)
}

# get the block where the binary file is currently at
fetch_current_block_idx <- function(bfile, pos = bfile$pos) {
  fetch_block_idx(bfile, filter = .data$start <= !!pos & !!pos <= .data$end, occurence = 1)
}

# retrieve block entry/entries
# @param block_idx - provide an index 
# @param ... passed on to fetch_block_idx() to retrieve the indices (only used if block_idx is not provided)
fetch_block_entry <- function(bfile, ..., block_idx = NULL) {
  check_bfile(bfile)
  if (is.null(block_idx)) block_idx <- fetch_block_idx(bfile, ...)
  return(bfile$blocks[block_idx,])
}

# move to a specific control block (sets current control block idx for the binary file)
# @inheritParams fetch_block_idx
# @param move_to_end whether to move the binary file pos to the beginning or end of the control block (careful: default is FALSE because it's better for debugging but it used to be TRUE!)
# @param reset_cap whether to reset the cap
# @param update_current_nav_block whether to update the navigation block position (typically/default yes)
# @param ... additional parameters passed to fetch_block_idx (e.g. for using regex matching or a broader filter criterion)
# move_to_control_block(my_test$source, "NO")
# move_to_control_block(my_test$source, "CData", occurence = 2)
move_to_control_block <- function(bfile, block = NULL, type = "C block", min_pos = 1, occurence = 1, require_n = 1, move_to_end = FALSE, reset_cap = TRUE, update_current_nav_block = TRUE, ...) {
  
  # fetch right C block
  block_idx <- fetch_block_idx(bfile, block = block, type = type, min_pos = min_pos, occurence = occurence, require_n = require_n, ...)
  block <- fetch_block_entry(bfile, block_idx = block_idx)
  
  # reset position cap
  if (reset_cap) bfile$max_pos <- length(bfile$raw)
  new_pos <- if(move_to_end) block$end[1] + 1L else block$start[1]
  if (update_current_nav_block) bfile <- set_current_nav_block_idx(bfile, block_idx)
  return(move_to_pos(bfile, new_pos))
}

# move to next control block (does not reset cap by default)
move_to_next_control_block <- function(bfile, block = NULL, reset_cap = FALSE, ...) {
  move_to_control_block(bfile, block = block, min_pos = bfile$pos + 1L, reset_cap = reset_cap, ...)
}

# CBlock Navigation (TODO: deprecate in favor of move_to_control_block) =======

# fetch a specific C_block
# @param C_block name of the block
# @param min_pos minimum position where to find the block
# @param occurence which occurence to find? (use -1 for last occurence, use NULL for all)
# @FIXME: testing
fetch_C_block <- function(bfile, C_block, min_pos = 1, occurence = NULL, regexp_match = FALSE) {
  # basic checks
  check_bfile(bfile)
  if (nrow(bfile$C_blocks) == 0) stop("no C_blocks available", call. = FALSE)
  if (missing(C_block) || !is.character(C_block)) stop("C_block name not provided", call. = FALSE)
  
  # find C_blocks
  if (regexp_match)
    C_blocks <- filter(bfile$C_blocks, str_detect(.data$block, C_block), .data$start >= min_pos)
  else
    C_blocks <- filter(bfile$C_blocks, .data$block == C_block, .data$start >= min_pos)
  if (nrow(C_blocks) == 0) {
    iso_source_file_op_error(bfile, sprintf("block '%s' not found after position %.0f", C_block, min_pos))
  }
  
  if (!is.null(occurence) && occurence > 1 && nrow(C_blocks) < occurence) {
    iso_source_file_op_error(
      bfile, sprintf("occurence %.0f of block '%s' not found (only %.0f occurences) after position %.0f", 
                     occurence, C_block, nrow(C_blocks), min_pos))
  }
  
  # return right occurrence
  if (!is.null(occurence) && occurence == -1)
    return(tail(C_blocks, 1))
  else if (!is.null(occurence))
    return(C_blocks[occurence, ])
  else
    return(C_blocks)
}

# moves position to the end of a specific C_block
# @inheritParams fetch_C_block
# @param reset_cap whether to reset the cap
# @FIXME: testing
# move_to_C_block(my_test$source, "NO")
# move_to_C_block(my_test$source, "CData", occurence = 2)
move_to_C_block <- function(bfile, C_block, min_pos = 1, occurence = 1, move_to_end = TRUE, reset_cap = TRUE, regexp_match = FALSE) {
  # fetch right C block
  cblock <- fetch_C_block(bfile, C_block, min_pos = min_pos, occurence = occurence, regexp_match = regexp_match)
  if (is.null(cblock)) {
    iso_source_file_op_error(bfile, sprintf("cannot move to C block '%s'", C_block))
  } 
  # reset position cap
  if (reset_cap) bfile$max_pos <- length(bfile$raw)
  new_pos <- if(move_to_end) cblock$end[1] + 1L else cblock$start[1]
  return(move_to_pos(bfile, new_pos))
}

# move to next C block (does not reet cap by default)
move_to_next_C_block <- function(bfile, C_block, reset_cap = FALSE, regexp_match = FALSE) {
  move_to_C_block(bfile, C_block, min_pos = bfile$pos, occurence = 1, reset_cap = reset_cap, regexp_match = regexp_match)
}

# cap valid position at the beginning of a specific C_block
cap_at_C_block <- function(bfile, C_block, min_pos = 1, occurence = 1, regexp_match = FALSE) {
  # fetch right C block
  cblock <- fetch_C_block(bfile, C_block, min_pos = min_pos, occurence = occurence, regexp_match = regexp_match)
  if (is.null(cblock)) {
    iso_source_file_op_error(bfile, sprintf("cannot cap at C block '%s'", C_block))
  } 
  return(cap_at_pos(bfile, cblock$start[1]))
}

# cap at next C block
cap_at_next_C_block <- function(bfile, C_block, regexp_match = FALSE) {
  cap_at_C_block(bfile, C_block, min_pos = bfile$pos, occurence = 1, regexp_match = regexp_match)
}

# move to C block range
# Note: if need to specific occurence, use move_to_C_block, cap_at_C_block
move_to_C_block_range <- function(bfile, from_C_block, to_C_block, min_pos = 1){
  # range move safety check (a bit redundant because also checked in 
  # move_to_pos and cap_at_pos but errors there are out of context)
  from <- fetch_C_block(bfile, from_C_block, min_pos = min_pos)
  to <- fetch_C_block(bfile, to_C_block, min_pos = min_pos)
  if (!is.null(from) && !is.null(to) && to$start[1] < from$end[1]) {
    iso_source_file_op_error(bfile, sprintf("cannot move to Cblock range, first occurence of block '%s' before first block '%s'",
                            from_C_block, to_C_block))
  }

  # move to blocks
  bfile |> 
    move_to_C_block(from_C_block, min_pos = min_pos, reset_cap = TRUE) |> 
    cap_at_C_block(to_C_block, min_pos = min_pos)
}

# move to next C block range
move_to_next_C_block_range <- function(bfile, from_C_block, to_C_block){
  move_to_C_block_range(bfile, from_C_block, to_C_block, min_pos = bfile$pos)
}

# regular expression for control block OBSOLETE?
re_block <- function(block_name) {
  block <- get_ctrl_blocks_config()[[block_name]]
  if (is.null(block)) stop("encountered unknown block: ", block_name, call. = FALSE)
  structure(
    list(
      label = sprintf("<%s>", block_name),
      regexp = block$regexp,
      size = block$size
    ),
    class = "binary_regexp")
}

# Regular expression searches ======
# Note: this should have more units tests
# Note: it is often much faster to use regular expressions when searching for consecutive block patterns instead of using the $blocks tibble for this kind of search. The functions below reflect the corresponding elements in a blocks tibble.

# regular expression or x-000 control block
re_x_000 <- function() {
  structure(
    list(
      label = "<x-000>",
      regexp = "[\x01-\x1f]\\x00{3}",
      size = 4L
    ),
    class = "binary_regexp")
}

# regular expression for text-0 (=empty text) sequence
re_text_0 <- function() {
  structure(
    list(
      label = "{text-0}",
      regexp = "\xff\xfe\xff\\x00",
      size = 4L
    ),
    class = "binary_regexp")
}

# regular expression for text-x (text with x characters) start sequence
re_text_x <- function() {
  structure(
    list(
      label = "{text-x}",
      regexp = "\xff\xfe\xff.",
      size = 4L
    ),
    class = "binary_regexp")
}

# regular expression for null characters FINISHED
re_null <- function(n) {
  structure(
    list(
      label = sprintf("<%.0fx00>", n),
      # NOTE: this works both with paste0 and str_c
      regexp = paste0("\\x00{", n, "}"),
      size = n
    ),
    class = "binary_regexp")
}

# regular expression for anything but null characters
re_not_null <- function(n) {
  structure(
    list(
      label = sprintf("<x01-xff{%.0f}>", n),
      # NOTE: this does not work on windows if it's str_c!
      regexp = paste0("[\x01-\xff]{", n, "}"),
      size = n
    ),
    class = "binary_regexp")
}

# regular expression for control sequences (00-0f)
re_control <- function(raw) {
  # can't easily assemble so it's hard copy for now
  hex <- c(`00` = "\\x00", `01` = "\x01", `02` = "\x02", `03` = "\x03", `04` = "\x04", `05` = "\x05", `06` = "\x06", `07` = "\x07", `08` = "\x08", `09` = "\x09", `0a` = "\x0a", `0b` = "\x0b", `0c` = "\x0c", `0d` = "\x0d", `0e` = "\x0e", `0f` = "\x0f")
 
  # return hex regexps of the control sequence
  ctrls <- as.character(raw)
  structure(
    list(
      label = sprintf("{%s}", str_c(ctrls, collapse = " ")),
      regexp = hex[ctrls] |> str_c(collapse = ""),
      size = length(raw)
    ),
    class = "binary_regexp")
}

# regular expression for unicode text elements
re_unicode <- function(text) {
  # can't easily assemble the \x5a structure so using a hard copy
  hex <- c(`20` = "\x20", `21` = "\x21", `22` = "\x22", `23` = "\x23", `24` = "\x24", `25` = "\x25", `26` = "\x26", `27` = "\x27", `28` = "\x28", `29` = "\x29", `2a` = "\x2a", `2b` = "\x2b", `2c` = "\x2c", `2d` = "\x2d", `2e` = "\x2e", `2f` = "\x2f", `30` = "\x30", `31` = "\x31", `32` = "\x32", `33` = "\x33", `34` = "\x34", `35` = "\x35", `36` = "\x36", `37` = "\x37", `38` = "\x38", `39` = "\x39", `3a` = "\x3a", `3b` = "\x3b", `3c` = "\x3c", `3d` = "\x3d", `3e` = "\x3e", `3f` = "\x3f", `40` = "\x40", `41` = "\x41", `42` = "\x42", `43` = "\x43", `44` = "\x44", `45` = "\x45", `46` = "\x46", `47` = "\x47", `48` = "\x48", `49` = "\x49", `4a` = "\x4a", `4b` = "\x4b", `4c` = "\x4c", `4d` = "\x4d", `4e` = "\x4e", `4f` = "\x4f", `50` = "\x50", `51` = "\x51", `52` = "\x52", `53` = "\x53", `54` = "\x54", `55` = "\x55", `56` = "\x56", `57` = "\x57", `58` = "\x58", `59` = "\x59", `5a` = "\x5a", `5b` = "\x5b", `5c` = "\x5c", `5d` = "\x5d", `5e` = "\x5e", `5f` = "\x5f", `60` = "\x60", `61` = "\x61", `62` = "\x62", `63` = "\x63", `64` = "\x64", `65` = "\x65", `66` = "\x66", `67` = "\x67", `68` = "\x68", `69` = "\x69", `6a` = "\x6a", `6b` = "\x6b", `6c` = "\x6c", `6d` = "\x6d", `6e` = "\x6e", `6f` = "\x6f", `70` = "\x70", `71` = "\x71", `72` = "\x72", `73` = "\x73", `74` = "\x74", `75` = "\x75", `76` = "\x76", `77` = "\x77", `78` = "\x78", `79` = "\x79", `7a` = "\x7a", `7b` = "\x7b", `7c` = "\x7c", `7d` = "\x7d", `7e` = "\x7e")
  
  # return hex regexps of the text
  structure(
    list(
      label = sprintf("{%s}", text),
      regexp = hex[as.character(charToRaw(text))] |> str_c("\\x00") |> str_c(collapse = ""),
      size = 2*nchar(text)
    ),
    class = "binary_regexp")
}

# plain regexp (default size is an estimate)
# @param label changing to proper name to avoid character errors
re_direct <- function(regexp, label = "re-direct", size = length(charToRaw(regexp))) {
  structure(
    list(
      label = sprintf("[%s]", label),
      regexp = regexp,
      size = size
    ),
    class = "binary_regexp")
}

# helper funtion to combine regexps
combine_regexps <- function(regexps, collapse, var = "regexp") {
  # WARNING WARNING WARNING
  # do not change this
  # on all platforms: map_chr messes up the regexps
  # on windows: str_c instead of paste leads to regexp fail in grepRaw
  # on unix: paste instead of str_c breaks concatenating a few specific regexps (e.g. re_combine(re_not_null(2), re_text_x()))
  if (.Platform$OS.type == "windows") 
    paste(sapply(regexps, `[[`, var), collapse = collapse)
  else
    stringr::str_c(sapply(regexps, `[[`, var), collapse = collapse)
}

# or regexp
re_or <- function(..., size = estimate_size()) {
  regexps <- list(...)
  if(!all(sapply(regexps, is, "binary_regexp"))) stop("needs binary regexps for re_or", call. = FALSE)
  estimate_size <- function() sum(map_dbl(regexps, "size")) 
  structure(
    list(
      label = sprintf("(%s)", str_c(map_chr(regexps, "label"), collapse = "|")),
      # NOTE: on windows, the following command with str_c instead of paste or map_chr instead of sapply strangly leads to the regexp not getting recognized anymore in grepRaw
      regexp = paste0("((", combine_regexps(regexps, collapse = ")|("), "))"),
      size = size
    ),
    class = "binary_regexp")
}

# combine regular expression patterns
re_combine <- function(...) {
  regexps <- list(...)
  if (!all(sapply(regexps, is, "binary_regexp"))) 
    stop("can only combine binary regexps, generate with re_x() functions", call. = FALSE)
  structure(
    list(
      label = str_c(map_chr(regexps, "label"), collapse = ""),
      size = sum(map_dbl(regexps, "size")),
      regexp = combine_regexps(regexps, collapse = "")
    ),
    class = "binary_regexp")
}

# find next occurence of supplied regular expression pattern
find_next_pattern <- function(bfile, ..., max_gap = NULL, value = FALSE, all = FALSE) {
  check_bfile(bfile)
  regexps <- re_combine(...)
  pos <- grepRaw(regexps$regexp, bfile$raw, offset = bfile$pos, value = value, all = all) 
  if (length(pos) == 0) return(NULL) # return NULL if not found
  else if (!is.null(max_gap) && !value && pos > bfile$pos + max_gap) return(NULL) # return NULL if outside max gap
  else if (!value && all(pos > bfile$max_pos)) return (NULL) # return NULL if bigger than allowed
  else return(pos)
}

# find the next occurences of a supplised regular expression pattern
find_next_patterns <- function(bfile, ...) {
  positions <- find_next_pattern(bfile, ..., value = FALSE, all = TRUE)
  positions <- positions[positions <= bfile$max_pos] # don't allow those that exceed the allowed positions
  if (length(positions) == 0) return(NULL)
  return(positions)
}

# move to next regular expression pattern
# @param ... construct with the re... functions
# @param max_gap maximum number of bytes until the pattern
# @param move_to_end whether to move to the end of the pattern (default is yes)
move_to_next_pattern <- function(bfile, ..., max_gap = NULL, move_to_end = TRUE) {
  # safety check
  check_bfile(bfile)
  if(!is.null(max_gap) && !is.numeric(max_gap)) stop("max gap must be a number", call. = FALSE)
  
  # find pattern
  pos <- find_next_pattern(bfile, ..., max_gap = max_gap)
  
  # move to new position
  if ( !is.null(pos) ) {
    n <- if (move_to_end) length(find_next_pattern(bfile, ..., value = TRUE)) else 0
    return(move_to_pos(bfile, pos + n))
  } 
  
  # encountered a problem
  regexps <- re_combine(...)
  gap_text <- if (!is.null(max_gap)) sprintf(" after maximal gap of %.0f bytes", max_gap) else ""
  iso_source_file_op_error(
    bfile, 
    sprintf("could not find '%s'%s in search interval %.0f to %.0f, found '%s...'",
            regexps$label, 
            gap_text, bfile$pos, bfile$max_pos,
            bfile |> 
              iso_print_source_file_structure(
                length = regexps$size + (if(!is.null(max_gap)) max_gap else 50) + 10
              )
    )
  )
}

# cap at next regular expression pattern
# @param ... construct with the re... functions
# @param max_gap maximum number of bytes until the pattern
# @param move_to_end whether to move to the end of the pattern (default is yes)
cap_at_next_pattern <- function(bfile, ..., max_gap = NULL) {
  # safety check
  check_bfile(bfile)
  if(!is.null(max_gap) && !is.numeric(max_gap)) stop("max gap must be a number", call. = FALSE)
  
  # find pattern
  pos <- find_next_pattern(bfile, ..., max_gap = max_gap)
  
  # cap at new position
  if ( !is.null(pos) ) {
    return(cap_at_pos(bfile, pos))
  } 
  
  # encountered a problem
  regexps <- re_combine(...)
  gap_text <- if (!is.null(max_gap)) sprintf(" after maximal gap of %.0f bytes", max_gap) else ""
  iso_source_file_op_error(
    bfile, 
    sprintf("could not find '%s'%s in search interval %.0f to %.0f, found '%s...'",
            regexps$label, 
            gap_text, bfile$pos, bfile$max_pos,
            bfile |> 
              iso_print_source_file_structure(
                length = regexps$size + (if(!is.null(max_gap)) max_gap else 50) + 10
              )
    )
  )
}

# capture data block data in specified type
# uses parse_raw_data and therefore can handle multiple data types
# @inheritParams parse_raw_data
capture_data_till_pattern <- function(bfile, id, type, ..., data_bytes_min = 0, data_bytes_max = NULL, 
                         move_past_dots = FALSE, re_size_exact = TRUE,
                         ignore_trailing_zeros = TRUE, exact_length = TRUE, sensible = NULL) {
  
  # reset existing data in this field
  bfile$data[[id]] <- NULL
  
  # regexp
  regexps <- re_combine(...)
  
  # move to begining of target ... after the data
  check_bfile(bfile)
  start <- bfile$pos
  bfile$pos <- bfile$pos + data_bytes_min # offset until starting to look for next pattern
  bfile <- move_to_next_pattern(bfile, regexps, max_gap = data_bytes_max, move_to_end = FALSE)
  end <- bfile$pos - 1
  
  # store data
  if (end > start) {
    if (length(type) == 1 && type[1] == "raw"){
      # simplify for speed
      bfile$data[[id]] <- bfile$raw[start:end]
    } else {
      id_text <- sprintf("'%s' capture failed at pos %d: ", id, start)
      bfile$data[[id]] <-
        parse_raw_data(bfile$raw[start:end], type,
                       ignore_trailing_zeros = ignore_trailing_zeros,
                       exact_length = exact_length, sensible = sensible,
                       errors = str_c(bfile$error_prefix, id_text))
    }
  }
  
  # whether to move past the dots
  if(move_past_dots) {
    if (re_size_exact)
      bfile <- move_to_pos(bfile, end + 1 + regexps$size)
    else
      bfile <- move_to_next_pattern(bfile, regexps, max_gap = 0)
  }
  
  return(bfile)
}

# capture specific number of data points following after the current position
# instead of capturing data until the next pattern, this captures a specific
# number of data points (i.e. primarily for numeric data), moves to after the data
# also uses parse_raw_data and therefore can handle multiple data types
# @inheritParams parse_raw_data
capture_n_data <- function(bfile, id, type, n, sensible = NULL) {
  
  # reset existing data in this field
  bfile$data[[id]] <- NULL
  
  # find raw length
  check_bfile(bfile)
  
  # data type safety check
  if (length(missing <- setdiff(type, names(get_data_blocks_config()))) > 0) 
    stop("ecountered invalid data type(s): ", str_c(missing, collapse = ", "), call. = FALSE)
  
  dbc <- get_data_blocks_config()[type]
  size <- sum(map_int(dbc, "size"))
  
  # store data
  id_text <- sprintf("'%s' capture failed: ", id)
  bfile$data[[id]] <- 
    parse_raw_data(bfile$raw[bfile$pos:(bfile$pos + size * n - 1)], type, n = n,
                   ignore_trailing_zeros = FALSE,
                   exact_length = FALSE, sensible = sensible,
                   errors = str_c(bfile$error_prefix, id_text)) 
  
  # move to after the data
  return(move_to_pos(bfile, bfile$pos + size * n))
}

# capture data from a nav block (by default the current block)
# uses parse_raw_data and therefore can handle multiple data types
# moves after the block when done
# @note: careful using it for unknown blocks, they may not always have clean data, usually safer to use capture_data_till_pattern
# @note: deprecate?
# @inheritParams parse_raw_data
# @param block_idx the index of the block to capture data from, by default the current block where the file is
capture_block_data <- function(bfile, id, type, block_idx = fetch_current_block_idx(bfile), ignore_trailing_zeros = TRUE, exact_length = TRUE, sensible = NULL) {
  
  # reset existing data in this field
  bfile$data[[id]] <- NULL
  
  # get block info
  block <- fetch_block_entry(bfile, block_idx = block_idx)
  if (nrow(block) != 1L)
    stop("cannot capture block data for block_idx: ", paste(block_idx, collapse = ", "), call. = FALSE)
  start <- block$start
  end <- block$end
  
  # store data
  if (length(type) == 1 && type[1] == "raw"){
    # simplify for speed
    bfile$data[[id]] <- bfile$raw[start:end]
  } else {
    id_text <- sprintf("'%s' capture failed for block %.0f: ", id, block_idx)
    bfile$data[[id]] <-
      parse_raw_data(bfile$raw[start:end], type,
                     ignore_trailing_zeros = ignore_trailing_zeros,
                     exact_length = exact_length, sensible = sensible,
                     errors = str_c(bfile$error_prefix, id_text))
  }
  
  # move to after the data
  return(move_to_pos(bfile, end + 1L))
}


# Keys ======

# find all isodat C_blocks in the binary (marked by the below regexp pattern followed by a string of ascii characters)
find_C_blocks <- function(raw) {
  regexp <- paste0("\xff\xff(\\x00|[\x01-\x0f])\\x00.\\x00\x43[\x20-\x7e]+")
  re_positions <- grepRaw(regexp, raw, all = TRUE, value = FALSE) 
  re_matches <- grepRaw(regexp, raw, all = TRUE, value = TRUE) 
  
  # values
  lapply(re_matches, function(x) {
    list(
      id1 = as.character(readBin(x[3], "raw")),
      id2 = as.character(readBin(x[5], "raw")),
      block = str_c(readBin(x[7:length(x)], "character", n = length(x) - 7), collapse = "")
    )
  }) |> bind_rows() |> 
    # byte positions
    mutate(
      start = re_positions,
      end = .data$start + nchar(.data$block) + 6 - 1
    )
}

# Parse data (TODO: deprecate ctrl blocks in favor of the isodat control blocks) =====

# configuration information for the control blocks
# good info at http://www.aboutmyip.com/AboutMyXApp/AsciiChart.jsp
# those with auto = TRUE are considered when making an automatic map_binary_structure
# 
# @FIXME: testing
get_ctrl_blocks_config <- function() {
  list(
    
    # specific sequences
    `del-nl`   = list(size = 2L, auto = TRUE, regexp = "\x7f\x85"), # 7F 85 delete next line
    `eop-nl`   = list(size = 2L, auto = TRUE, regexp = "\xdc\x85"), # DC 85 end of proof?? next line
    `0b-80`    = list(size = 2L, auto = TRUE, regexp = "\x0b\x80"), # 0b 80 = vertical tab, divider in tables?
    `e0-81`    = list(size = 2L, auto = TRUE, regexp = "\xe0\x81"), # e0 81 = some sort of table divider?
    `ce-80`    = list(size = 2L, auto = TRUE, regexp = "\xce\x80"), # ce 80 = not sure what it means but common divider in tables
    `ce-8a`    = list(size = 2L, auto = TRUE, regexp = "\xce\x8a"), # ce 80 = not sure what it means but sometimes divider in tables
    `ee-85`    = list(size = 2L, auto = TRUE, regexp = "\xee\x85"), # ce 80 = not sure what it means but sometimes in arrays
    `75-84`    = list(size = 2L, auto = TRUE, regexp = "\x75\x84"), # 75 84 - no idea what it means but it's special somehow
    `ff-80`    = list(size = 2L, auto = TRUE, regexp = "\\x00\xff\x80\\x00"), # ff 80 - no idea what it means 
    `07-80-id` = list(size = 6L, auto = TRUE, regexp = "\x05\x80.\xff(\\x00|\x80|\xff){2}", # some sort of counter or id
                   replace = function(b) str_c("id-", str_c(readBin(b[c(3,5,6)], "raw", n=6), collapse="-"))),
    `54-fc` = list(size = 4L, auto = TRUE, regexp = "\x54\xfc\\x00\\x00"), #another type of data start indicator
    
    # FFF and EEE and combination blocks
    nl      = list(size = 4L, auto = TRUE, regexp = "\xff\xfe\xff\x0a"), # FF FE FF 0A new line
    # `fef-0` = list(size = 4L, auto = TRUE, regexp = "\xff\xfe\xff\\x00"), # meaning = ? # DEPRECATED
    # `fef-x` = list(size = 4L, auto = TRUE, regexp = "\xff\xfe\xff.", # meaning = ? # DEPRECATED
    #                replace = function(b) str_c("fef-", readBin(b[4], "raw"))),
    `eee-0` = list(size = 4L, auto = TRUE, regexp = "\xef\xef\xef\\x00."),
    ffff    = list(size = 4L, auto = TRUE, regexp = "\xff{4}"), # meaning = ?
    
    # x000 blocks
    stx        = list(size = 4L, auto = TRUE, regexp = "\x02\\x00{3}"), # start of text
    etx        = list(size = 4L, auto = TRUE, regexp = "\x03\\x00{3}"), # end of text
    # `x-000`    = list(size = 4L, auto = TRUE, regexp = "[\x01-\x1f]\\x00{3}", replace = # meaning = ? maybe 1-000 has special meaning?
    #                  function(b) str_c(str_replace(readBin(b[1], "raw"), "^0", ""), "-000")), # DEPRECATED
    `f-000`    = list(size = 4L, auto = TRUE, regexp = "\xff\\x00{3}"), # unclear
    
    # c block (not auto processed because Cblocks are found separately)
    `C-block`  = list(size = 20L, auto = FALSE, regexp = "\xff\xff(\\x00|[\x01-\x0f])\\x00.\\x00\x43[\x20-\x7e]+"),
    
    # text block (not auto processed) - note that this does NOT include non standard characters
    latin   = list(size = 20L, auto = FALSE, regexp = "([\x41-\x5a\x61-\x7a]\\x00)+"), #a-zA-Z
    alpha   = list(size = 20L, auto = FALSE, regexp = "([\x41-\x5a\x61-\x7a\x30-\x39]\\x00)+"), #a-zA-Z0-9
    text    = list(size = 20L, auto = FALSE, regexp = "([\x20-\x7e]\\x00)+"),
    `text0` = list(size = 20L, auto = FALSE, regexp = "([\x20-\x7e]\\x00)*"), # allows no text
    permil  = list(size = 2L, auto = FALSE, regexp = "\x30\x20")
  )
}

# helper function to get the control blokcs as a data frame
get_ctrl_blocks_config_df <- function() {
  blocks <- get_ctrl_blocks_config()
  tibble(
    block = names(blocks),
    regexp = map_chr(blocks, "regexp"),
    hexadecimal = map_chr(
      blocks,
      ~.x$regexp |> charToRaw() |> as.character() |> paste(collapse = " ")
    )
  )
}

# get configuration information for the data blocks
# 
# @FIXME: testing
get_data_blocks_config <- function() {
  list(
    raw     = list(type = "raw", auto = FALSE, size = 1L),
    # extended unicode characters
    text    = list(type = "character", auto = TRUE, size = 2L, regexp = "[\x20-\xff]\\x00"), 
    # numeric data types
    integer = list(type = "integer", auto = TRUE, size = 4L),
    float   = list(type = "numeric", auto = TRUE, size = 4L),
    double  = list(type = "numeric", auto = TRUE, size = 8L)
  )
}

# parses binary data block
# can parse multiple type data blocks efficienctly, to do so provide multiple types (Details elow)
#
# @param raw the raw data, will be processed from position 1
# @param type data type(s), can be a single unit (e.g. "double") or a vector of sequential data types (e.g. c("float", "double")) see get_data_blocks_config() for details. Note that text cannot be combined with other data types.
# @param n how many instances to read (of EACH type if more than 1 supplied), default is to try to read the whole raw sequence
# @param ignore_trailing_zeros - whether to remove trailing zeros
# @param exact_length - whether to require exactly the right length
# @param sensible an expected value bracket that if not met will throw an error (will turn on error reporting if errors not already set)
# @param errors if set, triggers errors instead of returning NULL (use to set as custom error message used as error prefix)
# @return NULL if data problem encountered or throws error if sensible not met
# @FIXME: testing!!!
parse_raw_data <- function(raw, type, n = full_raw(), ignore_trailing_zeros = FALSE, exact_length = FALSE, 
                           sensible = NULL, errors = NULL) {
  
  # data type
  if (length(missing <- setdiff(type, names(get_data_blocks_config()))) > 0) 
    stop("ecountered invalid data type(s): ", str_c(missing, collapse = ", "), call. = FALSE)
  
  # check for text
  if (any(type == "text") && length(type) > 1) 
    stop("data type 'text' cannot reliably be parsed jointly with other data types", call. = FALSE)
  
  # errors
  if (!is.null(sensible) && is.null(errors)) errors <- "" 
  error_prefix <- if(is.null(errors)) "" else errors
  
  # total size
  dbc <- get_data_blocks_config()[type]
  size <- sum(map_int(dbc, "size"))
  
  # take off trailing 00 (down to the size of the data)
  raw_trim <- if (ignore_trailing_zeros) remove_trailing_zeros(raw, size) else raw
  raw_trim_text <- str_c(c(as.character(head(raw_trim, 30)), if(length(raw_trim) > 30) "..."), collapse = " ")
  
  # find full raw n
  full_raw <- function() floor(length(raw_trim)/size)
  
  # check that long enough for single read (other n = 0 and next errors don't make much sense)
  if (length(raw_trim) < size) {
    if (!is.null(errors)) 
      stop(sprintf("%sraw data (%d bytes) not long enough for single read of requested data type (%.0f): %s", 
                   error_prefix, length(raw_trim), size, raw_trim_text), call. = FALSE)
    return(NULL)
  }
  
  # check that long enough to be possible fit
  if (length(raw_trim) < size * n) {
    if (!is.null(errors)) 
      stop(sprintf("%sraw data (%d bytes) not long enough for requested byte read (%.0f): %s", 
                   error_prefix, length(raw_trim), size*n, raw_trim_text), call. = FALSE)
    return(NULL)
  }
  
  # if strict_length, make sure it is multiple of the data size
  if (exact_length && length(raw_trim) != (size * n)) {
    if (!is.null(errors)) 
      stop(sprintf("%sraw data length (%d bytes) does not match the requested byte read (%.0f): %s", 
                   error_prefix, length(raw_trim), size*n, raw_trim_text), call. = FALSE)
    return(NULL)
  }
  
  # if text capture, check that it is truly all text
  if (any(type == "text")) {
    
    if (n == 0) {
      stop(sprintf("%stext of length 0 does not make sense: %s", error_prefix, raw_trim_text), call. = FALSE)
    } else if (n > 250) {
      stop(sprintf("%stext with more than 250 characters (%.0f) is likely an error in finding the termination pattern: %s", 
                   error_prefix, n, raw_trim_text), call. = FALSE)
    }

    regexp <- sprintf("(%s){%.0f}", dbc[[1]]$regexp, n)
    if (length(grepRaw(regexp, raw_trim)) == 0) {
      # something that is NOT text is in the raw_trim
      
      # check for common extended unicode characters (micro)
      ext_uni <- grepRaw("\xb5\xff", raw_trim)
      if (length(ext_uni) > 0L) {
        raw_trim[ext_uni + 1L] <- as.raw(00)
        raw[ext_uni + 1L] <- as.raw(00)
      }
      
      # check again if any extended unicodes were found
      if (length(ext_uni) == 0L || length(grepRaw(regexp, raw_trim)) == 0) {
        non_text_pos <- grepRaw("([\x20-\xff][\x01-\xff])|(\\x00\\x00)", raw_trim)
        actual_text <- intToUtf8(raw_trim[1:(non_text_pos - 1)])
        if (!is.null(errors)) {
          err_raw <- raw_trim[(non_text_pos + 1):length(raw_trim)]
          stop(
            sprintf("%sexpected unicode data for %.0f bytes but found only %.0f ('%s'), non-text raw data afterwards: %s", 
                    error_prefix, n*2, non_text_pos, actual_text, 
                    if (length(err_raw) > 10) 
                      c(as.character(head(err_raw, 10)), sprintf("+%d more", length(err_raw) - 10)) |>
                      paste(collapse = " ")
                    else 
                      as.character(err_raw) |>
                      paste(collapse = " ")
            ), 
            call. = FALSE)
        }
        return(NULL)
      }
    }
  }
  
  # process data
  type_bytes <- seq_along(dbc) |> rep(times = map_int(dbc, "size")) |> rep(times = n)
  data <- list()
  for (i in 1:length(dbc)) {
    
    if (dbc[[i]]$type == "character") {
      parsed_data <- raw[type_bytes == i] |> intToUtf8()
    } else {
      parsed_data <- readBin(raw[type_bytes == i], what = dbc[[i]]$type, size = dbc[[i]]$size, n = n)
    }
    data <- c(data, list(parsed_data))
  }
  names(data) <- 1:length(dbc)
  
  # sensible check
  if (!is.null(errors) && !is.null(sensible)) {
    
    # make sure supplied sensible information is in proper format
    if (!is.list(sensible)) sensible <- list(sensible) 
    if (length(type) != length(sensible)) 
      stop("for multiple data types, need to supply list of sensible data pairs", call. = FALSE)
    
    # check all data 
    for (i in 1:length(type)) {
      if (!inherits(data[[i]], class(sensible[[i]])) && 
          !is(data[[i]], "integer") && !is(sensible[[i]], "numeric") ) # allow integer to numeric comparison
        stop(sprintf("%scannot compare data (%s) to expected values (%s), data type mismatch", 
                     error_prefix, class(data[[i]]), class(sensible[[i]])), call. = FALSE)
      if (is.character(sensible[[i]]) && !all(good_data <- str_detect(sensible[[i]], data[[i]]))) 
        stop(sprintf("%sparsed data (%s, ...) does not match expected pattern (%s)",
                     error_prefix, str_c(head(data[[i]][!good_data]), collapse = ", "), sensible), 
             call. = FALSE)
      else if (is.numeric(sensible[[i]]) && !all(good_data <- data[[i]] >= sensible[[i]][1] & data[[i]] <= sensible[[i]][2]))
        stop(sprintf("%sparsed data (%s, ...) does not match expected value range (%s)",
                     error_prefix,  str_c(signif(head(data[[i]][!good_data])), collapse = ", "), 
                     str_c(sensible[[i]][1], " to ", sensible[[i]][2])),
             call. = FALSE)
    }
    
  }
  
  # return data
  if (length(type) == 1) return(data[[1]]) # return single data
  return(data) # return list
}

# removes trailing 00 but only up to making the residual data still a multiple of the data size (leave at least 1 multiple), if can't take enough off to make the data a multiple of the data size, don't cut anything at all
# @param raw the raw data
# @param size the size of a data unit
remove_trailing_zeros <- function(raw, size) {
  
  # leave at least 1 multiple
  if (length(raw) <= size) return(raw)
  
  # find number of trailing 0s (i.e. everything after the highest byte that is not 00)
  is_null_block <- raw == as.raw(0)
  if (any(is_null_block == FALSE))
    trailing_00s <- length(is_null_block) - max(which(is_null_block == FALSE)) 
  else
    trailing_00s <- length(is_null_block)
  
  # leave at least 1 data length
  take_off_00s <- min(trailing_00s, length(raw) - size) 
  
  # size excess can always be taken off
  size_excess <- length(raw) %% size
  
  # see if enough 00s to take anything off
  if (take_off_00s < size_excess)
    take_off_00s <- 0
  
  # how many data size lengths okay to take off?
  if (take_off_00s > size_excess) {
    n_size <- floor((take_off_00s - size_excess)/size)
    take_off_00s <- n_size * size + size_excess
  } 
  
  # return
  raw[1:(length(raw) - take_off_00s)]
}

# Isodat File Structure ======

# @TODO: write tests
# @NOTE: speed optimized
# returns a tibble with control blacks for isodat files
# @return tibble with control blocks for isodat
get_isodat_control_blocks_config <- function() {
  # global vars
  regex <- NULL
  bind_rows(
    # C blocks
    list(
      type = "C block",
      regex = "\xff\xff(\\x00|[\x01-\x0f])\\x00.\\x00\x43[\x20-\x7e]",
      start_expr = rlang::exprs(.data$pos),
      len_expr = rlang::exprs(
        6L + readBin(raw[rep(.data$start, each = 2) + c(4,5)], "int", size = 2, n = length(.data$start))
      ),
      data_len_expr = rlang::exprs(.data$len - 6L),
      block_expr = rlang::exprs(
        purrr::map2_chr(.data$start + 5L, .data$len - 6L, ~intToUtf8(raw[.x+c(1L:.y)]))
      )
    ),
    # text blocks
    list(
      type = "text",
      regex = "\xff\xfe\xff",
      start_expr = rlang::exprs(.data$pos),
      len_expr = rlang::exprs(
        4L + readBin(raw[.data$start + 3L], "int", size = 1,n = length(.data$start)) * 2L
      ),
      data_len_expr = rlang::exprs((.data$len - 4L)/2L),
      block_expr = rlang::exprs(
        purrr::map2_chr(
          .data$start + 3L, .data$len - 4L,
          ~if (.y > 0L) {
            intToUtf8(readBin(raw[.x + c(1:.y)], "int", n = .y/2L, size = 2))
          } else {
            NA_character_
          }
        )
      )
    ),
    # x-000 blocks
    list(
      type = "x-000",
      regex = "[\x01-\x1f]\\x00{3}",
      start_expr = rlang::exprs(.data$pos),
      len_expr = rlang::exprs(4L),
      data_len_expr = rlang::exprs(0L),
      block_expr = rlang::exprs(sprintf("%s-000", raw[.data$start]))
    ),
    # 0000+ blocks (zeros in multiples of 2, at least 4 at a time ending in a non-zero)
    list(
      type = "0000+",
      regex = "(\\x00\\x00){2,}[\x01-\xff]",
      start_expr = rlang::exprs(.data$pos),
      len_expr = rlang::exprs(lengths(grepRaw(regex, raw, all = TRUE, value = TRUE)) - 1L),
      data_len_expr = rlang::exprs(0L),
      block_expr = rlang::exprs(sprintf("%dx00", .data$len))
    )
  ) 
}


# @TODO: write tests
# find regular expression pattern and turn into a block tibble
# @param raw binary vector
# @param regex regular expression to match
# @param start_expr expression to calculate starting point (relative to the regexp match var 'pos')
# @param len_expr expression to calculate the length of the block
# @param data_len_expr expression to calculate the length of the data in the block
# @param block_expr expression to construct the block text
find_pattern_blocks <- function(raw, regex, start_expr, len_expr, data_len_expr, block_expr) {
  # safety checks
  stopifnot(rlang::is_expression(start_expr))
  stopifnot(rlang::is_expression(len_expr))
  stopifnot(rlang::is_expression(data_len_expr))
  stopifnot(rlang::is_expression(block_expr))
  
  # find positions
  re_positions <- grepRaw(regex, raw, all = TRUE, value = FALSE) 
  
  # blocks
  tibble(
    pos = re_positions,
    start = rlang::eval_tidy(start_expr),
    len = rlang::eval_tidy(len_expr),
    end = .data$start + .data$len - 1L,
    data_len = rlang::eval_tidy(data_len_expr),
    block = rlang::eval_tidy(block_expr)
  ) |> 
    filter(.data$len > 0)
}

# @TODO: write tests
# find unknown patterns and turn into a block tibble (speed optimized)
# @param raw binary vector
# @param blocks tibble with identified blocks, must have columns start & end
find_unknown_blocks <- function(raw, blocks) {
  # blocks inbetween the identified ones
  blocks <- arrange(blocks, .data$start)
  tibble(
      type = "unknown",
      start = c(1L, blocks$end + 1L),
      end = c(blocks$start - 1L, length(raw)),
      len = .data$end - .data$start + 1L,
      data_len = .data$len,
      priority = max(blocks$priority) + 1L,
      block = NA_character_
    ) |>
    filter(.data$len > 0)
}

# updates block information for unknown blocks
# @param unknown_block_n_chars how many characters before abbreviating with ...
get_unknown_blocks_text <- function(blocks, raw, unknown_block_n_chars = 8L) {
  # block text for unknown blocks
  blocks |> 
    mutate(
      block =
        dplyr::case_when(
          type == "unknown" & start + unknown_block_n_chars < length(raw) ~ 
            rlang::eval_tidy(rlang::expr(paste(!!!map(
              0:(unknown_block_n_chars - 1L), ~ rlang::expr(raw[start+!!.x])
            ), "..."))) |>
            stringr::str_sub(end = data_len *  3L - 1L),
          TRUE ~ block
        )
    )
}


# @TODO: write tests
# find all isodat structure blocks - main function called by read_binary_isodat_file
# @param bfile the isodat binary file object (must have $raw set)
# @param unknown_block_n_chars the number of chars to preview as 'block' text in the resulting tibble
find_isodat_structure_blocks <- function(bfile, unknown_block_n_chars = 8L) {
  # safety checks
  if (!is(bfile, "binary_isodat_file")) stop("this function is for isodat binary files only", call. = FALSE)
  
  ctrl_blocks <- 
    get_isodat_control_blocks_config() |> 
    mutate(
      priority = dplyr::row_number(),
      blocks = purrr::pmap(
        list(
          regex = .data$regex,
          start_expr = .data$start_expr,
          len_expr = .data$len_expr,
          data_len_expr = .data$data_len_expr,
          block_expr = .data$block_expr
        ), 
        find_pattern_blocks, 
        raw = bfile$raw
      )
    ) |>
    dplyr::select(-"start_expr", -"block_expr", -"len_expr") |> 
    tidyr::unnest("blocks") 

  unknown_blocks <- 
    find_unknown_blocks(raw = bfile$raw, blocks = ctrl_blocks) |>
    get_unknown_blocks_text(raw = bfile$raw, unknown_block_n_chars = unknown_block_n_chars)

  all_blocks <- 
    dplyr::bind_rows(
      ctrl_blocks,
      unknown_blocks
    ) |> 
    dplyr::arrange(.data$start) |>
    dplyr::mutate(
      block_idx = dplyr::row_number()
    ) |>
    dplyr::select("block_idx", "start", "end", "len", "data_len", "type", "priority", "block")
  
  return(all_blocks)
}

# @TODO: write tests
# format isodat structure blocks for printout
# @param bfile the isodat binary file object (must have $raw set)
# @param new_line_blocks expression when to create a new line
# @param indent_blocks expression when to indent a line (only if also matched by new_line_blocks)
# @param unknown_block_n_chars the number of chars to preview as 'block' text in the resulting tibble
# @param data_blocks expression to mark data blocks
# @param data_highlight expression to insert a 'HIGHLIGHT' marker in the text, example `len > 1000` to highlight large data blocks
# @param pos_info whether to include position information for each line  (highly recommended)
format_isodat_structure_blocks <- function(
  bfile, 
  new_line_blocks = .data$type %in% c("C block", "x-000"),
  indent_blocks = .data$type == "x-000",
  unknown_block_n_chars = 8L,
  data_blocks = .data$type %in% c("text", "unknown"),
  data_highlight = FALSE,
  pos_info = TRUE) {
  
  # safety checks
  if (!is(bfile, "binary_isodat_file")) stop("this function is for isodat binary files only", call. = FALSE)
  
  # expressions
  new_line_blocks_expr <- rlang::enexpr(new_line_blocks)
  indent_blocks_expr <- rlang::enexpr(indent_blocks)
  data_blocks_expr <- rlang::enexpr(data_blocks)
  data_highlight_expr <- rlang::enexpr(data_highlight)
  data_highlight_text <- rlang::as_label(data_highlight_expr)
  
  # generate printout
  indent_width <- 2
  blocks_formatted <- bfile$blocks |> 
    get_unknown_blocks_text(raw = bfile$raw, unknown_block_n_chars = unknown_block_n_chars) |>
    mutate(
      # new lines
      nl_block = !!new_line_blocks_expr, 
      nl_text = ifelse(c(FALSE, .data$nl_block[-1]), "\n", ""),
      # indents
      indent_block = !!indent_blocks_expr,
      indent_text = ifelse(.data$nl_block & .data$indent_block, sprintf("%%%ds", !!indent_width) |> sprintf(""), ""),
      # position markers
      pos_text = ifelse(!!pos_info & .data$nl_block, sprintf("%07d: ", .data$start),  ""),
      # block text
      data_block = !!data_blocks_expr,
      data_highlight = !!data_highlight_expr,
      block_text = case_when(
        .data$data_block & .data$data_highlight ~ 
          sprintf("{HIGHLIGHT: '%s'; %d: '%s'}", data_highlight_text, .data$len, .data$block),
        .data$data_block ~ 
          sprintf("{%s-%d: '%s'}", .data$type, .data$data_len, .data$block),
        TRUE ~ sprintf("<%s>", .data$block)
      ),
      # everything
      block_formatted = sprintf(
        "%s%s%s%s", 
        .data$nl_text, .data$pos_text, .data$indent_text, .data$block_text
      )
    )
  
  return(blocks_formatted)
}

