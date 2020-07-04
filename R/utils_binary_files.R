# Binary file utils =============
# @TODO: remove deprecated functions

# Read binary file
# @note this does not work for very large files probably because of the 2^31-1 limit on vector size! think about ways to fix this...
# --> might have to acually read directly from the conection instead of the raw data buffer!
read_binary_file <- function(filepath) {
  
  if (!file.exists(filepath) || file.info(filepath)$isdir == TRUE)
    stop("file does not exist or is a directory: ", filepath, call. = TRUE)
  
  bfile <- 
    structure(
      list(
        raw = raw(),
        C_blocks = tibble(),
        data = list(),
        pos = 1L, # current position within the file
        max_pos = NULL, # max position to consider during operations
        error_prefix = "" # what prefix to append to errors
      ),
      class = "binary_file"
    )
  
  # read
  size <- file.info(filepath)$size
  con <- file(filepath, "rb")
  bfile$raw <- readBin(con, raw(), n = size)
  bfile$max_pos <- length(bfile$raw)
  close(con)
  
  # find C_blocks
  bfile$C_blocks <- find_C_blocks(bfile$raw)
  
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

# throw binary file error if error mode is active
op_error <- function(bfile, msg) {
  stop(sprintf("%s%s (pos %.0f)", bfile$error_prefix, msg, bfile$pos), call. = FALSE)
}

# Navigation ======

# skip nbyte number of bytes in the raw data stream
skip_pos <- function(bfile, nbyte) {
  move_to_pos(bfile, bfile$pos + nbyte)
}

# move to position
move_to_pos <- function(bfile, pos, reset_cap = FALSE) {
  if (reset_cap) bfile$max_pos <- length(bfile$raw)
  if (pos > bfile$max_pos) {
    op_error(
      bfile, sprintf("cannot move to position %.0f as it exceeds position max set at %.0f", 
                     pos, bfile$max_pos))
  } 
  bfile$pos <- as.integer(pos)
  return(bfile)
}

# cap at position
cap_at_pos <- function(bfile, pos) {
  if(is.null(pos)) stop("cannot cap at position NULL", call. = FALSE)
  if (pos < bfile$pos) {
    op_error(
      bfile, sprintf("cannot cap at position %.0f as it is smaller than current position %.0f", 
                     pos, bfile$pos))
  }
  bfile$max_pos <- as.integer(pos)
  return(bfile)
}

# fetch a specific C_block
# @param C_block name of the block
# @param min_pos minimum position where to find the block
# @param occurence which occurence to find? (use -1 for last occurence, use NULL for all)
# @FIXME: testing
fetch_C_block <- function(bfile, C_block, min_pos = 1, occurence = NULL, regexp_match = FALSE) {
  # basic checks
  if (!is(bfile, "binary_file")) stop("need binary file object", call. = FALSE)
  if (nrow(bfile$C_blocks) == 0) stop("no C_blocks available", call. = FALSE)
  if (missing(C_block) || !is.character(C_block)) stop("C_block name not provided", call. = FALSE)
  
  # find C_blocks
  if (regexp_match)
    C_blocks <- filter(bfile$C_blocks, str_detect(.data$block, C_block), .data$start >= min_pos)
  else
    C_blocks <- filter(bfile$C_blocks, .data$block == C_block, .data$start >= min_pos)
  if (nrow(C_blocks) == 0) {
    op_error(bfile, sprintf("block '%s' not found after position %.0f", C_block, min_pos))
  }
  
  if (!is.null(occurence) && occurence > 1 && nrow(C_blocks) < occurence) {
    op_error(
      bfile, sprintf("occurence %.0f of block '%s' not found (only %.0f occurences) after position %.0f", 
                     occurence, C_block, nrow(C_blocks), min_pos))
  }
  
  # return right occurence
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
# move_to_C_block(my_test$binary, "NO")
# move_to_C_block(my_test$binary, "CData", occurence = 2)
move_to_C_block <- function(bfile, C_block, min_pos = 1, occurence = 1, move_to_end = TRUE, reset_cap = TRUE, regexp_match = FALSE) {
  # fetch right C block
  cblock <- fetch_C_block(bfile, C_block, min_pos = min_pos, occurence = occurence, regexp_match = regexp_match)
  if (is.null(cblock)) {
    op_error(bfile, sprintf("cannot move to C block '%s'", C_block))
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
    op_error(bfile, sprintf("cannot cap at C block '%s'", C_block))
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
    op_error(bfile, sprintf("cannot move to Cblock range, first occurence of block '%s' before first block '%s'",
                            from_C_block, to_C_block))
  }

  # move to blocks
  bfile %>% 
    move_to_C_block(from_C_block, min_pos = min_pos, reset_cap = TRUE) %>% 
    cap_at_C_block(to_C_block, min_pos = min_pos)
}

# move to next C block range
move_to_next_C_block_range <- function(bfile, from_C_block, to_C_block){
  move_to_C_block_range(bfile, from_C_block, to_C_block, min_pos = bfile$pos)
}

# regular expression for control block
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

# regular expression for null characters
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
      regexp = hex[ctrls] %>% str_c(collapse = ""),
      size = length(raw)
    ),
    class = "binary_regexp")
}

# regular expression for text (unicode) elements
re_text <- function(text) {
  # can't easily assemble the \x5a structure so using a hard copy
  hex <- c(`20` = "\x20", `21` = "\x21", `22` = "\x22", `23` = "\x23", `24` = "\x24", `25` = "\x25", `26` = "\x26", `27` = "\x27", `28` = "\x28", `29` = "\x29", `2a` = "\x2a", `2b` = "\x2b", `2c` = "\x2c", `2d` = "\x2d", `2e` = "\x2e", `2f` = "\x2f", `30` = "\x30", `31` = "\x31", `32` = "\x32", `33` = "\x33", `34` = "\x34", `35` = "\x35", `36` = "\x36", `37` = "\x37", `38` = "\x38", `39` = "\x39", `3a` = "\x3a", `3b` = "\x3b", `3c` = "\x3c", `3d` = "\x3d", `3e` = "\x3e", `3f` = "\x3f", `40` = "\x40", `41` = "\x41", `42` = "\x42", `43` = "\x43", `44` = "\x44", `45` = "\x45", `46` = "\x46", `47` = "\x47", `48` = "\x48", `49` = "\x49", `4a` = "\x4a", `4b` = "\x4b", `4c` = "\x4c", `4d` = "\x4d", `4e` = "\x4e", `4f` = "\x4f", `50` = "\x50", `51` = "\x51", `52` = "\x52", `53` = "\x53", `54` = "\x54", `55` = "\x55", `56` = "\x56", `57` = "\x57", `58` = "\x58", `59` = "\x59", `5a` = "\x5a", `5b` = "\x5b", `5c` = "\x5c", `5d` = "\x5d", `5e` = "\x5e", `5f` = "\x5f", `60` = "\x60", `61` = "\x61", `62` = "\x62", `63` = "\x63", `64` = "\x64", `65` = "\x65", `66` = "\x66", `67` = "\x67", `68` = "\x68", `69` = "\x69", `6a` = "\x6a", `6b` = "\x6b", `6c` = "\x6c", `6d` = "\x6d", `6e` = "\x6e", `6f` = "\x6f", `70` = "\x70", `71` = "\x71", `72` = "\x72", `73` = "\x73", `74` = "\x74", `75` = "\x75", `76` = "\x76", `77` = "\x77", `78` = "\x78", `79` = "\x79", `7a` = "\x7a", `7b` = "\x7b", `7c` = "\x7c", `7d` = "\x7d", `7e` = "\x7e")
  
  # return hex regexps of the text
  structure(
    list(
      label = sprintf("{%s}", text),
      regexp = hex[as.character(charToRaw(text))] %>% str_c("\\x00") %>% str_c(collapse = ""),
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
  # on unix: paste instead of str_c breaks concatenating a few specific regexps (e.g. re_combine(re_not_null(2), re_block("fef-x")))
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
  if (!is(bfile, "binary_file")) stop("need binary file object", call. = FALSE)
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
  if (!is(bfile, "binary_file")) stop("need binary file object", call. = FALSE)
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
  op_error(
    bfile, 
    sprintf("could not find '%s'%s in search interval %.0f to %.0f, found '%s...'",
            regexps$label, 
            gap_text, bfile$pos, bfile$max_pos,
            bfile %>% 
              map_binary_structure(length = regexps$size + 
                                     (if(!is.null(max_gap)) max_gap else 50) + 10) %>% 
              generate_binary_structure_map_printout()))
}

# cap at next regular expression pattern
# @param ... construct with the re... functions
# @param max_gap maximum number of bytes until the pattern
# @param move_to_end whether to move to the end of the pattern (default is yes)
cap_at_next_pattern <- function(bfile, ..., max_gap = NULL) {
  # safety check
  if (!is(bfile, "binary_file")) stop("need binary file object", call. = FALSE)
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
  op_error(
    bfile, 
    sprintf("could not find '%s'%s in search interval %.0f to %.0f, found '%s...'",
            regexps$label, 
            gap_text, bfile$pos, bfile$max_pos,
            bfile %>% 
              map_binary_structure(length = regexps$size + 
                                     (if(!is.null(max_gap)) max_gap else 50) + 10) %>% 
              generate_binary_structure_map_printout()))
}

# capture data block data in specified type
# uses parse_raw_data and therefore can handle multiple data types
# @inheritParams parse_raw_data
# note: consider renaming to capture_data_till_pattern
capture_data <- function(bfile, id, type, ..., data_bytes_min = 0, data_bytes_max = NULL, 
                         move_past_dots = FALSE, re_size_exact = TRUE,
                         ignore_trailing_zeros = TRUE, exact_length = TRUE, sensible = NULL) {
  
  # reset existing data in this field
  bfile$data[[id]] <- NULL
  
  # regexp
  regexps <- re_combine(...)
  
  # move to begining of target ... after the data
  if (!is(bfile, "binary_file")) stop("need binary file object", call. = FALSE)
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
  if (!is(bfile, "binary_file")) stop("need binary file object", call. = FALSE)
  
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
  }) %>% bind_rows() %>% 
    # byte positions
    mutate(
      start = re_positions,
      end = .data$start + nchar(.data$block) + 6 - 1
    )
}


# Parse data =====

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
    `fef-0` = list(size = 4L, auto = TRUE, regexp = "\xff\xfe\xff\\x00"), # meaning = ?
    `fef-x` = list(size = 4L, auto = TRUE, regexp = "\xff\xfe\xff.", # meaning = ?
                   replace = function(b) str_c("fef-", readBin(b[4], "raw"))),
    `eee-0` = list(size = 4L, auto = TRUE, regexp = "\xef\xef\xef\\x00."),
    ffff    = list(size = 4L, auto = TRUE, regexp = "\xff{4}"), # meaning = ?
    
    # x000 blocks
    stx        = list(size = 4L, auto = TRUE, regexp = "\x02\\x00{3}"), # start of text
    etx        = list(size = 4L, auto = TRUE, regexp = "\x03\\x00{3}"), # end of text
    `x-000`    = list(size = 4L, auto = TRUE, regexp = "[\x01-\x1f]\\x00{3}", replace = # meaning = ? maybe 1-000 has special meaning?
                     function(b) str_c(str_replace(readBin(b[1], "raw"), "^0", ""), "-000")),
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
  get_ctrl_blocks_config() %>%
    {
      tibble(
        block = names(.),
        regexp = map_chr(., "regexp"),
        hexadecimal = map_chr(
          .,
          ~.x$regexp %>% charToRaw() %>% as.character() %>% paste(collapse = " ")
        )
      )
    }
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
      non_text_pos <- grepRaw("([\x20-\xff][\x01-\xff])|(\\x00\\x00)", raw_trim)
      actual_text <- intToUtf8(raw_trim[1:(non_text_pos - 1)])
      if (!is.null(errors)) {
        stop(
          sprintf("%sexpected unicode data for %.0f bytes but found only %.0f ('%s'), non-text raw data afterwards: %s", 
                  error_prefix, n*2, non_text_pos, actual_text, 
                  raw_trim[(non_text_pos + 1):length(raw_trim)] %>% {
                    if (length(.) > 10) c(as.character(head(., 10)), sprintf("+%d more", length(.) - 10))
                    else as.character(.)
                  } %>% paste(collapse = " ")
          ), 
          call. = FALSE)
      }
      return(NULL)
    }
  }
  
  # process data
  type_bytes <- seq_along(dbc) %>% rep(times = map_int(dbc, "size")) %>% rep(times = n)
  data <- list()
  for (i in 1:length(dbc)) {
    
    if (dbc[[i]]$type == "character") {
      parsed_data <- raw[type_bytes == i] %>% intToUtf8()
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
      if (class(data[[i]]) != class(sensible[[i]]) && 
          class(data[[i]]) != "integer" && class(sensible[[i]]) != "numeric" ) # allow integer to numeric comparison
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

# @todo for testing
# take_off_trailing_00s(as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)), size = 1)
# take_off_trailing_00s(as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)), size = 2)
# take_off_trailing_00s(as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)), size = 4)
# take_off_trailing_00s(as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)), size = 8)
# 
# removes trailing 00 but only up to making the residual data still a multiple of the data size (leave at least 1 multiple), if can't take enough off to make the data a multiple of the data size, don't cut anything at all
# @param raw the raw data
# @param size the size of a data unit
remove_trailing_zeros <- function(raw, size) {
  
  # leave at least 1 multiple
  if (length(raw) <= size) return(raw)
  
  # find number of trailing 0s (i.e. everything after the highest byte that is not 00)
  is_null_block <- raw == as.raw(0)
  if (any(is_null_block == FALSE))
    trailing_00s <- is_null_block %>% { length(.) - max(which(. == FALSE)) } 
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

# Binary structure =======

# map binary structure starting at a specific Cblock (used mostly for error messages and debugging)
# @param C_block name of the Cblock to start the structure from
# @param length how many bytes to map
# @inheritParams fetch_C_block
# @inheritParams map_binary_structure
# @FIXME: testing
map_C_block_structure <- function(bfile, C_block, occurence = 1, length = 100, ctrl_blocks = get_ctrl_blocks_config()) {
  cblock <- fetch_C_block(bfile, C_block, occurence = occurence)
  map_binary_structure(bfile, length = length, start = cblock$start, ctrl_blocks = ctrl_blocks)
}

#' Map isodat file binary structure.
#'
#' Map out binary structure for easy visualization (used mostly for error messages and debugging). See the development vignette for details and example application.
#' 
#' @param bfile the binary file, stored in each iso_file under \code{$binary} if (and only if) the file was read with \link{iso_turn_debug_on} activated before.
#' @param length how many bytes to map
#' @param start at which byte position to start mapping (index 1 based)
#' @param ctrl_blocks named list of block patterns with size, regexp and [optional] replace function
map_binary_structure <- function(bfile, length = 100, start = bfile$pos, ctrl_blocks = get_ctrl_blocks_config()) {
  
  if(!is(bfile, "binary_file")) 
    stop("can only generate map for binary file object, passed in: ", 
         str_c(class(bfile), collapse = ", "), call. = FALSE)
  
  # generate new block record
  new_block <- function(start, length, type, rep_text = NA_character_) {
    block <- list(
      start = start, end = start + length - 1, type = type, 
      rep_text = rep_text, raw = bfile$raw[start:(start + length - 1)])
    rlang::set_names(list(block), start)
  }
  
  # loop variables
  blocks <- list()  
  data_buffer <- c()
  pos <- last_block_end <- start
  while (pos < (start+length)) {
    
    # check for cblocks
    if ( nrow(cblock <- filter(bfile$C_blocks, start == pos)) > 0 ) {
      if (pos > last_block_end) {
        blocks <- c(blocks, new_block(start = last_block_end, length = pos - last_block_end, type = "data"))
      }
      blocks <- c(blocks, new_block(start = pos, length = 0, type = "cblock", rep_text = 
                                      with(cblock, sprintf("C-%s-%s %s", id1, id2, block))))
      pos <- last_block_end <- cblock$end + 1
      next
    }
    
    # check for other control blocks
    found_match <- FALSE
    for (b in names(ctrl_blocks)) {
      # lock for first regexp that matches exactly at the current position
      if ( ctrl_blocks[[b]]$auto && length(b_pos <- grepRaw(ctrl_blocks[[b]]$regexp, bfile$raw, offset = pos)) > 0 && b_pos == pos) {
        
        # data block
        if (pos > last_block_end) {
          blocks <- c(blocks, new_block(start = last_block_end, length = pos - last_block_end, type = "data"))
        }
        
        # ctrl block representative text replacement function
        replace_text <- b
        replace_function <- ctrl_blocks[[b]]$replace
        if (!is.null(replace_function) && is.function(replace_function)) {
          replace_text <- replace_function(bfile$raw[pos:(pos+ctrl_blocks[[b]]$size-1)])
        }
        
        # new control block
        blocks <- c(blocks, new_block(start = pos, length = ctrl_blocks[[b]]$size, type = b, rep_text = replace_text))
        
        # move position
        pos <- last_block_end <- pos + ctrl_blocks[[b]]$size
        found_match <- TRUE
        break
      }
    }
    
    # check match - if no match, advance by just 1
    if (!found_match) pos <- pos + 1
  }
  
  
  # data blocks processing
  data_block_configs <- get_data_blocks_config() %>% {.[map_lgl(., "auto")] }
  data_block_types <- names(data_block_configs)
  data_matches <- tibble(data_type = data_block_types, matches = FALSE, 
                             trailing_zeros = 0, n_values = 0, rep_value = NA_character_, 
                             min_value = NA_real_, max_value = NA_real_)
  
  # loop through blocks to interpret data
  if (length(blocks) == 0) # no structure blocks found
    blocks <- new_block(start, length, "raw")
  
  for (i in 1:length(blocks)) {
    
    # only process if data blocks
    bl <- blocks[[i]]
    if (bl$type != "data") next
    
    # check for all 0 blocks
    if (all(bl$raw == as.raw(0))) {
      # recode, this is not a data block
      blocks[[i]]$type <- "null"
      blocks[[i]]$rep_text <- str_c(length(bl$raw), "x00")
      next
    }
    
    # set matches
    blocks[[i]]$matches <- data_matches
    blocks[[i]]$data <- list()
    
    # search through data patterns
    for (j in 1:length(data_block_types)) {
      
      # parse data tblock
      raw_trim <- remove_trailing_zeros(bl$raw, data_block_configs[[data_block_types[j]]]$size)
      data <- parse_raw_data(raw_trim, data_block_types[j], exact_length = TRUE, errors = NULL)
      if (is.null(data)) next
      
      # store data
      if (length(data) == 1) {
        rep_value <- if(is.numeric(data)) str_c(signif(data, 8)) else data
      } else {
        rep_value <- str_c(signif(data[1:min(3, length(data))], 8), collapse = ",")
        if (length(data) > 3) rep_value <- str_c(rep_value, ",... (", length(data), ")")
      }
      blocks[[i]]$data[[data_block_types[j]]] <- data
      blocks[[i]]$matches[j, "matches"] <- TRUE
      blocks[[i]]$matches[j, "n_values"] <- length(data)
      blocks[[i]]$matches[j, "rep_value"] <- rep_value
      blocks[[i]]$matches[j, "trailing_zeros"] <- length(bl$raw) - length(raw_trim)
      
    }
  }
  
  return(structure(list(blocks = blocks), class="binary_structure_map"))
}

# generate binary structure map printout
# @inheritParams print.binary_structure_map
# @FIXME: testing
generate_binary_structure_map_printout <- function(bsm, data_as_raw = FALSE, line_break_blocks = c(), pos_info = FALSE) {
  
  # indentation function
  nl_indent <- function(nls, lvl, byte_start) {
    mapply(function(nl, n, bs) {
      n <- if(is.na(n)) 0 else n
      if(!nl) "" 
      else 
        str_c(c(if (nl) "\n" else "", 
                if (pos_info) sprintf("%07d: ", bs), 
                rep("  ", n)), collapse = "")
    }, nls, lvl, byte_start)
  }
  
  # get blocks
  blocks <- lapply(bsm$blocks, function(block) {
    block$raw <- str_c(block$raw, collapse = " ")
    block[c("start", "type", "rep_text", "raw")]
  }) %>% bind_rows() 
  
  # data overview
  if (data_as_raw) {
    # raw data
    data_overview <- 
      lapply(bsm$blocks, function(block) {
        if (block$type == "data") 
          tibble(
            rep_value = sprintf("{%s}", str_c(as.character(block$raw), collapse = " ")), 
            start = block$start)
        else NULL
      }) %>% bind_rows() 
  } else {
    # processed data
    data_overview <- 
      lapply(bsm$blocks, function(block) {
        if (block$type == "data") mutate(block$matches, start = block$start)
        else NULL
      }) %>% bind_rows() %>% 
      # only consider actual matches
      filter(matches) 
    
    if (nrow(data_overview) > 0) {
      data_overview <- data_overview %>% 
        # trailing zeros block
        mutate(trailing00_block = 
                 ifelse(.data$trailing_zeros > 0,  str_c("<", .data$trailing_zeros, "x00>"), "")) %>% 
        group_by(.data$start) %>% 
        do({
          # figure out what to do in case of text supplied together with other data types
          data <- .
          text_value <- if ("text" %in% data$data_type) filter(data, .data$data_type == "text") else NULL
          if (!is.null(text_value) && nrow(data > 1) && nchar(text_value$rep_value) >= 4) {
            # more than 4 chars in the text, chances are likely this is actually  text
            data <- text_value
          }
          
          # combine
          data_text <- with(data, str_c("{", rep_value, "}", trailing00_block))
          if (length(data_text) > 1) tibble(rep_value = str_c("{", str_c(data_text, collapse = "|"), "}"))
          else tibble(rep_value = data_text)
        }) 
    }
  }
  
  # process blocks and block data for printing
  if (nrow(data_overview) == 0) data_overview <- tibble(start = integer(0), rep_value = character(0))
  all_blocks <- blocks %>% left_join(data_overview, by = "start") 
  
  # indentation
  if (nrow(all_blocks) == 1)
    all_blocks <- mutate(all_blocks, nl = TRUE, text_level = 0)
  else 
    all_blocks <- all_blocks %>% 
    mutate(
      nl = .data$start == min(.data$start) | .data$type %in% line_break_blocks | c("", .data$type[1:(n()-1)]) %in% line_break_blocks,
      text_level = c(NA, (head(cumsum(.data$type == "stx"), -1) - tail(cumsum(.data$type == "etx"), -1))))
  
  # calculate indentation level
  all_blocks <- all_blocks %>% 
    mutate(indent = .data$text_level - min(.data$text_level, na.rm = TRUE))
  
  # text blocks
  all_blocks %>% 
    mutate(
      rep_text = ifelse(is.na(.data$rep_text), str_c("<", .data$raw, ">"), str_c("<", .data$rep_text, ">")),
      rep_value = ifelse(is.na(.data$rep_value), str_c("{", .data$raw, "}"), .data$rep_value),
      block_text = ifelse(.data$type == "data", .data$rep_value, .data$rep_text), 
      indent_text = str_c(nl_indent(.data$nl, .data$indent, .data$start), .data$block_text)
    ) %>% 
    # combine text
    { str_c(.$indent_text, collapse = "") }
}

#' Print binary structure map
#' @param x object to show.
#' @param ... additional parameters passed to print.default
#' @param data_as_raw whether to show data as raw
#' @param line_break_blocks at which blocks to introduce a line break
#' @param pos_info whether to include position information
#' @export
print.binary_structure_map <- function(x, ..., data_as_raw = FALSE, line_break_blocks = c("cblock", "stx", "etx"), pos_info = TRUE) {
  cat("# Binary data structure: ", generate_binary_structure_map_printout(x, data_as_raw, line_break_blocks, pos_info))
}

