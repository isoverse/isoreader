# Binary file utils =============

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
        keys = data_frame(),
        pos = 1L
      ),
      class = "binary_file"
    )
  
  # read
  size <- file.info(filepath)$size
  con <- file(filepath, "rb")
  bfile$raw <- readBin(con, raw(), n = size)
  close(con)
  
  # find C_blocks
  bfile$C_blocks <- find_C_blocks(bfile$raw)
  
  # find keys
  bfile$keys <- find_binary_keys(bfile$raw)
  
  return(bfile)
}


# Navigation ======

# skip nbyte number of bytes in the raw data stream
skip_pos <- function(bfile, nbyte) {
  bfile$pos <- as.integer(bfile$pos + nbyte)
  return(bfile)
}

# skip certain number of data
skip_type <- function(bfile, type, length) {
  mapped_type <- map_binary_data_type(type)
  return(skip_pos(file, mapped_type$nbyte * length) )
}

# move to position
move_to_pos <- function(bfile, pos) {
  bfile$pos <- as.integer(pos)
  return(bfile)
}

# moves position to the end of a specific key or occurence of a key 
# @param key either a string or a data.frame line with key value and byteEnd (the way it is returned by find_key)
# @param occurence if key is a string, which occurence to move to? (use -1 for last occurence)
# @param fixed whether to find the key (if a string) by regexp match or fixed string (default = fixed string)
# @param ... passed on to find_binary_file_key
move_to_key <- function(bfile, key, occurence = 1, fixed = TRUE, ...) {
  
  key <- fetch_keys(bfile, pattern = key, occurence = occurence, fixed = fixed, ...)
  
  if (nrow(key) != 1) stop("not a valid key entry, can't move there: ", key, call. = FALSE)
  
  bfile$pos <- as.integer(key$byte_end + 1)
  return(bfile)
}

# moves position to the end of a specific C_block
# @inheritParams fetch_C_block
# @FIXME: testing
# move_to_C_block(my_test$binary, "NO", error_prefix = "haha")
# move_to_C_block(my_test$binary, "CData", occurence = 2)
move_to_C_block <- function(bfile, C_block, occurence = 1, error_prefix = NULL) {
  
  # fetch right C block
  cblock <- fetch_C_block(bfile, C_block, occurence = occurence, error_prefix = error_prefix)
  
  # move to right occurence
  move_to_pos(bfile, cblock$end[1] + 1)
}

# fetch a specific C_block
# @param C_block name of the block
# @param occurence which occurence to find? (use -1 for last occurence, use NULL for all)
# @param error_prefix custom error message if set, this is prefixed
# @FIXME: testing
fetch_C_block <- function(bfile, C_block, occurence = NULL, error_prefix = NULL) {
  # basic checks
  if (!is(bfile, "binary_file")) stop("need binary file object", call. = FALSE)
  if (nrow(bfile$C_blocks) == 0) stop("no C_blocks available", call. = FALSE)
  error_prefix <- if(is.null(error_prefix)) "" else str_c(error_prefix, ": ")
  
  # find C_blocks
  C_blocks <- filter(bfile$C_blocks, block == C_block)
  if (nrow(C_blocks) == 0) 
    stop(sprintf("%sblock '%s' not found", error_prefix, C_block), call. = FALSE)
  if (!is.null(occurence) && occurence > 1 && nrow(C_blocks) < occurence) 
    stop(sprintf("%soccurence %.0f of block '%s' not found (only %.0f occurences)",
                 error_prefix, occurence, C_block, nrow(C_blocks)), call. = FALSE)
  
  # return right occurence
  if (!is.null(occurence) && occurence == -1)
    return(tail(C_blocks, 1))
  else if (!is.null(occurence))
    return(C_blocks[occurence, ])
  else
    return(C_blocks)
}

# finds all keys matching 'key' or a specific occurence of it (use -1 for last occurence)
# @param fixed whether to find the key(s) by regexp match or fixed string (default = pattern)
# @param require if a result is required - valid values are specific integers (1, 5, 42) or character "1+", "2+" meaning at least
# @param error_prefix custom error message if set, this is prefixed
# @param byte_min only look for keys that start after this position
# @param byte_max only look for keys that start before this position
# @return the lines of the keys data frame with all the information about the found key(s)
fetch_keys <- function(bfile, pattern, occurence = NULL, fixed = FALSE, require = NULL, error_prefix = NULL,
                       byte_min = 0, byte_max = length(bfile$raw)) {
  
  # basic checks
  if (!is(bfile, "binary_file")) stop("need binary file object", call. = FALSE)
  if (nrow(bfile$keys) == 0) stop("no keys available", call. = FALSE)
  
  # get all keys in requested byte interval
  sub_keys <- filter(bfile$keys, byte_start > byte_min & byte_start < byte_max)
  
  # start looking for keys
  error_prefix <- if(is.null(error_prefix)) "" else str_c(error_prefix, ": ")
  if (nrow(sub_keys) == 0 && is.null(require)) return(filter(bfile$keys, FALSE))
  else if (nrow(sub_keys) == 0) stop(error_prefix, "no keys in this byte interval: ", byte_min, " - ", byte_max, call. = FALSE)
  
  # check if any keys with pattern
  if (length(idx <- grep(pattern, sub_keys$value, fixed = fixed)) == 0) {
    if (is.null(require)) return(filter(bfile$keys, FALSE))
    else stop(error_prefix, "key '", pattern, "' was not found", call. = FALSE)
  }
  
  if (!is.null(occurence)) {
    if (occurence == -1) occurence <- length(idx)
    
    if (occurence > length(idx))
      stop(sprintf("%skey '%s' was found but only has %.0f occurences (trying to select occurence # %.0f", 
                   error_prefix, pattern, length(idx), occurence), call. = FALSE)
    
  } else {
    occurence <- 1:length(idx) # return ALL found occurences
  }
  
  # subset occurences
  keys <- sub_keys[idx[occurence], ]
  
  # check if any require condition must be met
  if (!is.null(require)) {
    if ( is.integer(require) || is.numeric(require)) {
      if (nrow(keys) != require)
        stop(sprintf("%skey '%s' was found %.0f times but required %.0f times", 
                     error_prefix, pattern, nrow(keys), require), call. = FALSE)
    } else {
      require <- suppressWarnings(as.numeric(str_match(require,  "^(\\d+)\\+$") %>% { .[1,2] }))
      if (is.na(require)) stop("cannot interpret 'require' parameter ", require, call. = FALSE)
      if (nrow(keys) < require) 
        stop(sprintf("%skey '%s' was found %.0f times but required at least %.0f times", 
                     error_prefix, pattern, nrow(keys), require), call. = FALSE)
    }
  }
  
  return(keys)
}


# Keys ======

# find all isodat C_blocks in the binary (marked by the below regexp pattern followed by a string of ascii characters)
find_C_blocks = function(raw) {
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
      end = start + nchar(block) + 6 - 1
    )
}

# finds all unicode and ascii strings and stores them for navigation around the file
# @param raw the raw binary data
find_binary_keys = function(raw, asciiL = 8, unicodeL = 5) {
  ascii <- find_binary_ascii(raw, asciiL)
  unicode <- find_binary_unicode(raw, unicodeL)
  bind_rows(ascii, unicode) %>% 
    # arrange keys by byteStart
    arrange(byte_start) %>% 
    # add byte gap
    mutate(
      byte_gap = diff(sort(c(byte_start, byte_end, length(raw))))[c(FALSE,TRUE)] 
    )
}

# find all ascii strings in the raw data
# @param minlength minimum length of continuous ascii characters
# @return data frame of found ascii strings
find_binary_ascii = function(raw, minlength) {
  data_frame(
    byte_start = get_ascii(raw, minlength = minlength, value = FALSE, all = TRUE),
    value = get_ascii(raw, minlength = minlength, value = TRUE, all = TRUE),
    encoding='ASCII',
    byte_end = byte_start + nchar(value) - 1,
    byte_length = byte_end - byte_start + 1,
    str_length = byte_length)
}

# find all unicode strings in the raw binary data
# @param minlength minimun length of unicode characters (each are 2 bytes long)
# @return data frame with found unicode strings
find_binary_unicode = function(raw, minlength) {
  data_frame(
    byte_start = get_unicode(raw, minlength = minlength, value = FALSE, all = TRUE),
    value = get_unicode(raw, minlength = minlength, value = TRUE, all = TRUE),
    encoding='Unicode',
    byte_end = byte_start + nchar(value) * 2 - 1,
    byte_length = byte_end - byte_start + 1,
    str_length = byte_length/2)
}


# Parse data =====

# configuration information for the control blocks
get_ctrl_blocks_config <- function() {
  list(
    
    # FEFx blocks
    nl = list(size = 4, regexp = "\xff\xfe\xff\x0a"), # FF FE FF 0A new line
    `fef-x` = list(size = 4, regexp = "\xff\xfe\xff[\x01-\x13]", # meaning = ?
                   replace = function(b) str_c("fef-", readBin(b[4], "raw"))),
    `fef-0` = list(size = 4, regexp = "\xff\xfe\xff\\x00"), # meaning = ?
    ffff = list(size = 4, regexp = "\xff{4}"), # meaning = ?
    
    # x000 blocks
    stx = list(size = 4, regexp = "\x02\\x00{3}"), # start of text
    etx = list(size = 4, regexp = "\x03\\x00{3}"), # end of text
    `x-000` = list(size = 4, regexp = "[\x01-\x0b]\\x00{3}", replace = # meaning = ?
                     function(b) str_c(str_replace(readBin(b[1], "raw"), "^0", ""), "-000")),
    
    # specific text sequences
    `del-nl` = list(size = 2, regexp = "\x7f\x85"), # 7F 85 delete next line
    `eop-nl` = list(size = 2, regexp = "\xdc\x85"), # DC 85 end of proof?? next line
    `75_84` = list(size = 2, regexp = "\x75\x84") # 75 84 - no idea what it means but it's special somehow
  )
}

## deprecate after this?

# Wrapper for parsing binary data.
# 
# Convenience wrapper for parsing binary data for specific data types, includes sensibility checks.
# 
# @param raw either raw data stream or binary_file class
# @param type data type see \code{\link{map_binary_data_type}} for details
# @param length how many instances of this object (for characters and raw this means length of string, all others a vector)
# @param start the start position, if not provided but raw is a binary_file calss
# @param sensible an expected value bracket that if not met will throw an error
# @param error_prefix custom error message if set, this is prefixed
# @param print_binary this is for debugging, prints the binary sequence that is being interpreted but only if in debug mode!
# @return read data
parse_binary_data <- function(raw, type, length = 1, start = 1, sensible = NULL, error_prefix = NULL,
                              print_binary = FALSE) {
  
  # map data type
  mapped_type <- map_binary_data_type(type)
  
  # interpret raw correctly (either as binary_file or direct raw data)
  if (is(raw, "binary_file")) {
    start <- if (missing(start)) raw$pos else 1
    raw <- raw$raw
  }
  if (!is(raw, "raw")) stop("cannot parse non-binary data", call. = FALSE)
  
  # subset raw data to right point
  raw <- raw[start:length(raw)]
  
  # different reads
  if (type == "binary")
    read <- paste(readBin(raw, "raw", n = length, size = 1), collapse=" ")
  else if (type == "UTF8")
    read <- rawToChar(readBin(raw, "raw", n = length, size=1))
  else if (mapped_type$class == "character")
    read <- paste(readBin(raw, "character", n = length, size = mapped_type$nbyte), collapse="")
  else
    read <- readBin(raw, mapped_type$class, n = length, size = mapped_type$nbyte)
  
  # print binary debug
  if (print_binary && setting("debug")) {
    print(get_raw_binary_text(raw[1:(1 + length * mapped_type$nbyte)]))
  }
  
  # sensible check
  error_prefix <- if(is.null(error_prefix)) "" else str_c(error_prefix, ": ")
  if (!is.null(sensible)) {
    if (class(read) != class(sensible)) 
      stop(sprintf("%scannot compare data (%s) to expected values (%s), data type mismatch", 
                   error_prefix, class(read), class(sensible)), call. = FALSE)
    if (is.character(sensible) && !all(good_data <- str_detect(sensible, read))) 
      stop(sprintf("%sparsed data (%s, ...) does not match expected pattern (%s)",
                   error_prefix, str_c(head(read[!good_data]), collapse = ", "), sensible), call. = FALSE)
    else if (is.numeric(sensible) && !all(good_data <- read >= sensible[1] & read <= sensible[2]))
      stop(sprintf("%sparsed data (%s, ...) does not match expected value range (%s)",
                   error_prefix,  str_c(signif(head(read[!good_data])), collapse = ", "), 
                   str_c(sensible[1], " to ", sensible[2])),
           call. = FALSE)
  }
  
  return(read)
}

# Binary data type mapping
# 
# Maps binary C data types to proper R data types and byte lengths
# @param type
# \itemize{
#    \item{'binary'}{ = raw with 1 byte (raw data)}
#    \item{'UTF8'}{ = character with 1 byte (ascii)}
#    \item{'UTF16'}{ = character with 2 bytes (unicode)}
#    \item{'UTF32'}{ = character with 4 bytes (unicode)}
#    \item{'short'}{ = integer with 2 bytes (16bit)}
#    \item{'long'}{ = integer with 4 bytes (32bit)}
#    \item{'longlong'}{ = integer with 8 bytes (64bit)}
#    \item{'float'}{ = numeric with 4 bytes (32bit)}
#    \item{'double'}{ = numeric with 8 bytes (64bit)}
# }
# @note implemented signed int and complex if needed
map_binary_data_type <- function(
  type = c('binary', 'UTF8', 'UTF16', 'UTF32', 'short', 
           'long', 'long long', 'float', 'double')) {
  switch(
    type,
    binary = list(class = 'raw', nbyte = 1),
    UTF8 = list(class = 'character', nbyte = 1),
    UTF16 = list(class = 'character', nbyte = 2),
    UTF32 = list(class = 'character', nbyte = 4),
    short = list(class = 'integer', nbyte = 2),
    long = list(class = 'integer', nbyte = 4),
    longlong = list(class = 'integer', nbyte = 8),
    float = list(class = 'numeric', nbyte = 4),
    double = list(class = 'numeric', nbyte = 8),
    stop("not a valid data type: '", type, "'"))
}

# Get raw binary data in text form
# @return character string version of raw binary data
get_raw_binary_text <- function(rawdata) {
  toupper(parse_binary_data(rawdata, "binary", length = length(rawdata)))
}

# Get UNICODE text from raw binary data
# @param value if FALSE, returns pattern indices instead
# @param all if TRUE, returns vector of all found patterns
# @return vector of value(s) or start indices, NULL if none foud
get_unicode <- function(rawdata, minlength = 1, value = TRUE, all = FALSE) {
  regexp <- paste0("([\x20-\x7e]\\x00){", minlength, ",}")
  re_matches <- grepRaw(regexp, rawdata, all = all, value = value) 
  if (value) {
    # convert to text
    if (!all) re_matches <- list(re_matches)
    return(sapply(re_matches, function(x) rawToChar(x[c(TRUE, FALSE)])))
  } else {
    # index vector
    return(re_matches)
  }
}

# Get ASCII text from raw binary data
# @param value if FALSE, returns pattern indices instead
# @param all if TRUE, returns vector of all found patterns
# @return vector of value(s) or start indices, NULL if none foud
get_ascii <- function(rawdata, minlength = 1, value = TRUE, all = FALSE) {
  regexp <- paste0("([\x20-\x7e]){", minlength, ",}")
  re_matches <- grepRaw(regexp, rawdata, all = all, value = value) 
  if (value) {
    # convert to text
    if (!all) re_matches <- list(re_matches)
    return(sapply(re_matches, function(x) rawToChar(x)))
  } else {
    # index vector
    return(re_matches)
  }
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
  trailing_00s <- is_null_block %>% { length(.) - max(which(. == FALSE)) } 
  
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
map_C_block_structure <- function(bfile, C_block, occurence = 1, length = 100, ctrl_blocks = get_ctrl_blocks_config()) {
  cblock <- fetch_C_block(bfile, C_block, occurence = occurence, 
                          error_prefix = "could not map C block structure")
  map_binary_structure(bfile, length = length, start = cblock$start, ctrl_blocks = ctrl_blocks)
}

# map out binary structure for easy visualization (used mostly for error messages and debugging)
# @param start at which byte positio to start mapping (index 1 based)
# @param length how many bytes to map
# @param ctrl_blocks named list of block patterns with size, regexp and [optional] replace function
map_binary_structure <- function(bfile, start = bfile$pos, length = 100, ctrl_blocks = get_ctrl_blocks_config()) {
  
  print(start)
  print(bfile$raw[start:(start+length-1)])
  
  # generate new block record
  new_block <- function(start, length, type, rep_text = NA_character_) {
    block <- list(
      start = start, end = start + length - 1, type = type, text_level = text_level, 
      rep_text = rep_text, raw = bfile$raw[start:(start + length - 1)])
    setNames(list(block), start)
  }
  
  # loop variables
  blocks <- list()  
  data_buffer <- c()
  text_level <- 0
  pos <- last_block_end <- start
  while (pos < (start+length)) {
    
    # check for cblocks
    if ( nrow(cblock <- filter(bfile$C_blocks, start == pos)) > 0 ) {
      blocks <- c(blocks, new_block(start = pos, length = 0, type = "cblock", rep_text = 
                                      with(cblock, sprintf("C-%s-%s block='%s'", id1, id2, block))))
      pos <- last_block_end <- cblock$end + 1
      next
    }
    
    # check for other control blocks
    found_match <- FALSE
    for (b in names(ctrl_blocks)) {
      # lock for first regexp that matches exactly at the current position
      if ( length(b_pos <- grepRaw(ctrl_blocks[[b]]$regexp, bfile$raw, offset = pos)) && b_pos == pos ) {
        
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
        if (b == "etx") text_level <- text_level - 1
        blocks <- c(blocks, new_block(start = pos, length = ctrl_blocks[[b]]$size, type = b, rep_text = replace_text))
        if (b == "stx") text_level <- text_level + 1
        
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
  data_block_configs <- get_data_blocks_config()
  data_block_types <- names(data_block_configs)
  data_matches <- data_frame(data_type = data_block_types, matches = FALSE, 
                             trailing_zeros = 0, n_values = 0, rep_value = NA_character_, 
                             min_value = NA_real_, max_value = NA_real_)
  
  # loop through blocks to interpret data
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
      data <- parse_binary_data_block(raw_trim, data_block_types[j], exact_length = TRUE)
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

print.binary_structure_map <- function(x, ..., data_as_raw = FALSE) {
  
  # indentation function
  nl_indent <- function(lvl, byte_start) {
    mapply(function(n, bs) {
      if(is.na(n)) "" else str_c(c("\n", sprintf("%07d", bs), ": ", rep("  ", n)), collapse = "")
    }, lvl, byte_start)
  }
  
  # get blocks
  blocks <- map(x$blocks, `[`, c("start", "type", "text_level", "rep_text")) %>% bind_rows() 
  
  # data overview
  if (data_as_raw) {
    data_overview <- 
      lapply(x$blocks, function(block) {
        if (block$type == "data") 
          data_frame(
            rep_value = str_c(as.character(block$raw), collapse = " "), 
            start = block$start)
        else NULL
      }) %>% bind_rows() 
  } else {
    
    data_overview <- 
      lapply(x$blocks, function(block) {
        if (block$type == "data") mutate(block$matches, start = block$start)
        else NULL
      }) %>% bind_rows() %>% 
      # only consider actual matches
      filter(matches) %>% 
      # trailing zeros block
      mutate(trailing00_block = ifelse(trailing_zeros > 0,  str_c("<", trailing_zeros, "x00>"), "")) %>% 
      group_by(start) %>% 
      do({
        # figure out what to do in case of text supplied together with other data types
        data <- .
        text_value <- if ("text" %in% data$data_type) filter(data, data_type == "text") else NULL
        if (!is.null(text_value) && nrow(data > 1) && nchar(text_value$rep_value) >= 4) {
          # more than 4 chars in the text, chances are likely this is actually  text
          data <- text_value
        }
        
        # combine
        data_text <- with(data, str_c("[", rep_value, "]", trailing00_block))
        if (length(data_text) > 1) data_frame(rep_value = str_c("{", str_c(data_text, collapse = "|"), "}"))
        else data_frame(rep_value = data_text)
      }) 
  }
  
  # process blocks and block data for printing
  blocks %>% 
    left_join(data_overview, by = "start") %>% 
    mutate(
      # indentation
      nl = start == min(start) | type == "cblock" | type %in% c("stx", "etx") | c("", type[1:(n()-1)]) %in% c("stx", "etx"),
      text_level = text_level - min(text_level),
      indent = ifelse(nl, text_level, NA),
      
      # text blocks
      rep_text = ifelse(is.na(rep_text), "NA", rep_text),
      block_text = ifelse(type == "data", rep_value, str_c("<", rep_text, ">")), 
      indent_text = str_c(nl_indent(indent, start), block_text)
    ) %>% 
    # combine text
    { cat(str_c(.$indent_text, collapse = "")) }
  
}
