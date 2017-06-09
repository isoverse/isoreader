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
  regexp<-paste("[\x20-\x7e]{", minlength, ",}", sep="")
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
  regexp <- paste("([\x20-\x7e]\\x00){", minlength, ",}", sep="")
  data_frame(
    byte_start = get_unicode(raw, minlength = minlength, value = FALSE, all = TRUE),
    value = get_unicode(raw, minlength = minlength, value = TRUE, all = TRUE),
    encoding='Unicode',
    byte_end = byte_start + nchar(value) * 2 - 1,
    byte_length = byte_end - byte_start + 1,
    str_length = byte_length/2)
}


# Parse data =====

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


