context("File utility functions")

# binary isodat file templates =====

test_that("binary isodat file templates", {
  
  tl <- template_binary_isodat_file_object()
  expect_equal(class(tl), c("binary_isodat_file", "binary_file"))
  expect_output(print(tl), "Textual representation .* isodat file")
  expect_equal(names(tl), c("raw", "data", "pos", "max_pos", "error_prefix", "blocks", "current_nav_block_idx"))
  expect_equal(names(tl$blocks), c("block_idx", "start", "end", "len", "data_len", "type", "priority", "block"))
    
})


test_that("binary isodat file navigation", {
  
  tl <- template_binary_isodat_file_object()
  tl$raw <- as.raw(c(0xff, 0xff, 0x06, 0x00, 0x0b, 0x00, 0x43, 0x46, 0x69, 0x6c))
  tl$max_pos <- 8L
  
  expect_error(iso_source_file_op_error(tl, "test"), "test.*nav block.*pos 1.*max 8")
  expect_error(move_to_pos(tl, 9), "exceeds position max")
  expect_equal(move_to_pos(tl, 9, reset_cap = TRUE)$pos, 9L)
  expect_error(cap_at_pos(tl, NULL), "cannot cap at position")
  expect_error(move_to_pos(tl, 5) %>% cap_at_pos(4), "smaller than current position")
  expect_equal(cap_at_pos(tl, 5)$max_pos, 5L)
  expect_equal(set_pos_and_cap(tl, 2, 4)[c("pos", "max_pos")], list(pos = 2L, max_pos = 4L))
  
  tl$blocks <- 
    tibble(
      block_idx = 1L:5L,
      start = c(1L, 15L, 42L, 51L, 92L),
      end = c(14L, 41L, 50L, 91L, 104L),
      len = end-start + 1L,
      data_len = c(0L, 0L, 0L, 0L, 0L),
      type = c("C block", "unknown", "text", "text", "C block"),
      priority = 1L,
      block = c("CTest", "fe f7 31 01", "test", "long test", "CTest2")
    )

  # nav block location
  expect_error(set_current_nav_block_idx(tl), "block_idx.*missing")  
  expect_error(set_current_nav_block_idx(tl, "1"), "cannot set")
  expect_error(set_current_nav_block_idx(tl, 0), "cannot set")
  expect_error(set_current_nav_block_idx(tl, c(1,2)), "cannot set")
  expect_error(set_current_nav_block_idx(tl, 10), "cannot set")
  expect_equal(tl$current_nav_block_idx, 1L)
  expect_equal(set_current_nav_block_idx(tl, 3)$current_nav_block_idx, 3L)
  
  # fetch block index
  expect_equal(fetch_block_idx(tl), 1:5)
  expect_equal(fetch_block_idx(tl, filter = len > 20), c(2L, 4L))
  expect_equal(fetch_block_idx(tl, filter = FALSE), integer(0))
  expect_equal(fetch_block_idx(tl, type = "text"), c(3L, 4L))
  expect_equal(fetch_block_idx(tl, block = "test"), 3L)
  expect_equal(fetch_block_idx(tl, block = "[tT]est", block_regex_match = TRUE), c(1L, 3L, 4L, 5L))
  expect_equal(fetch_block_idx(tl, min_pos = 42), c(3L, 4L, 5L))
  expect_equal(fetch_block_idx(tl, max_pos = 42), c(1L, 2L))
  expect_equal(fetch_block_idx(tl, min_block_idx = 4), c(4L, 5L))
  expect_equal(fetch_block_idx(tl, max_block_idx = 4), 1:4)
  expect_equal(fetch_block_idx(tl, occurence = 3), 3L)
  expect_equal(fetch_block_idx(tl, occurence = c(3:10)), 3:5)
  expect_equal(fetch_block_idx(tl, occurence = 10), integer(0))
  
  # fetch current block index
  tl$max_pos <- 120
  expect_equal(fetch_current_block_idx(tl), 1L)
  expect_equal(fetch_current_block_idx(move_to_pos(tl, 15)), 2L)
  expect_equal(fetch_current_block_idx(move_to_pos(tl, 25)), 2L)
  expect_equal(fetch_current_block_idx(move_to_pos(tl, 41)), 2L)
  
  # require_n
  expect_equal(fetch_block_idx(tl, occurence = 3, require_n = 1), 3L)
  expect_equal(fetch_block_idx(tl, occurence = 10, require_n = 0), integer(0))
  expect_equal(fetch_block_idx(tl, require_n = 5), 1:5)
  expect_error(
    fetch_block_idx(tl, filter = start > 100, type = "DNE", block = "bDNE", min_pos = 1000, max_pos = 1200, 
                    min_block_idx = 10, max_block_idx = 12, occurence = 10, require_n = 1),
               "could not find.*occurence 10.*block 'bDNE'.*type 'DNE'.*start > 100.*position >= 1000.*<= 1200.*block index >= 10.*<= 12.*nav block#1 'CTest'.*pos 1.* max 120")
  
  # fetch block entry
  expect_equal(fetch_block_entry(tl), tl$blocks)
  expect_equal(fetch_block_entry(tl, occurence = 3), tl$blocks[3, ])
  expect_equal(fetch_block_entry(tl, block_idx = c(2,4)), tl$blocks[c(2,4),])
  
  # move to control blocks
  expect_error(move_to_control_block(tl, block = "DNE", reset_cap = FALSE), "could not find")
  expect_error(move_to_control_block(tl, block = "CTest", occurence = 2, reset_cap = FALSE), "could not find")
  expect_equal(move_to_control_block(tl, block = "CTest")$pos, 1L)
  expect_equal(move_to_control_block(tl, block = "CTest")$max_pos, 10L)
  expect_equal(move_to_control_block(tl, block = "CTest")$current_nav_block, 1L)
  expect_error(expect_equal(move_to_control_block(tl, block = "CTest", move_to_end = TRUE)$pos, 15L), "exceeds position max")
  expect_equal(move_to_control_block(tl, block = "CTest", reset_cap = FALSE, move_to_end = TRUE)$pos, 15L)
  expect_equal(move_to_control_block(tl, block = "CTest", reset_cap = FALSE, move_to_end = TRUE)$current_nav_block, 1L)
  expect_error(move_to_control_block(tl, block = "CTest", occurence = 3, block_regex_match = TRUE, reset_cap = FALSE), "could not find.*'*CTest*'")
  expect_equal(move_to_control_block(tl, block = "CTest", occurence = 2, block_regex_match = TRUE, reset_cap = FALSE)$pos, 92L)
  expect_equal(move_to_control_block(tl, block = "CTest", occurence = 2, block_regex_match = TRUE, reset_cap = FALSE)$current_nav_block, 5L)
  expect_equal(move_to_control_block(tl, block = "CTest", occurence = 2, block_regex_match = TRUE, reset_cap = FALSE, update_current_nav_block = FALSE)$current_nav_block, 1L)
  expect_equal(move_to_control_block(tl, block = "CTest2", reset_cap = FALSE)$pos, 92L)
  expect_equal(move_to_control_block(tl, block = "CTest2", reset_cap = FALSE)$current_nav_block, 5L)
  expect_equal(move_to_control_block(tl, block = "CTest2", reset_cap = FALSE, move_to_end = TRUE)$pos, 105L)
  expect_equal(move_to_next_control_block(tl, block = "CTest", block_regex_match = TRUE, reset_cap = FALSE)$pos, 92L)
  
})


test_that("parsing raw data", {
  
  expect_equal(remove_trailing_zeros(as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)), size = 1), as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1)))
  expect_equal(remove_trailing_zeros(as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)), size = 2), as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0)))
  expect_equal(remove_trailing_zeros(as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)), size = 4), as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0)))
  expect_equal(remove_trailing_zeros(as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)), size = 8), as.raw(c(2, 6, 2, 0, 3, 5, 0, 2, 1, 0, 0, 0, 0, 0, 0)))
  
})

# FIXME: continue tests for utils_binary_files.R, utils_binary_isodat_files.R