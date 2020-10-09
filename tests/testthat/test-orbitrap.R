context("Orbitrap functions")

# example files =====

test_that("Orbitrap files and sequences work", {
  
  expect_error(read_obitrap_seq_info_file(), "no.*sequence file")
  expect_error(read_obitrap_seq_info_file("DNE"), "does not exist")
  
})