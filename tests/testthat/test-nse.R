context("Standard and non-standard evaluation")

test_that("quos to text works", {
  
  expect_error(quos_to_text(quo(a^2)), "not .* valid")
  expect_error(quos_to_text(quo(NULL)), "not .* valid")
  expect_error(quos_to_text(quo(~mean)), "not .* valid")
  expect_error(quos_to_text(quos(a^2, NULL, mean)), "not .* valid")
  expect_equal(quos_to_text(quo("a")), "a")
  expect_equal(quos_to_text(quo(a)), "a")
  expect_equal(quos_to_text(quos(a, "b")) %>% unlist(use.name = FALSE), c("a", "b"))
  expect_equal(quos_to_text(quos(x = a)), list(x="a"))
  
})