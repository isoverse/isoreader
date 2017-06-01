context("Continuous flow")

test_that("test that supported cf files are correct", {
  expect_is(exts <- get_supported_cf_files(), "data.frame")
  expect_equal(exts$extension, c("cf", "dxf", "iarc"))
  expect_true(all(exts$fun %>% sapply(class) == "character"))
  expect_true(all(exts$fun %>% sapply(exists)))
  expect_true(all(exts$fun %>% sapply(get) %>% sapply(class) == "function"))
})