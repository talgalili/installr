context("Test install.* functions")

test_that("install.URL errors on multiple exe URLs", {
  expect_error(install.URL(c("https://dummy.url1", "https://dummy.url1")), "exe_URL is not a single URL")
})
