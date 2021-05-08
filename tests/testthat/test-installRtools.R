# library(testthat)
context("Test Rtools-related functionality")

test_that("get_compatible_rtools_version works", {
 
  # should fail, because of bogus R version 
  expect_error(get_compatible_rtools_version("0.0"))
  
  # Test some frozen versions
  expect_equal(get_compatible_rtools_version("3.6.3"), "3.5")
  expect_equal(get_compatible_rtools_version("2.9"), "3.0")
})

# get_rtools_url <- installr:::get_rtools_url
test_that("getRtoolsUrl returns correct urls", {
  skip_if_offline(host = "cran.r-project.org")
  
  # Download rtools history page
  rtools_history <- readLines("https://cran.r-project.org/bin/windows/Rtools/history.html")
  # See if we can find the urls we get on this page
  expect_true(any(stringr::str_detect(rtools_history, basename(get_rtools_url("4.0", "x86_64")))))
  expect_true(any(stringr::str_detect(rtools_history, basename(get_rtools_url("4.0", "i686")))))
  expect_true(any(stringr::str_detect(rtools_history, basename(get_rtools_url("3.5", "x86_64")))))
  expect_true(any(stringr::str_detect(rtools_history, basename(get_rtools_url("3.5", "i686")))))
})


# For some reason, this test fails on CRAN (but not on local machine)
# TODO: debug why this test is failing.
# get_compatible_rtools_version <- installr:::get_compatible_rtools_version
# Get an early warning for future problems
# test_that("get_rtools_url returns correct url for latest version of R", {
#   skip_if_offline(host = "cran.r-project.org")
#   
#   # Download rtools history page
#   rtools_history <- readLines("https://cran.r-project.org/bin/windows/Rtools/history.html")
#   expect_true(any(stringr::str_detect(rtools_history, basename(get_rtools_url(get_compatible_rtools_version(get_latest_r_version()))))))
# })
