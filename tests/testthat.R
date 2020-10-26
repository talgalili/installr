library(testthat)
library(installr)

options(repos = c(CRAN = "https://cran.rstudio.com"))

test_check("installr")
