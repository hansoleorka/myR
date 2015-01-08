library(roxygen2)
roxygenize(package.dir="D:\\github\\myR\\")

library(devtools)
library(test_that)
test_dir("test")
