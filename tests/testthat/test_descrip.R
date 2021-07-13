### error handling

mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)

test_that("descrip() throws error if strata are not the same lengths", {
  expect_error(descrip(mri, strata = list(a = "a", b = c("b", "c"))), 
               "all elements in strata must be same length")
})

test_that("descrip() throws error if above, below, labove, rbelow are not atomic", {
  expect_error(descrip(mri, above = list(a = 1)), "above must be a vector")
  expect_error(descrip(mri, below = list(a = 1)), "below must be a vector")
  expect_error(descrip(mri, labove = list(a = 1)), "labove must be a vector")
  expect_error(descrip(mri, rbelow = list(a = 1)), "rbelow must be a vector")
  expect_error(descrip(mri, lbetween = list(a = 1)), "lbetween must be a vector")
  expect_error(descrip(mri, rbetween = list(a = 1)), "rbetween must be a vector")
})

test_that("descrip() throws error if above, below, labove, rbelow are not numeric", {
  expect_error(descrip(mri, above = "a"), "above must be numeric")
  expect_error(descrip(mri, below = "a"), "below must be numeric")
  expect_error(descrip(mri, labove = "a"), "labove must be numeric")
  expect_error(descrip(mri, rbelow = "a"), "rbelow must be numeric")
  expect_error(descrip(mri, lbetween = "a"), "lbetween must be numeric")
  expect_error(descrip(mri, rbetween = "a"), "rbetween must be numeric")
})

test_that("descrip() throws error if interval is a matrix with more than 2 columns", {
  expect_error(descrip(mri$atrophy, interval = matrix(0, nrow = 1, ncol = 3)),
               "interval must be specified in a 2 column matrix")
  expect_error(descrip(mri$atrophy, linterval = matrix(0, nrow = 1, ncol = 3)),
               "linterval must be specified in a 2 column matrix")
  expect_error(descrip(mri$atrophy, rinterval = matrix(0, nrow = 1, ncol = 3)),
               "rinterval must be specified in a 2 column matrix")
  expect_error(descrip(mri$atrophy, lrinterval = matrix(0, nrow = 1, ncol = 3)),
               "lrinterval must be specified in a 2 column matrix")
})

### warning handling

test_that("descrip() gives warning if interval is specified as a vector", {
  expect_warning(descrip(mri$atrophy, interval = c(5,10,15)), "Assuming intervals between points specified in vector. To specify specific intervals, enter interval argument as a 2 column matrix instead of a vector")
  expect_warning(descrip(mri$atrophy, linterval = c(5,10,15)), "Assuming lintervals between points specified in vector. To specify specific lintervals, enter linterval argument as a 2 column matrix instead of a vector")
  expect_warning(descrip(mri$atrophy, rinterval = c(5,10,15)), "Assuming rintervals between points specified in vector. To specify specific rintervals, enter rinterval argument as a 2 column matrix instead of a vector")
  expect_warning(descrip(mri$atrophy, lrinterval = c(5,10,15)), "Assuming lrintervals between points specified in vector. To specify specific lrintervals, enter lrinterval argument as a 2 column matrix instead of a vector")
})



