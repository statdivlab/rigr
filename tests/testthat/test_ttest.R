### error handling

x1 <- c(-1, seq(1, 100))

test_that("ttest() throws error if geo mean requested with non-pos data", {
  expect_error(ttest(x1, geom = TRUE), 
               "Geometric mean requires that all numeric data are positive")
})

x1 <- seq(1, 100)

test_that("ttest() throws error if geo mean requested with non-pos null", {
  expect_error(ttest(x1, geom = TRUE, null.hypoth = -1), 
               "Geometric mean cannot be less than zero")
})

x1 <- rnorm(100)
x2 <- rnorm(100)

test_that("ttest() throws error if alternative is not 'two.sided', 'less', or 'greater", {
  expect_error(ttest(x1, x2, alternative = "blah"), 
               "'alternative' must be either 'less', 'two.sided', or 'greater'")
})

test_that("ttest() throws error if exact test requested for non-proportion", {
  expect_error(ttest(x1, exact = TRUE), 
               "Exact binomial confidence intervals cannot be computed for anything but a proportion.")
})

test_that("ttest() throws error if var.eq is not logical", {
  expect_error(ttest(x1, var.eq = 2), 
               "Please specify a logical value for variable 'var.eq'")
})

x3 <- rnorm(99)

test_that("ttest() throws error if matched test performedm on different numbers of observations", {
  expect_error(ttest(x1, x3, matched = TRUE), 
               "Cannot perform matched t-test on variable of unequal length")
})

test_that("ttest() throws error for non-numeric data", {
  expect_error(ttest(c("a", "B", "c")), 
               "Cannot perform t-test on non-numeric data")
})

test_that("ttest() throws error for non-numeric more.digits argument", {
  expect_error(ttest(x1, more.digits = TRUE), 
               "Argument 'more.digits' must be numeric")
})

test_that("ttest() throws error if by argument contains only one value", {
  expect_error(ttest(x1, by = rep(1, 100)), 
               "Variable 'by' only has one unique value")
})

test_that("ttest() throws error if by argument contains >2 unique values", {
  expect_error(ttest(x1, by = c(rep(1, 50), rep(2, 49), 3)), 
               "Variable 'by' has more than two unique values.")
})

test_that("ttest() throws error ifi by argument is not of same length as data", {
  expect_error(ttest(x1, by = c(rep(1, 50), rep(2, 51))), 
               "Variable 'by' is not of equal length to data vector")
})





