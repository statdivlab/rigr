### error handling

test_that("lincom() throws an error if object is not of class uRegress", {
  expect_error(lincom(1, c(1)), 
               "uRegress object must be entered")
  expect_error(lincom("cat", c(1)), 
               "uRegress object must be entered")
  expect_error(lincom(ttest(rnorm(100)), c(1)), 
               "uRegress object must be entered")
})

dat <- data.frame(y = rnorm(100), x = rnorm(100))
reg <- regress("mean", y ~ x, data = dat, robustSE = FALSE)

test_that("lincom() throws an error if conf.level is not in (0,1)", {
  expect_error(lincom(reg, c(1,1), conf.level = 1.01),
               "Confidence Level must be between 0 and 1")
})

test_that("lincom() throws error if comb vector is not right length", {
  expect_error(lincom(reg, c(1)),
               "Vector of constants must be equal to number of coefficients in model, including intercept if applicable")
  expect_error(lincom(reg, c(1,1,1)), 
               "Vector of constants must be equal to number of coefficients in model, including intercept if applicable")
})

test_that("lincom() throws error if robust SEs requested but were not used in regress()", {
  expect_error(lincom(reg, c(1,1), robustSE = TRUE), 
               "uRegress object must be created with robust standard errors")
})

test_that("lincom() throws error if comb matrix has incorrect number of columns", {
  expect_error(lincom(reg, matrix(c(1,1, 2, 2, 3, 3), nrow = 2), robustSE = FALSE),
               "Matrix of constants must have columns equal to the number of coefficients")
})

test_that("lincom() throws error if hyp is wrong dimension or not numeric", {
  expect_error(lincom(reg, c(1,1), robustSE = FALSE, hyp = "a"),
               "Null hypothesis must a scalar.")
  expect_error(lincom(reg, c(1,1), robustSE = FALSE, hyp = c(1,2)),
               "Null hypothesis must a scalar.")
  expect_error(lincom(reg, comb = matrix(c(1,1,2,2, 3, 3), nrow = 3), 
                      robustSE = FALSE, hyp = matrix(c(0, 0, "a"), nrow = 3)),
               "Null hypothesis must numeric and of the same dimension as the number of combinations being tested.")
  expect_error(lincom(reg, comb = matrix(c(1,1,2,2, 3, "a"), nrow = 3), 
                      robustSE = FALSE, hyp = matrix(c(0, 0, "a"), nrow = 3)),
               "Null hypothesis must numeric and of the same dimension as the number of combinations being tested.")
})

test_that("lincom() throws error if eform not logical", {
  expect_error(lincom(reg, c(1,1), robustSE = FALSE, eform = 2),
               "Argument eform must be a logical.")
})

### single test

dat <- data.frame(y = rnorm(100), x = rnorm(100))
reg <- regress("mean", y ~ x, data = dat, robustSE = FALSE)