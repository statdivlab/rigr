### error handling

x1 <- rnorm(100)
x2 <- rnorm(100)

test_that("ttest() throws error if alternative is not 'two.sided', 'less', or 'greater", {
  expect_error(ttest(x1, x2, alternative = "blah"), "'alternative' must be either 'less', 'two.sided', or 'greater'")
})

