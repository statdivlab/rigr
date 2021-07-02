### error handling

y <- rnorm(100)

test_that("wilcoxon() throws error if mu is not a scalar", {
  expect_error(wilcoxon(y, mu = c(1,2)), "'mu' must be a single number")
})
