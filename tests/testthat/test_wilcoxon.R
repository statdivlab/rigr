### error handling

y <- rnorm(100)

test_that("wilcoxon() throws error if mu is not a (finite) scalar", {
  expect_error(wilcoxon(y, mu = c(1,2)), "'mu' must be a single number")
  expect_error(wilcoxon(y, mu = Inf), "'mu' must be a single number")
})

test_that("wilcoxon() throws error if conf.level is not a number between 0 and 1", {
  expect_error(wilcoxon(y, conf.level = 1.01, conf.int = TRUE), 
               "'conf.level' must be a single number between 0 and 1")
  expect_error(wilcoxon(y, conf.level = Inf, conf.int = TRUE),
               "'conf.level' must be a single number between 0 and 1")
  expect_error(wilcoxon(y, conf.level = c(0.9, 0.95), conf.int = TRUE),
               "'conf.level' must be a single number between 0 and 1")
})

test_that("wilcoxon() throws error if y is non-numeric", {
  expect_error(wilcoxon(c("a", 2, 1)),
               "'y' must be numeric")
})

test_that("wilcoxon() throws error if x is non-numeric in a paired test", {
  expect_error(wilcoxon(y = y, x = rep("hello world", 100), paired = TRUE),
               "'x' must be numeric")
})

test_that("wilcoxon() throws error y and x have different lengths in a paired test", {
  expect_error(wilcoxon(y = y, x = rnorm(99), paired = TRUE),
               "'y' and 'x' must have the same length")
})

test_that("wilcoxon() throws error if x is missing in a paired test", {
  expect_error(wilcoxon(y = y, paired = TRUE),
               "'x' is missing for paired test")
})

test_that("wilcoxon() throws error if y has zero non-infinite values", {
  expect_error(wilcoxon(y = c(Inf)), "not enough finite 'y' observations")
})

test_that("wilcoxon() throws error if x has zero non-infinite values in a paired test", {
  expect_error(wilcoxon(y = y, x = rep(Inf, 100)), "not enough finite 'x' observations")
})

test_that("wilcoxon() throws error if y values are all identical and CI is requested", {
  expect_error(wilcoxon(y = rep(1, 100), conf.int = TRUE), 
               "cannot compute confidence interval when all observations are tied")
})

test_that("wilcoxon() throws error if y and x values are all identical and CI is requested", {
  expect_error(wilcoxon(y = rep(100, 100), x = rep(100, 100), conf.int = TRUE), 
               "cannot compute confidence interval when all observations are tied")
})