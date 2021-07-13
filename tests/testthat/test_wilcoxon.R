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


### one-sample test
set.seed(1)
a <- rnorm(50)
b <- rnorm(50)
y <- a - b

wil1 <- wilcoxon(y, correct = FALSE, conf.int = TRUE)
wil2 <- wilcox.test(y, correct = FALSE, conf.int = TRUE)

### TO-DO: really I should also be testing other quantities returned, but they mostly aren't provided
### by wilcox.test() so maybe by-hand using online calculator? e.g. vars, rank sums, z
test_that("wilcoxon() returns correct numbers for one-sample uncorrected test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(wil1$statistic, wil2$statistic)
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1:2,2]), digits = 4), 
               round(rep(wil2$p.value, 2), digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = TRUE, conf.int = TRUE)
wil2 <- wilcox.test(y, correct = TRUE, conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample corrected test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(wil1$statistic, wil2$statistic)
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1:2,2]), digits = 4), 
               round(rep(wil2$p.value, 2), digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = FALSE, alternative = "less", conf.int = TRUE)
wil2 <- wilcox.test(y, correct = FALSE, alternative = "less", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample uncorrected test (left)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(wil1$statistic, wil2$statistic)
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1:2,2]), digits = 4), 
               round(rep(wil2$p.value, 2), digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil2 <- wilcox.test(y, correct = FALSE, alternative = "greater", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample uncorrected test (right)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(wil1$statistic, wil2$statistic)
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1:2,2]), digits = 4), 
               round(rep(wil2$p.value, 2), digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(y, correct = FALSE, conf.int = TRUE, conf.level = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (uncorrected)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = TRUE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(y, correct = TRUE, conf.int = TRUE, conf.level = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (corrected)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, exact = TRUE, correct = FALSE, conf.int = TRUE)
wil2 <- wilcox.test(y, exact = TRUE, correct = FALSE, conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(wil1$statistic, wil2$statistic)
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1:2,2]), digits = 4), 
               round(rep(wil2$p.value, 2), digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = FALSE, alternative = "less", conf.int = TRUE)
wil2 <- wilcox.test(y, correct = FALSE, alternative = "less", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test (left)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(wil1$statistic, wil2$statistic)
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1:2,2]), digits = 4), 
               round(rep(wil2$p.value, 2), digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil2 <- wilcox.test(y, correct = FALSE, alternative = "greater", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test (right)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(wil1$statistic, wil2$statistic)
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1:2,2]), digits = 4), 
               round(rep(wil2$p.value, 2), digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(y, correct = FALSE, conf.int = TRUE, conf.level = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (exact)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})

wil1 <- wilcoxon(y, correct = TRUE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(y, correct = TRUE, conf.int = TRUE, conf.level = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (exact)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int))
})


