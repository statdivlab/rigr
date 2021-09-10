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

test_that("ttest() throws error if two variables and by also given", {
  expect_error(ttest(x1, x2, by = rep(1, 100)),
               "Please specify only one of the variables 'by' or 'var2'")
})

test_that("ttest() throws error if var.eq is not logical", {
  expect_error(ttest(x1, var.eq = 2), 
               "Please specify a logical value for variable 'var.eq'")
})

x3 <- rnorm(99)

test_that("ttest() throws error if matched test performed on different numbers of observations", {
  expect_error(ttest(x1, x3, matched = TRUE),
              "Cannot perform matched t-test on variable of unequal length")
  expect_error(ttest(x3, x1, matched = TRUE),
               "Cannot perform matched t-test on variable of unequal length")
  expect_error(ttest(x1, by = c(rep(1, 51), rep(2, 49)), matched = TRUE),
               "Cannot perform matched t-test on variable of unequal length")
})

test_that("ttest() throws error for non-numeric data", {
  expect_error(ttest(c("a", "B", "c")), 
               "Cannot perform t-test on non-numeric data")
  expect_error(ttest(x1, rep(NA, 100)), 
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

test_that("ttest() throws error if by argument is not of same length as data", {
  expect_error(ttest(x1, by = c(rep(1, 50), rep(2, 51))), 
               "Variable 'by' is not of equal length to data vector")
  expect_error(ttest(x1, by = c(rep(1, 50), rep(2, 50), NA)), "Variable 'by' is not of equal length to data vector")
})

test_that("ttest() warns about 0 null in geometric mean test", {
  expect_warning(ttest(exp(x1), geom = TRUE), 
                 "Geometric mean of zero not allowable: alternative hypothesis default changed to 1")
})

### one-sample test (or two-sample paired test)

set.seed(1)
a <- rnorm(50)
b <- rnorm(50)
d <- a - b

t1 <- ttest(d)
t2 <- t.test(d)
t3 <- ttest(a, b, matched = TRUE)
t4 <- t.test(a, b, paired = TRUE)

### TO-DO: really I should also be testing other quantities returned, but they mostly aren't provided
### by t.test() so maybe by-hand?
test_that("ttest() returns correct numbers for one-sample test", {
  expect_s3_class(t1, "ttest")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[7]], start = 2, stop = nchar(t1$tab[[7]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(t1$tab[[1]], "d") # var name
  expect_equal(as.numeric(t1$tab[[2]]), length(d)) # n obs
  expect_equal(as.numeric(t1$tab[[3]]), sum(is.na(d))) # NAs
  expect_equal(as.numeric(t1$tab[[4]]), t2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[5]]), t2$stderr[[1]], tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[6]]), sd(d), tolerance = 1e-3) # std dev of data
  expect_equal(t1$var1, d) 
  expect_equal(t1$var2, NA)
  expect_equal(t1$by, NA)
  expect_false(as.logical(t1$par[[1]])) # geom parameter
  expect_equal(as.numeric(t1$par[[2]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[3]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[4]])) # equal var
  expect_equal(as.numeric(t1$par[[5]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(t1$par[[6]])) # matched
  expect_equal(as.numeric(t1$par[[7]]), 3) # digits
  
  expect_s3_class(t3, "ttest")
  expect_equal(t3$df, t4$parameter[[1]]) # df
  expect_equal(t3$tstat, t4$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t3$p, t4$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t3$tab[[3, 7]], start = 2, stop = nchar(t3$tab[[3, 7]])-1), ", ")[[1]]),
               t4$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t3$tab[[1, 7]], start = 2, stop = nchar(t3$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t3$tab[[2, 7]], start = 2, stop = nchar(t3$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(t3$tab[[1,1]], "a") # var name
  expect_equal(t3$tab[[2,1]], "b") # var name
  expect_equal(t3$tab[[3,1]], "Difference") # var name
  expect_equal(as.numeric(t3$tab[[1, 2]]), length(a)) # n obs
  expect_equal(as.numeric(t3$tab[[2, 2]]), length(b)) # n obs
  expect_equal(as.numeric(t3$tab[[3, 2]]), length(d)) # n obs
  expect_equal(as.numeric(t3$tab[[1, 3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(t3$tab[[2, 3]]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(t3$tab[[3, 3]]), sum(is.na(d))) # NAs
  expect_equal(as.numeric(t3$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t3$tab[[2, 4]]), mean(b), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t3$tab[[3, 4]]), t4$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(t3$var1, a)
  expect_equal(t3$var2, b)
  expect_equal(t3$by, NA)
  expect_false(as.logical(t3$par[[1]])) # geom parameter
  expect_equal(as.numeric(t3$par[[2]]), t4$null.value[[1]]) # null
  expect_equal(t3$par[[3]], t4$alternative) # alternative
  expect_false(as.logical(t3$par[[4]])) # equal var
  expect_equal(as.numeric(t3$par[[5]]), attr(t4$conf.int, "conf.level")) # conf level
  expect_true(as.logical(t3$par[[6]])) # matched
  expect_equal(as.numeric(t3$par[[7]]), 3) # digits
})

t1 <- ttest(d, null.hypoth = 0.5)
t2 <- t.test(d, mu = 0.5)
t3 <- ttest(a, b, matched = TRUE, null.hypoth = 0.5)
t4 <- t.test(a, b, paired = TRUE, mu = 0.5)

test_that("ttest() returns correct numbers for one-sample test (non-zero null)", {
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(t1$par[[2]]), t2$null.value[[1]]) # null
  
  expect_equal(t3$tstat, t4$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t3$p, t4$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(t3$par[[2]]), t4$null.value[[1]]) # null
})

t1 <- ttest(d, alternative = "less")
t2 <- t.test(d, alternative = "less")
t3 <- ttest(a, b, matched = TRUE, alternative = "less")
t4 <- t.test(a, b, paired = TRUE, alternative = "less")

test_that("ttest() returns correct numbers for one-sample test (left)", {
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(t1$par[[3]], t2$alternative) # alternative
  
  expect_equal(t3$tstat, t4$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t3$p, t4$p.value, tolerance = 1e-3) # p-value
  expect_equal(t3$par[[3]], t4$alternative) # alternative
})

t1 <- ttest(d, alternative = "greater")
t2 <- t.test(d, alternative = "greater")
t3 <- ttest(a, b, matched = TRUE, alternative = "greater")
t4 <- t.test(a, b, paired = TRUE, alternative = "greater")

test_that("ttest() returns correct numbers for one-sample test (right)", {
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(t1$par[[3]], t2$alternative) # alternative

  expect_equal(t3$p, t4$p.value, tolerance = 1e-3) # p-value
  expect_equal(t3$par[[3]], t4$alternative) # alternative
})

t1 <- ttest(d, conf.level = 0.8)
t2 <- t.test(d, conf.level = 0.8)
t3 <- ttest(a, b, matched = TRUE, conf.level = 0.8)
t4 <- t.test(a, b, paired = TRUE, conf.level = 0.8)

test_that("ttest() returns correct numbers for one-sample test with conf.level other than 0.95", {
  expect_equal(as.numeric(strsplit(substr(t1$tab[[7]], start = 2, stop = nchar(t1$tab[[7]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$par[[5]]), attr(t2$conf.int, "conf.level")) # conf level
  
  expect_equal(as.numeric(strsplit(substr(t3$tab[[3, 7]], start = 2, stop = nchar(t3$tab[[3, 7]])-1), ", ")[[1]]),
               t4$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t3$tab[[1, 7]], start = 2, stop = nchar(t3$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.9, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.9, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t3$tab[[2, 7]], start = 2, stop = nchar(t3$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.9, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.9, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t3$par[[5]]), attr(t4$conf.int, "conf.level")) # conf level

})

t1 <- ttest(exp(d), geom = TRUE, null.hypoth = 1)
t2 <- t.test(d, geom = TRUE)
t3 <- ttest(exp(a), exp(b), matched = TRUE, geom = TRUE, null.hypoth = 1)
t4 <- t.test(a, b, paired = TRUE, geom = TRUE)

test_that("ttest() returns correct numbers for one-sample test of geometric mean", {
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[7]], start = 2, stop = nchar(t1$tab[[7]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$tab[[4]]), t2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[5]]), t2$stderr[[1]], tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[6]]), sd(d), tolerance = 1e-3) # std dev of data
  expect_true(as.logical(t1$par[[1]])) # geom parameter
  expect_equal(as.numeric(t1$par[[2]]), 0) # null 
  
  expect_equal(t3$tstat, t4$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t3$p, t4$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t3$tab[[3, 7]], start = 2, stop = nchar(t3$tab[[3, 7]])-1), ", ")[[1]]),
               t4$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t3$tab[[1, 7]], start = 2, stop = nchar(t3$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t3$tab[[2, 7]], start = 2, stop = nchar(t3$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t3$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t3$tab[[2, 4]]), mean(b), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t3$tab[[3, 4]]), t4$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_true(as.logical(t3$par[[1]])) # geom parameter
  expect_equal(as.numeric(t3$par[[2]]), 0) # null
})

### two-sample unpaired test, unpooled variance

t1 <- ttest(a, b, matched = FALSE)
t2 <- t.test(a, b, paired = FALSE)

test_that("ttest() returns correct numbers for two-sample test,  unpooled variance", {
  expect_s3_class(t1, "ttest")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 7]], start = 2, stop = nchar(t1$tab[[3, 7]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 7]], start = 2, stop = nchar(t1$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 7]], start = 2, stop = nchar(t1$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(t1$tab[[1,1]], "a") # var name
  expect_equal(t1$tab[[2,1]], "b") # var name
  expect_equal(t1$tab[[3,1]], "Difference") # var name
  expect_equal(as.numeric(t1$tab[[1, 2]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 2]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 2]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(t1$tab[[2, 3]]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(t1$tab[[3, 3]]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(t1$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 4]]), mean(b), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 4]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(t1$var1, a)
  expect_equal(t1$var2, b)
  expect_equal(t1$by, NA)
  expect_false(as.logical(t1$par[[1]])) # geom parameter
  expect_equal(as.numeric(t1$par[[2]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[3]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[4]])) # equal var
  expect_equal(as.numeric(t1$par[[5]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(t1$par[[6]])) # matched
  expect_equal(as.numeric(t1$par[[7]]), 3) # digits
})


### two-sample unpaired test, pooled variance

t1 <- ttest(a, b, matched = FALSE, var.eq = TRUE)
t2 <- t.test(a, b, paired = FALSE, var.equal = TRUE)

test_that("ttest() returns correct numbers for two-sample test,  pooled variance", {
  expect_s3_class(t1, "ttest")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 7]], start = 2, stop = nchar(t1$tab[[3, 7]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 7]], start = 2, stop = nchar(t1$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 7]], start = 2, stop = nchar(t1$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(t1$tab[[1,1]], "a") # var name
  expect_equal(t1$tab[[2,1]], "b") # var name
  expect_equal(t1$tab[[3,1]], "Difference") # var name
  expect_equal(as.numeric(t1$tab[[1, 2]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 2]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 2]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(t1$tab[[2, 3]]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(t1$tab[[3, 3]]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(t1$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 4]]), mean(b), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 4]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(t1$var1, a)
  expect_equal(t1$var2, b)
  expect_equal(t1$by, NA)
  expect_false(as.logical(t1$par[[1]])) # geom parameter
  expect_equal(as.numeric(t1$par[[2]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[3]], t2$alternative) # alternative
  expect_true(as.logical(t1$par[[4]])) # equal var
  expect_equal(as.numeric(t1$par[[5]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(t1$par[[6]])) # matched
  expect_equal(as.numeric(t1$par[[7]]), 3) # digits
})

### two-sample unpaired test, unpooled variance, using by 
e <- c(a,b)
groups <- c(rep(1, 50), rep(2, 50))
t1 <- ttest(e, by = groups, matched = FALSE)
t2 <- t.test(a, b, paired = FALSE)

test_that("ttest() returns correct numbers for two-sample test using by,  unpooled variance", {
  expect_s3_class(t1, "ttest")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 7]], start = 2, stop = nchar(t1$tab[[3, 7]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 7]], start = 2, stop = nchar(t1$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 7]], start = 2, stop = nchar(t1$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(t1$tab[[1,1]], "groups = 1") # var name
  expect_equal(t1$tab[[2,1]], "groups = 2") # var name
  expect_equal(t1$tab[[3,1]], "Difference") # var name
  expect_equal(as.numeric(t1$tab[[1, 2]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 2]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 2]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(t1$tab[[2, 3]]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(t1$tab[[3, 3]]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(t1$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 4]]), mean(b), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 4]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(t1$var1, a)
  expect_equal(t1$var2, b)
  expect_equal(t1$by, groups)
  expect_false(as.logical(t1$par[[1]])) # geom parameter
  expect_equal(as.numeric(t1$par[[2]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[3]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[4]])) # equal var
  expect_equal(as.numeric(t1$par[[5]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(t1$par[[6]])) # matched
  expect_equal(as.numeric(t1$par[[7]]), 3) # digits
})

### two-sample paired test, unpooled variance, using factor version of by
e <- c(a,b)
groups <- factor(c(rep("group 1", 50), rep("group 2", 50)))
t1 <- ttest(e, by = groups, matched = TRUE)
t2 <- t.test(a, b, paired = TRUE)

test_that("ttest() returns correct numbers for two-sample test using by,  unpooled variance", {
  expect_s3_class(t1, "ttest")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 7]], start = 2, stop = nchar(t1$tab[[3, 7]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 7]], start = 2, stop = nchar(t1$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 7]], start = 2, stop = nchar(t1$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(t1$tab[[1,1]], "groups = group 1") # var name
  expect_equal(t1$tab[[2,1]], "groups = group 2") # var name
  expect_equal(t1$tab[[3,1]], "Difference") # var name
  expect_equal(as.numeric(t1$tab[[1, 2]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 2]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 2]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(t1$tab[[2, 3]]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(t1$tab[[3, 3]]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(t1$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 4]]), mean(b), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 4]]), t2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(t1$var1, a)
  expect_equal(t1$var2, b)
  expect_equal(t1$by, c(rep(1, 50), rep(2, 50)))
  expect_false(as.logical(t1$par[[1]])) # geom parameter
  expect_equal(as.numeric(t1$par[[2]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[3]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[4]])) # equal var
  expect_equal(as.numeric(t1$par[[5]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(t1$par[[6]])) # matched
  expect_equal(as.numeric(t1$par[[7]]), 3) # digits
})

### NAs
a_na <- c(NA, a)
b_na <- c(b, NA)
a_matched <- a_na
a_matched[51] <- NA
b_matched <- b_na
b_matched[1] <- NA
b_short <- b[1:49]
by <- c(rep(1, 51), rep(2,49), NA, NA)
e <- c(a_na, b_na)

t1 <- ttest(a_na, b_na, matched = TRUE)
t2 <- t.test(a_na, b_na, paired = TRUE)
t3 <- ttest(a_na, b_na, matched = FALSE)
t4 <- t.test(a_na, b_na, paired = FALSE)
t5 <- ttest(e, by = by)
t6 <- t.test(a_na, b[-50])

test_that("ttest() counts NAs correctly", {
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 7]], start = 2, stop = nchar(t1$tab[[3, 7]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 7]], start = 2, stop = nchar(t1$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 7]], start = 2, stop = nchar(t1$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(t1$tab[[1,1]], "a_na") # var name
  expect_equal(t1$tab[[2,1]], "b_na") # var name
  expect_equal(t1$tab[[3,1]], "Difference") # var name
  expect_equal(as.numeric(t1$tab[[1, 2]]), length(a_na)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 2]]), length(b_na)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 2]]), length(a_na)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 3]]), sum(is.na(a_na))) # NAs
  expect_equal(as.numeric(t1$tab[[2, 3]]), sum(is.na(b_na))) # NAs
  expect_equal(as.numeric(t1$tab[[3, 3]]), sum(is.na(a_na)) + sum(is.na(b_na))) # NAs
  expect_equal(as.numeric(t1$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 4]]), mean(b), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 4]]), mean(a_matched - b_matched, na.rm=TRUE), tolerance = 1e-2) # estimate of mean
  expect_equal(t1$var1, a_matched)
  expect_equal(t1$var2, b_matched)
  expect_equal(t1$by, NA)
  
  expect_equal(t3$df, t4$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t3$tstat, t4$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t3$p, t4$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t3$tab[[3, 7]], start = 2, stop = nchar(t3$tab[[3, 7]])-1), ", ")[[1]]),
               t4$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t3$tab[[1, 7]], start = 2, stop = nchar(t3$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t3$tab[[2, 7]], start = 2, stop = nchar(t3$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))), 
               tolerance = 1e-3) # conf int
  expect_equal(t3$tab[[1,1]], "a_na") # var name
  expect_equal(t3$tab[[2,1]], "b_na") # var name
  expect_equal(t3$tab[[3,1]], "Difference") # var name
  expect_equal(as.numeric(t3$tab[[1, 2]]), length(a_na)) # n obs
  expect_equal(as.numeric(t3$tab[[2, 2]]), length(b_na)) # n obs
  expect_equal(as.numeric(t3$tab[[3, 2]]), length(a_na) + length(b_na)) # n obs
  expect_equal(as.numeric(t3$tab[[1, 3]]), sum(is.na(a_na))) # NAs
  expect_equal(as.numeric(t3$tab[[2, 3]]), sum(is.na(b_na))) # NAs
  expect_equal(as.numeric(t3$tab[[3, 3]]), sum(is.na(a_na)) + sum(is.na(b_na))) # NAs
  expect_equal(as.numeric(t3$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t3$tab[[2, 4]]), mean(b), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t3$tab[[3, 4]]), mean(a_na, na.rm = TRUE) - mean(b_na, na.rm=TRUE), 
               tolerance = 1e-2) # estimate of mean
  expect_equal(t3$var1, a_na)
  expect_equal(t3$var2, b_na)
  expect_equal(t3$by, NA)
  
  expect_equal(t5$df, t6$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t5$tstat, t6$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t5$p, t6$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t5$tab[[3, 7]], start = 2, stop = nchar(t5$tab[[3, 7]])-1), ", ")[[1]]),
               t6$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t5$tab[[1, 7]], start = 2, stop = nchar(t5$tab[[1, 7]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t5$tab[[2, 7]], start = 2, stop = nchar(t5$tab[[2, 7]])-1), ", ")[[1]]),
               c(mean(b_short) - qt(0.975, 48)*sd(b_short)/sqrt(49), 
                 mean(b_short) + qt(0.975, 48)*sd(b_short)/sqrt(49)), 
               tolerance = 1e-2) # conf int
  expect_equal(t5$tab[[1,1]], "by = 1") # var name
  expect_equal(t5$tab[[2,1]], "by = 2") # var name
  expect_equal(t5$tab[[3,1]], "Difference") # var name
  expect_equal(as.numeric(t5$tab[[1, 2]]), length(a_na)) # n obs
  expect_equal(as.numeric(t5$tab[[2, 2]]), length(b_short)) # n obs
  expect_equal(as.numeric(t5$tab[[3, 2]]), length(a_na) + length(b_na)-2) # n obs
  expect_equal(as.numeric(t5$tab[[1, 3]]), sum(is.na(a_na))) # NAs
  expect_equal(as.numeric(t5$tab[[2, 3]]), sum(is.na(b_short))) # NAs
  expect_equal(as.numeric(t5$tab[[3, 3]]), sum(is.na(a_na)) + sum(is.na(b_short))) # NAs
  expect_equal(as.numeric(t5$tab[[1, 4]]), mean(a), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t5$tab[[2, 4]]), mean(b_short), tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(t5$tab[[3, 4]]), mean(a_na, na.rm = TRUE) - mean(b_short), 
               tolerance = 1e-2) # estimate of mean
  expect_equal(t5$var1, a_na)
  expect_equal(t5$var2, b_short)
  expect_equal(t5$by, by[!is.na(by)])
})