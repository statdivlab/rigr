### error handling

x1 <- rnorm(100)
x2 <- rnorm(100)

test_that("ttesti() throws error if alternative is not 'two.sided', 'less', or 'greater", {
  expect_error(ttesti(length(x1), mean(x1), sd(x1), 
                      length(x2), mean(x2), sd(x2), alternative = "blah"), 
               "'alternative' must be either 'less', 'two.sided', or 'greater'")
})

test_that("ttesti() throws error if var.eq is not logical", {
  expect_error(ttesti(length(x1), mean(x1), sd(x1), 
                      length(x2), mean(x2), sd(x2), var.eq = 2), 
               "Please specify a logical value for variable 'var.eq'")
})

test_that("ttesti() throws error for non-numeric more.digits argument", {
  expect_error(ttesti(length(x1), mean(x1), sd(x1), more.digits = TRUE), 
               "Argument 'more.digits' must be numeric")
})

test_that("ttesti() throws error for invalid conf.level", {
  expect_error(ttesti(length(x1), mean(x1), sd(x1), conf.level = 1.1),
               "'conf.level' must be a single number between 0 and 1")
  expect_error(ttesti(length(x1), mean(x1), sd(x1), conf.level = "blah"),
               "'conf.level' must be a single number between 0 and 1")
  expect_error(ttesti(length(x1), mean(x1), sd(x1), conf.level = TRUE),
               "'conf.level' must be a single number between 0 and 1")
})

test_that("ttesti() throws error if all three arguments for var2 aren't given", {
  expect_error(ttesti(length(x1), mean(x1), sd(x1), length(x2), mean(x2)),
               "SD and mean for the second sample must be entered for two sample test")
  expect_error(ttesti(length(x1), mean(x1), sd(x1), mean2 = mean(x2), sd2 = sd(x2)),
               "A second number of observations must be entered for two sample test")
})

test_that("ttesti() throws error for non-scalar null", {
  expect_error(ttesti(length(x1), mean(x1), sd(x1), null.hypoth = c(1,2)),
               "Null must be a scalar.")
  expect_error(ttesti(length(x1), mean(x1), sd(x1), null.hypoth = "1"),
               "Null must be a scalar.")
})

test_that("ttesti() throws error if summary stats are not of correct type", {
  expect_error(ttesti(10.1, mean(x1), sd(x1)),
               "Number of observations must be a positive integer.")
  expect_error(ttesti(length(x1), mean(x1), sd(x1), 10.1, mean(x2), sd(x2)),
               "Number of observations must be a positive integer.")
  expect_error(ttesti(length(x1), "10", sd(x1)),
               "Mean must be scalar.")
  expect_error(ttesti(length(x1), mean(x1), sd(x1), length(x2), c(1,2), sd(x2)),
               "'obs', 'mean', and 'sd' must be variables of length 1.")
  expect_error(ttesti(length(x1), mean(x1), "1"),
               "SD must be scalar.")
  expect_error(ttesti(length(x1), mean(x1), sd(x1), length(x2), mean(x2), c(1,2)),
               "'obs', 'mean', and 'sd' must be variables of length 1.")
})

# test_that("ttest() throws error for non-numeric data", {
#   expect_error(ttest(c("a", "B", "c")), 
#                "Cannot perform t-test on non-numeric data")
# })





### one-sample test (or two-sample paired test)

set.seed(1)
a <- rnorm(50)

t1 <- ttesti(length(a), mean(a), sd(a))
t2 <- t.test(a)

test_that("ttesti() returns correct numbers for one-sample test", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_false(as.logical(t1$par[[5]]))
})

t1 <- ttesti(length(a), mean(a), sd(a), null.hypoth = 1)
t2 <- t.test(a, mu = 1)

test_that("ttesti() returns correct numbers for one-sample test, non-0 null", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_false(as.logical(t1$par[[5]]))
})

t1 <- ttesti(length(a), mean(a), sd(a), alternative = "less")
t2 <- t.test(a, alternative = "less")
t3 <- t.test(a)

test_that("ttesti() returns correct numbers for one-sample test, left-sided", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t3$conf.int[1:2], tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_false(as.logical(t1$par[[5]]))
})

t1 <- ttesti(length(a), mean(a), sd(a), alternative = "greater")
t2 <- t.test(a, alternative = "greater")

test_that("ttesti() returns correct numbers for one-sample test, right-sided", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t3$conf.int[1:2], tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_false(as.logical(t1$par[[5]]))
})

t1 <- ttesti(length(a), mean(a), sd(a), conf.level = 0.9, more.digits = 1)
t2 <- t.test(a, conf.level =  0.9)

test_that("ttesti() returns correct numbers for one-sample test, different conf.level", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 4) # digits
  expect_false(as.logical(t1$par[[5]]))
})

### two-sample unpaired test, unpooled variance

b <- rnorm(50)

t1 <- ttesti(length(a), mean(a), sd(a), length(b), mean(b), sd(b))
t2 <- t.test(a, b)

test_that("ttest() returns correct numbers for two-sample test, unpooled variance", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-2) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 5]], start = 2, stop = nchar(t1$tab[[3, 5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 5]], start = 2, stop = nchar(t1$tab[[1, 5]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 5]], start = 2, stop = nchar(t1$tab[[2, 5]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1, 1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 1]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 1]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 2]]), mean(a), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 2]]), mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 2]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[1, 3]]), sd(a)/sqrt(length(a)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[2, 3]]), sd(b)/sqrt(length(b)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[3, 3]]), sqrt(sd(a)^2/length(a) + sd(b)^2/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[1, 4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$tab[[2, 4]]), sd(b), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_true(as.logical(t1$par[[5]]))
})

t1 <- ttesti(length(a), mean(a), sd(a), length(b), mean(b), sd(b), alternative = "less")
t2 <- t.test(a, b, alternative = "less")
t3 <- t.test(a, b)

test_that("ttest() returns correct numbers for two-sample test, unpooled variance, left-sided alt", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-2) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 5]], start = 2, stop = nchar(t1$tab[[3, 5]])-1), ", ")[[1]]),
               t3$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 5]], start = 2, stop = nchar(t1$tab[[1, 5]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 5]], start = 2, stop = nchar(t1$tab[[2, 5]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1, 1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 1]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 1]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 2]]), mean(a), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 2]]), mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 2]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[1, 3]]), sd(a)/sqrt(length(a)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[2, 3]]), sd(b)/sqrt(length(b)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[3, 3]]), sqrt(sd(a)^2/length(a) + sd(b)^2/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[1, 4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$tab[[2, 4]]), sd(b), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_true(as.logical(t1$par[[5]]))
})

t1 <- ttesti(length(a), mean(a), sd(a), length(b), mean(b), sd(b), alternative = "greater")
t2 <- t.test(a, b, alternative = "greater")
t3 <- t.test(a, b)

test_that("ttest() returns correct numbers for two-sample test, unpooled variance, right-sided alt", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-2) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 5]], start = 2, stop = nchar(t1$tab[[3, 5]])-1), ", ")[[1]]),
               t3$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 5]], start = 2, stop = nchar(t1$tab[[1, 5]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 5]], start = 2, stop = nchar(t1$tab[[2, 5]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1, 1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 1]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 1]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 2]]), mean(a), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 2]]), mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 2]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[1, 3]]), sd(a)/sqrt(length(a)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[2, 3]]), sd(b)/sqrt(length(b)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[3, 3]]), sqrt(sd(a)^2/length(a) + sd(b)^2/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[1, 4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$tab[[2, 4]]), sd(b), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_true(as.logical(t1$par[[5]]))
})

### two-sample unpaired test, pooled variance

t1 <- ttesti(length(a), mean(a), sd(a), length(b), mean(b), sd(b), var.eq = TRUE)
t2 <- t.test(a, b, var.equal = TRUE)

test_that("ttest() returns correct numbers for two-sample test, pooled variance", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-2) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 5]], start = 2, stop = nchar(t1$tab[[3, 5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 5]], start = 2, stop = nchar(t1$tab[[1, 5]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 5]], start = 2, stop = nchar(t1$tab[[2, 5]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1, 1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 1]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 1]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 2]]), mean(a), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 2]]), mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 2]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[1, 3]]), sd(a)/sqrt(length(a)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[2, 3]]), sd(b)/sqrt(length(b)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[3, 3]]), 
               sqrt((sd(a)^2*(length(a)-1)+sd(b)^2*(length(b)-1))/(length(a)+length(b)-2))*sqrt(1/length(a)+1/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[1, 4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$tab[[2, 4]]), sd(b), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_true(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_true(as.logical(t1$par[[5]]))
})

t1 <- ttesti(length(a), mean(a), sd(a), length(b), mean(b), sd(b), var.eq = TRUE, alternative = "less")
t2 <- t.test(a, b, var.equal = TRUE, alternative = "less")
t3 <- t.test(a, b, var.equal = TRUE)

test_that("ttest() returns correct numbers for two-sample test, pooled variance, left-sided alt", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-2) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 5]], start = 2, stop = nchar(t1$tab[[3, 5]])-1), ", ")[[1]]),
               t3$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 5]], start = 2, stop = nchar(t1$tab[[1, 5]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 5]], start = 2, stop = nchar(t1$tab[[2, 5]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1, 1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 1]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 1]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 2]]), mean(a), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 2]]), mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 2]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[1, 3]]), sd(a)/sqrt(length(a)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[2, 3]]), sd(b)/sqrt(length(b)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[3, 3]]), 
               sqrt((sd(a)^2*(length(a)-1)+sd(b)^2*(length(b)-1))/(length(a)+length(b)-2))*sqrt(1/length(a)+1/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[1, 4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$tab[[2, 4]]), sd(b), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_true(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_true(as.logical(t1$par[[5]]))
})

t1 <- ttesti(length(a), mean(a), sd(a), length(b), mean(b), sd(b), var.eq = TRUE, alternative = "greater")
t2 <- t.test(a, b, var.equal = TRUE, alternative = "greater")
t3 <- t.test(a, b, var.equal = TRUE)

test_that("ttest() returns correct numbers for two-sample test, pooled variance, right-sided alt", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-2) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-2) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 5]], start = 2, stop = nchar(t1$tab[[3, 5]])-1), ", ")[[1]]),
               t3$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 5]], start = 2, stop = nchar(t1$tab[[1, 5]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 5]], start = 2, stop = nchar(t1$tab[[2, 5]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(t1$tab[[1, 1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 1]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 1]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 2]]), mean(a), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 2]]), mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 2]]), mean(a) - mean(b), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(t1$tab[[1, 3]]), sd(a)/sqrt(length(a)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[2, 3]]), sd(b)/sqrt(length(b)), tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[3, 3]]), 
               sqrt((sd(a)^2*(length(a)-1)+sd(b)^2*(length(b)-1))/(length(a)+length(b)-2))*sqrt(1/length(a)+1/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[1, 4]]), sd(a), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$tab[[2, 4]]), sd(b), tolerance = 1e-2) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_true(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[6]]), 3) # digits
  expect_true(as.logical(t1$par[[5]]))
})