### error handling


### one-sample test (or two-sample paired test)

set.seed(1)
a <- rnorm(50)

t1 <- ttesti(length(a), mean(a), sd(a))
t2 <- t.test(a)

test_that("ttesti() returns correct numbers for one-sample test", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[5]]), 3) # digits
  expect_false(t1$twosamp)
})

t1 <- ttesti(length(a), mean(a), sd(a), null.hypoth = 1)
t2 <- t.test(a, mu = 1)

test_that("ttesti() returns correct numbers for one-sample test, non-0 null", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[5]]), 3) # digits
  expect_false(t1$twosamp)
})

t1 <- ttesti(length(a), mean(a), sd(a), alternative = "less")
t2 <- t.test(a, alternative = "less")
t3 <- t.test(a)

test_that("ttesti() returns correct numbers for one-sample test, left-sided", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t3$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[5]]), 3) # digits
  expect_false(t1$twosamp)
})

t1 <- ttesti(length(a), mean(a), sd(a), alternative = "greater")
t2 <- t.test(a, alternative = "greater")

test_that("ttesti() returns correct numbers for one-sample test, right-sided", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t3$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[5]]), 3) # digits
  expect_false(t1$twosamp)
})

t1 <- ttesti(length(a), mean(a), sd(a), conf.level = 0.9, more.digits = 1)
t2 <- t.test(a, conf.level =  0.9)

test_that("ttesti() returns correct numbers for one-sample test, different conf.level", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]]) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[5]], start = 2, stop = nchar(t1$tab[[5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$tab[[1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2]]), t2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3]]), t2$stderr[[1]], tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[4]]), sd(a), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null 
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[5]]), 4) # digits
  expect_false(t1$twosamp)
})

### two-sample unpaired test, unpooled variance

b <- rnorm(50)

t1 <- ttesti(length(a), mean(a), sd(a), length(b), mean(b), sd(b))
t2 <- t.test(a, b)

test_that("ttest() returns correct numbers for two-sample test,  unpooled variance", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 5]], start = 2, stop = nchar(t1$tab[[3, 5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 5]], start = 2, stop = nchar(t1$tab[[1, 5]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))),
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 5]], start = 2, stop = nchar(t1$tab[[2, 5]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))),
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$tab[[1, 1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 1]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 1]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 2]]), mean(a), tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 2]]), mean(b), tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 2]]), t2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[1, 3]]), sd(a)/sqrt(length(a)), tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[2, 3]]), sd(b)/sqrt(length(b)), tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[3, 3]]), sqrt(sd(a)^2/length(a) + sd(b)^2/length(b)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[1, 4]]), sd(a), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$tab[[2, 4]]), sd(b), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_false(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[5]]), 3) # digits
})

### two-sample unpaired test, pooled variance

t1 <- ttesti(length(a), mean(a), sd(a), length(b), mean(b), sd(b), var.eq = TRUE)
t2 <- t.test(a, b, var.equal = TRUE)

test_that("ttest() returns correct numbers for two-sample test,  unpooled variance", {
  expect_s3_class(t1, "ttesti")
  expect_equal(t1$df, t2$parameter[[1]], tolerance = 1e-3) # df
  expect_equal(t1$tstat, t2$statistic[[1]], tolerance = 1e-3) # test statistic
  expect_equal(t1$p, t2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(t1$tab[[3, 5]], start = 2, stop = nchar(t1$tab[[3, 5]])-1), ", ")[[1]]),
               t2$conf.int[1:2], tolerance = 5e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[1, 5]], start = 2, stop = nchar(t1$tab[[1, 5]])-1), ", ")[[1]]),
               c(mean(a) - qt(0.975, 49)*sd(a)/sqrt(length(a)), mean(a) + qt(0.975, 49)*sd(a)/sqrt(length(a))),
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(t1$tab[[2, 5]], start = 2, stop = nchar(t1$tab[[2, 5]])-1), ", ")[[1]]),
               c(mean(b) - qt(0.975, 49)*sd(b)/sqrt(length(b)), mean(b) + qt(0.975, 49)*sd(b)/sqrt(length(b))),
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(t1$tab[[1, 1]]), length(a)) # n obs
  expect_equal(as.numeric(t1$tab[[2, 1]]), length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[3, 1]]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(t1$tab[[1, 2]]), mean(a), tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[2, 2]]), mean(b), tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[3, 2]]), t2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(t1$tab[[1, 3]]), sd(a)/sqrt(length(a)), tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[2, 3]]), sd(b)/sqrt(length(b)), tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[3, 3]]), 
               sqrt((sd(a)^2*(length(a)-1)+sd(b)^2*(length(b)-1))/(length(a)+length(b)-2))*sqrt(1/length(a)+1/length(b)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(t1$tab[[1, 4]]), sd(a), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$tab[[2, 4]]), sd(b), tolerance = 3) # std dev of data
  expect_equal(as.numeric(t1$par[[1]]), t2$null.value[[1]]) # null
  expect_equal(t1$par[[2]], t2$alternative) # alternative
  expect_true(as.logical(t1$par[[3]])) # equal var
  expect_equal(as.numeric(t1$par[[4]]), attr(t2$conf.int, "conf.level")) # conf level
  expect_equal(as.numeric(t1$par[[5]]), 3) # digits
})