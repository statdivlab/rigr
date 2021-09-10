### error handling

### error handling

test_that("proptesti() throws error for improper data types", {
  expect_error(proptesti(c(1,1), 10), "'x1' and 'n1' must be nonnegative integers")
  expect_error(proptesti(1, "10"), "'x1' and 'n1' must be nonnegative integers")
  expect_error(proptesti(-1, 10), "'x1' and 'n1' must be nonnegative integers")
  expect_error(proptesti(1, 10, c(1,1), 10), "'x2' and 'n2' must be nonnegative integers")
  expect_error(proptesti(1, 10, 1, "10"), "'x2' and 'n2' must be nonnegative integers")
  expect_error(proptesti(1, 10, -1, 10), "'x2' and 'n2' must be nonnegative integers")
  expect_error(proptesti(10, 1), "Number of trials must be at least as large as number of succeses.")
  expect_error(proptesti(1, 10, 10, 1), "Number of trials must be at least as large as number of succeses.")
  expect_error(proptesti(1, 10, 1), "A second number of trials must be entered for two sample test")
  expect_error(proptesti(1, 10, n2 = 1), "Number of successes for the second sample must be entered for two sample test")
})

x1 <- rbinom(100, 1, 0.5)
x2 <- rbinom(100, 1, 0.5)

test_that("proptesti() throws error if alternative is not 'two.sided', 'less', or 'greater", {
  expect_error(proptesti(sum(x1), length(x1), alternative = "blah"),
               "'alternative' must be either 'less', 'two.sided', or 'greater'")
})

test_that("proptesti() throws error if exact is not logical", {
  expect_error(proptesti(sum(x1), length(x1), exact = 2),
               "'exact' must be a logical.")
})

test_that("proptesti() throws error for invalid conf.level", {
  expect_error(proptesti(sum(x1), length(x1), conf.level = 1.1), "'conf.level' must a scalar between 0 and 1.")
  expect_error(proptesti(sum(x1), length(x1), conf.level = TRUE), "'conf.level' must a scalar between 0 and 1.")
})

test_that("proptesti() throws error for non-numeric more.digits argument", {
  expect_error(proptesti(sum(x1), length(x1), more.digits = TRUE),
               "Argument 'more.digits' must be numeric")
})

test_that("proptesti() throws error for non-scalar null", {
  expect_error(proptesti(sum(x1), length(x1), null.hypoth = c(0.5, 0.6)), "Null must be a scalar")
  expect_error(proptesti(sum(x1), length(x1), null.hypoth = TRUE), "Null must be a scalar")
})

test_that("proptesit() throws error for exact test on 2 samples", {
  expect_error(proptesti(sum(x1), length(x1), sum(x2), length(x2),
                        exact = TRUE), "Exact binomial test not available for two samples.")
})

### one-sample test, approximate 

set.seed(1)
a <- rbinom(100, 1, 0.5)

p1 <- proptesti(sum(a), length(a))
p2 <- prop.test(sum(a), length(a), correct = FALSE)

test_that("proptesti() returns correct numbers for one-sample test", {
  expect_s3_class(p1, "proptesti")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "var1") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[4]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_false(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})


p1 <- proptesti(sum(a), length(a), alternative = "less")
p2 <- prop.test(sum(a), length(a), correct = FALSE, alternative = "less")

test_that("proptesti() returns correct numbers for one-sample test, left-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "var1") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[4]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_false(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})


p1 <- proptesti(sum(a), length(a), alternative = "greater")
p2 <- prop.test(sum(a), length(a), correct = FALSE, alternative = "greater")

test_that("proptesti() returns correct numbers for one-sample test, right-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "var1") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[4]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_false(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), conf.level = 0.9)
p2 <- prop.test(sum(a), length(a), correct = FALSE, conf.level = 0.9)

test_that("proptesti() returns correct numbers for one-sample test, non 0.95-level", {
  expect_s3_class(p1, "proptesti")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "var1") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[4]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_false(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

### one-sample test, exact

p1 <- proptesti(sum(a), length(a), exact = TRUE)
p2 <- binom.test(sum(a), length(a))

test_that("proptesti() returns correct numbers for one-sample test, exact", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "var1") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[4]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_false(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), exact = TRUE, alternative = "less")
p2 <- binom.test(sum(a), length(a), alternative = "less")

test_that("proptesti() returns correct numbers for one-sample test, exact, left-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "var1") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[4]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_false(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), exact = TRUE, alternative = "greater")
p2 <- binom.test(sum(a), length(a), alternative = "greater")

test_that("proptesti() returns correct numbers for one-sample test, exact, right-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "var1") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[4]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_false(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), exact = TRUE, conf.level = 0.9)
p2 <- binom.test(sum(a), length(a), conf.level = 0.9)

test_that("proptesti() returns correct numbers for one-sample test, exact, non-0.95 conf", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "var1") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[4]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_false(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

### two-sample test, approximate 

b <- rbinom(100, 1, 0.5)

p1 <- proptesti(sum(a), length(a), sum(b), length(b))
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE)

test_that("proptesti() returns correct numbers for two-sample test", {
  expect_s3_class(p1, "proptesti")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,5], start = 2, stop = nchar(p1$tab[1,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,5], start = 2, stop = nchar(p1$tab[2,5])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,5], start = 2, stop = nchar(p1$tab[3,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "var1") # var name
  expect_equal(p1$tab[2,1], "var2") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,3]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,3]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,4]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_true(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), sum(b), length(b), alternative = "less")
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, alternative = "less")

test_that("proptesti() returns correct numbers for two-sample test, left-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,5], start = 2, stop = nchar(p1$tab[1,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,5], start = 2, stop = nchar(p1$tab[2,5])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,5], start = 2, stop = nchar(p1$tab[3,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "var1") # var name
  expect_equal(p1$tab[2,1], "var2") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,3]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,3]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,4]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_true(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), sum(b), length(b), alternative = "greater")
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, alternative = "greater")

test_that("proptesti() returns correct numbers for two-sample test, right-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,5], start = 2, stop = nchar(p1$tab[1,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,5], start = 2, stop = nchar(p1$tab[2,5])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,5], start = 2, stop = nchar(p1$tab[3,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "var1") # var name
  expect_equal(p1$tab[2,1], "var2") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,3]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,3]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,4]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_true(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), sum(b), length(b), conf.level = 0.9)
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, conf.level = 0.9)

test_that("proptesti() returns correct numbers for two-sample test, right-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,5], start = 2, stop = nchar(p1$tab[1,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,5], start = 2, stop = nchar(p1$tab[2,5])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - qnorm(0.95)*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + qnorm(0.95)*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,5], start = 2, stop = nchar(p1$tab[3,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - qnorm(0.95)*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + qnorm(0.95)*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "var1") # var name
  expect_equal(p1$tab[2,1], "var2") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,3]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,3]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,4]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_true(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[7]]), 3) # digits
})

# p1 <- proptesti(sum(a), length(a), sum(b), length(b), null.hypoth = 0.6)
# p2 <- proptest(a,b, null.hypoth = 0.6)
# test_that("proptesti() returns correct numbers for two-sample test, nonzero null", {
#   expect_equal(p1$zstat, p1$zstat, tolerance = 1e-3) # test statistic
#   expect_equal(p1$pval, p2$pval, tolerance = 1e-3) # p-value
#   expect_equal(as.numeric(p1$par[[1]]), 0.6) # null 
# 
# })

### continuity correction
p1 <- proptesti(16, 20, null.hypoth = 0.6, correct = TRUE, alternative = "two.sided")
p2 <- prop.test(16, 20, correct = TRUE, p = 0.6, alternative = "two.sided")

test_that("proptesti() continuity correction works, one sample, two-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptesti(16, 20, null.hypoth = 0.6, correct = TRUE, alternative = "less")
p2 <- prop.test(16, 20, correct = TRUE, p = 0.6, alternative = "less")

test_that("proptesti() continuity correction works, one sample, left-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptesti(16, 20, null.hypoth = 0.6, correct = TRUE, alternative = "greater")
p2 <- prop.test(16, 20, correct = TRUE, p = 0.6, alternative = "greater")

test_that("proptesti() continuity correction works, one sample, right-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptesti(160, 200, 150, 200, correct = TRUE, alternative = "two.sided")
p2 <- prop.test(c(160, 150), c(200, 200), correct = TRUE, alternative = "two.sided")

test_that("proptesti() continuity correction works, one sample, two-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptesti(160, 200, 150, 200, correct = TRUE, alternative = "less")
p2 <- prop.test(c(160, 150), c(200, 200), correct = TRUE, alternative = "less")

test_that("proptesti() continuity correction works, one sample, left-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptesti(160, 200, 150, 200, correct = TRUE, alternative = "greater")
p2 <- prop.test(c(160, 150), c(200, 200), correct = TRUE, alternative = "greater")

test_that("proptesti() continuity correction works, one sample, right-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})


