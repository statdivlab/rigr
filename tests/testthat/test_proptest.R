### error handling

test_that("proptest() throws error for non-binary data", {
  expect_error(proptest(c(-1, 0, 1)),
               "Only binary 0-1 data, two-level factors, and logicals are allowed.")
  expect_error(proptest(c(1,1,2,2)),
               "Only binary 0-1 data, two-level factors, and logicals are allowed.")
  expect_error(proptest(c("0", "0", "1", "1")),
               "Only binary 0-1 data, two-level factors, and logicals are allowed.")
  expect_error(proptest(as.factor(c("0", "0", "1", "1", "2"))),
               "Only binary 0-1 data, two-level factors, and logicals are allowed.")
  expect_error(proptest(as.factor(c("0", "0", "1", "1")), as.factor(c("0", "0", "2", "2"))),
               "Only binary 0-1 data, two-level factors, and logicals are allowed.")
})

x1 <- rbinom(100, 1, 0.5)
x2 <- rbinom(100, 1, 0.5)

test_that("proptest() throws error if alternative is not 'two.sided', 'less', or 'greater", {
  expect_error(proptest(x1, x2, alternative = "blah"),
               "'alternative' must be either 'less', 'two.sided', or 'greater'")
})

test_that("proptest() throws error if two variables and by also given", {
  expect_error(proptest(x1, x2, by = rep(1, 100)),
               "Please specify only one of the variables 'by' or 'var2'")
})

test_that("proptest() throws error if exact is not logical", {
  expect_error(proptest(x1, exact = 2),
               "'exact' must be a logical.")
})

test_that("proptest() throws error for invalid conf.level", {
  expect_error(proptest(x1, conf.level = 1.1), "'conf.level' must a scalar between 0 and 1.")
  expect_error(proptest(x1, conf.level = TRUE), "'conf.level' must a scalar between 0 and 1.")
})

test_that("proptest() throws error for non-numeric more.digits argument", {
  expect_error(proptest(x1, more.digits = TRUE),
               "Argument 'more.digits' must be numeric")
})

test_that("proptest() throws error if by argument contains only one value", {
  expect_error(proptest(x1, by = rep(1, 100)),
               "Variable 'by' only has one unique value")
})

test_that("proptest() throws error if by argument contains >2 unique values", {
  expect_error(proptest(x1, by = c(rep(1, 50), rep(2, 49), 3)),
               "Variable 'by' has more than two unique values.")
})

test_that("proptest() throws error if by argument is not of same length as data", {
  expect_error(proptest(x1, by = c(rep(1, 50), rep(2, 51))),
               "Variable 'by' is not of equal length to data vector")
  expect_error(proptest(x1, by = c(rep(1, 50), rep(2, 50), NA)), 
               "Variable 'by' is not of equal length to data vector")
})

test_that("proptest() throws error for non-scalar null", {
  expect_error(proptest(x1, null.hypoth = c(0.5, 0.6)), "Null must be a scalar")
  expect_error(proptest(x1, null.hypoth = TRUE), "Null must be a scalar")
})

test_that("proptest() throws error for exact test on 2 samples", {
  expect_error(proptest(x1, x2, exact = TRUE), "Exact binomial test not available for two samples.")
})

### one-sample test, approximate 

set.seed(1)
a <- rbinom(100, 1, 0.5)

p1 <- proptest(a)
p2 <- prop.test(sum(a), length(a), correct = FALSE)

test_that("proptest() returns correct numbers for one-sample test", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, null.hypoth = 0.6)
p2 <- prop.test(sum(a), length(a), correct = FALSE, p = 0.6)

test_that("proptest() returns correct numbers for one-sample test, non-0.5 null", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, alternative = "less")
p2 <- prop.test(sum(a), length(a), correct = FALSE, alternative = "less")

test_that("proptest() returns correct numbers for one-sample test, left-sided", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, alternative = "greater")
p2 <- prop.test(sum(a), length(a), correct = FALSE, alternative = "greater")

test_that("proptest() returns correct numbers for one-sample test, right-sided", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, conf.level = 0.9)
p2 <- prop.test(sum(a), length(a), correct = FALSE, conf.level = 0.9)

test_that("proptest() returns correct numbers for one-sample test, non-0.95 level", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - qnorm(.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + qnorm(.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

### one-sample test, exact

p1 <- proptest(a, exact = TRUE)
p2 <- binom.test(sum(a), length(a))

test_that("proptest() returns correct numbers for one-sample exact test", {
  expect_s3_class(p1, "proptest")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, exact = TRUE, null.hypoth = 0.6)
p2 <- binom.test(sum(a), length(a), p = 0.6)

test_that("proptest() returns correct numbers for one-sample exact test, non-0.5 null", {
  expect_s3_class(p1, "proptest")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, exact = TRUE, alternative = "less")
p2 <- binom.test(sum(a), length(a), alternative = "less")

test_that("proptest() returns correct numbers for one-sample exact test, left-sided", {
  expect_s3_class(p1, "proptest")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, exact = TRUE, alternative = "greater")
p2 <- binom.test(sum(a), length(a), alternative = "greater")

test_that("proptest() returns correct numbers for one-sample exact test, right-sided", {
  expect_s3_class(p1, "proptest")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, exact = TRUE, conf.level = 0.9)
p2 <- binom.test(sum(a), length(a), conf.level =0.9)

test_that("proptest() returns correct numbers for one-sample exact test, non-0.95 conf", {
  expect_s3_class(p1, "proptest")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_true(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})


### two-sample test, approximate 

b <- rbinom(100, 1, 0.5)

p1 <- proptest(a, b)
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE)

test_that("proptest() returns correct numbers for two-sample test", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,6], start = 2, stop = nchar(p1$tab[1,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,6], start = 2, stop = nchar(p1$tab[2,6])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,6], start = 2, stop = nchar(p1$tab[3,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "a") # var name
  expect_equal(p1$tab[2,1], "b") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[2,3]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[3,3]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[1,4]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,4]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,4]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,5]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_equal(p1$var2, b)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, b, alternative = "less")
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, alternative = "less")

test_that("proptest() returns correct numbers for two-sample test, left-sided", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,6], start = 2, stop = nchar(p1$tab[1,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,6], start = 2, stop = nchar(p1$tab[2,6])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,6], start = 2, stop = nchar(p1$tab[3,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "a") # var name
  expect_equal(p1$tab[2,1], "b") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[2,3]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[3,3]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[1,4]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,4]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,4]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,5]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_equal(p1$var2, b)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, b, alternative = "greater")
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, alternative = "greater")

test_that("proptest() returns correct numbers for two-sample test, right-sided", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,6], start = 2, stop = nchar(p1$tab[1,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,6], start = 2, stop = nchar(p1$tab[2,6])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,6], start = 2, stop = nchar(p1$tab[3,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "a") # var name
  expect_equal(p1$tab[2,1], "b") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[2,3]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[3,3]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[1,4]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,4]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,4]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,5]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_equal(p1$var2, b)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a, b, conf.level = 0.9)
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, conf.level = 0.9)

test_that("proptest() returns correct numbers for two-sample test, right-sided", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,6], start = 2, stop = nchar(p1$tab[1,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,6], start = 2, stop = nchar(p1$tab[2,6])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - qnorm(0.95)*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + qnorm(0.95)*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,6], start = 2, stop = nchar(p1$tab[3,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - qnorm(0.95)*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + qnorm(0.95)*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "a") # var name
  expect_equal(p1$tab[2,1], "b") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[2,3]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[3,3]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[1,4]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,4]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,4]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,5]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_equal(p1$var2, b)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

### two-sample test, using by 
e <- c(a,b)
groups <- c(rep(1, 100), rep(2, 100))
p1 <- proptest(e, by = groups)
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE)

test_that("proptest() returns correct numbers for two-sample test, using by", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,6], start = 2, stop = nchar(p1$tab[1,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,6], start = 2, stop = nchar(p1$tab[2,6])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,6], start = 2, stop = nchar(p1$tab[3,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "groups = 1") # var name
  expect_equal(p1$tab[2,1], "groups = 2") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[2,3]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[3,3]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[1,4]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,4]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,4]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,5]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_equal(p1$var2, b)
  expect_equal(p1$by, groups)
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

groups <- factor(c(rep("group 1", 100), rep("group 2", 100)))
p1 <- proptest(e, by = groups)
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE)

test_that("proptest() returns correct numbers for two-sample test, using by", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,6], start = 2, stop = nchar(p1$tab[1,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,6], start = 2, stop = nchar(p1$tab[2,6])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,6], start = 2, stop = nchar(p1$tab[3,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "groups = group 1") # var name
  expect_equal(p1$tab[2,1], "groups = group 2") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[2,3]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[3,3]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[1,4]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,4]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,4]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,5]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_equal(p1$var2, b)
  expect_equal(p1$by, c(rep(1, 100), rep(2, 100)))
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

### factor data
a_fac <- as.factor(ifelse(a == 0, "no", "yes"))
b_fac <- as.factor(ifelse(b == 0, "no", "yes"))

p1 <- proptest(a_fac)
p2 <- prop.test(sum(a), length(a), correct = FALSE)

test_that("proptest() returns correct numbers for one-sample test with factor", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[6]], start = 2, stop = nchar(p1$tab[[6]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[[1]], "a_fac") # var name
  expect_equal(as.numeric(p1$tab[[2]]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[[3]]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[[4]]), p2$estimate[[1]], tolerance = 3) # estimate of mean
  expect_equal(as.numeric(p1$tab[[5]]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 3) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_null(p1$var2)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), p2$null.value[[1]]) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptest(a_fac, b_fac)
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE)

test_that("proptest() returns correct numbers for two-sample test, factor", {
  expect_s3_class(p1, "proptest")
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,6], start = 2, stop = nchar(p1$tab[1,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,6], start = 2, stop = nchar(p1$tab[2,6])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,6], start = 2, stop = nchar(p1$tab[3,6])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - p2$estimate[[2]] - 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[1]] - p2$estimate[[2]] + 1.96*
                   sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "a_fac") # var name
  expect_equal(p1$tab[2,1], "b_fac") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a) + length(b)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), sum(is.na(a))) # NAs
  expect_equal(as.numeric(p1$tab[2,3]), sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[3,3]), sum(is.na(a)) + sum(is.na(b))) # NAs
  expect_equal(as.numeric(p1$tab[1,4]), p2$estimate[[1]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,4]), p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,4]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,5]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,5]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a) 
  expect_equal(p1$var2, b)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

### continuity correction
p1 <- proptest(a, null.hypoth = 0.6, correct = TRUE, alternative = "two.sided")
p2 <- prop.test(sum(a), length(a), correct = TRUE, p = 0.6, alternative = "two.sided")

test_that("proptesti() continuity correction works, one sample, two-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptest(a, null.hypoth = 0.6, correct = TRUE, alternative = "less")
p2 <- prop.test(sum(a), length(a), correct = TRUE, p = 0.6, alternative = "less")

test_that("proptesti() continuity correction works, one sample, left-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptest(a, null.hypoth = 0.6, correct = TRUE, alternative = "greater")
p2 <- prop.test(sum(a), length(a), correct = TRUE, p = 0.6, alternative = "greater")

test_that("proptesti() continuity correction works, one sample, right-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptest(a,b, correct = TRUE, alternative = "two.sided")
p2 <- prop.test(c(sum(a),sum(b)), c(length(a), length(b)), correct = TRUE, alternative = "two.sided")

test_that("proptesti() continuity correction works, one sample, two-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptest(a,b, correct = TRUE, alternative = "less")
p2 <- prop.test(c(sum(a),sum(b)), c(length(a), length(b)), correct = TRUE, alternative = "less")

test_that("proptesti() continuity correction works, one sample, left-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

p1 <- proptest(a,b, correct = TRUE, alternative = "greater")
p2 <- prop.test(c(sum(a),sum(b)), c(length(a), length(b)), correct = TRUE, alternative = "greater")

test_that("proptesti() continuity correction works, one sample, right-sided", {
  expect_equal(abs(p1$zstat), sqrt(p2$statistic[[1]]), tolerance = 1e-2)
  expect_equal(abs(p1$pval), p2$p.value, tolerance = 1e-2)
})

### NAs
a_na <- c(NA, a)
b_na <- c(b, NA)
a_short <- a_na[2:101]
b_short <- b_na[1:100]
by <- c(rep(1, 101), rep(2,99), NA, NA)
e <- c(a_na, b_na)

p1 <- proptest(a_na, b_na)
p2 <- proptest(a_short, b_short)
p3 <- proptest(e, by = by)
p4 <- proptest(a_short, b_short[-100])

test_that("ttest() counts NAs correctly", {
  expect_s3_class(p1, "proptest")
  expect_equal(p1$zstat, p2$zstat, tolerance = 1e-2) # test statistic
  expect_equal(p1$pval, p2$pval, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,6], start = 2, stop = nchar(p1$tab[1,6])-1), ", ")[[1]]),
               as.numeric(strsplit(substr(p2$tab[1,6], start = 2, stop = nchar(p2$tab[1,6])-1), ", ")[[1]]),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,6], start = 2, stop = nchar(p1$tab[2,6])-1), ", ")[[1]]),
               as.numeric(strsplit(substr(p2$tab[2,6], start = 2, stop = nchar(p2$tab[2,6])-1), ", ")[[1]]),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[3,6], start = 2, stop = nchar(p1$tab[3,6])-1), ", ")[[1]]),
               as.numeric(strsplit(substr(p2$tab[3,6], start = 2, stop = nchar(p2$tab[3,6])-1), ", ")[[1]]),
               tolerance = 1e-2) # conf int
  expect_equal(p1$tab[1,1], "a_na") # var name
  expect_equal(p1$tab[2,1], "b_na") # var name
  expect_equal(p1$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p1$tab[1,2]), length(a_na)) # n obs
  expect_equal(as.numeric(p1$tab[2,2]), length(b_na)) # n obs
  expect_equal(as.numeric(p1$tab[3,2]), length(a_na) + length(b_na)) # n obs
  expect_equal(as.numeric(p1$tab[1,3]), sum(is.na(a_na))) # NAs
  expect_equal(as.numeric(p1$tab[2,3]), sum(is.na(b_na))) # NAs
  expect_equal(as.numeric(p1$tab[3,3]), sum(is.na(a_na)) + sum(is.na(b_na))) # NAs
  expect_equal(as.numeric(p1$tab[1,4]), as.numeric(p2$tab[1,4]), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,4]), as.numeric(p2$tab[2,4]), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,4]), as.numeric(p2$tab[3,4]), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,5]), as.numeric(p2$tab[1,5]), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,5]), as.numeric(p2$tab[2,5]), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,5]), as.numeric(p2$tab[3,5]), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p1$var1, a_short) 
  expect_equal(p1$var2, b_short)
  expect_null(p1$by)
  expect_equal(as.numeric(p1$par[[1]]), 0) # null 
  expect_equal(p1$par[[2]], p1$par[[2]]) # alternative
  expect_equal(as.numeric(p1$par[[3]]), as.numeric(p2$par[[3]])) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
  
  expect_s3_class(p3, "proptest")
  expect_equal(p3$zstat, p4$zstat, tolerance = 1e-2) # test statistic
  expect_equal(p3$pval, p4$pval, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p3$tab[1,6], start = 2, stop = nchar(p3$tab[1,6])-1), ", ")[[1]]),
               as.numeric(strsplit(substr(p4$tab[1,6], start = 2, stop = nchar(p4$tab[1,6])-1), ", ")[[1]]),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p3$tab[2,6], start = 2, stop = nchar(p3$tab[2,6])-1), ", ")[[1]]),
               as.numeric(strsplit(substr(p4$tab[2,6], start = 2, stop = nchar(p4$tab[2,6])-1), ", ")[[1]]),
               tolerance = 1e-2) # conf int
  expect_equal(as.numeric(strsplit(substr(p3$tab[3,6], start = 2, stop = nchar(p3$tab[3,6])-1), ", ")[[1]]),
               as.numeric(strsplit(substr(p4$tab[3,6], start = 2, stop = nchar(p4$tab[3,6])-1), ", ")[[1]]),
               tolerance = 1e-2) # conf int
  expect_equal(p3$tab[1,1], "by = 1") # var name
  expect_equal(p3$tab[2,1], "by = 2") # var name
  expect_equal(p3$tab[3,1], "Difference") # var name
  expect_equal(as.numeric(p3$tab[1,2]), length(a_na)) # n obs
  expect_equal(as.numeric(p3$tab[2,2]), length(b_na)-2) # n obs
  expect_equal(as.numeric(p3$tab[3,2]), length(a_na) + length(b_na)-2) # n obs
  expect_equal(as.numeric(p3$tab[1,3]), sum(is.na(a_na))) # NAs
  expect_equal(as.numeric(p3$tab[2,3]), 0) # NAs
  expect_equal(as.numeric(p3$tab[3,3]), sum(is.na(a_na)) + 0) # NAs
  expect_equal(as.numeric(p3$tab[1,4]), as.numeric(p4$tab[1,4]), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p3$tab[2,4]), as.numeric(p4$tab[2,4]), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p3$tab[3,4]), as.numeric(p4$tab[3,4]), tolerance = 1e-2) # estimate of mean
  expect_equal(as.numeric(p3$tab[1,5]), as.numeric(p4$tab[1,5]), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p3$tab[2,5]), as.numeric(p4$tab[2,5]), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(as.numeric(p3$tab[3,5]), as.numeric(p4$tab[3,5]), 
               tolerance = 1e-2) # standard error of mean est
  expect_equal(p3$var1, a_short) 
  expect_equal(p3$var2, b_short[-100])
  expect_equal(p3$by, by[!is.na(by)])
  expect_equal(as.numeric(p3$par[[1]]), 0) # null 
  expect_equal(p3$par[[2]], p3$par[[2]]) # alternative
  expect_equal(as.numeric(p3$par[[3]]), as.numeric(p4$par[[3]])) # conf level
  expect_false(as.logical(p3$par[[4]]))
  expect_equal(as.numeric(p3$par[[6]]), 3) # digits
})
