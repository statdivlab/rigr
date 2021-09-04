### error handling

# x1 <- c(-1, seq(1, 100))
# 
# test_that("ttest() throws error if geo mean requested with non-pos data", {
#   expect_error(ttest(x1, geom = TRUE), 
#                "Geometric mean requires that all numeric data are positive")
# })
# 
# x1 <- seq(1, 100)
# 
# test_that("ttest() throws error if geo mean requested with non-pos null", {
#   expect_error(ttest(x1, geom = TRUE, null.hypoth = -1), 
#                "Geometric mean cannot be less than zero")
# })
# 
# x1 <- rnorm(100)
# x2 <- rnorm(100)
# 
# test_that("ttest() throws error if alternative is not 'two.sided', 'less', or 'greater", {
#   expect_error(ttest(x1, x2, alternative = "blah"), 
#                "'alternative' must be either 'less', 'two.sided', or 'greater'")
# })
# 
# test_that("ttest() throws error if two variables and by also given", {
#   expect_error(ttest(x1, x2, by = rep(1, 100)),
#                "Please specify only one of the variables 'by' or 'var2'")
# })
# 
# test_that("ttest() throws error if var.eq is not logical", {
#   expect_error(ttest(x1, var.eq = 2), 
#                "Please specify a logical value for variable 'var.eq'")
# })
# 
# x3 <- rnorm(99)
# 
# test_that("ttest() throws error if matched test performedm on different numbers of observations", {
#   expect_error(ttest(x1, x3, matched = TRUE),
#               "Cannot perform matched t-test on variable of unequal length")
#   expect_error(ttest(x3, x1, matched = TRUE),
#                "Cannot perform matched t-test on variable of unequal length")
#   expect_error(ttest(x1, by = c(rep(1, 51), rep(2, 49)), matched = TRUE),
#                "Cannot perform matched t-test on variable of unequal length")
# })
# 
# test_that("ttest() throws error for non-numeric data", {
#   expect_error(ttest(c("a", "B", "c")), 
#                "Cannot perform t-test on non-numeric data")
# })
# 
# test_that("ttest() throws error for non-numeric more.digits argument", {
#   expect_error(ttest(x1, more.digits = TRUE), 
#                "Argument 'more.digits' must be numeric")
# })
# 
# test_that("ttest() throws error if by argument contains only one value", {
#   expect_error(ttest(x1, by = rep(1, 100)), 
#                "Variable 'by' only has one unique value")
# })
# 
# test_that("ttest() throws error if by argument contains >2 unique values", {
#   expect_error(ttest(x1, by = c(rep(1, 50), rep(2, 49), 3)), 
#                "Variable 'by' has more than two unique values.")
# })
# 
# test_that("ttest() throws error if by argument is not of same length as data", {
#   expect_error(ttest(x1, by = c(rep(1, 50), rep(2, 51))), 
#                "Variable 'by' is not of equal length to data vector")
# })
# 
# test_that("ttest() warns about 0 null in geometric mean test", {
#   expect_warning(ttest(exp(x1), geom = TRUE), 
#                  "Geometric mean of zero not allowable: alternative hypothesis default changed to 1")
# })

### one-sample test, approximate 

set.seed(1)
a <- rbinom(100, 1, 0.5)

p1 <- proptesti(sum(a), length(a))
p2 <- prop.test(sum(a), length(a), correct = FALSE)

test_that("proptesti() returns correct numbers for one-sample test", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$zstat, sqrt(p2$statistic[[1]]), tolerance = 1e-3) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})


p1 <- proptesti(sum(a), length(a), alternative = "less")
p2 <- prop.test(sum(a), length(a), correct = FALSE, alternative = "less")

test_that("proptesti() returns correct numbers for one-sample test, left-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$zstat, sqrt(p2$statistic[[1]]), tolerance = 1e-3) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-2) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})


p1 <- proptesti(sum(a), length(a), alternative = "greater")
p2 <- prop.test(sum(a), length(a), correct = FALSE, alternative = "greater")

test_that("proptesti() returns correct numbers for one-sample test, right-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$zstat, sqrt(p2$statistic[[1]]), tolerance = 1e-3) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), conf.level = 0.9)
p2 <- prop.test(sum(a), length(a), correct = FALSE, conf.level = 0.9)

test_that("proptesti() returns correct numbers for one-sample test, non 0.95-level", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$zstat, sqrt(p2$statistic[[1]]), tolerance = 1e-3) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

### one-sample test, exact

p1 <- proptesti(sum(a), length(a), exact = TRUE)
p2 <- binom.test(sum(a), length(a))

test_that("proptesti() returns correct numbers for one-sample test, exact", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), exact = TRUE, alternative = "less")
p2 <- binom.test(sum(a), length(a), alternative = "less")

test_that("proptesti() returns correct numbers for one-sample test, exact, left-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), exact = TRUE, alternative = "greater")
p2 <- binom.test(sum(a), length(a), alternative = "greater")

test_that("proptesti() returns correct numbers for one-sample test, exact, right-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), exact = TRUE, conf.level = 0.9)
p2 <- binom.test(sum(a), length(a), conf.level = 0.9)

test_that("proptesti() returns correct numbers for one-sample test, exact, non-0.95 conf", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[[5]], start = 2, stop = nchar(p1$tab[[5]])-1), ", ")[[1]]),
               p2$conf.int[1:2], 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

### two-sample test, approximate 

b <- rbinom(100, 1, 0.5)

p1 <- proptesti(sum(a), length(a), sum(b), length(b))
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE)

test_that("proptesti() returns correct numbers for two-sample test", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$zstat, sqrt(p2$statistic[[1]]), tolerance = 1e-3) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,5], start = 2, stop = nchar(p1$tab[1,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,5], start = 2, stop = nchar(p1$tab[2,5])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$tab[1,3]), p2$estimate[[1]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,3]), p2$estimate[[2]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,3]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,4]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), 0.5) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_true(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), sum(b), length(b), alternative = "less")
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, alternative = "less")

test_that("proptesti() returns correct numbers for two-sample test, left-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$zstat, sqrt(p2$statistic[[1]]), tolerance = 1e-3) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,5], start = 2, stop = nchar(p1$tab[1,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,5], start = 2, stop = nchar(p1$tab[2,5])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$tab[1,3]), p2$estimate[[1]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,3]), p2$estimate[[2]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,3]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,4]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), 0.5) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_true(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), sum(b), length(b), alternative = "greater")
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, alternative = "greater")

test_that("proptesti() returns correct numbers for two-sample test, right-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$zstat, sqrt(p2$statistic[[1]]), tolerance = 1e-3) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,5], start = 2, stop = nchar(p1$tab[1,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + 1.96*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,5], start = 2, stop = nchar(p1$tab[2,5])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + 1.96*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$tab[1,3]), p2$estimate[[1]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,3]), p2$estimate[[2]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,3]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,4]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), 0.5) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_true(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})

p1 <- proptesti(sum(a), length(a), sum(b), length(b), conf.level = 0.9)
p2 <- prop.test(c(sum(a), sum(b)), c(length(a), length(b)), correct = FALSE, conf.level = 0.9)

test_that("proptesti() returns correct numbers for two-sample test, right-sided", {
  expect_s3_class(p1, "proptesti")
  expect_equal(p1$zstat, sqrt(p2$statistic[[1]]), tolerance = 1e-3) # test statistic
  expect_equal(p1$pval, p2$p.value, tolerance = 1e-3) # p-value
  expect_equal(as.numeric(strsplit(substr(p1$tab[1,5], start = 2, stop = nchar(p1$tab[1,5])-1), ", ")[[1]]),
               c(p2$estimate[[1]] - qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
                 p2$estimate[[1]] + qnorm(0.95)*sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a))), 
               tolerance = 1e-3) # conf int
  expect_equal(as.numeric(strsplit(substr(p1$tab[2,5], start = 2, stop = nchar(p1$tab[2,5])-1), ", ")[[1]]),
               c(p2$estimate[[2]] - qnorm(0.95)*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
                 p2$estimate[[2]] + qnorm(0.95)*sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b))), 
               tolerance = 1e-3) # conf int
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
  expect_equal(as.numeric(p1$tab[1,3]), p2$estimate[[1]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[2,3]), p2$estimate[[2]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[3,3]), p2$estimate[[1]] - p2$estimate[[2]], tolerance = 1e-3) # estimate of mean
  expect_equal(as.numeric(p1$tab[1,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$tab[2,4]), sqrt(p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$tab[3,4]), sqrt(p2$estimate[[1]]*(1-p2$estimate[[1]])/length(a) + 
                                               p2$estimate[[2]]*(1-p2$estimate[[2]])/length(b)), 
               tolerance = 1e-3) # standard error of mean est
  expect_equal(as.numeric(p1$par[[1]]), 0.5) # null 
  expect_equal(p1$par[[2]], p2$alternative) # alternative
  expect_equal(as.numeric(p1$par[[3]]), attr(p2$conf.int, "conf.level")) # conf level
  expect_false(as.logical(p1$par[[4]]))
  expect_true(as.logical(p1$par[[5]]))
  expect_equal(as.numeric(p1$par[[6]]), 3) # digits
})