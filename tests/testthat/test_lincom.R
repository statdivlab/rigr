### error handling

test_that("lincom() throws an error if object is not of class uRegress", {
  expect_error(lincom(1, c(1)), 
               "uRegress object must be entered")
  expect_error(lincom("cat", c(1)), 
               "uRegress object must be entered")
  expect_error(lincom(ttest(rnorm(100)), c(1)), 
               "uRegress object must be entered")
})

dat <- data.frame(y = rnorm(100), x = rnorm(100))
reg <- regress("mean", y ~ x, data = dat, robustSE = FALSE)

test_that("lincom() throws an error if conf.level is not in (0,1)", {
  expect_error(lincom(reg, c(1,1), conf.level = 1.01),
               "Confidence Level must be between 0 and 1")
})

test_that("lincom() throws error if comb vector is not right length", {
  expect_error(lincom(reg, c(1)),
               "Vector of constants must be equal to number of coefficients in model, including intercept if applicable")
  expect_error(lincom(reg, c(1,1,1)), 
               "Vector of constants must be equal to number of coefficients in model, including intercept if applicable")
})

test_that("lincom() throws error if robust SEs requested but were not used in regress()", {
  expect_error(lincom(reg, c(1,1), robustSE = TRUE), 
               "uRegress object must be created with robust standard errors")
})

test_that("lincom() throws error if comb matrix has incorrect number of columns", {
  expect_error(lincom(reg, matrix(c(1,1, 2, 2, 3, 3), nrow = 2), robustSE = FALSE),
               "Matrix of constants must have columns equal to the number of coefficients")
})

test_that("lincom() throws error if null.hypoth is wrong dimension or not numeric", {
  expect_error(lincom(reg, c(1,1), robustSE = FALSE, null.hypoth = "a"),
               "Null hypothesis must a scalar.")
  expect_error(lincom(reg, c(1,1), robustSE = FALSE, null.hypoth = c(1,2)),
               "Null hypothesis must a scalar.")
  expect_error(lincom(reg, comb = matrix(c(1,1,2,2, 3, 3), nrow = 3), 
                      robustSE = FALSE, null.hypoth = matrix(c(0, 0, "a"), nrow = 3)),
               "Null hypothesis must numeric and of the same dimension as the number of combinations being tested.")
  expect_error(lincom(reg, comb = matrix(c(1,1,2,2, 3, "a"), nrow = 3), 
                      robustSE = FALSE, null.hypoth = matrix(c(0, 0, "a"), nrow = 3)),
               "Null hypothesis must numeric and of the same dimension as the number of combinations being tested.")
})

test_that("lincom() throws error if eform not logical", {
  expect_error(lincom(reg, c(1,1), robustSE = FALSE, eform = 2),
               "Argument eform must be a logical.")
})

### single test
library(sandwich)
dat <- data.frame(y = rnorm(100), x = rnorm(100))
reg <- regress("mean", y ~ x, data = dat, robustSE = FALSE)
mod <- lm(y ~ x, data = dat)
comb <- matrix(c(1.2, -3.9), nrow = 1)
orig_coef <- matrix(mod$coefficients, nrow = 2)
new_coef <- comb %*% orig_coef
new_se <- sqrt(comb %*% vcov(mod) %*% t(comb))
tstat <- (new_coef - 0)/new_se
pval <- 2*pt(-abs(tstat), reg$df[2]) 
CIL <- new_coef - abs(qt((0.05)/2,df=reg$df[2])*new_se)
CIU <- new_coef + abs(qt((0.05)/2,df=reg$df[2])*new_se)


lc1 <- lincom(reg, c(1.2, -3.9), robustSE = FALSE)

test_that("lincom() returns correct numbers for a single comb, model SE", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 0)
})

tstat <- (new_coef - 1)/new_se
pval <- 2*pt(-abs(tstat), reg$df[2]) 
lc1 <- lincom(reg, c(1.2, -3.9), robustSE = FALSE, null.hypoth = 1)

test_that("lincom() returns correct numbers for a single comb, model SE, non-zero null", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 1)
})

CIL <- new_coef - abs(qt((0.1)/2,df=reg$df[2])*new_se)
CIU <- new_coef + abs(qt((0.1)/2,df=reg$df[2])*new_se)
lc1 <- lincom(reg, c(1.2, -3.9), robustSE = FALSE, null.hypoth = 1, conf.level = 0.9)

test_that("lincom() returns correct numbers for a single comb, model SE, non-zero null, different conf level", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 1)
})

exp_coef <- exp(new_coef)
exp_CIL <- exp(CIL)
exp_CIU <- exp(CIU)

lc1 <- lincom(reg, c(1.2, -3.9), robustSE = FALSE, null.hypoth = 1, conf.level = 0.9, eform = TRUE)

test_that("lincom() returns correct numbers for a single comb, model SE, non-zero null, different conf level, exp form", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(exp_coef), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(exp_CIL), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(exp_CIU), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 1)
})

reg <- regress("mean", y ~ x, data = dat, robustSE = TRUE)
new_se <- sqrt(comb %*% sandwich(mod, adjust = TRUE) %*% t(comb))
tstat <- (new_coef - 0)/new_se
pval <- 2*pt(-abs(tstat), reg$df[2]) 
CIL <- new_coef - abs(qt((0.05)/2,df=reg$df[2])*new_se)
CIU <- new_coef + abs(qt((0.05)/2,df=reg$df[2])*new_se)


lc1 <- lincom(reg, c(1.2, -3.9), robustSE = TRUE)

test_that("lincom() returns correct numbers for a single comb, model SE", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 0)
})

### single test including 0s
reg <- regress("mean", y ~ x, data = dat, robustSE = FALSE)
comb <- matrix(c(0, -3.9), nrow = 1)
new_coef <- comb %*% orig_coef
new_se <- sqrt(comb %*% vcov(mod) %*% t(comb))
tstat <- (new_coef - 0)/new_se
pval <- 2*pt(-abs(tstat), reg$df[2]) 
CIL <- new_coef - abs(qt((0.05)/2,df=reg$df[2])*new_se)
CIU <- new_coef + abs(qt((0.05)/2,df=reg$df[2])*new_se)

lc1 <- lincom(reg, c(0, -3.9), robustSE = FALSE)

test_that("lincom() returns correct numbers for a single comb, model SE", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 0)
})


### multiple tests
dat <- data.frame(y = rnorm(100), x = rnorm(100))
reg <- regress("mean", y ~ x, data = dat, robustSE = FALSE)
mod <- lm(y ~ x, data = dat)
comb <- matrix(c(1.2, -0.8, -3.9, 2.1), nrow = 2)
orig_coef <- matrix(mod$coefficients, nrow = 2)
new_coef1 <- comb[1,] %*% orig_coef
new_coef2 <- comb[2,] %*% orig_coef
new_se1 <- sqrt(t(comb[1,]) %*% vcov(mod) %*% (comb[1,]))
new_se2 <- sqrt(t(comb[2,]) %*% vcov(mod) %*% (comb[2,]))
tstat1 <- (new_coef1 - 0)/new_se1
tstat2 <- (new_coef2 - 0)/new_se2
pval1 <- 2*pt(-abs(tstat1), reg$df[2]) 
CIL1 <- new_coef1 - abs(qt((0.05)/2,df=reg$df[2])*new_se1)
CIU1 <- new_coef1 + abs(qt((0.05)/2,df=reg$df[2])*new_se1)
pval2 <- 2*pt(-abs(tstat2), reg$df[2]) 
CIL2 <- new_coef2 - abs(qt((0.05)/2,df=reg$df[2])*new_se2)
CIU2 <- new_coef2 + abs(qt((0.05)/2,df=reg$df[2])*new_se2)

lc1 <- lincom(reg, matrix(c(1.2, -0.8, -3.9, 2.1), nrow = 2), robustSE = FALSE)

test_that("lincom() returns correct numbers for two combs, model SE", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval1), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 0)
  
  expect_equal(lc1$comb2$printMat[1], as.numeric(new_coef2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[2], as.numeric(new_se2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[3], as.numeric(CIL2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[4], as.numeric(CIU2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[5], as.numeric(tstat2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[6], as.numeric(pval2), tolerance = 1e-4)
  expect_equal(lc1$comb2$nms, "-0.8*(Intercept)+2.1*x")
  expect_equal(lc1$comb2$null.hypoth, 0)
})

tstat1 <- (new_coef1 - 1)/new_se1
pval1 <- 2*pt(-abs(tstat1), reg$df[2]) 
tstat2 <- (new_coef2 - 2)/new_se2
pval2 <- 2*pt(-abs(tstat2), reg$df[2]) 

lc1 <- lincom(reg, matrix(c(1.2, -0.8, -3.9, 2.1), nrow = 2), null.hypoth = c(1, 2), robustSE = FALSE)

test_that("lincom() returns correct numbers for two combs, model SE, non-zero (vector) null", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval1), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 1)
  
  expect_equal(lc1$comb2$printMat[1], as.numeric(new_coef2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[2], as.numeric(new_se2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[3], as.numeric(CIL2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[4], as.numeric(CIU2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[5], as.numeric(tstat2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[6], as.numeric(pval2), tolerance = 1e-4)
  expect_equal(lc1$comb2$nms, "-0.8*(Intercept)+2.1*x")
  expect_equal(lc1$comb2$null.hypoth, 2)
})

lc1 <- lincom(reg, matrix(c(1.2, -0.8, -3.9, 2.1), nrow = 2), null.hypoth = matrix(c(1, 2), nrow = 2), robustSE = FALSE)

test_that("lincom() returns correct numbers for two combs, model SE, non-zero (matrix) null", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval1), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 1)
  
  expect_equal(lc1$comb2$printMat[1], as.numeric(new_coef2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[2], as.numeric(new_se2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[3], as.numeric(CIL2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[4], as.numeric(CIU2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[5], as.numeric(tstat2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[6], as.numeric(pval2), tolerance = 1e-4)
  expect_equal(lc1$comb2$nms, "-0.8*(Intercept)+2.1*x")
  expect_equal(lc1$comb2$null.hypoth, 2)
})

CIL1 <- new_coef1 - abs(qt((0.1)/2,df=reg$df[2])*new_se1)
CIU1 <- new_coef1 + abs(qt((0.1)/2,df=reg$df[2])*new_se1)
CIL2 <- new_coef2 - abs(qt((0.1)/2,df=reg$df[2])*new_se2)
CIU2 <- new_coef2 + abs(qt((0.1)/2,df=reg$df[2])*new_se2)
lc1 <- lincom(reg,  matrix(c(1.2, -0.8, -3.9, 2.1), nrow = 2), robustSE = FALSE, null.hypoth = c(1,2), conf.level = 0.9)
test_that("lincom() returns correct numbers for two combs, model SE, non-zero null, different conf level", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval1), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 1)
  
  expect_equal(lc1$comb2$printMat[1], as.numeric(new_coef2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[2], as.numeric(new_se2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[3], as.numeric(CIL2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[4], as.numeric(CIU2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[5], as.numeric(tstat2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[6], as.numeric(pval2), tolerance = 1e-4)
  expect_equal(lc1$comb2$nms, "-0.8*(Intercept)+2.1*x")
  expect_equal(lc1$comb2$null.hypoth, 2)
})

exp_coef1 <- exp(new_coef1)
exp_CIL1 <- exp(CIL1)
exp_CIU1 <- exp(CIU1)
exp_coef2 <- exp(new_coef2)
exp_CIL2 <- exp(CIL2)
exp_CIU2 <- exp(CIU2)

lc1 <- lincom(reg,  matrix(c(1.2, -0.8, -3.9, 2.1), nrow = 2), robustSE = FALSE, null.hypoth = c(1,2), conf.level = 0.9, eform = TRUE)
test_that("lincom() returns correct numbers for two combs, model SE, non-zero null, different conf level, eform", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(exp_coef1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(exp_CIL1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(exp_CIU1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval1), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 1)
  
  expect_equal(lc1$comb2$printMat[1], as.numeric(exp_coef2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[2], as.numeric(new_se2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[3], as.numeric(exp_CIL2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[4], as.numeric(exp_CIU2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[5], as.numeric(tstat2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[6], as.numeric(pval2), tolerance = 1e-4)
  expect_equal(lc1$comb2$nms, "-0.8*(Intercept)+2.1*x")
  expect_equal(lc1$comb2$null.hypoth, 2)
})

reg <- regress("mean", y ~ x, data = dat, robustSE = TRUE)
new_se1 <- sqrt(t(comb[1,]) %*% sandwich(mod, adjust = TRUE) %*% (comb[1,]))
new_se2 <- sqrt(t(comb[2,]) %*% sandwich(mod, adjust = TRUE) %*% (comb[2,]))
tstat1 <- (new_coef1 - 0)/new_se1
tstat2 <- (new_coef2 - 0)/new_se2
pval1 <- 2*pt(-abs(tstat1), reg$df[2]) 
CIL1 <- new_coef1 - abs(qt((0.05)/2,df=reg$df[2])*new_se1)
CIU1 <- new_coef1 + abs(qt((0.05)/2,df=reg$df[2])*new_se1)
pval2 <- 2*pt(-abs(tstat2), reg$df[2]) 
CIL2 <- new_coef2 - abs(qt((0.05)/2,df=reg$df[2])*new_se2)
CIU2 <- new_coef2 + abs(qt((0.05)/2,df=reg$df[2])*new_se2)

lc1 <- lincom(reg,  matrix(c(1.2, -0.8, -3.9, 2.1), nrow = 2), robustSE = TRUE)

test_that("lincom() returns correct numbers for two combs, model SE, non-zero null, different conf level, eform", {
  expect_equal(lc1$comb1$printMat[1], as.numeric(new_coef1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[2], as.numeric(new_se1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[3], as.numeric(CIL1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[4], as.numeric(CIU1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[5], as.numeric(tstat1), tolerance = 1e-4)
  expect_equal(lc1$comb1$printMat[6], as.numeric(pval1), tolerance = 1e-4)
  expect_equal(lc1$comb1$nms, "1.2*(Intercept)-3.9*x")
  expect_equal(lc1$comb1$null.hypoth, 0)
  
  expect_equal(lc1$comb2$printMat[1], as.numeric(new_coef2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[2], as.numeric(new_se2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[3], as.numeric(CIL2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[4], as.numeric(CIU2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[5], as.numeric(tstat2), tolerance = 1e-4)
  expect_equal(lc1$comb2$printMat[6], as.numeric(pval2), tolerance = 1e-4)
  expect_equal(lc1$comb2$nms, "-0.8*(Intercept)+2.1*x")
  expect_equal(lc1$comb2$null.hypoth, 0)
})

### Zeroes!

data(mri)
testReg <- regress ("mean", ldl~age+stroke, data = mri)
testReg2 <- regress("mean", atrophy ~ age + sex, data = mri)
testC <- matrix(c(0, 0.5, -1, 1, 60, 0, -0.5, 0, 50), byrow = TRUE, nrow = 3)
testC2 <- c(0, -10, 1)
lc1 <- lincom(testReg, testC, null.hypoth = c(0, 125, 50))
lc2 <- lincom(testReg2, testC2)

test_that("lincom() works with 0 coefficients", {
  expect_equal(lc1$comb1$nms, "0.5*age-1*stroke")
  expect_equal(lc1$comb1$null.hypoth, 0)
  expect_equal(lc1$comb2$nms, "1*(Intercept)+60*age")
  expect_equal(lc1$comb2$null.hypoth, 125)
  expect_equal(lc1$comb3$nms, "-0.5*(Intercept)+50*stroke")
  expect_equal(lc1$comb3$null.hypoth, 50)
  expect_equal(lc2$comb1$nms, "-10*age+1*sexMale")
  expect_equal(lc2$comb1$null.hypoth, 0)
})