### error handling

data(mri)

test_that("descrip() throws error if strata are not the same lengths", {
  expect_error(descrip(mri, strata = list(a = "a", b = c("b", "c"))), 
               "all elements in strata must be same length")
})

test_that("descrip() throws error if above, below, labove, rbelow are not atomic", {
  expect_error(descrip(mri, above = list(a = 1)), "above must be a vector")
  expect_error(descrip(mri, below = list(a = 1)), "below must be a vector")
  expect_error(descrip(mri, labove = list(a = 1)), "labove must be a vector")
  expect_error(descrip(mri, rbelow = list(a = 1)), "rbelow must be a vector")
  expect_error(descrip(mri, lbetween = list(a = 1)), "lbetween must be a vector")
  expect_error(descrip(mri, rbetween = list(a = 1)), "rbetween must be a vector")
})

test_that("descrip() throws error if above, below, labove, rbelow are not numeric", {
  expect_error(descrip(mri, above = "a"), "above must be numeric")
  expect_error(descrip(mri, below = "a"), "below must be numeric")
  expect_error(descrip(mri, labove = "a"), "labove must be numeric")
  expect_error(descrip(mri, rbelow = "a"), "rbelow must be numeric")
  expect_error(descrip(mri, lbetween = "a"), "lbetween must be numeric")
  expect_error(descrip(mri, rbetween = "a"), "rbetween must be numeric")
})

test_that("descrip() throws error if interval is a matrix with more than 2 columns", {
  expect_error(descrip(mri$atrophy, interval = matrix(0, nrow = 1, ncol = 3)),
               "interval must be specified in a 2 column matrix")
  expect_error(descrip(mri$atrophy, linterval = matrix(0, nrow = 1, ncol = 3)),
               "linterval must be specified in a 2 column matrix")
  expect_error(descrip(mri$atrophy, rinterval = matrix(0, nrow = 1, ncol = 3)),
               "rinterval must be specified in a 2 column matrix")
  expect_error(descrip(mri$atrophy, lrinterval = matrix(0, nrow = 1, ncol = 3)),
               "lrinterval must be specified in a 2 column matrix")
})

### warning handling

test_that("descrip() gives warning if interval is specified as a vector", {
  expect_warning(descrip(mri$atrophy, interval = c(5,10,15)), "Assuming intervals between points specified in vector. To specify specific intervals, enter interval argument as a 2 column matrix instead of a vector")
  expect_warning(descrip(mri$atrophy, linterval = c(5,10,15)), "Assuming lintervals between points specified in vector. To specify specific lintervals, enter linterval argument as a 2 column matrix instead of a vector")
  expect_warning(descrip(mri$atrophy, rinterval = c(5,10,15)), "Assuming rintervals between points specified in vector. To specify specific rintervals, enter rinterval argument as a 2 column matrix instead of a vector")
  expect_warning(descrip(mri$atrophy, lrinterval = c(5,10,15)), "Assuming lrintervals between points specified in vector. To specify specific lrintervals, enter lrinterval argument as a 2 column matrix instead of a vector")
})

### dataframe

a <- rnorm(10,0,1)
b <- rnorm(10,5,1)
df <- data.frame(a, b)
descrip_out <- descrip(df)

# Test relies on the ordering of the columns remaining the same
test_that("descrip() returns correct numbers for dataframe", {
  expect_s3_class(descrip_out, "uDescriptives")
  expect_equal(descrip_out[1,1], 10) # check N
  expect_equal(descrip_out[2,1], 10)
  expect_equal(descrip_out[1,2], 0) # check missing values
  expect_equal(descrip_out[2,2], 0)
  expect_equal(descrip_out[1,3], mean(a)) # check means
  expect_equal(descrip_out[2,3], mean(b))
  expect_equal(descrip_out[1,4], sd(a)) # check SDs
  expect_equal(descrip_out[2,4], sd(b))
  expect_equal(descrip_out[1,5], min(a)) # check mins
  expect_equal(descrip_out[2,5], min(b))
  expect_equal(descrip_out[1,6], unname(quantile(a, 0.25))) # check 25th percentiles
  expect_equal(descrip_out[2,6], unname(quantile(b, 0.25)))
  expect_equal(descrip_out[1,7], unname(quantile(a, 0.5))) # check medians
  expect_equal(descrip_out[2,7], unname(quantile(b, 0.5)))
  expect_equal(descrip_out[1,8], unname(quantile(a, 0.75))) # check 75th percentiles
  expect_equal(descrip_out[2,8], unname(quantile(b, 0.75)))
  expect_equal(descrip_out[1,9], max(a)) # check maxs
  expect_equal(descrip_out[2,9], max(b))
  expect_equal(descrip_out[1,10], Inf) # check restriction (should be Inf if none specified)
  expect_equal(descrip_out[2,10], Inf)
  #expect_equal(descrip_out[1,11], Inf) # check firstEvent
  #expect_equal(descrip_out[2,11], Inf)
  expect_true(is.na(descrip_out[1,12])) # check lastEvent
  expect_true(is.na(descrip_out[2,12]))
  expect_equal(descrip_out[1,13], 0) # check isDate
  expect_equal(descrip_out[2,13], 0)
})

### matrix

a <- rnorm(10,0,1)
b <- rnorm(10,5,1)
mat <- cbind(a, b)
descrip_out <- descrip(mat)

# Test relies on the ordering of the columns remaining the same
test_that("descrip() returns correct numbers for matrix", {
  expect_s3_class(descrip_out, "uDescriptives")
  expect_equal(descrip_out[1,1], 10) # check N
  expect_equal(descrip_out[2,1], 10)
  expect_equal(descrip_out[1,2], 0) # check missing values
  expect_equal(descrip_out[2,2], 0)
  expect_equal(descrip_out[1,3], mean(a)) # check means
  expect_equal(descrip_out[2,3], mean(b))
  expect_equal(descrip_out[1,4], sd(a)) # check SDs
  expect_equal(descrip_out[2,4], sd(b))
  expect_equal(descrip_out[1,5], min(a)) # check mins
  expect_equal(descrip_out[2,5], min(b))
  expect_equal(descrip_out[1,6], unname(quantile(a, 0.25))) # check 25th percentiles
  expect_equal(descrip_out[2,6], unname(quantile(b, 0.25)))
  expect_equal(descrip_out[1,7], unname(quantile(a, 0.5))) # check medians
  expect_equal(descrip_out[2,7], unname(quantile(b, 0.5)))
  expect_equal(descrip_out[1,8], unname(quantile(a, 0.75))) # check 75th percentiles
  expect_equal(descrip_out[2,8], unname(quantile(b, 0.75)))
  expect_equal(descrip_out[1,9], max(a)) # check maxs
  expect_equal(descrip_out[2,9], max(b))
  expect_equal(descrip_out[1,10], Inf) # check restriction (should be Inf if none specified)
  expect_equal(descrip_out[2,10], Inf)
  #expect_equal(descrip_out[1,11], Inf) # check firstEvent
  #expect_equal(descrip_out[2,11], Inf)
  expect_true(is.na(descrip_out[1,12])) # check lastEvent
  expect_true(is.na(descrip_out[2,12]))
  expect_equal(descrip_out[1,13], 0) # check isDate
  expect_equal(descrip_out[2,13], 0)
})

### list

a <- rnorm(10,0,1)
b <- rnorm(20,5,1)
lst <- list(a, b)
descrip_out <- descrip(lst)

# Test relies on the ordering of the columns remaining the same
test_that("descrip() returns correct numbers for list", {
  expect_s3_class(descrip_out, "uDescriptives")
  expect_equal(descrip_out[1,1], 10) # check N
  expect_equal(descrip_out[2,1], 20)
  expect_equal(descrip_out[1,2], 0) # check missing values
  expect_equal(descrip_out[2,2], 0)
  expect_equal(descrip_out[1,3], mean(a)) # check means
  expect_equal(descrip_out[2,3], mean(b))
  expect_equal(descrip_out[1,4], sd(a)) # check SDs
  expect_equal(descrip_out[2,4], sd(b))
  expect_equal(descrip_out[1,5], min(a)) # check mins
  expect_equal(descrip_out[2,5], min(b))
  expect_equal(descrip_out[1,6], unname(quantile(a, 0.25))) # check 25th percentiles
  expect_equal(descrip_out[2,6], unname(quantile(b, 0.25)))
  expect_equal(descrip_out[1,7], unname(quantile(a, 0.5))) # check medians
  expect_equal(descrip_out[2,7], unname(quantile(b, 0.5)))
  expect_equal(descrip_out[1,8], unname(quantile(a, 0.75))) # check 75th percentiles
  expect_equal(descrip_out[2,8], unname(quantile(b, 0.75)))
  expect_equal(descrip_out[1,9], max(a)) # check maxs
  expect_equal(descrip_out[2,9], max(b))
  expect_equal(descrip_out[1,10], Inf) # check restriction (should be Inf if none specified)
  expect_equal(descrip_out[2,10], Inf)
  #expect_equal(descrip_out[1,11], Inf) # check firstEvent
  #expect_equal(descrip_out[2,11], Inf)
  expect_true(is.na(descrip_out[1,12])) # check lastEvent
  expect_true(is.na(descrip_out[2,12]))
  expect_equal(descrip_out[1,13], 0) # check isDate
  expect_equal(descrip_out[2,13], 0)
})

### dataframe - stratified

a <- rnorm(10,0,1)
d <- c(rep(1,6),rep(0,4))
df <- data.frame(a)
descrip_out <- descrip(df, strata = d)


# Test relies on the ordering of the columns remaining the same
test_that("descrip() returns correct numbers for stratified dataframe", {
  expect_s3_class(descrip_out, "uDescriptives")
  expect_equal(unname(descrip_out[,1]), c(10, 4, 6)) # check N
  expect_equal(unname(descrip_out[,2]), rep(0, 3)) # check missing values
  expect_equal(descrip_out[1,3], mean(a)) # check means
  expect_equal(descrip_out[2,3], mean(a[d == 0]))
  expect_equal(descrip_out[3,3], mean(a[d == 1]))
  expect_equal(descrip_out[1,4], sd(a)) # check SDs
  expect_equal(descrip_out[2,4], sd(a[d == 0]))
  expect_equal(descrip_out[3,4], sd(a[d == 1]))
  expect_equal(descrip_out[1,5], min(a)) # check mins
  expect_equal(descrip_out[2,5], min(a[d == 0]))
  expect_equal(descrip_out[3,5], min(a[d == 1]))
  expect_equal(descrip_out[1,6], unname(quantile(a, 0.25))) # check 25th percentiles
  expect_equal(descrip_out[2,6], unname(quantile(a[d == 0], 0.25)))
  expect_equal(descrip_out[3,6], unname(quantile(a[d == 1], 0.25)))
  expect_equal(descrip_out[1,7], unname(quantile(a, 0.5))) # check medians
  expect_equal(descrip_out[2,7], unname(quantile(a[d == 0], 0.5)))
  expect_equal(descrip_out[3,7], unname(quantile(a[d == 1], 0.5)))
  expect_equal(descrip_out[1,8], unname(quantile(a, 0.75))) # check 75th percentiles
  expect_equal(descrip_out[2,8], unname(quantile(a[d == 0], 0.75)))
  expect_equal(descrip_out[3,8], unname(quantile(a[d == 1], 0.75)))
  expect_equal(descrip_out[1,9], max(a)) # check maxs
  expect_equal(descrip_out[2,9], max(a[d == 0])) 
  expect_equal(descrip_out[3,9], max(a[d == 1])) 
  expect_equal(unname(descrip_out[,10]), rep(Inf, 3)) # check restriction (should be Inf if none specified)
  #expect_equal(is.na(descrip_out[1,11])) # check firstEvent
  expect_true(is.na(descrip_out[1,12])) # check lastEvent
  expect_true(is.na(descrip_out[2,12]))
  expect_true(is.na(descrip_out[3,12]))
  expect_equal(unname(descrip_out[,13]), rep(0, 3)) # check isDate
})

### matrix - stratified

a <- rnorm(10,0,1)
d <- c(rep(1,6),rep(0,4))
mat <- matrix(a)
descrip_out <- descrip(mat, strata = d)

# Test relies on the ordering of the columns remaining the same
test_that("descrip() returns correct numbers for stratified matrix", {
  expect_s3_class(descrip_out, "uDescriptives")
  expect_equal(unname(descrip_out[,1]), c(10, 4, 6)) # check N
  expect_equal(unname(descrip_out[,2]), rep(0, 3)) # check missing values
  expect_equal(descrip_out[1,3], mean(a)) # check means
  expect_equal(descrip_out[2,3], mean(a[d == 0]))
  expect_equal(descrip_out[3,3], mean(a[d == 1]))
  expect_equal(descrip_out[1,4], sd(a)) # check SDs
  expect_equal(descrip_out[2,4], sd(a[d == 0]))
  expect_equal(descrip_out[3,4], sd(a[d == 1]))
  expect_equal(descrip_out[1,5], min(a)) # check mins
  expect_equal(descrip_out[2,5], min(a[d == 0]))
  expect_equal(descrip_out[3,5], min(a[d == 1]))
  expect_equal(descrip_out[1,6], unname(quantile(a, 0.25))) # check 25th percentiles
  expect_equal(descrip_out[2,6], unname(quantile(a[d == 0], 0.25)))
  expect_equal(descrip_out[3,6], unname(quantile(a[d == 1], 0.25)))
  expect_equal(descrip_out[1,7], unname(quantile(a, 0.5))) # check medians
  expect_equal(descrip_out[2,7], unname(quantile(a[d == 0], 0.5)))
  expect_equal(descrip_out[3,7], unname(quantile(a[d == 1], 0.5)))
  expect_equal(descrip_out[1,8], unname(quantile(a, 0.75))) # check 75th percentiles
  expect_equal(descrip_out[2,8], unname(quantile(a[d == 0], 0.75)))
  expect_equal(descrip_out[3,8], unname(quantile(a[d == 1], 0.75)))
  expect_equal(descrip_out[1,9], max(a)) # check maxs
  expect_equal(descrip_out[2,9], max(a[d == 0])) 
  expect_equal(descrip_out[3,9], max(a[d == 1])) 
  expect_equal(unname(descrip_out[,10]), rep(Inf, 3)) # check restriction (should be Inf if none specified)
  #expect_equal(is.na(descrip_out[1,11])) # check firstEvent
  expect_true(is.na(descrip_out[1,12])) # check lastEvent
  expect_true(is.na(descrip_out[2,12]))
  expect_true(is.na(descrip_out[3,12]))
  expect_equal(unname(descrip_out[,13]), rep(0, 3)) # check isDate
})

### list - stratified

a <- rnorm(10,0,1)
d <- c(rep(1,6),rep(0,4))
lst <- list(a)
descrip_out <- descrip(lst, strata = d)

# Test relies on the ordering of the columns remaining the same
test_that("descrip() returns correct numbers for stratified list", {
  expect_s3_class(descrip_out, "uDescriptives")
  expect_equal(unname(descrip_out[,1]), c(10, 4, 6)) # check N
  expect_equal(unname(descrip_out[,2]), rep(0, 3)) # check missing values
  expect_equal(descrip_out[1,3], mean(a)) # check means
  expect_equal(descrip_out[2,3], mean(a[d == 0]))
  expect_equal(descrip_out[3,3], mean(a[d == 1]))
  expect_equal(descrip_out[1,4], sd(a)) # check SDs
  expect_equal(descrip_out[2,4], sd(a[d == 0]))
  expect_equal(descrip_out[3,4], sd(a[d == 1]))
  expect_equal(descrip_out[1,5], min(a)) # check mins
  expect_equal(descrip_out[2,5], min(a[d == 0]))
  expect_equal(descrip_out[3,5], min(a[d == 1]))
  expect_equal(descrip_out[1,6], unname(quantile(a, 0.25))) # check 25th percentiles
  expect_equal(descrip_out[2,6], unname(quantile(a[d == 0], 0.25)))
  expect_equal(descrip_out[3,6], unname(quantile(a[d == 1], 0.25)))
  expect_equal(descrip_out[1,7], unname(quantile(a, 0.5))) # check medians
  expect_equal(descrip_out[2,7], unname(quantile(a[d == 0], 0.5)))
  expect_equal(descrip_out[3,7], unname(quantile(a[d == 1], 0.5)))
  expect_equal(descrip_out[1,8], unname(quantile(a, 0.75))) # check 75th percentiles
  expect_equal(descrip_out[2,8], unname(quantile(a[d == 0], 0.75)))
  expect_equal(descrip_out[3,8], unname(quantile(a[d == 1], 0.75)))
  expect_equal(descrip_out[1,9], max(a)) # check maxs
  expect_equal(descrip_out[2,9], max(a[d == 0])) 
  expect_equal(descrip_out[3,9], max(a[d == 1])) 
  expect_equal(unname(descrip_out[,10]), rep(Inf, 3)) # check restriction (should be Inf if none specified)
  #expect_equal(is.na(descrip_out[1,11])) # check firstEvent
  expect_true(is.na(descrip_out[1,12])) # check lastEvent
  expect_true(is.na(descrip_out[2,12]))
  expect_true(is.na(descrip_out[3,12]))
  expect_equal(unname(descrip_out[,13]), rep(0, 3)) # check isDate
})

### variable - Date

a <- as.Date(c("1996-01-25", "1995-07-16", "2019-03-26", NA))
descrip_out <- descrip(a)

test_that("descrip() handles Date variables and missing values correctly", {
  expect_s3_class(descrip_out, "uDescriptives")
  expect_equal(unname(descrip_out[,1]), 4) # check N
  expect_equal(unname(descrip_out[,2]), 1) # check missing values
  expect_equal(descrip_out[1,3], as.numeric(mean(a, na.rm = TRUE))) # check means
  expect_equal(descrip_out[1,4], as.numeric(sd(a, na.rm = TRUE))) # check SDs
  expect_equal(descrip_out[1,5], as.numeric(min(a, na.rm = TRUE))) # check mins
  expect_equal(descrip_out[1,6], unname(quantile(unclass(a), 0.25, na.rm = TRUE))) # check 25th percentiles
  expect_equal(descrip_out[1,7], unname(quantile(unclass(a), 0.5, na.rm = TRUE))) # check medians
  expect_equal(descrip_out[1,8], unname(quantile(unclass(a), 0.75, na.rm = TRUE))) # check 75th percentiles
  expect_equal(descrip_out[1,9], as.numeric(max(a, na.rm = TRUE))) # check maxs
  expect_equal(unname(descrip_out[,10]), Inf) # check restriction (should be Inf if none specified)
  #expect_equal(is.na(descrip_out[1,11])) # check firstEvent
  expect_true(is.na(descrip_out[1,12])) # check lastEvent
  expect_equal(unname(descrip_out[,13]), 1) # check isDate
})

### variable - Surv

library(survival)
a <- Surv(aml$time, aml$status)
descrip_out <- descrip(a)
KM_curve_df <- rigr:::KM(a) # relies on KM function being correct, which it should be

test_that("descrip() handles Surv variables correctly", {
  expect_s3_class(descrip_out, "uDescriptives")
  expect_equal(unname(descrip_out[,1]), 23) # check N
  expect_equal(unname(descrip_out[,2]), 0) # check missing values
  
  # calculate E[X] and E[X^2] for checking restricted mean survival time and standard deviation
  Ex <- sum(diff(KM_curve_df$t) * KM_curve_df$S[-19])
  Ex2 <- sum(diff(KM_curve_df$t^2) * KM_curve_df$S[-19])
    
  expect_equal(descrip_out[1,3], Ex) # check means
  expect_equal(descrip_out[1,4], sqrt(Ex2 - Ex^2)) # check SDs
  expect_equal(descrip_out[1,5], min(a[,1])) # check mins
  expect_equal(descrip_out[1,6], KM_curve_df$t[which((KM_curve_df$S - 0.75) < 0)[1]]) # check 25th percentiles
  expect_equal(descrip_out[1,7], KM_curve_df$t[which((KM_curve_df$S - 0.5) < 0)[1]]) # check medians
  expect_equal(descrip_out[1,8], KM_curve_df$t[which((KM_curve_df$S - 0.25) < 0)[1]]) # check 75th percentiles
  expect_equal(descrip_out[1,9], max(a[,1])) # check maxs
  expect_equal(unname(descrip_out[,10]), max(a[,1])) # check restriction (should be latest event)
  
  # events only dataframe
  KM_events_df <- KM_curve_df[KM_curve_df$censored == 0 & KM_curve_df$events != 0,]
  
  expect_equal(descrip_out[1,11], KM_events_df$t[1]) # check firstEvent
  expect_equal(descrip_out[1,12], KM_events_df$t[nrow(KM_events_df)]) # check lastEvent
  expect_equal(unname(descrip_out[,13]), 0) # check isDate
})

### unnamed list adds .V# to end of variable name

descrip_out <- descrip(list(c(1,2,3,4,5)))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(rownames(descrip_out),
               "list(c(1, 2, 3, 4, 5)).V1:  ")
})

### strata list

a <- rnorm(10,0,1)
d <- c(rep(1,6),rep(0,4))
d2 <- c(rep(1,3), rep(0,7))
df <- data.frame(a)
descrip_out <- descrip(df, strata = list(d, d2))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(nrow(descrip_out),
               4)
  expect_equal(unname(descrip_out[,"N"]),
               c(10,4,3,3))
})


### factor variable
descrip_out <- descrip(factor(mri$weight))
a <- as.numeric(factor(mri$weight))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Mean"],
               mean(a))
  expect_equal(descrip_out[," Min"],
               min(a))
  expect_equal(descrip_out[," Mdn"],
               median(a))
  expect_equal(descrip_out[," Max"],
               max(a))
})

### above

a <- mri$weight
descrip_out <- descrip(a, above = 150)

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr>150"],
               length(which(a>150))/length(a))
})

### below

descrip_out <- descrip(a, below = 140)

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr<140"],
               length(which(a<140))/length(a))
})

### labove

descrip_out <- descrip(a, labove = 143)

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr>=143"],
               length(which(a>=143))/length(a))
})

### rbelow

descrip_out <- descrip(a, rbelow = 143)

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr<=143"],
               length(which(a<=143))/length(a))
})

### lbetween

descrip_out <- descrip(a, lbetween = c(100,150))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr<100"],
               length(which(a<100))/length(a))
  expect_equal(descrip_out[,"Pr[100,150)"],
               length(which(a>=100 & a<150))/length(a))
  expect_equal(descrip_out[,"Pr>=150"],
               length(which(a>=150))/length(a))
})

### rbetween

descrip_out <- descrip(a, rbetween = c(100,150))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr<=100"],
               length(which(a<=100))/length(a))
  expect_equal(descrip_out[,"Pr(100,150]"],
               length(which(a>100 & a<=150))/length(a))
  expect_equal(descrip_out[,"Pr>150"],
               length(which(a>150))/length(a))
})

### interval

descrip_out <- descrip(a, interval = matrix(c(80,90,100,150), nrow = 2, byrow = TRUE))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr( 80, 90)"],
               length(which(a<90 & a>80))/length(a))
  expect_equal(descrip_out[,"Pr(100,150)"],
               length(which(a>100 & a<150))/length(a))
})

### linterval

descrip_out <- descrip(a, linterval = matrix(c(80,90,100,150), nrow = 2, byrow = TRUE))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr[ 80, 90)"],
               length(which(a<90 & a>=80))/length(a))
  expect_equal(descrip_out[,"Pr[100,150)"],
               length(which(a>=100 & a<150))/length(a))
})

### lrinterval

descrip_out <- descrip(a, lrinterval = matrix(c(80,90,100,150), nrow = 2, byrow = TRUE))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr[ 80, 90]"],
               length(which(a<=90 & a>=80))/length(a))
  expect_equal(descrip_out[,"Pr[100,150]"],
               length(which(a>=100 & a<=150))/length(a))
})

### rinterval

descrip_out <- descrip(a, rinterval = matrix(c(80,90,100,150), nrow = 2, byrow = TRUE))

test_that("descrip() adds .V# to end of unnamed variables", {
  expect_equal(descrip_out[,"Pr( 80, 90]"],
               length(which(a<=90 & a>80))/length(a))
  expect_equal(descrip_out[,"Pr(100,150]"],
               length(which(a>100 & a<=150))/length(a))
})





