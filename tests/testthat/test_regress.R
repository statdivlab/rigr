### error handling

data(mri)
mri$sex_bin <- ifelse(mri$sex == "Male",1,0)

test_that("regress() throws error if formula is not specified", {
  expect_error(regress("mean", data = mri), 
               "You must enter a formula")
})

test_that("regress() throws error if fnctl is not supported", {
  expect_error(regress("blah", atrophy~age, data = mri), 
               "unsupported functional")
  expect_error(regress("hazard", atrophy~age, data = mri), # make sure hazard is unsupported now
               "unsupported functional")
  expect_error(regress("mea", atrophy~age, data = mri), # shortened strings no longer supported
               "unsupported functional")
})

test_that("regress() throws error if fnctl = 'geometric mean', replaceZeroes = FALSE, and zeroes are present in outcome", {
  expect_error(regress("geometric mean", packyrs~age, data = mri, replaceZeroes = FALSE), 
               "replaceZeroes cannot be false if fnctl = 'geometric mean' and y contains any zeroes")
})

test_that("regress() throws error if weights are not numeric", {
  expect_error(regress("mean", atrophy~age, data = mri, weights = as.character(1:nrow(mri))), 
               "'weights' must be a numeric vector")
  expect_error(regress("odds", sex_bin~age, data = mri, weights = as.character(1:nrow(mri))), 
               "'weights' must be a numeric vector")
})

test_that("regress() throws error if weights are negative", {
  expect_error(regress("mean", atrophy~age, data = mri, weights = -(1:nrow(mri))), 
               "negative weights not allowed")
  expect_error(regress("odds", sex_bin~age, data = mri, weights = -(1:nrow(mri))), 
               "negative weights not allowed")
})

test_that("regress() throws error if weights is not the same length as nrow(data)", {
  expect_error(regress("mean", atrophy~age, data = mri, weights = 1), 
               "Response variable and weights must be of same length")
})

test_that("regress() throws error if subset is not the same length as nrow(data)", {
  expect_error(regress("mean", atrophy~age, data = mri, subset = 1), 
               "Response variable and subsetting variable must be of same length")
})

test_that("regress() throws error if method is neither a function nor string", {
  expect_error(regress("mean", atrophy~age, data = mri, method = 1), 
               "invalid 'method' argument")
})


### warning handling

test_that("regress() gives warning if fnctl != 'geometric mean' and replaceZeroes != FALSE", {
  expect_warning(regress("mean", packyrs ~ age, data = mri, replaceZeroes = TRUE), 
               "replaceZeroes does not do anything for this fnctl, zeroes will not be replaced")
  expect_warning(regress("mean", packyrs ~ age, data = mri, replaceZeroes = 7), 
                 "replaceZeroes does not do anything for this fnctl, zeroes will not be replaced")
})

test_that("regress() gives method warnings", {
  expect_warning(regress("mean", packyrs ~ age, data = mri, method = "glm.fit"), 
                 "method = 'glm.fit' is not supported. Using 'qr'")
  # expect_warning(regress("rate", packyrs ~ age, data = mri, method = "blah"), 
  #                "")
})


### linear regression output

mod_rigr <- regress("mean", atrophy ~ age, data = mri)
mod_lm <- lm(data = mri, atrophy ~ age)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 2, lower.tail = FALSE)

test_that("regress() returns same output as lm()", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_lm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_lm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_lm_robust_se)
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%L"],
               mod_lm_robust_ci_lower)
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%H"],
               mod_lm_robust_ci_higher)
  # t value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"],
               mod_lm$coefficients/ mod_lm_robust_se)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"],
               mod_lm_robust_p)
})

### logistic regression output, useFdstn = FALSE

mod_rigr <- regress("odds", sex_bin ~ age, data = mri, useFdstn = FALSE)
mod_glm <- glm(data = mri, sex_bin ~ age, family = binomial(link = "logit"))
mod_glm_robust_se <- sqrt(diag(sandwich::sandwich(mod_glm, adjust = TRUE)))
mod_glm_robust_ci_lower <- mod_glm$coefficients + qnorm((1 - 0.95)/2) * mod_glm_robust_se
mod_glm_robust_ci_higher <- mod_glm$coefficients - qnorm((1 - 0.95)/2) * mod_glm_robust_se
mod_glm_robust_p <- 2 * pnorm(abs(mod_glm$coefficients/ mod_glm_robust_se), lower.tail = FALSE)

test_that("regress() returns same output as glm() for logistic regression with useFdstn = FALSE", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_glm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_glm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_glm_robust_se)
  # e^Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(Est)"],
               exp(mod_glm$coefficients))
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%L)"],
               exp(mod_glm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%H)"],
               exp(mod_glm_robust_ci_higher))
  # z value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "z value"],
               mod_glm$coefficients/ mod_glm_robust_se)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|z|)"],
               mod_glm_robust_p)
})

### logistic regression output, useFdstn = TRUE

mod_rigr <- regress("odds", sex_bin ~ age, data = mri, useFdstn = TRUE)
mod_glm <- glm(data = mri, sex_bin ~ age, family = binomial(link = "logit"))
mod_glm_robust_se <- sqrt(diag(sandwich::sandwich(mod_glm, adjust = TRUE)))
mod_glm_robust_ci_lower <- mod_glm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_glm_robust_se
mod_glm_robust_ci_higher <- mod_glm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_glm_robust_se
mod_glm_robust_p <- 2 * pt(abs(mod_glm$coefficients/ mod_glm_robust_se), df = nrow(mri) - 2, lower.tail = FALSE)

test_that("regress() returns same output as glm() for logistic regression with useFdstn = TRUE", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_glm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_glm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_glm_robust_se)
  # e^Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(Est)"],
               exp(mod_glm$coefficients))
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%L)"],
               exp(mod_glm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%H)"],
               exp(mod_glm_robust_ci_higher))
  # z value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"],
               mod_glm$coefficients/ mod_glm_robust_se)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"],
               mod_glm_robust_p)
})

### poisson regression output, useFdstn = FALSE

mod_rigr <- regress("rate", yrsquit ~ age, data = mri, useFdstn = FALSE)
mod_glm <- glm(data = mri, yrsquit ~ age, family = poisson(link = "log"))
mod_glm_robust_se <- sqrt(diag(sandwich::sandwich(mod_glm, adjust = TRUE)))
mod_glm_robust_ci_lower <- mod_glm$coefficients + qnorm((1 - 0.95)/2) * mod_glm_robust_se
mod_glm_robust_ci_higher <- mod_glm$coefficients - qnorm((1 - 0.95)/2) * mod_glm_robust_se
mod_glm_robust_p <- 2 * pnorm(abs(mod_glm$coefficients/ mod_glm_robust_se), lower.tail = FALSE)

test_that("regress() returns same output as glm() for poisson regression with useFdstn = FALSE", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_glm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_glm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_glm_robust_se)
  # e^Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(Est)"],
               exp(mod_glm$coefficients))
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%L)"],
               exp(mod_glm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%H)"],
               exp(mod_glm_robust_ci_higher))
  # z value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "z value"],
               mod_glm$coefficients/ mod_glm_robust_se)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|z|)"],
               mod_glm_robust_p)
})

### poisson regression output, useFdstn = TRUE

mod_rigr <- regress("rate", yrsquit ~ age, data = mri, useFdstn = TRUE)
mod_glm <- glm(data = mri, yrsquit ~ age, family = poisson(link = "log"))
mod_glm_robust_se <- sqrt(diag(sandwich::sandwich(mod_glm, adjust = TRUE)))
mod_glm_robust_ci_lower <- mod_glm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_glm_robust_se
mod_glm_robust_ci_higher <- mod_glm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_glm_robust_se
mod_glm_robust_p <- 2 * pt(abs(mod_glm$coefficients/ mod_glm_robust_se), df = nrow(mri) - 2, lower.tail = FALSE)

test_that("regress() returns same output as glm() for poisson regression with useFdstn = TRUE", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_glm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_glm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_glm_robust_se)
  # e^Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(Est)"],
               exp(mod_glm$coefficients))
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%L)"],
               exp(mod_glm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%H)"],
               exp(mod_glm_robust_ci_higher))
  # z value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"],
               mod_glm$coefficients/ mod_glm_robust_se)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"],
               mod_glm_robust_p)
})

### geometric mean 

mod_rigr <- regress("geometric mean", atrophy ~ age, data = mri)
mod_lm <- lm(data = mri, log(atrophy) ~ age)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 2, lower.tail = FALSE)

test_that("regress() returns same output as lm() for fnctl = 'geometric mean'", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_lm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_lm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_lm_robust_se)
  # e^Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(Est)"],
               exp(mod_lm$coefficients))
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%L)"],
               exp(mod_lm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%H)"],
               exp(mod_lm_robust_ci_higher))
  # z value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"],
               mod_lm$coefficients/ mod_lm_robust_se)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"],
               mod_lm_robust_p)
})

### interaction terms in lms
fev_df <- read.table("http://www.emersonstatistics.com/datasets/fev.txt",header=TRUE)
mod_rigr <- regress("mean",fev~height*sex, fev_df)
mod_lm <- lm(fev ~ height * sex, fev_df)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(fev_df) - 4) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(fev_df) - 4) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(fev_df) - 4, lower.tail = FALSE)

test_that("regress() returns same output as lm() for when interaction terms included", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_lm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_lm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_lm_robust_se)
  # z value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"],
               mod_lm$coefficients/ mod_lm_robust_se)
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%L"],
               mod_lm_robust_ci_lower)
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%H"],
               mod_lm_robust_ci_higher)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"],
               mod_lm_robust_p)
})

### utilization of U

### various regress arguments

# replaceZeroes = TRUE, fnctl = 'geometric mean'

mod_rigr <- regress("geometric mean", packyrs ~ age, data = mri, replaceZeroes = TRUE)
replace_val <- sort(unique(mri$packyrs))[2]/2
#replace_val <- TRUE
mri_temp <- mri
mri_temp$packyrs <- log(ifelse(mri_temp$packyrs == 0, replace_val, mri_temp$packyrs))
mod_lm <- lm(data = mri_temp, packyrs ~ age)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
# - 3 because there's one missing observation
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 3) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 3) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 3, lower.tail = FALSE)

test_that("regress() returns same output as lm() for fnctl = 'geometric mean', replaceZeroes = TRUE", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_lm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_lm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_lm_robust_se)
  # e^Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(Est)"],
               exp(mod_lm$coefficients))
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%L)"],
               exp(mod_lm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%H)"],
               exp(mod_lm_robust_ci_higher))
  # t value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"],
               mod_lm$coefficients/ mod_lm_robust_se)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"],
               mod_lm_robust_p)
})

# replaceZeroes = value, fnctl = 'geometric mean'

mod_rigr <- regress("geometric mean", packyrs ~ age, data = mri, replaceZeroes = 7)
replace_val <- 7
mri_temp <- mri
mri_temp$packyrs <- log(ifelse(mri_temp$packyrs == 0, replace_val, mri_temp$packyrs))
mod_lm <- lm(data = mri_temp, packyrs ~ age)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
# - 3 because there's one missing observation
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 3) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 3) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 3, lower.tail = FALSE)

test_that("regress() returns same output as lm() for fnctl = 'geometric mean', replaceZeroes = TRUE", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_lm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_lm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_lm_robust_se)
  # e^Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(Est)"],
               exp(mod_lm$coefficients))
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%L)"],
               exp(mod_lm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%H)"],
               exp(mod_lm_robust_ci_higher))
  # t value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"],
               mod_lm$coefficients/ mod_lm_robust_se)
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"],
               mod_lm_robust_p)
})

# utilization of U for F-tests

# categorical predictor with > 2 levels

# interaction term without using U function

# conf.level not 0.95

# can specify factors in different ways

# these three models should return the same coefficients
mod1 <- regress("mean", fev ~ height+factor(sex),data=fev_df) 
fev_df$fsex <- factor(fev_df$sex)
mod2 <- regress("mean", fev ~ height+fsex, data=fev_df) 
mod3 <- lm(fev ~ height + factor(sex), data = fev_df)

test_that("can input factor variables into regress in multiple ways", {
  # Both rigr models should be the same
  expect_equal(unname(mod1$coefficients[,1]),
               unname(mod2$coefficients[,1]))
  # Both rigr models should be the same as an lm call
  expect_equal(unname(mod1$coefficients[,1]),
               unname(mod3$coefficients))
})





