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
  #expect_error(regress("hazard", atrophy~age, data = mri), # make sure hazard is unsupported now
  #             "unsupported functional")
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

test_that("regress() throws error if formula includes 'lspline'", {
  expect_error(regress("mean", atrophy~age + lspline(sex), data = mri), 
               "'lspline' functionality no longer supported")
})

test_that("polynomial() throws errors if degree misspecified", {
  expect_error(regress("mean", atrophy ~ polynomial(age, degree = 1), data = mri), 
               "inappropriate degree of polynomial")
  expect_error(regress("mean", atrophy ~ polynomial(age, degree = c(1,2)), data = mri), 
               "polynomial degree must be a single number")
})

test_that("regress() throws error if fnctl=`hazard` but the outcome is not a survival object", {
  expect_error(regress("hazard", atrophy~age, data = mri), 
               "Response must be a survival object")
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

### variables can be logged

mod_rigr <- regress("mean", atrophy ~ log(age), data = mri)
mod_lm <- lm(data = mri, atrophy ~ log(age))
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

### test cox proportional hazards regression model
# Create the simplest test data set 
test1 <- data.frame(time=c(4,3,1,1,2,2,3), 
              status=c(1,1,1,0,1,1,0), 
              x=c(0,2,1,1,1,0,0), 
              sex=c(0,0,0,0,1,1,1)) 
# Fit a stratified model without F distribution or robust SE
mod_coxph <- survival::coxph(Surv(time, status) ~ x + strata(sex), test1)
mod_rigr <- regress("hazard", Surv(time, status) ~ x + strata(sex), data = test1, 
                    useFdstn = FALSE, robustSE = FALSE)

test_that("regress() returns same output as coxph() for fnctl = hazard", {
  # Estimate
  expect_equal(unname(mod_rigr$augCoefficients[, colnames(mod_rigr$augCoefficients) == "Estimate"]),
               unname(mod_coxph$coefficients))
  # naive SE
  expect_equal(as.vector(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "se(coef)"]),
               as.vector(sqrt(mod_coxph$var)))
  
})

# Fit a stratified model without F distribution or robust SE
mod_coxph <- survival::coxph(Surv(obstime, death) ~ sex + age + strata(race), mri)
mod_rigr <- regress("hazard", Surv(obstime, death) ~ sex + age + strata(race), data = mri, 
                    useFdstn = FALSE, robustSE = FALSE)

test_that("regress() returns same output as coxph() for fnctl = hazard", {
  # Estimate
  expect_equal(unname(mod_rigr$augCoefficients[, colnames(mod_rigr$augCoefficients) == "Estimate"]),
               unname(mod_coxph$coefficients))
  # naive SE
  expect_equal(as.vector(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "se(coef)"]),
               as.vector(sqrt(diag(mod_coxph$var))))
  
})

mri2 <- read.table("https://rct-design.com/TeachingMaterials/Datasets/mri.txt", header = T)
mri2$obstime_yrs <- mri2$obstime/365.25
mri2$ldlcat <- cut(mri2$ldl, breaks=c(0, 70, 100, 130, 160, 190, 250), right=FALSE)
mri2$surv <- Surv(mri2$obstime_yrs, mri2$death)
mod_rigr_missing <-  regress("hazard", surv~factor(ldlcat), data = mri2)
mri_rmna <- mri2[!is.na(mri2$ldlcat), ]
mod_rigr_subset <- regress("hazard", surv~factor(ldlcat), data = mri_rmna)

test_that("regress() properly removes missing data for fnctl = hazard", {
  # Estimate
  expect_equal(unname(mod_rigr_missing$augCoefficients[, colnames(mod_rigr_missing$augCoefficients) == "Estimate"]),
               unname(mod_rigr_subset$augCoefficients[, colnames(mod_rigr_subset$augCoefficients) == "Estimate"]))
  # naive SE
  expect_equal(as.vector(mod_rigr_missing$coefficients[, colnames(mod_rigr_missing$coefficients) == "se(coef)"]),
               as.vector(mod_rigr_subset$coefficients[, colnames(mod_rigr_subset$coefficients) == "se(coef)"]))
  
})

### interaction terms in lms
data(fev)
fev_df <- fev
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

mod_rigr <- regress("mean", atrophy ~ age + U(smoke = ~packyrs + yrsquit), data = mri)
mod_lm <- lm(atrophy ~ age + packyrs + yrsquit, data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 5) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 5) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 5, lower.tail = FALSE)

R <- matrix(c(0,0,1,0,
              0,0,0,1), nrow = 2, byrow = TRUE)
theta_hat <- mod_lm$coefficients
n <- 2
V_hat <- sandwich::sandwich(mod_lm, adjust = TRUE)
smoke_F <- as.vector(t(R %*% theta_hat) %*% solve(R %*% (V_hat) %*% t(R)) %*% (R %*% theta_hat) / n)
smoke_p <- 1 - pf(smoke_F, 2, nrow(mri) - 5)


test_that("regress() returns same output as lm() when doing F-test using U() function", {
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
  # F-stat for smoke
  expect_equal(mod_rigr$augCoefficients["smoke","F stat"],
               smoke_F)
  # p-value for smoke
  expect_equal(mod_rigr$augCoefficients["smoke","Pr(>F)"],
               smoke_p)
})

### Utilization of U() in a glm

mod_rigr <- regress("odds", sex_bin ~ age + U(smoke = ~packyrs + yrsquit), data = mri, useFdstn = FALSE)
mod_glm <- glm(data = mri, sex_bin ~ age + packyrs + yrsquit, family = binomial(link = "logit"))
mod_glm_robust_se <- sqrt(diag(sandwich::sandwich(mod_glm, adjust = TRUE)))
mod_glm_robust_ci_lower <- mod_glm$coefficients + qnorm((1 - 0.95)/2) * mod_glm_robust_se
mod_glm_robust_ci_higher <- mod_glm$coefficients - qnorm((1 - 0.95)/2) * mod_glm_robust_se
mod_glm_robust_p <- 2 * pnorm(abs(mod_glm$coefficients/ mod_glm_robust_se), lower.tail = FALSE)

R <- matrix(c(0,0,1,0,
              0,0,0,1), nrow = 2, byrow = TRUE)
theta_hat <- mod_glm$coefficients
n <- 2
V_hat <- sandwich::sandwich(mod_glm, adjust = TRUE)
smoke_F <- as.vector(t(R %*% theta_hat) %*% solve(R %*% (V_hat) %*% t(R)) %*% (R %*% theta_hat) / n)
smoke_p <- 1 - pchisq(2 * smoke_F, 2)

test_that("regress() returns same output as lm() when doing F-test using U() function", {
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               mod_glm$coefficients)
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_glm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               mod_glm_robust_se)
  # z value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "z value"],
               mod_glm$coefficients/ mod_glm_robust_se)
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%L)"],
               exp(mod_glm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "e(95%H)"],
               exp(mod_glm_robust_ci_higher))
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|z|)"],
               mod_glm_robust_p)
  # F-stat for smoke
  expect_equal(mod_rigr$augCoefficients["smoke","Chi2 stat"],
               smoke_F)
  # p-value for smoke
  expect_equal(mod_rigr$augCoefficients["smoke","Pr(>Chi2)"],
               smoke_p)
})

### replaceZeroes = TRUE, fnctl = 'geometric mean'

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

### replaceZeroes = value, fnctl = 'geometric mean'

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

### replaceZeroes = FALSE, fnctl != 'geometric mean'

mod_rigr1 <- regress("mean", atrophy ~ age, data = mri, replaceZeroes = FALSE)
mod_rigr2 <- regress("mean", atrophy ~ age, data = mri)

test_that("regress() returns same output for replaceZeroes = FALSE and non-specified replaceZeroes for fnctl != 'geometric mean'", {
  # Estimate
  expect_equal(mod_rigr1$augCoefficients,
               mod_rigr2$augCoefficients)
})

### categorical predictor with > 2 levels - coefficient labels

# also ensure that the F-test is done correctly
mod_rigr <- regress("mean", atrophy ~ race + age, data=mri)
mod_lm <- lm(atrophy ~ race + age, data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 5) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 5) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 5, lower.tail = FALSE)

R <- matrix(c(0,1,0,0,0,
              0,0,1,0,0,
              0,0,0,1,0), nrow = 3, byrow = TRUE)
theta_hat <- mod_lm$coefficients
n <- 3
V_hat <- sandwich::sandwich(mod_lm, adjust = TRUE)
race_F <- as.vector(t(R %*% theta_hat) %*% solve(R %*% (V_hat) %*% t(R)) %*% (R %*% theta_hat) / n)
race_p <- 1 - pf(race_F, 3, nrow(mri) - 5)

test_that("regress() F-test and coefficient labels are correct for multi-level categorical variables", {
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
  # F-stat for smoke
  expect_equal(mod_rigr$augCoefficients["race","F stat"],
               race_F)
  # p-value for smoke
  expect_equal(mod_rigr$augCoefficients["race","Pr(>F)"],
               race_p)
  # ensure coefficient names are appropriately indented
  expect_equal(rownames(mod_rigr$augCoefficients),
               c("Intercept","race"," Black"," Subject did not identify White, Black or Asian",
                 " White","age"))
})

### multiple multi-level categorical predictors

mri_temp <- mri
mri_temp$var1 <- sample(c("a","b","c"), nrow(mri), replace = TRUE)
mod_rigr <- regress("mean", atrophy ~ race + var1, data=mri_temp)

test_that("coefficient names appropriately indented in regress() for multiple, multi-level categorical variables", {
  # ensure coefficient names are appropriately indented
  expect_equal(rownames(mod_rigr$augCoefficients),
               c("Intercept","race"," Black"," Subject did not identify White, Black or Asian",
                 " White","var1"," b"," c"))
})

### interaction between multi-level categorical variable & quantitative variable

mod_rigr <- regress("mean", atrophy ~ race*age, data=mri)

test_that("coefficient names appropriately indented in regress() for interaction between multi-level categorical var and quantitative var", {
  # ensure coefficient names are appropriately indented
  expect_equal(rownames(mod_rigr$augCoefficients),
               c("Intercept","race",
                 " Black"," Subject did not identify White, Black or Asian"," White",
                 "age","race:age",
                 " Black:age"," Subject did not identify White, Black or Asian:age"," White:age"))
})

### between two multi-level categorical variables

mod_rigr <- regress("mean", atrophy ~ race*var1, data=mri_temp)

test_that("coefficient names appropriately indented in regress() for interaction between two multi-level categorical vars", {
  # ensure coefficient names are appropriately indented
  expect_equal(rownames(mod_rigr$augCoefficients),
               c("Intercept","race",
                 " Black"," Subject did not identify White, Black or Asian"," White",
                 "var1"," b"," c", "race:var1",
                 " b:Black"," b:Subject did not identify White, Black or Asian",
                 " b:White", " c:Black",
                 " c:Subject did not identify White, Black or Asian", " c:White"))
})

### conf.level not 0.95

### can specify factors in different ways

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

### intercept = FALSE removes intercept from model

mod_rigr <- regress("mean", atrophy ~ age, data = mri, intercept = FALSE)
mod_lm <- lm(data = mri, atrophy ~ age - 1)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 1) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 1) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 1, lower.tail = FALSE)

test_that("intercept = FALSE argument removes intercept from model", {
  # "Intercept" is not in coefficient names
  expect_true(!("Intercept" %in% rownames(mod_rigr$coefficients)))
  # Estimate
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"],
               unname(mod_lm$coefficients))
  # Naive SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"],
               summary(mod_lm)$coefficients[,2])
  # Robust SE
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"],
               unname(mod_lm_robust_se))
  # 95%L (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%L"],
               unname(mod_lm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%H"],
               unname(mod_lm_robust_ci_higher))
  # t value (robust)
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"],
               unname(mod_lm$coefficients/ mod_lm_robust_se))
  # p-value
  expect_equal(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"],
               unname(mod_lm_robust_p))
})

### if method = "model.frame", the model frame is returned

mod_rigr <- regress("mean", atrophy ~ age, data = mri, method = "model.frame")
mod_lm <- lm(data = mri, atrophy ~ age, method = "model.frame")

test_that("method = 'model.frame' returns the same model frame as lm() call", {
  # dimensions of model frame from mod_rigr is the same as mod_lm
  expect_equal(dim(mod_rigr), dim(mod_lm))
  # colnames are the same
  expect_equal(colnames(mod_rigr), colnames(mod_lm))
  # each column is the same
  expect_equal(mod_rigr$atrophy, mod_lm$atrophy)
  expect_equal(mod_rigr$age, mod_lm$age)
})

### U() with only a single variable does not do an additional pointless f-test

mod_rigr <- regress("mean", atrophy ~ height + age + U(ha = ~height:age), data = mri)

test_that("regress() does not add an extra row for an f-test when it doesn't need to", {
  # there should only be four coefficients, not five
  expect_equal(rownames(mod_rigr$augCoefficients), 
               c("Intercept","height","age","height:age"))
  # degrees of freedom should all be 1
  expect_equal(unname(mod_rigr$augCoefficients[,"df"]), rep(1,4))
})

### U() with only a single variable returns the exact same thing as without the U() specification
mod_rigr1 <- regress("mean", atrophy ~ race + age + U(~race:age), data = mri)
mod_rigr2 <- regress("mean", atrophy ~ race + age + race:age, data = mri)

test_that("regress() functions when U() specification only has a single variable", {
  # there should only be four coefficients, not five
  expect_equal(mod_rigr1$augCoefficients, 
               mod_rigr2$augCoefficients)
})

### U() with only a single variable and no other predictors does not throw an error
mod_rigr1 <- regress("mean", atrophy ~ U(~race), data = mri)
mod_rigr2 <- regress("mean", atrophy ~ race, data = mri)

test_that("regress() functions when U() specification only has a single variable and no other predictors", {
  # there should only be four coefficients, not five
  expect_equal(mod_rigr1$augCoefficients, 
               mod_rigr2$augCoefficients)
})

### U() works with two variables specified, one of which is multi-level categorical
mod_rigr <- regress("mean", atrophy ~ age + U(~race + weight), data = mri)
mod_lm <- lm(atrophy ~ age + race + weight, data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 6) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 6) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 6, lower.tail = FALSE)

R <- matrix(c(0,0,1,0,0,0,
              0,0,0,1,0,0,
              0,0,0,0,1,0,
              0,0,0,0,0,1), nrow = 4, byrow = TRUE)
theta_hat <- mod_lm$coefficients
n <- 4
V_hat <- sandwich::sandwich(mod_lm, adjust = TRUE)
smoke_F <- as.vector(t(R %*% theta_hat) %*% solve(R %*% (V_hat) %*% t(R)) %*% (R %*% theta_hat) / n)
smoke_p <- 1 - pf(smoke_F, 4, nrow(mri) - 6)


test_that("regress() returns same output as lm() when doing F-test using U() function, multi-level categorical variable included", {
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
  # F-stat for smoke
  expect_equal(mod_rigr$augCoefficients["U(race + weight)","F stat"],
               smoke_F)
  # p-value for smoke
  expect_equal(mod_rigr$augCoefficients["U(race + weight)","Pr(>F)"],
               smoke_p)
})

### U() works with two variables specified, one of which is multi-level categorical interaction
mod_rigr <- regress("mean", atrophy ~ age + race + U(~race:age + weight), data = mri)
mod_lm <- lm(atrophy ~ age + race + race:age + weight, data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 9) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 9) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 9, lower.tail = FALSE)

R <- matrix(c(0,0,0,0,0,1,0,0,0,
              0,0,0,0,0,0,1,0,0,
              0,0,0,0,0,0,0,1,0,
              0,0,0,0,0,0,0,0,1), nrow = 4, byrow = TRUE)
theta_hat <- mod_lm$coefficients
n <- 4
V_hat <- sandwich::sandwich(mod_lm, adjust = TRUE)
smoke_F <- as.vector(t(R %*% theta_hat) %*% solve(R %*% (V_hat) %*% t(R)) %*% (R %*% theta_hat) / n)
smoke_p <- 1 - pf(smoke_F, 4, nrow(mri) - 9)


test_that("regress() returns same output as lm() when doing F-test using U() function, multi-level categorical variable included", {
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
  # F-stat for smoke
  expect_equal(mod_rigr$augCoefficients["U(race:age + weight)","F stat"],
               smoke_F)
  # p-value for smoke
  expect_equal(mod_rigr$augCoefficients["U(race:age + weight)","Pr(>F)"],
               smoke_p)
})


### dummy works as it's supposed to

mod_rigr <- regress("mean", atrophy ~ dummy(stroke), data = mri)
mri$stroke.1 <- ifelse(mri$stroke == 1, 1, 0)
mri$stroke.2 <- ifelse(mri$stroke == 2, 1, 0)
mod_lm <- lm(atrophy ~ stroke.1 + stroke.2, data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 3) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 3) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 3, lower.tail = FALSE)

R <- matrix(c(0,1,0,
              0,0,1), nrow = 2, byrow = TRUE)
theta_hat <- mod_lm$coefficients
n <- 2
V_hat <- sandwich::sandwich(mod_lm, adjust = TRUE)
smoke_F <- as.vector(t(R %*% theta_hat) %*% solve(R %*% (V_hat) %*% t(R)) %*% (R %*% theta_hat) / n)
smoke_p <- 1 - pf(smoke_F, 2, nrow(mri) - 3)


test_that("regress() returns same output as lm() when doing F-test using U() function, multi-level categorical variable included", {
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
  # F-stat for smoke
  expect_equal(mod_rigr$augCoefficients["dummy(stroke)","F stat"],
               smoke_F)
  # p-value for smoke
  expect_equal(mod_rigr$augCoefficients["dummy(stroke)","Pr(>F)"],
               smoke_p)
})

### dummy works as it's supposed to for binary covariates

mod_rigr <- regress("mean", atrophy ~ dummy(sex), data = mri)
mri$sex.Male <- ifelse(mri$sex == "Female", 0, 1)
mod_lm <- lm(atrophy ~ sex.Male, data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 2, lower.tail = FALSE)

test_that("regress() works with binary dummy variables", {
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

### a few unit tests for dummy

rigr_dum <- dummy(mri$sex, includeAll = TRUE)
my_dum <- cbind(Female = ifelse(mri$sex == "Female", 1, 0),
                Male = ifelse(mri$sex == "Male", 1, 0))

test_that("dummy() returns all columns when includeAll = TRUE", {
  # vectors are the same
  expect_equal(rigr_dum[,1],
               my_dum[,1])
  expect_equal(rigr_dum[,2],
               my_dum[,2])
  # dimnames are returned currently
  expect_equal(dimnames(rigr_dum)[[2]],
               c("Female","Male"))
  
})

# check that reference works appropriately
male_ref <- dummy(mri$sex, reference = "Male") 
fem_ref <- dummy(mri$sex, reference = "Female") 

test_that("dummy() returns all columns when includeAll = TRUE", {
  # vectors are the same
  expect_equal(male_ref[,1],
               my_dum[,1])
  expect_equal(fem_ref[,1],
               my_dum[,2])
})

### weights work as expected

mod_rigr <- regress("mean", atrophy ~ age, data = mri, weights = as.numeric(1:nrow(mri)))
mod_lm <- lm(data = mri, atrophy ~ age, weights = as.numeric(1:nrow(mri)))
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 2) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 2, lower.tail = FALSE)

test_that("regress() returns same output as lm() when weights included", {
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

### polynomial works as it's supposed to

mod_rigr <- regress("mean", atrophy ~ polynomial(age, degree = 2), data = mri)
mri$age_centered <- mri$age - mean(mri$age)
mod_lm <- lm(atrophy ~ age_centered + I(age_centered^2), data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 3) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 3) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 3, lower.tail = FALSE)

R <- matrix(c(0,1,0,
              0,0,1), nrow = 2, byrow = TRUE)
theta_hat <- mod_lm$coefficients
n <- 2
V_hat <- sandwich::sandwich(mod_lm, adjust = TRUE)
smoke_F <- as.vector(t(R %*% theta_hat) %*% solve(R %*% (V_hat) %*% t(R)) %*% (R %*% theta_hat) / n)
smoke_p <- 1 - pf(smoke_F, 2, nrow(mri) - 3)

test_that("regress() returns same output as lm() when polynomial included in predictors", {
  # Estimate
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"]),
               unname(mod_lm$coefficients))
  # Naive SE
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"]),
               unname(summary(mod_lm)$coefficients[,2]))
  # Robust SE
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"]),
               unname(mod_lm_robust_se))
  # z value (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"]),
               unname(mod_lm$coefficients/ mod_lm_robust_se))
  # 95%L (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%L"]),
               unname(mod_lm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%H"]),
               unname(mod_lm_robust_ci_higher))
  # p-value
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"]),
               unname(mod_lm_robust_p))
  # F-stat for smoke
  expect_equal(mod_rigr$augCoefficients["polynomial(age, degree = 2)","F stat"],
               smoke_F)
  # p-value for smoke
  expect_equal(mod_rigr$augCoefficients["polynomial(age, degree = 2)","Pr(>F)"],
               smoke_p)
})

### three-way interaction terms

mod_rigr <- regress("mean", atrophy ~ age*race*weight, data = mri)
mod_lm <- lm(atrophy ~ age*race*weight, data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 16) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 16) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 16, lower.tail = FALSE)

test_that("regress() returns same output as lm() for three-way interaction", {
  # Estimate
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"]),
               unname(mod_lm$coefficients))
  # Naive SE
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"]),
               unname(summary(mod_lm)$coefficients[,2]))
  # Robust SE
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"]),
               unname(mod_lm_robust_se))
  # z value (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"]),
               unname(mod_lm$coefficients/ mod_lm_robust_se))
  # 95%L (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%L"]),
               unname(mod_lm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%H"]),
               unname(mod_lm_robust_ci_higher))
  # p-value
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"]),
               unname(mod_lm_robust_p))
})

### intercept removal can be specified in multiple ways

mod1 <- regress("mean", atrophy ~ -1 + age, data = mri, intercept = TRUE)
mod2 <- regress("mean", atrophy ~ -1 + age, data = mri)
mod3 <- regress("mean", atrophy ~ age, data = mri, intercept = FALSE)

test_that("intercept removal can be done via a '-1' in formula", {
  expect_equal(mod1$coefficients,
               mod2$coefficients)
  expect_equal(mod3$coefficients,
               mod2$coefficients)
})

### polynomial error testing

mod_rigr <- regress("mean", atrophy ~ polynomial(age, degree = 4), data = mri)
mri$age_centered <- mri$age - mean(mri$age)
mod_lm <- lm(atrophy ~ age_centered + I(age_centered^2) + I(age_centered^3) + I(age_centered^4), data = mri)
mod_lm_robust_se <- sqrt(diag(sandwich::sandwich(mod_lm, adjust = TRUE)))
mod_lm_robust_ci_lower <- mod_lm$coefficients + qt((1 - 0.95)/2, df = nrow(mri) - 5) * mod_lm_robust_se
mod_lm_robust_ci_higher <- mod_lm$coefficients - qt((1 - 0.95)/2, df = nrow(mri) - 5) * mod_lm_robust_se
mod_lm_robust_p <- 2 * pt(abs(mod_lm$coefficients/ mod_lm_robust_se), df = nrow(mri) - 5, lower.tail = FALSE)

test_that("polynomial() works with degree > 3", {
  # Estimate
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Estimate"]),
               unname(mod_lm$coefficients))
  # Naive SE
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Naive SE"]),
               unname(summary(mod_lm)$coefficients[,2]))
  # Robust SE
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Robust SE"]),
               unname(mod_lm_robust_se))
  # z value (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "t value"]),
               unname(mod_lm$coefficients/ mod_lm_robust_se))
  # 95%L (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%L"]),
               unname(mod_lm_robust_ci_lower))
  # 95%H (robust)
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "95%H"]),
               unname(mod_lm_robust_ci_higher))
  # p-value
  expect_equal(unname(mod_rigr$coefficients[, colnames(mod_rigr$coefficients) == "Pr(>|t|)"]),
               unname(mod_lm_robust_p))
})

### predict method

newdat <- data.frame(age = rnorm(10,50,5),
                     atrophy = rnorm(10,35,10))
newdat_glm <- data.frame(age = rnorm(10,50,5),
                         sex_bin = sample(c(0,1), size = 10, replace = TRUE))
mod_rigr_lm <- regress("mean", atrophy ~ age, data = mri)
mod_rigr_glm <- regress("odds", sex_bin ~ age, data = mri, useFdstn = FALSE)
mod_lm <- lm(atrophy ~ age, data = mri)
mod_glm <- glm(sex_bin ~ age, data = mri, family = binomial(link = "logit"))
pred_mod_rigr_lm <- predict(mod_rigr_lm, newdata = newdat)[,1]
pred_mod_lm <- predict(mod_lm, newdata = newdat)
pred_mod_rigr_glm <- predict(mod_rigr_glm, newdata = newdat_glm)
pred_mod_glm <- predict(mod_glm, newdata = newdat_glm)

test_that("predict method works for lms, glms", {
  # lm
  expect_equal(pred_mod_rigr_lm,
               pred_mod_lm)
  # glm
  expect_equal(pred_mod_rigr_glm,
               pred_mod_glm)
})

### residuals method

mod_rigr_lm <- regress("mean", atrophy ~ age, data = mri)
sigmahat <- (sum(mod_rigr_lm$residuals^2))/(length(mod_rigr_lm$residuals)-mod_rigr_lm$df[1])

test_that("residuals method works for all type arguments", {
  expect_equal(residuals(mod_rigr_lm),
               mod_rigr_lm$residuals)
  expect_error(residuals(mod_rigr_lm, type = "blah"),
               "The type of residual must either be not entered or must be standardized, studentized, or jackknife.")
  expect_equal(residuals(mod_rigr_lm, type = "jackknife"),
               rstudent(mod_rigr_lm$fit))
  expect_equal(residuals(mod_rigr_lm, type = "studentized"),
               rstandard(mod_rigr_lm$fit))
  expect_equal(residuals(mod_rigr_lm, type = "standardized"),
               (mod_rigr_lm$residuals)/sigmahat)
})

### issue with variables names
## FAILING
# test_that("variables can be called dummy", {
#   set.seed(1)
#   dd <- data.frame(y=rnorm(100), x=rnorm(100))
#   dd$ydummy <- dd$y>0
#   dd$ybinned <- dd$y>0
#   
#   expect_s3_class(regress("mean", ybinned~x, data=dd), "uRegress") # no issue
#   expect_s3_class(regress("mean", ydummy~x, data=dd), "uRegress")  # Error in rep(varname, att$dim[2]) : invalid 'times' argument
# })


### diagnostics

test_that("case diagnostics are correct", {
  mri_reg1 <- regress("mean", atrophy ~ age * sex, data = mri)
  mri_lm1 <- lm(atrophy ~ age * sex, data = mri)
  
  ## these measures should be the same
  expect_true(all(residuals(mri_reg1) == residuals(mri_lm1)))
  expect_true(all(dfbeta(mri_reg1) == dfbeta(mri_lm1)))
  expect_true(all(dfbetas(mri_reg1) == dfbetas(mri_lm1)))
  expect_true(all(cooks.distance(mri_reg1) == cooks.distance(mri_lm1)))
  expect_true(all(hatvalues(mri_reg1) == hatvalues(mri_lm1)))
  
  ## these measures should be perfectly linearly correlated (down to numerical diffs)
  expect_equal(cor(rstandard(mri_reg1),  rstandard(mri_lm1)), 1, tolerance=1e-4) 
  expect_equal(cor(rstudent(mri_reg1),  rstudent(mri_lm1)), 1, tolerance=1e-4) 
  
})

## FAILING
# test_that("harder case diagnostics are finished", {
#   mri_reg1 <- regress("mean", atrophy ~ age * sex, data = mri)
#   mri_lm1 <- lm(atrophy ~ age * sex, data = mri)
#   
#   # still failing
#   # both require lm.influence(model, do.coef = FALSE)$hat and lm.influence(model, do.coef = FALSE)$sigma
#   expect_equal(cor(covratio(mri_reg1),  covratio(mri_lm1)), 1, tolerance=1e-4) 
#   expect_equal(cor(dffits(mri_reg1),  dffits(mri_lm1)), 1, tolerance=1e-4) 
#   expect_s3_class(influence.measures(mri_reg1), "infl") 
#   
# })

test_that("regress works for a long formula", {
  mri_complete <- mri[mri %>% complete.cases, ]
  mri_tte <- survival::Surv(mri_complete$obstime, mri_complete$death)
  mri_complete$mri_tte <- mri_tte
  expect_silent({
    my_regress <- regress("hazard", mri_tte ~ height + weight + race + sex + age + dsst + atrophy +
                          plt + sbp + fev + dsst + atrophy + chf + chf + alcoh, 
                        data=mri_complete )
  })
  expect_equal(nrow(my_regress$coefficients), 14)
  
})

### F-stat and Chi-squared tests are asymptotically equivalent using U(.)

mod1 <- regress("mean", atrophy ~ age + U(smoke = ~packyrs + yrsquit),
                data = mri, useFdstn = TRUE)
mod2 <- regress("mean", atrophy ~ age + U(smoke = ~packyrs + yrsquit),
                data = mri, useFdstn = FALSE)

test_that("F-stat and chi-squared tests are asymptotically equivalennt for U()", {
  expect_equal(mod2$augCoefficients["smoke", "Pr(>Chi2)"] -
                 mod1$augCoefficients["smoke", "Pr(>F)"],
               0,
               tolerance = 0.001) # tolerance to accommodate finite sample approximation
})
