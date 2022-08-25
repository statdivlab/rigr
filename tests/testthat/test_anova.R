### error handling

# test doesn't have to be deterministic
dat <- data.frame(y = rnorm(100), x = rnorm(100), z = rnorm(100))
reg_null <- regress("mean", y ~ x, data = dat, robustSE = FALSE)
reg_full <- regress("mean", y ~ x + z, data = dat, robustSE = FALSE)
lm_null <- stats::lm(y ~ x, data = dat)
lm_full <- stats::lm(y ~ x + z, data = dat)


test_that("anova.uRegress() throws an error if at least one of the two input 
          objects is not of class uRegress", {
  expect_error(anova(reg_null, lm_full), "uRegress objects must be entered!")
})


test_that("anova.uRegress() throws an error if robust SEs requested but were not used in regress()", {
  expect_error(anova(reg_null, reg_full, robustSE = TRUE),
               "uRegress objects must be created with robust standard errors.")
})


test_that("anova.uRegress() throws an error if the specified test is neither Wald nor LRT", {
  expect_error(anova(reg_null, reg_full, robustSE = FALSE, test="score"), 
               "Only Wald and LRT tests are available.")
  expect_error(anova(reg_null, reg_full, robustSE = FALSE, test="wald"), 
               "Only Wald and LRT tests are available.")
  expect_error(anova(reg_null, reg_full, robustSE = FALSE, test="lrt"), 
               "Only Wald and LRT tests are available.")
})


library(survival)
data(mri)
hazard_reg_null <- regress("hazard", Surv(obstime, death)~age, data=mri)
hazard_reg_full <- regress("hazard", Surv(obstime, death)~age+height+weight, data=mri)
rate_reg_full <- regress("rate", obstime ~ age+height+weight, data = mri, robustSE = FALSE)


test_that("anova.uRegress() throws an error if the two input objects are 
           regressions with different fnctls", {
            expect_error(anova(hazard_reg_null, reg_full, robustSE = FALSE), 
                         "uRegress objects must be created with the same fnctl!")
            expect_error(anova(reg_null, hazard_reg_full, robustSE = FALSE), 
                         "uRegress objects must be created with the same fnctl!")
          })

hazard_reg_false_full <- regress("hazard", Surv(obstime, death)~height+race, data=mri)

# error if wald + (symbolically) non-nested
test_that("anova.uRegress() throws an error if the two input models are not
          symbolically nested when using Wald test", {
             expect_error(anova(hazard_reg_null, hazard_reg_false_full, robustSE = FALSE,
                                test = "Wald"), 
                          "Only LRT is supported when the input models are not symbolically nested.")
           })

# error if wald + (symbolically) non-nested
test_that("anova.uRegress() throws a warning if the two input models are not
          symbolically nested when using LRT test", {
            expect_warning(anova(hazard_reg_null, hazard_reg_false_full, robustSE = FALSE,
                               test = "LRT"), 
        "The two models do not appear to be nested -- double check the input.")
          })

### test for correctness in the linear regression case
anova_lm_1 <- anova(reg_null, reg_full, robustSE = FALSE, test = "Wald")
anova_lm_2 <- stats::anova(lm_null, lm_full)

# first check the non robust version is correct
test_that("anova.uRegress() works with two nested models with fnctl=mean", {
  expect_equal(anova_lm_1$printMat[2], 1)
  expect_equal(anova_lm_1$printMat[1], anova_lm_2$`F`[2], 
               tolerance = 1e-4)
  expect_equal(anova_lm_1$printMat[4], anova_lm_2$`Pr(>F)`[2], 
               tolerance = 1e-4)
})

### test for correctness in the glm case
mri$sex_bin <- ifelse(mri$sex == "Female", 1, 0)
odds_reg_null <- regress("odds", sex_bin ~ atrophy, data = mri)
odds_reg_full <- regress("odds", sex_bin ~ atrophy+weight+height, data = mri)
log_reg_null <- glm(sex_bin ~ atrophy, data = mri, family = binomial())
log_reg_full <- glm(sex_bin ~ atrophy+weight+height, data = mri, family = binomial())

anova_glm_1 <- anova(odds_reg_null, odds_reg_full, robustSE = FALSE, test = "LRT")
anova_glm_2 <- anova(log_reg_null, log_reg_full, test = "LRT")

test_that("anova.uRegress() works with two nested models with fnctl=odds", {
  expect_equal(anova_glm_1$printMat[2], 2)
  expect_equal(anova_glm_1$printMat[1], anova_glm_2$`Deviance`[2], 
               tolerance = 1e-4)
  expect_equal(log(anova_glm_1$printMat[3]), log(anova_glm_2$`Pr(>Chi)`[2]), 
               tolerance = 1e-6)
})

### test for correctness in the coxph/hazard regression case
coxph_reg_null <- coxph(Surv(obstime, death)~age, data=mri)
coxph_reg_full <- coxph(Surv(obstime, death)~age+height+weight, data=mri)



anova_coxph_1 <- anova(hazard_reg_null, hazard_reg_full, robustSE = FALSE, test = "LRT")
anova_coxph_2 <- anova(coxph_reg_null, coxph_reg_full, test = "Chisq")


test_that("anova.uRegress() works with two nested models with fnctl=survival", {
  expect_equal(anova_coxph_1$printMat[2], 2)
  expect_equal(anova_coxph_1$printMat[1], anova_coxph_2$`Chisq`[2], 
               tolerance = 1e-4)
  #expect_equal(anova_coxph_1$printMat[3], anova_coxph_2$`P(>|Chi|)`[2], 
  #             tolerance = 1e-4)
})




