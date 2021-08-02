### error handling

data(mri)

test_that("regress() throws error if formula is not specified", {
  expect_error(regress("mean", data = mri), 
               "You must enter a formula")
})

test_that("regress() throws error if fnctl is not supported", {
  expect_error(regress("blah", atrophy~age, data = mri), 
               "unsupported functional")
})

test_that("regress() throws error if fnctl = 'hazard'", {
  expect_error(regress("hazard", atrophy~age, data = mri), 
               "proportional hazards regression no longer supported")
})

# if "hazard" is supported in the future, this error should throw
# test_that("regress() throws error if fnctl = 'hazard' and intercept = TRUE", {
#   expect_error(regress("hazard", atrophy~age, intercept = TRUE, data = mri), 
#                "proportional hazards regression no longer supported")
# })



