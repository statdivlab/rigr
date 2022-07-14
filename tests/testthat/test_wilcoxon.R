### error handling

y <- rnorm(100)

test_that("wilcoxon() throws error if null.hypoth is not a (finite) scalar", {
  expect_error(wilcoxon(y, null.hypoth = c(1,2)), "'null.hypoth' must be a single number")
  expect_error(wilcoxon(y, null.hypoth = Inf), "'null.hypoth' must be a single number")
})

test_that("wilcoxon() throws error if conf.level is not a number between 0 and 1", {
  expect_error(wilcoxon(y, conf.level = 1.01, conf.int = TRUE), 
               "'conf.level' must be a single number between 0 and 1")
  expect_error(wilcoxon(y, conf.level = Inf, conf.int = TRUE),
               "'conf.level' must be a single number between 0 and 1")
  expect_error(wilcoxon(y, conf.level = c(0.9, 0.95), conf.int = TRUE),
               "'conf.level' must be a single number between 0 and 1")
})

test_that("wilcoxon() throws error if y is non-numeric", {
  expect_error(wilcoxon(c("a", 2, 1)),
               "'y' must be numeric")
})

test_that("wilcoxon() throws error if x is non-numeric in a paired test", {
  expect_error(wilcoxon(var1 = y, var2 = rep("hello world", 100), paired = TRUE),
               "'x' must be numeric")
})

test_that("wilcoxon() throws error y and x have different lengths in a paired test", {
  expect_error(wilcoxon(var1 = y, var2 = rnorm(99), paired = TRUE),
               "'y' and 'x' must have the same length")
})

test_that("wilcoxon() throws error if x is missing in a paired test", {
  expect_error(wilcoxon(var1 = y, paired = TRUE),
               "'x' is missing for paired test")
})

test_that("wilcoxon() throws error if y has zero non-infinite values", {
  expect_error(wilcoxon(var1 = c(Inf)), "not enough finite 'y' observations")
})

test_that("wilcoxon() throws error if x has zero non-infinite values in a paired test", {
  expect_error(wilcoxon(var1 = y, var2 = rep(Inf, 100)), "not enough finite 'x' observations")
})

test_that("wilcoxon() throws error if y values are all identical and CI is requested", {
  expect_error(wilcoxon(var1 = rep(1, 100), conf.int = TRUE), 
               "cannot compute confidence interval when all observations are tied")
})

test_that("wilcoxon() throws error if y and x values are all identical and CI is requested", {
  expect_error(wilcoxon(var1 = rep(100, 100), var2 = rep(100, 100), conf.int = TRUE), 
               "cannot compute confidence interval when all observations are tied")
})


### one-sample test (or two-sample paired test), approximate
set.seed(1)
a <- rnorm(50)
b <- rnorm(50)
d <- a - b

wil1 <- wilcoxon(d, correct = FALSE, conf.int = TRUE)
wil2 <- wilcox.test(d, correct = FALSE, conf.int = TRUE)
wil3 <- wilcoxon(a, b, paired = TRUE, correct = FALSE, conf.int = TRUE)
wil4 <- wilcox.test(a, b, paired = TRUE, correct = FALSE, conf.int = TRUE)

### TO-DO: really I should also be testing other quantities returned, but they mostly aren't provided
### by wilcox.test() so maybe by-hand using online calculator? e.g. vars, z
test_that("wilcoxon() returns correct numbers for one-sample uncorrected test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int, tolerance = 1e-4)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_equal(wil1$table[1,1], sum(d > 0))
  expect_equal(wil1$table[2,1], sum(d < 0))
  expect_equal(wil1$table[3,1], sum(d == 0))
  expect_equal(wil1$table[4,1], length(d))
  expect_equal(wil1$table[1,2], sum(rank(abs(d))[d > 0]))
  expect_equal(wil1$table[2,2], sum(rank(abs(d))[d < 0]))
  expect_equal(wil1$table[3,2], sum(rank(abs(d))[d == 0]))
  expect_equal(wil1$table[4,2], sum(rank(abs(d))))
  expect_equal(wil1$table[1,3], (sum(rank(abs(d))[d > 0]) + sum(rank(abs(d))[d < 0]))/2)
  expect_equal(wil1$table[2,3], (sum(rank(abs(d))[d > 0]) + sum(rank(abs(d))[d < 0]))/2)
  expect_equal(wil1$table[3,3], 0)
  expect_equal(wil1$table[4,3], sum(rank(abs(d))))
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil4$data.name)
  #expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
  expect_equal(wil3$table[1,1], sum(d > 0))
  expect_equal(wil3$table[2,1], sum(d < 0))
  expect_equal(wil3$table[3,1], sum(d == 0))
  expect_equal(wil3$table[4,1], length(d))
  expect_equal(wil3$table[1,2], sum(rank(abs(d))[d > 0]))
  expect_equal(wil3$table[2,2], sum(rank(abs(d))[d < 0]))
  expect_equal(wil3$table[3,2], sum(rank(abs(d))[d == 0]))
  expect_equal(wil3$table[4,2], sum(rank(abs(d))))
  expect_equal(wil3$table[1,3], (sum(rank(abs(d))[d > 0]) + sum(rank(abs(d))[d < 0]))/2)
  expect_equal(wil3$table[2,3], (sum(rank(abs(d))[d > 0]) + sum(rank(abs(d))[d < 0]))/2)
  expect_equal(wil3$table[3,3], 0)
  expect_equal(wil3$table[4,3], sum(rank(abs(d))))
})

wil1 <- wilcoxon(d, correct = TRUE, conf.int = TRUE)
wil2 <- wilcox.test(d, correct = TRUE, conf.int = TRUE)
wil3 <- wilcoxon(a, b, paired = TRUE, correct = TRUE, conf.int = TRUE)
wil4 <- wilcox.test(a, b, paired = TRUE, correct = TRUE, conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample corrected test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil4$data.name)
  #expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, correct = FALSE, alternative = "less", conf.int = TRUE)
wil2 <- wilcox.test(d, correct = FALSE, alternative = "less", conf.int = TRUE)
wil3 <- wilcoxon(a, b, paired = TRUE, correct = FALSE, alternative = "less", conf.int = TRUE)
wil4 <- wilcox.test(a, b, paired = TRUE, correct = FALSE, alternative = "less", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample uncorrected test (left)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil4$data.name)
  #expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil2 <- wilcox.test(d, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil3 <- wilcoxon(a, b, paired = TRUE, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil4 <- wilcox.test(a, b, paired = TRUE, correct = FALSE, alternative = "greater", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample uncorrected test (right)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil4$data.name)
  #expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(d, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil3 <- wilcoxon(a, b, paired = TRUE, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil4 <- wilcox.test(a, b, paired = TRUE, correct = FALSE, conf.int = TRUE, conf.leve = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (uncorrected)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil3$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, correct = TRUE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(d, correct = TRUE, conf.int = TRUE, conf.level = 0.8)
wil3 <- wilcoxon(a, b, paired = TRUE, correct = TRUE, conf.int = TRUE, conf.level = 0.8)
wil4 <- wilcox.test(a, b, paired = TRUE, correct = TRUE, conf.int = TRUE, conf.leve = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (corrected)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, correct = FALSE, conf.int = TRUE, null.hypoth = 0.5)
wil2 <- wilcox.test(d, correct = FALSE, conf.int = TRUE, mu = 0.5)
wil3 <- wilcoxon(a, b, paired = TRUE, correct = FALSE, conf.int = TRUE, null.hypoth = 0.5)
wil4 <- wilcox.test(a, b, paired = TRUE, correct = FALSE, conf.int = TRUE, mu = 0.5)

test_that("wilcoxon() returns correct inference for non-0 null (approximate)", {
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  #expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, correct = FALSE, conf.int = FALSE)
wil2 <- wilcox.test(d, correct = FALSE, conf.int = FALSE)
wil3 <- wilcoxon(a, b, paired = TRUE, correct = FALSE, conf.int = FALSE)
wil4 <- wilcox.test(a, b, paired = TRUE, correct = FALSE, conf.int = FALSE)

test_that("wilcoxon() returns correct numbers for one-sample uncorrected test, no CI", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil4$data.name)
  #expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
})

### one-sample test, exact

wil1 <- wilcoxon(d, exact = TRUE, correct = FALSE, conf.int = TRUE)
wil2 <- wilcox.test(d, exact = TRUE, correct = FALSE, conf.int = TRUE)
wil3 <- wilcoxon(a, b, paired = TRUE, exact = TRUE, correct = FALSE, conf.int = TRUE)
wil4 <- wilcox.test(a, b, paired = TRUE, exact = TRUE, correct = FALSE, conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  # Yiqun: remove this method test since stats::wilcox.test does not provide the exact distinction
  #expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  #expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil4$data.name)
  expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, exact = TRUE, correct = FALSE, alternative = "less", conf.int = TRUE)
wil2 <- wilcox.test(d, exact = TRUE, correct = FALSE, alternative = "less", conf.int = TRUE)
wil3 <- wilcoxon(a, b, paired = TRUE, exact = TRUE, correct = FALSE, alternative = "less", conf.int = TRUE)
wil4 <- wilcox.test(a, b, paired = TRUE, exact = TRUE, correct = FALSE, alternative = "less", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test (left)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  # Yiqun: remove this method test since stats::wilcox.test does not provide the exact distinction
  #expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  #expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil4$data.name)
  expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, exact = TRUE, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil2 <- wilcox.test(d, exact = TRUE, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil3 <- wilcoxon(a, b, paired = TRUE, exact = TRUE, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil4 <- wilcox.test(a, b, paired = TRUE, exact = TRUE, correct = FALSE, alternative = "greater", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test (right)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  # yiqun: remove this method test since stats::wilcox.test does not provide the exact distinction
  # expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  #expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil4$data.name)
  expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, exact = TRUE, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(d, exact = TRUE, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil3 <- wilcoxon(a, b, paired = TRUE, exact = TRUE, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil4 <- wilcox.test(a, b, paired = TRUE, exact = TRUE, correct = FALSE, conf.int = TRUE, conf.level = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (exact)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(d, exact = TRUE, correct = FALSE, conf.int = TRUE, null.hypoth = 0.5)
wil2 <- wilcox.test(d, exact = TRUE, correct = FALSE, conf.int = TRUE, mu = 0.5)
wil3 <- wilcoxon(a, b, paired = TRUE, exact = TRUE, correct = FALSE, conf.int = TRUE, null.hypoth = 0.5)
wil4 <- wilcox.test(a, b, paired = TRUE, exact = TRUE, correct = FALSE, conf.int = TRUE, mu = 0.5)

test_that("wilcoxon() returns correct inference for non-0 null (exact)", {
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_equal(wil3$statistic, wil4$statistic)
  expect_equal(wil3$p.value, wil4$p.value)
  expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$conf.int, wil4$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil3$inf[1,3],start =2, 
                                          stop = nchar(wil3$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil4$conf.int), tolerance = 1e-4)
})

### two-sample unpaired test approximate

wil1 <- wilcoxon(a, b, paired = FALSE, correct = FALSE, conf.int = TRUE)
wil2 <- wilcox.test(a, b, paired = FALSE, correct = FALSE, conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for two-sample uncorrected test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
  expect_equal(wil1$table[1,1], length(a))
  expect_equal(wil1$table[2,1], length(b))
  expect_equal(wil1$table[3,1], length(a) + length(b))
  expect_equal(wil1$table[1,2], sum(rank(c(a,b))[1:length(a)]))
  expect_equal(wil1$table[2,2], sum(rank(c(a,b))[(length(a) + 1):(length(a) + length(b))]))
  expect_equal(wil1$table[3,2], sum(rank(c(a,b))))
  expect_equal(wil1$table[1,3], length(a)*(length(a) + length(b) + 1)/2)
  expect_equal(wil1$table[2,3], length(b)*(length(a) + length(b) + 1)/2)
  expect_equal(wil1$table[3,3], sum(rank(c(a,b))))
})

wil1 <- wilcoxon(a, b, paired = FALSE, correct = TRUE, conf.int = TRUE)
wil2 <- wilcox.test(a, b, paired = FALSE, correct = TRUE, conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for two-sample corrected test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, correct = FALSE, alternative = "less", conf.int = TRUE)
wil2 <- wilcox.test(a, b, paired = FALSE, correct = FALSE, alternative = "less", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample uncorrected test (left)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil2 <- wilcox.test(a, b, paired = FALSE, correct = FALSE, alternative = "greater", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample uncorrected test (right)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(a, b, paired = FALSE, correct = FALSE, conf.int = TRUE, conf.leve = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (uncorrected)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, correct = TRUE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(a, b, paired = FALSE, correct = TRUE, conf.int = TRUE, conf.leve = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (corrected)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, correct = FALSE, conf.int = TRUE, null.hypoth = 0.5)
wil2 <- wilcox.test(a, b, paired = FALSE, correct = FALSE, conf.int = TRUE, mu = 0.5)

test_that("wilcoxon() returns correct inference for non-0 null (approximate)", {
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

### two-sample unpaired test, exact

wil1 <- wilcoxon(a, b, paired = FALSE, exact = TRUE, correct = FALSE, conf.int = TRUE)
wil2 <- wilcox.test(a, b, paired = FALSE, exact = TRUE, correct = FALSE, conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  # Yiqun: remove this method test since stats::wilcox.test does not provide the exact distinction
  # expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, exact = TRUE, correct = FALSE, alternative = "less", conf.int = TRUE)
wil2 <- wilcox.test(a, b, paired = FALSE, exact = TRUE, correct = FALSE, alternative = "less", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test (left)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  # Yiqun: removing this test since stats::wilcoxon does not provide the same method name
  #expect_equal(wil1$method, wil2$method) 
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, exact = TRUE, correct = FALSE, alternative = "greater", conf.int = TRUE)
wil2 <- wilcox.test(a, b, paired = FALSE, exact = TRUE, correct = FALSE, alternative = "greater", conf.int = TRUE)

test_that("wilcoxon() returns correct numbers for one-sample exact test (right)", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  # Yiqun: removing this test since stats::wilcoxon does not provide the same method name
  #expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, exact = TRUE, correct = FALSE, conf.int = TRUE, conf.level = 0.8)
wil2 <- wilcox.test(a, b, paired = FALSE, exact = TRUE, correct = FALSE, conf.int = TRUE, conf.level = 0.8)

test_that("wilcoxon() returns correct CIs other than 95% (exact)", {
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, exact = TRUE, correct = FALSE, conf.int = TRUE, null.hypoth = 0.5)
wil2 <- wilcox.test(a, b, paired = FALSE, exact = TRUE, correct = FALSE, conf.int = TRUE, mu = 0.5)

test_that("wilcoxon() returns correct inference for non-0 null (exact)", {
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$conf.int, wil2$conf.int)
  expect_equal(as.numeric(strsplit(substr(wil1$inf[1,3],start =2, 
                                          stop = nchar(wil1$inf[1,3] )-1),", ")[[1]]), 
               as.numeric(wil2$conf.int), tolerance = 1e-4)
})

wil1 <- wilcoxon(a, b, paired = FALSE, correct = FALSE, conf.int = FALSE)
wil2 <- wilcox.test(a, b, paired = FALSE, correct = FALSE, conf.int = FALSE)

test_that("wilcoxon() returns correct numbers for two-sample uncorrected test, no CI", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
})


### NAs
a_na <- c(NA, a)
b_na <- c(b, NA)
wil1 <- wilcoxon(a_na, b_na, paired = TRUE)
wil2 <- wilcox.test(a_na, b_na, paired = TRUE, correct = FALSE, exact = FALSE)
wil3 <- wilcoxon(a_na, b_na, paired = FALSE)
wil4 <- wilcox.test(a_na, b_na, paired = FALSE, correct = FALSE, exact = FALSE)

test_that("wilcoxon() handles NAs", {
  expect_s3_class(wil1, "wilcoxon")
  expect_equal(as.numeric(wil1$statistic), as.numeric(wil2$statistic))
  expect_equal(wil1$p.value, wil2$p.value)
  expect_equal(wil1$method, wil2$method)
  expect_equal(wil1$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil1$inf[1,1]), as.numeric(wil2$statistic))
  expect_equal(round(as.numeric(wil1$inf[1,2]), digits = 4), 
               round(wil2$p.value, digits = 4))
  expect_equal(wil1$alternative, wil2$alternative)
  expect_equal(as.numeric(wil1$hyps[1,1]), as.numeric(wil1$null.value))
  expect_equal(wil1$hyps[1,2], wil1$alternative)
  expect_equal(wil1$null.value, wil2$null.value) 
  expect_equal(wil1$parameter, wil2$parameter)
  
  expect_s3_class(wil3, "wilcoxon")
  expect_equal(as.numeric(wil3$statistic), as.numeric(wil4$statistic))
  expect_equal(wil3$p.value, wil4$p.value)
  expect_equal(wil3$method, wil4$method)
  expect_equal(wil3$data.name, wil2$data.name)
  #expect_equal(as.numeric(wil3$inf[1,1]), as.numeric(wil4$statistic))
  expect_equal(round(as.numeric(wil3$inf[1,2]), digits = 4), 
               round(wil4$p.value, digits = 4))
  expect_equal(wil3$alternative, wil4$alternative)
  expect_equal(as.numeric(wil3$hyps[1,1]), as.numeric(wil3$null.value))
  expect_equal(wil3$hyps[1,2], wil3$alternative)
  expect_equal(wil3$null.value, wil4$null.value) 
  expect_equal(wil3$parameter, wil4$parameter)
})