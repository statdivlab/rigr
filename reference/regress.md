# General Regression for an Arbitrary Functional

Produces point estimates, interval estimates, and p values for an
arbitrary functional (mean, geometric mean, proportion, odds, hazard) of
a variable of class `integer`, or `numeric` when regressed on an
arbitrary number of covariates. Multiple Partial F-tests can be
specified using the
[`U`](https://statdivlab.github.io/rigr/reference/U.md) function.

## Usage

``` r
regress(
  fnctl,
  formula,
  data,
  intercept = TRUE,
  weights = rep(1, nrow(data.frame(data))),
  subset = rep(TRUE, nrow(data.frame(data))),
  robustSE = TRUE,
  conf.level = 0.95,
  exponentiate = fnctl != "mean",
  replaceZeroes,
  useFdstn = TRUE,
  suppress = FALSE,
  na.action,
  method = "qr",
  qr = TRUE,
  singular.ok = TRUE,
  contrasts = NULL,
  init = NULL,
  ties = "efron",
  offset,
  control = list(...),
  ...
)
```

## Arguments

- fnctl:

  a character string indicating the functional (summary measure of the
  distribution) for which inference is desired. Choices include
  `"mean"`, `"geometric mean"`, `"odds"`, `"rate"`, `"hazard"`.

- formula:

  an object of class `formula` as might be passed to `lm`, `glm`, or
  `coxph`. Functions of variables, specified using
  [`dummy`](https://statdivlab.github.io/rigr/reference/dummy.md) or
  [`polynomial`](https://statdivlab.github.io/rigr/reference/polynomial.md)
  may also be included in `formula`.

- data:

  a data frame, matrix, or other data structure with matching names to
  those entered in `formula`.

- intercept:

  a logical value indicating whether a intercept exists or not. Default
  value is `TRUE` for all functionals. Intercept may also be removed if
  a "-1" is present in `formula`. If "-1" is present in `formula` but
  `intercept = TRUE` is specified, the model will fit without an
  intercept. Note that when `fnctl = "hazard"`, the intercept is always
  set to `FALSE` because Cox proportional hazards regression models do
  not explicitly estimate an intercept.

- weights:

  vector indicating optional weights for weighted regression.

- subset:

  vector indicating a subset to be used for all inference.

- robustSE:

  a logical indicator that standard errors (and confidence intervals)
  are to be computed using the Huber-White sandwich estimator. The
  default is TRUE.

- conf.level:

  a numeric scalar indicating the level of confidence to be used in
  computing confidence intervals. The default is 0.95.

- exponentiate:

  a logical indicator that the regression parameters should be
  exponentiated. This is by default true for all functionals except the
  mean.

- replaceZeroes:

  if not `FALSE`, this indicates a value to be used in place of zeroes
  when computing a geometric mean. If `TRUE`, a value equal to one-half
  the lowest nonzero value is used. If a numeric value is supplied, that
  value is used. Defaults to `TRUE` when `fnctl = "geometric mean"`.
  This parameter is always `FALSE` for all other values of `fnctl`.

- useFdstn:

  a logical indicator that the F distribution should be used for test
  statistics instead of the chi squared distribution even in logistic
  regression models. When using the F distribution, the degrees of
  freedom are taken to be the sample size minus the number of
  parameters, as it would be in a linear regression model.

- suppress:

  if `TRUE`, and a model which requires exponentiation (for instance,
  regression on the geometric mean) is computed, then a table with only
  the exponentiated coefficients and confidence interval is returned.
  Otherwise, two tables are returned - one with the original
  unexponentiated coefficients, and one with the exponentiated
  coefficients.

- na.action, qr, singular.ok, offset, contrasts, control:

  optional arguments that are passed to the functionality of `lm` or
  `glm`.

- method:

  the method to be used in fitting the model. The default value for
  `fnctl = "mean"` and `fnctl = "geometric mean"` is `"qr"`, and the
  default value for `fnctl = "odds"` and `fnctl = "rate"` is
  `"glm.fit"`. This argument is passed into the lm() or glm() function,
  respectively. You may optionally specify `method = "model.frame"`,
  which returns the model frame and does no fitting.

- init:

  a numeric vector of initial values for the regression parameters for
  the hazard regression. Default initial value is zero for all
  variables.

- ties:

  a character string describing method for breaking ties in hazard
  regression. Only `efron`, `breslow`, or `exact` is accepted. See more
  details in the documentation for this argument in the survival::coxph
  function. Default to `efron`.

- ...:

  additional arguments to be passed to the `lm` function call

## Value

An object of class uRegress is returned. Parameter estimates, confidence
intervals, and p values are contained in a matrix \$augCoefficients.

## Details

Regression models include linear regression (for the “mean” functional),
logistic regression with logit link (for the “odds” functional), Poisson
regression with log link (for the “rate” functional), linear regression
of a log-transformed outcome (for the “geometric mean” functional), and
Cox proportional hazards regression (for the hazard functional).

Currently, for the hazard functional, only \`coxph\` syntax is
supported; in other words, using \`dummy\`, \`polynomial\`, and
[`U`](https://statdivlab.github.io/rigr/reference/U.md) functions will
result in an error when \`fnctl = hazard\`.

Note that the only possible link function in \`regress\` with \`fnctl =
odds"\` is the logit link. Similarly, the only possible link function in
\`regress\` with \`fnctl = "rate"\` is the log link.

Objects created using the
[`U`](https://statdivlab.github.io/rigr/reference/U.md) function can
also be passed in. If the
[`U`](https://statdivlab.github.io/rigr/reference/U.md) call involves a
partial formula of the form `~ var1 + var2`, then `regress` will return
a multiple-partial F-test involving `var1` and `var2`. If an F-statistic
will already be calculated regardless of the
[`U`](https://statdivlab.github.io/rigr/reference/U.md) specification,
then any naming convention specified via `name ~ var1` will be ignored.
The multiple partial tests must be the last terms specified in the model
(i.e. no other predictors can follow them).

## See also

Functions for fitting linear models
([`lm`](https://rdrr.io/r/stats/lm.html)), and generalized linear models
([`glm`](https://rdrr.io/r/stats/glm.html)). Also see the function to
specify multiple-partial F-tests,
[`U`](https://statdivlab.github.io/rigr/reference/U.md).

## Examples

``` r
# Loading dataset
data(mri)

# Linear regression of atrophy on age
regress("mean", atrophy ~ age, data = mri)
#> 
#> Call:
#> regress(fnctl = "mean", formula = atrophy ~ age, data = mri)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -36.870  -8.589  -0.870   7.666  51.203 
#> 
#> Coefficients:
#>                  Estimate  Naive SE  Robust SE    95%L      95%H     
#> [1] Intercept     -16.06     6.256     6.701       -29.22    -2.907  
#> [2] age            0.6980   0.08368   0.09002       0.5213    0.8747 
#>                     F stat    df Pr(>F)   
#> [1] Intercept            5.75 1    0.0168 
#> [2] age                 60.12 1  < 0.00005
#> 
#> Residual standard error: 12.36 on 733 degrees of freedom
#> Multiple R-squared:  0.08669,    Adjusted R-squared:  0.08545 
#> F-statistic: 60.12 on 1 and 733 DF,  p-value: 2.988e-14
#> 

# Linear regression of atrophy on sex and height and their interaction, 
# with a multiple-partial F-test on the height-sex interaction
regress("mean", atrophy ~ height + sex + U(hs=~height:sex), data = mri)
#> 
#> Call:
#> regress(fnctl = "mean", formula = atrophy ~ height + sex + U(hs = ~height:sex), 
#>     data = mri)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -28.092  -8.799  -0.935   8.100  48.862 
#> 
#> Coefficients:
#>                       Estimate  Naive SE  Robust SE    95%L      95%H     
#> [1] Intercept           47.60     16.28     14.75        18.64     76.57  
#> [2] height            -0.09272    0.1026   0.09296      -0.2752   0.08978 
#> [3] sexMale             36.80     23.93     23.60       -9.543     83.14  
#> [4] height:sexMale     -0.1690    0.1442    0.1411      -0.4460    0.1079 
#>                          F stat    df Pr(>F)   
#> [1] Intercept                10.41 1    0.0013 
#> [2] height                    0.99 1    0.3189 
#> [3] sexMale                   2.43 1    0.1194 
#> [4] height:sexMale            1.44 1    0.2312 
#> 
#> Residual standard error: 12.51 on 731 degrees of freedom
#> Multiple R-squared:  0.06687,    Adjusted R-squared:  0.06304 
#> F-statistic:  16.9 on 3 and 731 DF,  p-value: 1.274e-10
#> 

# Logistic regression of sex on atrophy
mri$sex_bin <- ifelse(mri$sex == "Female", 1, 0)
regress("odds", sex_bin ~ atrophy, data = mri)
#> 
#> Call:
#> regress(fnctl = "odds", formula = sex_bin ~ atrophy, data = mri)
#> 
#> Deviance Residuals: 
#>    Min      1Q  Median      3Q     Max  
#> -1.636  -1.129   0.741   1.110   1.985  
#> 
#> Coefficients:
#> 
#> Raw Model:
#>                  Estimate  Naive SE   Robust SE        F stat    df Pr(>F)   
#> [1] Intercept      1.430     0.2372     0.2421             34.87 1  < 0.00005
#> [2] atrophy      -0.03962   6.296e-03  6.504e-03           37.12 1  < 0.00005
#> 
#> Transformed Model:
#>                  e(Est)    e(95%L)   e(95%H)         F stat    df Pr(>F)   
#> [1] Intercept      4.177     2.597     6.719             34.87 1  < 0.00005
#> [2] atrophy        0.9612    0.9490    0.9735            37.12 1  < 0.00005
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 1018.91  on 734  degrees of freedom
#> Residual deviance:  975.38  on 733  degrees of freedom
#> AIC: 979.38
#> 
#> Number of Fisher Scoring iterations: 4
#> 

# Cox regression of age on survival 
library(survival)
regress("hazard", Surv(obstime, death)~age, data=mri)
#> 
#> Call:
#> regress(fnctl = "hazard", formula = Surv(obstime, death) ~ age, 
#>     data = mri)
#> 
#> 
#> Coefficients:
#> 
#> Raw Model:
#>            Estimate  Naive SE  Robust SE       F stat    df Pr(>F)   
#> [1] age     0.06795   0.01354   0.01412            23.17 1  < 0.00005
#> 
#> Transformed Model:
#>            e(Est)    e(95%L)   e(95%H)         F stat    df Pr(>F)   
#> [1] age      1.070     1.041     1.100             23.17 1  < 0.00005
#> n =  735, number of events= 133 
#> Overall significance test: 
#> Likelihood ratio test= 22.33  on 1 df,   p=2.292e-06
#> Wald test            = 25.17  on 1 df,   p=5.24e-07
#> Score (logrank) test = 25.44  on 1 df,   p=4.554e-07
#> 
#> 
```
