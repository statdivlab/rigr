# Regression in rigr

In the `rigr` package, we have set out to make regression and analysis
easier by

1.  allowing you to specify different types of regression from one
    function;
2.  automatically computing confidence intervals and p-values using
    robust standard errors;
3.  displaying output in a more intuitive fashion than base R; and
4.  allowing you to specify multiple-partial F-tests.

This capability is implemented in the function
[`regress()`](https://statdivlab.github.io/rigr/reference/regress.md).
The basic arguments to this function are

- `fnctl`: the functional
- `formula`: the formula for the linear model
- `data`: the data to use for the model

### `fnctl`: the functional

We use the concept of a *functional* to handle our first goal: allowing
you to specify different types of regression models using a single
function. A functional takes a *function* as its argument and returns a
number. The most common example of a functional in regression is the
*mean*. The allowed functionals to
[`regress()`](https://statdivlab.github.io/rigr/reference/regress.md)
are

| Functional         | Type of Regression                                 | base R command                                                    |
|--------------------|:---------------------------------------------------|:------------------------------------------------------------------|
| `"mean"`           | Linear Regression                                  | [`lm()`](https://rdrr.io/r/stats/lm.html)                         |
| `"geometric mean"` | Linear Regression on logarithmically transformed Y | [`lm()`](https://rdrr.io/r/stats/lm.html), with Y log-transformed |
| `"odds"`           | Logistic Regression                                | `glm(family = binomial)`                                          |
| `"rate"`           | Poisson Regression                                 | `glm(family = poisson)`                                           |
| `"hazard"`         | Proportional Hazards Regression                    | [`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)          |

### `formula` and `data`

The *formula* to
[`regress()`](https://statdivlab.github.io/rigr/reference/regress.md) is
the same as a formula given to [`lm()`](https://rdrr.io/r/stats/lm.html)
or [`glm()`](https://rdrr.io/r/stats/glm.html), but with additional
optional functionality to enable sophisticated analyses (multiple
partial F-tests) with fewer headaches.

The *data* argument is exactly the same as that in
[`lm()`](https://rdrr.io/r/stats/lm.html) or any of the other regression
commands.

## Linear Regression

As a first example, we run a linear regression of atrophy (a measure of
global brain activity) on age, sex and race, from the `mri` data. This
dataset is included in the `rigr` package; see its documentation by
running [`?mri`](https://statdivlab.github.io/rigr/reference/mri.md).

``` r
## Preparing our R session
library(rigr)
```

    ## rigr version 1.0.9: Regression, Inference, and General Data Analysis Tools in R

``` r
data(mri)
regress("mean", atrophy ~ age + sex + race, data = mri)
```

    ## 
    ## Call:
    ## regress(fnctl = "mean", formula = atrophy ~ age + sex + race, 
    ##     data = mri)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -34.120  -8.331  -0.434   7.325  53.915 
    ## 
    ## Coefficients:
    ##                                                          Estimate  Naive SE 
    ## [1] Intercept                                             -17.60     6.341  
    ## [2] age                                                    0.6866   0.08134 
    ## [3] sexMale                                                5.988     0.8867 
    ##     race                                                                    
    ## [4]    Black                                              -2.375     2.109  
    ## [5]    Subject did not identify White, Black or Asian     -3.078     3.885  
    ## [6]    White                                              -0.2664    1.822  
    ##                                                          Robust SE    95%L     
    ## [1] Intercept                                              6.893       -31.14  
    ## [2] age                                                   0.08836       0.5132 
    ## [3] sexMale                                                0.8895       4.242  
    ##     race                                                                       
    ## [4]    Black                                               2.049       -6.397  
    ## [5]    Subject did not identify White, Black or Asian      4.157       -11.24  
    ## [6]    White                                               1.780       -3.761  
    ##                                                          95%H         F stat   
    ## [1] Intercept                                             -4.072           6.52
    ## [2] age                                                    0.8601         60.38
    ## [3] sexMale                                                7.734          45.32
    ##     race                                                                   1.14
    ## [4]    Black                                               1.647           1.34
    ## [5]    Subject did not identify White, Black or Asian      5.082           0.55
    ## [6]    White                                               3.228           0.02
    ##                                                          df Pr(>F)   
    ## [1] Intercept                                            1    0.0109 
    ## [2] age                                                  1  < 0.00005
    ## [3] sexMale                                              1  < 0.00005
    ##     race                                                 3    0.3315 
    ## [4]    Black                                             1    0.2467 
    ## [5]    Subject did not identify White, Black or Asian    1    0.4592 
    ## [6]    White                                             1    0.8810 
    ## 
    ## Residual standard error: 12 on 729 degrees of freedom
    ## Multiple R-squared:  0.1439, Adjusted R-squared:  0.138 
    ## F-statistic: 21.32 on 5 and 729 DF,  p-value: < 2.2e-16

Notice that by default robust standard error estimates are returned in
addition to the naive estimates. The robust estimates are also used to
perform inference. Thus, the confidence intervals, statistics, and
p-values use these estimates of the standard error.

F-statistics are also displayed by default, including the multiple
partial F-tests for the levels of a multi-level category (such as
`race`) as well as the overall F-test for the variable.

## Generalized Linear Regression

### Logistic Regression

We can also run generalized linear regression using
[`regress()`](https://statdivlab.github.io/rigr/reference/regress.md).
For example, to model the odds of having diabetes for males compared to
females, we could run a logistic regression as follows:

``` r
regress("odds", diabetes ~ sex, data = mri)
```

    ## 
    ## Call:
    ## regress(fnctl = "odds", formula = diabetes ~ sex, data = mri)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5593  -0.5593  -0.3823  -0.3823   2.3034  
    ## 
    ## Coefficients:
    ## 
    ## Raw Model:
    ##                  Estimate  Naive SE  Robust SE       F stat    df Pr(>F)   
    ## [1] Intercept     -2.580     0.2034    0.2037           160.39 1  < 0.00005
    ## [2] sexMale        0.8037    0.2519    0.2522            10.15 1    0.0015 
    ## 
    ## Transformed Model:
    ##                  e(Est)    e(95%L)   e(95%H)         F stat    df Pr(>F)   
    ## [1] Intercept     0.07580   0.05082    0.1131           160.39 1  < 0.00005
    ## [2] sexMale        2.234     1.361     3.665             10.15 1    0.0015 
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 501.59  on 734  degrees of freedom
    ## Residual deviance: 490.82  on 733  degrees of freedom
    ## AIC: 494.82
    ## 
    ## Number of Fisher Scoring iterations: 5

In all of the generalized linear regression output we see two tables.
The `Raw Model` table displays estimated coefficients (and their
standard errors) on the log-odds scale. The `Transformed Model` table
exponentiates the estimated coefficients and their confidence intervals
so that the estimated parameters can be interpreted on the odds scale.

Note that the only possible link function in `regress` with
`fnctl = odds"` is the logit link. Similarly, the only possible link
function in `regress` with `fnctl = "rate"` is the log link.

### Poisson Regression

The next functional that `regress` supports is `"rate"`, for use in
Poisson regression. To regress `yrsquit` on `age`, we would run:

``` r
regress("rate", yrsquit ~ age, data = mri)
```

    ## 
    ## Call:
    ## regress(fnctl = "rate", formula = yrsquit ~ age, data = mri)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -5.385  -4.365  -4.186   2.395   9.920  
    ## 
    ## Coefficients:
    ## 
    ## Raw Model:
    ##                  Estimate  Naive SE   Robust SE        F stat    df Pr(>F)   
    ## [1] Intercept      1.011     0.1571     0.7239              1.95 1    0.1629 
    ## [2] age           0.01680   2.087e-03  9.688e-03            3.01 1    0.0834 
    ## 
    ## Transformed Model:
    ##                  e(Est)    e(95%L)   e(95%H)         F stat    df Pr(>F)   
    ## [1] Intercept      2.749     0.6637    11.39              1.95 1    0.1629 
    ## [2] age            1.017     0.9978    1.036              3.01 1    0.0834 
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 14574  on 734  degrees of freedom
    ## Residual deviance: 14511  on 733  degrees of freedom
    ## AIC: 16008
    ## 
    ## Number of Fisher Scoring iterations: 6

Note that again we have two tables of output, denoted by `Raw Model` and
`Transformed Model`, with `Transformed Model` displaying exponentiated
estimated coefficients.

## Proportional Hazards Regression

The final functional that `regress` supports is `"hazard"`, for use in
proportional hazards regression. To regress `age` on the death status
(note that we need to create a `Surv` object first), we would run:

``` r
library(survival)
regress("hazard", Surv(obstime, death)~age, data=mri)
```

    ## 
    ## Call:
    ## regress(fnctl = "hazard", formula = Surv(obstime, death) ~ age, 
    ##     data = mri)
    ## 
    ## 
    ## Coefficients:
    ## 
    ## Raw Model:
    ##            Estimate  Naive SE  Robust SE       F stat    df Pr(>F)   
    ## [1] age     0.06795   0.01354   0.01412            23.17 1  < 0.00005
    ## 
    ## Transformed Model:
    ##            e(Est)    e(95%L)   e(95%H)         F stat    df Pr(>F)   
    ## [1] age      1.070     1.041     1.100             23.17 1  < 0.00005
    ## n =  735, number of events= 133 
    ## Overall significance test: 
    ## Likelihood ratio test= 22.33  on 1 df,   p=2.292e-06
    ## Wald test            = 25.17  on 1 df,   p=5.24e-07
    ## Score (logrank) test = 25.44  on 1 df,   p=4.554e-07

Similar to the Poisson regression case, we have two tables of output,
denoted by `Raw Model` and `Transformed Model`, with `Transformed Model`
displaying exponentiated estimated coefficients (i.e., on the hazard
scale).

## Regression on the Geometric Mean

Most often in linear regression we are interested in modeling the mean
of the response variable. However, we are sometimes interested in
modeling the mean of the log-transformed response variable, which allows
us to make statements about the geometric mean of the response. In
[`regress()`](https://statdivlab.github.io/rigr/reference/regress.md),
we can use the `"geometric mean"` functional to fit this model.
Regression on the geometric mean of the `packyrs` variable in the `mri`
dataset can be performed as follows:

``` r
regress("geometric mean", packyrs ~ age, data = mri)
```

    ## ( 1  cases deleted due to missing values)
    ## 
    ## 
    ## Call:
    ## regress(fnctl = "geometric mean", formula = packyrs ~ age, data = mri)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.136 -3.417  1.283  2.970  5.219 
    ## 
    ## Coefficients:
    ## 
    ## Raw Model:
    ##                  Estimate  Naive SE  Robust SE       F stat    df Pr(>F)   
    ## [1] Intercept      6.410     1.586     1.537             17.39 1  < 0.00005
    ## [2] age          -0.07986   0.02121   0.02050            15.17 1     1e-04 
    ## 
    ## Transformed Model:
    ##                  e(Est)    e(95%L)   e(95%H)         F stat    df Pr(>F)   
    ## [1] Intercept      608.1     29.74     12431             17.39 1  < 0.00005
    ## [2] age            0.9232    0.8868    0.9612            15.17 1     1e-04 
    ## 
    ## Residual standard error: 3.13 on 732 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01899,    Adjusted R-squared:  0.01765 
    ## F-statistic: 15.17 on 1 and 732 DF,  p-value: 0.0001074

It should be noted that many of the `packyrs` observations are zero, but
the geometric mean of data including an observation of zero is zero…
regardless of how many non-zeros were also observed. Therefore, by
default, zeroes in the outcome variable are replaced by a value equal to
one-half the lowest nonzero value in the outcome variable. This is based
on the idea that the lowest observed value could be a proxy for the
lower limit of detection. If you wish to specify a different value with
which to replace zeroes, you may do say using the `replaceZeroes`
argument.

``` r
regress("geometric mean", packyrs ~ age, data = mri, replaceZeroes = 1)
```

    ## ( 1  cases deleted due to missing values)
    ## 
    ## 
    ## Call:
    ## regress(fnctl = "geometric mean", formula = packyrs ~ age, data = mri, 
    ##     replaceZeroes = 1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6437 -1.7460  0.0625  1.7259  3.8246 
    ## 
    ## Coefficients:
    ## 
    ## Raw Model:
    ##                  Estimate  Naive SE  Robust SE       F stat    df Pr(>F)   
    ## [1] Intercept      5.119     0.8887    0.8462            36.60 1  < 0.00005
    ## [2] age          -0.04498   0.01189   0.01127            15.93 1     1e-04 
    ## 
    ## Transformed Model:
    ##                  e(Est)    e(95%L)   e(95%H)         F stat    df Pr(>F)   
    ## [1] Intercept      167.2     31.76     880.7             36.60 1  < 0.00005
    ## [2] age            0.9560    0.9351    0.9774            15.93 1     1e-04 
    ## 
    ## Residual standard error: 1.754 on 732 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01919,    Adjusted R-squared:  0.01785 
    ## F-statistic: 15.93 on 1 and 732 DF,  p-value: 7.218e-05

In the output from regress using the geometric mean functional, we see a
table for the `Raw Model` and the `Transformed Model`. The `e(Est)`,
`e(95%L)`, and `e(95%H)` columns in the `Transformed Model` table
correspond to exponentiated values from the `Raw Model` - you’ll notice
that $e^{5.119} \approx 167.2.$

## Re-parameterizations of a Variable

There are two special functions in `rigr` which allow us to
re-parameterize variables:

- `dummy`: create dummy variables
- `polynomial`: create a polynomial

Both of these functions may be used in a
[`regress()`](https://statdivlab.github.io/rigr/reference/regress.md)
call, and will additionally give a multiple partial F-test of the entire
variable automatically.

### Specifying the reference group with `dummy`

The `dummy` function is useful for specifying the reference group that
you wish to use with categorical variables. Below we show an example of
using the reference group “Female” vs. the reference group “Male” in a
regression on `sex`.

``` r
regress("mean", atrophy ~ dummy(sex, reference = "Male"), data = mri)
```

    ## 
    ## Call:
    ## regress(fnctl = "mean", formula = atrophy ~ dummy(sex, reference = "Male"), 
    ##     data = mri)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -29.087  -9.087  -0.905   8.095  49.095 
    ## 
    ## Coefficients:
    ##                                         Estimate  Naive SE  Robust SE   
    ## [1] Intercept                             39.09     0.6563    0.6733    
    ## [2]   dummy(sex, reference = "Male")     -6.182     0.9263    0.9265    
    ##                                         95%L      95%H         F stat    df
    ## [1] Intercept                             37.77     40.41        3370.22 1 
    ## [2]   dummy(sex, reference = "Male")     -8.001    -4.363          44.53 1 
    ##                                         Pr(>F)   
    ## [1] Intercept                           < 0.00005
    ## [2]   dummy(sex, reference = "Male")    < 0.00005
    ## 
    ##  Dummy terms calculated from sex, reference = Male 
    ## 
    ## Residual standard error: 12.56 on 733 degrees of freedom
    ## Multiple R-squared:  0.05729,    Adjusted R-squared:  0.05601 
    ## F-statistic: 44.53 on 1 and 733 DF,  p-value: 4.944e-11

``` r
regress("mean", atrophy ~ dummy(sex, reference = "Female"), data = mri)
```

    ## 
    ## Call:
    ## regress(fnctl = "mean", formula = atrophy ~ dummy(sex, reference = "Female"), 
    ##     data = mri)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -29.087  -9.087  -0.905   8.095  49.095 
    ## 
    ## Coefficients:
    ##                                           Estimate  Naive SE  Robust SE   
    ## [1] Intercept                               32.91     0.6536    0.6364    
    ## [2]   dummy(sex, reference = "Female")      6.182     0.9263    0.9265    
    ##                                           95%L      95%H         F stat    df
    ## [1] Intercept                               31.66     34.15        2673.43 1 
    ## [2]   dummy(sex, reference = "Female")      4.363     8.001          44.53 1 
    ##                                           Pr(>F)   
    ## [1] Intercept                             < 0.00005
    ## [2]   dummy(sex, reference = "Female")    < 0.00005
    ## 
    ##  Dummy terms calculated from sex, reference = Female 
    ## 
    ## Residual standard error: 12.56 on 733 degrees of freedom
    ## Multiple R-squared:  0.05729,    Adjusted R-squared:  0.05601 
    ## F-statistic: 44.53 on 1 and 733 DF,  p-value: 4.944e-11

Notice that below the coefficients table in the output, the reference
category is reported.

### Polynomial regression

You can fit higher-order polynomials using `polynomial`:

``` r
regress("mean", atrophy ~ polynomial(age, degree = 2), data = mri)
```

    ## 
    ## Call:
    ## regress(fnctl = "mean", formula = atrophy ~ polynomial(age, degree = 2), 
    ##     data = mri)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -36.828  -9.063  -1.018   7.816  50.887 
    ## 
    ## Coefficients:
    ##                                    Estimate  Naive SE  Robust SE    95%L      
    ## [1] Intercept                        35.34     0.5598    0.5622        34.24  
    ##     polynomial(age, degree = 2)                                               
    ## [2]    age^1                         0.5869    0.1009   0.09808       0.3943  
    ## [3]    age^2                        0.02159   0.01099   0.01150     -9.897e-04
    ##                                    95%H         F stat    df Pr(>F)   
    ## [1] Intercept                        36.45        3952.13 1  < 0.00005
    ##     polynomial(age, degree = 2)                     32.91 2  < 0.00005
    ## [2]    age^1                         0.7794         35.80 1  < 0.00005
    ## [3]    age^2                        0.04417          3.52 1    0.0609 
    ## 
    ##  Polynomial terms calculated from age, centered at 74.566 
    ## 
    ## Residual standard error: 12.33 on 732 degrees of freedom
    ## Multiple R-squared:  0.09148,    Adjusted R-squared:  0.089 
    ## F-statistic: 32.91 on 2 and 732 DF,  p-value: 2.06e-14

Note that all polynomials less than or equal to the degree specified are
included in the model, and that the variables in the polynomial
specification are mean-centered by default. You can change the centering
using the `center` parameter in the polynomial function, an example of
which is as follows.

``` r
regress("mean", atrophy ~ polynomial(age, degree = 2, center = 65), data = mri)
```

    ## 
    ## Call:
    ## regress(fnctl = "mean", formula = atrophy ~ polynomial(age, degree = 2, 
    ##     center = 65), data = mri)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -36.828  -9.063  -1.018   7.816  50.887 
    ## 
    ## Coefficients:
    ##                                                 Estimate  Naive SE 
    ## [1] Intercept                                     31.70     1.528  
    ##     polynomial(age, degree = 2, center = 65)                       
    ## [2]    age^1                                      0.1738    0.2797 
    ## [3]    age^2                                     0.02159   0.01099 
    ##                                                 Robust SE    95%L      
    ## [1] Intercept                                     1.497         28.77  
    ##     polynomial(age, degree = 2, center = 65)                           
    ## [2]    age^1                                      0.2811      -0.3781  
    ## [3]    age^2                                     0.01150     -9.897e-04
    ##                                                 95%H         F stat    df
    ## [1] Intercept                                     34.64         448.45 1 
    ##     polynomial(age, degree = 2, center = 65)                     32.91 2 
    ## [2]    age^1                                      0.7257          0.38 1 
    ## [3]    age^2                                     0.04417          3.52 1 
    ##                                                 Pr(>F)   
    ## [1] Intercept                                   < 0.00005
    ##     polynomial(age, degree = 2, center = 65)    < 0.00005
    ## [2]    age^1                                      0.5367 
    ## [3]    age^2                                      0.0609 
    ## 
    ##  Polynomial terms calculated from age, centered at 65 
    ## 
    ## Residual standard error: 12.33 on 732 degrees of freedom
    ## Multiple R-squared:  0.09148,    Adjusted R-squared:  0.089 
    ## F-statistic: 32.91 on 2 and 732 DF,  p-value: 2.06e-14

## User-specified multiple partial F-tests

You can also perform multiple partial F-tests using formulas and the `U`
function. This is useful when want to test a subset of variables all at
once in your regression. For example, to test whether both the variables
`packyrs` and `yrsquit` are associated with `atrophy` in a model with
age as a predictor, we can run

``` r
regress("mean", atrophy ~ age + sex + U("Smoking variables" = ~packyrs + yrsquit), data = mri)
```

    ## ( 1  cases deleted due to missing values)
    ## 
    ## 
    ## Call:
    ## regress(fnctl = "mean", formula = atrophy ~ age + sex + U(`Smoking variables` = ~packyrs + 
    ##     yrsquit), data = mri)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -33.593  -8.613  -0.306   7.279  52.399 
    ## 
    ## Coefficients:
    ##                          Estimate   Naive SE  Robust SE    95%L      95%H     
    ## [1] Intercept              -18.82     6.167     6.690       -31.95    -5.683  
    ## [2] age                    0.6917    0.08212   0.08900       0.5170    0.8664 
    ## [3] sexMale                 5.628     0.9386    0.9596       3.744     7.512  
    ##     Smoking variables                                                         
    ## [4]   packyrs             9.782e-03  0.01684   0.01693     -0.02346   0.04302 
    ## [5]   yrsquit              0.02071   0.03282   0.03276     -0.04361   0.08503 
    ##                             F stat    df Pr(>F)   
    ## [1] Intercept                    7.91 1    0.0050 
    ## [2] age                         60.40 1  < 0.00005
    ## [3] sexMale                     34.39 1  < 0.00005
    ##     Smoking variables            0.37 2    0.6905 
    ## [4]   packyrs                    0.33 1    0.5636 
    ## [5]   yrsquit                    0.40 1    0.5275 
    ## 
    ## Residual standard error: 11.99 on 729 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.1419, Adjusted R-squared:  0.1372 
    ## F-statistic: 26.44 on 4 and 729 DF,  p-value: < 2.2e-16

`"Smoking variables"` is what we name the group of variables `packyrs`
and `yrsquit`. The overall F statistic and p-value associated with the
inclusion of these two smoking variables variables in the model are 4.37
and 0.0130, respectively.

## Testing contrasts: hypotheses about linear combinations of regression coefficients

You may be interested in testing a null hypothesis about a linear
combination of coefficients in our regression model. For example, to
investigate “Is the mean atrophy for a female subject equal to the mean
atrophy for a male subject who is 10 years younger?”, our hypothesis
test involves both the coefficients on age and sex. We can test this
hypothesis using the `lincom` function. First, we need to fit a linear
model of age and sex on atrophy:

``` r
mod_rigr <- regress("mean", atrophy ~ age + sex, data = mri)
mod_rigr
```

    ## 
    ## Call:
    ## regress(fnctl = "mean", formula = atrophy ~ age + sex, data = mri)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -33.765  -8.582  -0.356   7.344  52.100 
    ## 
    ## Coefficients:
    ##                  Estimate  Naive SE  Robust SE    95%L      95%H     
    ## [1] Intercept     -17.83     6.081     6.557       -30.70    -4.959  
    ## [2] age            0.6819   0.08129   0.08769       0.5097    0.8540 
    ## [3] sexMale        5.964     0.8857    0.8845       4.227     7.700  
    ##                     F stat    df Pr(>F)   
    ## [1] Intercept            7.40 1    0.0067 
    ## [2] age                 60.47 1  < 0.00005
    ## [3] sexMale             45.46 1  < 0.00005
    ## 
    ## Residual standard error: 12 on 732 degrees of freedom
    ## Multiple R-squared:   0.14,  Adjusted R-squared:  0.1376 
    ## F-statistic: 52.18 on 2 and 732 DF,  p-value: < 2.2e-16

We then create a vector giving the linear combination of the coefficient
that we hypothesized to be zero, and perform the test using `lincom`.
The elements in `mod_combo` correspond to `Intercept`, `age`, and
`sexMale`, because this was their order in the coefficient table shown
above.

``` r
mod_combo <- c(0, -10, 1)
lincom(mod_rigr, mod_combo)
```

    ## 
    ## H0: -10*age+1*sexMale   =  0 
    ## Ha: -10*age+1*sexMale  !=  0 
    ##      Estimate Std. Err.   95%L   95%H      T Pr(T > |t|)
    ## [1,]   -0.855     1.236 -3.282  1.571 -0.692       0.489

Note that the standard errors returned by default are robust, as are the
associated confidence intervals and p-values.

We could also test the null hypothesis that the mean difference in
atrophy between these two groups (females, and males 10 years younger)
is equal to -1 as follows:

``` r
lincom(mod_rigr, mod_combo, null.hypoth = -1)
```

    ## 
    ## H0: -10*age+1*sexMale   =  -1 
    ## Ha: -10*age+1*sexMale  !=  -1 
    ##      Estimate Std. Err.   95%L   95%H     T Pr(T > |t|)
    ## [1,]   -0.855     1.236 -3.282  1.571 0.117       0.907
