# PSA dataset

Data from a study of 50 men having hormonally treated prostate cancer.
Includes information on PSA levels, tumor characteristics, remission
status, age, and disease state. More information, including a coding
key, is available at rct-design.com (Teaching Materials, Datasets
section).

## Usage

``` r
psa
```

## Format

A data frame with 50 rows and 9 variables:

- ptid:

  patient identifier

- nadirpsa:

  lowest PSA value attained post therapy (ng/ml)

- pretxpsa:

  PSA value prior to therapy (ng/ml)

- ps:

  performance status (0= worst, 100= best)

- bss:

  bone scan score (1= least disease, 3= most)

- grade:

  tumor grade (1= least aggressive, 3= most)

- age:

  patient's age (years)

- obstime:

  time observed in remission (months)

- inrem:

  Indicator whether patient still in remission at last follow-up (yes or
  no)

## Source

Dataset originally distributed at rct-design.com (Teaching Materials,
Datasets section).
