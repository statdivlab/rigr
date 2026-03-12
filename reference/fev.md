# FEV dataset

Data from a study of 654 children on the relationship between smoking
status and lung function (measured by FEV). Each row corresponds to a
single clinic visit and contains information on age, height, sex, FEV,
and smoking status. More information, including a coding key, is
available at rct-design.com (Teaching Materials, Datasets section).

## Usage

``` r
fev
```

## Format

A data frame with 654 rows and 7 variables:

- seqnbr:

  case number (the numbers 1 to 654)

- subjid:

  subject identification number (unique for each different child)

- age:

  subject age at time of measurement (years)

- fev:

  measured forced exhalation volume (liters per second)

- height:

  subject height at time of measurement (inches)

- sex:

  subject sex

- smoke:

  smoking habits ("yes" or "no")

## Source

Dataset originally distributed at rct-design.com (Teaching Materials,
Datasets section).
