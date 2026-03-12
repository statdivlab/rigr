# Salary dataset

Data from a study of 1,597 faculty members at a single US university.
Includes information on monthly salary each year from 1976 through 1995,
as well as sex, highest degree attained, year of highest degree, field,
year hired, rank, and administrative duties. More information, including
a coding key, is available at rct-design.com (Teaching Materials,
Datasets section).

## Usage

``` r
salary
```

## Format

A data frame with 19792 rows and 11 variables:

- case:

  case number

- id:

  identification number for the faculty member

- sex:

  M (male) or F (female)

- deg:

  highest degree attained: PhD, Prof (professional degree, eg, medicine
  or law), or Other (Master's or Bachelor's degree)

- yrdeg:

  year highest degree attained

- field:

  Arts (Arts and Humanities), Prof (professional school, e.g., Business,
  Law, Engineering or Public Affairs), or Other

- startyr:

  year in which the faculty member was hired (2 digits)

- year:

  year (2 digits)

- rank:

  rank of the faculty member in this year: Assist (Assistant), Assoc
  (Associate), or Full (Full)

- admin:

  Indicator of whether the faculty member had administrative duties (eg,
  department chair) in this year: 1 (yes), or 0 (no)

- salary:

  monthly salary of the faculty member in this year in dollars

## Source

Dataset originally distributed at rct-design.com (Teaching Materials,
Datasets section).
