# MRI dataset

Data from an observational study of the incidence of cardiovascular
disease (especially heart attacks and congestive heart failure) and
cerebrovascular disease (especially strokes) in the U.S. elderly. More
information, including a coding key, is available at rct-design.com
(Teaching Materials, Datasets section).

## Usage

``` r
mri
```

## Format

A data frame with 735 rows and 30 variables:

- ptid:

  Participant identification number.

- mridate:

  The date on which the participant underwent MRI scan in MMDDYY format.

- age:

  Participant age at time of MRI, in years.

- sex:

  The sex of the partipant. Only \`Male\` and \`Female\` are
  represented.

- race:

  Participant's race. One of the following: \`White\`, \`Black\`,
  \`Asian\`, or \`Subject did not identify as White, Black or Asian\`.
  It is unclear if study participants self-identified their race, or if
  it was guessed by the study organisers.

- weight:

  Participant's weight at time of MRI (pounds).

- height:

  Participant's height at time of MRI (centimeters).

- packyrs:

  Participant smoking history in pack years (1 pack year = smoking 1
  pack of cigarettes per day for 1 year). A participant who has never
  smoked has 0 pack years.

- yrsquit:

  Number of years since quitting smoking. A current smoker will have a
  nonzero packyrs and a 0 for yrsquit. A never smoker will have a zero
  for both variables.

- alcoh:

  Average alcohol intake for the participant for the two weeks prior to
  MRI (drinks per week, where one drink is 1 oz. whiskey, 4 oz. wine, or
  12 oz.beer).

- physact:

  Physical activity of the participant for the week prior to MRI (1,000
  kcal).

- chf:

  Indicator of whether the participant had been diagnosed with
  congestive heart failure prior to MRI (0=no, 1=yes).

- chd:

  Indicator of whether the participant had been diagnosed with coronary
  heart disease prior to MRI (0=no, 1=diagnosis of angina, 2=diagnosis
  of myocardial infarction).

- stroke:

  Indicator of whether the participant had been diagnosed with a
  cerebrovascular event prior to MRI (0=no, 1=diagnosis of a transient
  ischemic attack, 2=diagnosis of stroke).

- diabetes:

  Indicator of whether the participant had been diagnosed with diabetes
  prior to MRI (0=no, 1=yes).

- genhlth:

  an indicator of the participant's view of their own health
  (1=excellent, 2=very good, 3=good, 4=fair, 5=poor)

- ldl:

  a laboratory measure of low density lipoprotein (a kind of
  cholesterol) in the participant's blood at the time of MRI (mg/dL).

- alb:

  a laboratory measure of albumin, a kind of protein, in the
  participant's blood at the time of MRI (g/L).

- crt:

  a laboratory measure of creatinine, a waste product, in the
  participant's blood at the time of MRI (mg/dL).

- plt:

  a laboratory measure of the number of platelets circulating in the
  participant's blood at the time of MRI (1000 per cubic mm).

- sbp:

  a measurement of the participant's systolic blood pressure in their
  arm at the time of MRI (mm Hg).

- aai:

  the ratio of systolic blood pressure measured in the participant's
  ankle at time of MRI to the systolic blood pressure in the
  participant's arm.

- fev:

  a measure of the forced expiratory volume in the participant at the
  time of MRI (L/sec).

- dsst:

  a measure of cognitive function (Digit Symbol Substitution Test) for
  the participant at the time of MRI. Maximum score possible is 100.

- atrophy:

  a measure of loss of neurons estimated by the degree of ventricular
  enlargement relative to the predicted ventricular size; with 0
  indicating no atrophy and 100 indicating the most severe degree of
  atrophy.

- whgrd:

  a measure of white matter changes detected on MRI. 0 means no changes,
  9 means marked changes.

- numinf:

  a count of the number of distinct regions identified on MRI scan which
  were suggestive of infarcts.

- volinf:

  a measure of the total volume of infarct-like lesions found on MRI
  scan (cubic cm).

- obstime:

  the total time (in days) that the participant was observed on study
  between the date of MRI and death or September 16, 1997, whichever
  came first.

- death:

  an indicator that the participant was observed to die while on study.
  If 1, the number of days recorded in `obstime` is the number of days
  from that participant's MRI to their death. If 0, the number of days
  in `obstime` is the number of days between that participant's MRI and
  September 16, 1997.

## Source

Dataset originally distributed at rct-design.com (Teaching Materials,
Datasets section).
