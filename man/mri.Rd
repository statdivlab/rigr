\name{mri}
\alias{mri}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
MRI data
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
An MRI data set hosted on Scott Emerson's webpage. More detailed description is hosted there, at ``http://www.emersonstatistics.com/datasets/mri.pdf''.
}
\usage{data(mri)}
\format{
  A data frame with 735 observations on the following 30 variables.
  \describe{
    \item{\code{ptid}}{participant identification number.}
    \item{\code{mridate}}{the date on which the participant underwent MRI scan in MMDDYY format.}
    \item{\code{age}}{participant age at time of MRI, in years.}
    \item{\code{male}}{indicator of whether participant is male (0=female, 1=male).}
    \item{\code{race}}{indicator of participant's race (1=white, 2=black, 3=Asian, 4=other).}
    \item{\code{weight}}{participant's weight at time of MRI (pounds).}
    \item{\code{height}}{participant's height at time of MRI (centimeters).}
    \item{\code{packyrs}}{participant smoking history in pack years (1 pack year = smoking 1 pack of cigarettes per day for 1 year). A participant who has never smoked has 0 pack years.}
    \item{\code{yrsquit}}{number of years since quitting smoking. A current smoker will have a nonzero packyrs and a 0 for yrsquit. A never smoker will have a zero for both variables.}
    \item{\code{alcoh}}{average alcohol intake for the participant for the two weeks prior to MRI (drinks per week, where one drink is 1 oz. whiskey, 4 oz. wine, or 12 oz.beer).}
    \item{\code{physact}}{physical activity of the participant for the week prior to MRI (1,000 kcal).}
    \item{\code{chf}}{indicator of whether the participant had been diagnosed with congestive heart failure prior to MRI (0=no, 1=yes).}
    \item{\code{chd}}{indicator of whether the participant had been diagnosed with coronary heart disease prior to MRI (0=no, 1=diagnosis of angina, 2=diagnosis of myocardial infarction).}
    \item{\code{stroke}}{indicator of whether the participant had been diagnosed with a cerebrovascular event prior to MRI (0=no, 1=diagnosis of a transient ischemic attack, 2=diagnosis of stroke).}
    \item{\code{diabetes}}{indicator of whether the participant had been diagnosed with diabetes prior to MRI (0=no, 1=yes).}
    \item{\code{genhlth}}{an indicator of the participant's view of their own health (1=excellent, 2=very good, 3=good, 4=fair, 5=poor)}
    \item{\code{ldl}}{a laboratory measure of low density lipoprotein (a kind of cholesterol) in the participant's blood at the time of MRI (mg/dL).}
    \item{\code{alb}}{a laboratory measure of albumin, a kind of protein, in the participant's blood at the time of MRI (g/L).}
    \item{\code{crt}}{a laboratory measure of creatinine, a waste product, in the participant's blood at the time of MRI (mg/dL).}
    \item{\code{plt}}{a laboratory measure of the number of platelets circulating in the participant's blood at the time of MRI (1000 per cubic mm).}
    \item{\code{sbp}}{a measurement of the participant's systolic blood pressure in their arm at the time of MRI (mm Hg).}
    \item{\code{aai}}{the ratio of systolic blood pressure measured in the participant's ankle at time of MRI to the systolic blood pressure in the participant's arm.}
    \item{\code{fev}}{a measure of the forced expiratory volume in the participant at the time of MRI (L/sec).}
    \item{\code{dsst}}{a measure of cognitive function (Digit Symbol Substitution Test) for the participant at the time of MRI. Maximum score possible is 100.}
    \item{\code{atrophy}}{a measure of global brain activity detected on MRI. Measurements range from 0 to 100, with 100 being the most severe atrophy.}
    \item{\code{whgrd}}{a measure of white matter changes detected on MRI. 0 means no changes, 9 means marked changes.}
    \item{\code{numinf}}{a count of the number of distinct regions identified on MRi scan which were suggestive of infarcts.}
    \item{\code{volinf}}{a measure of the total volume of infarct-like lesions found on MRI scan (cubic cm).}
    \item{\code{obstime}}{the total time (in days) that the participant was observed on study between the date of MRI and death or September 16, 1997, whichever came first.}
    \item{\code{death}}{an indicator that the participant was observed to die while on study. If 1, the number of days recorded in \code{obstime} is the number of days from that participant's MRI to their death. If 0, the number of days in \code{obstime} is the number of days between that participant's MRI and September 16, 1997.}
  }
}

\keyword{datasets}
