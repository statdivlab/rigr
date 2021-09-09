#' MRI dataset
#' 
#' Data from an observational study of the incidence of cardiovascular disease 
#' (especially heart attacks and congestive heart failure) and cerebrovascular disease 
#' (especially strokes) in the U.S. elderly. More information, including a coding key,
#'  is available at
#' \url{http://www.emersonstatistics.com/datasets/mri.doc}.
#' 
#'
#' @format A data frame with 735 rows and 30 variables:
#' \describe{
#' 
#' \item{ptid}{participant identification number.}
#' 
#' \item{mridate}{the date on which the participant underwent MRI scan
#' in MMDDYY format.} 
#' 
#' \item{age}{participant age at time of MRI, in
#' years.} 
#' 
#' \item{sex}{participant sex (male or female).} 
#' 
#' \item{race}{indicator of participant's race
#' (1=white, 2=black, 3=Asian, 4=other).} 
#' 
#' \item{weight}{participant's
#' weight at time of MRI (pounds).} 
#' 
#' \item{height}{participant's height
#' at time of MRI (centimeters).} 
#' 
#' \item{packyrs}{participant smoking
#' history in pack years (1 pack year = smoking 1 pack of cigarettes per day
#' for 1 year). A participant who has never smoked has 0 pack years.}
#' 
#' \item{yrsquit}{number of years since quitting smoking. A current
#' smoker will have a nonzero packyrs and a 0 for yrsquit. A never smoker will
#' have a zero for both variables.} 
#' 
#' \item{alcoh}{average alcohol intake
#' for the participant for the two weeks prior to MRI (drinks per week, where
#' one drink is 1 oz. whiskey, 4 oz. wine, or 12 oz.beer).}
#' 
#' \item{physact}{physical activity of the participant for the week
#' prior to MRI (1,000 kcal).} 
#' 
#' \item{chf}{indicator of whether the
#' participant had been diagnosed with congestive heart failure prior to MRI
#' (0=no, 1=yes).} 
#' 
#' \item{chd}{indicator of whether the participant had
#' been diagnosed with coronary heart disease prior to MRI (0=no, 1=diagnosis
#' of angina, 2=diagnosis of myocardial infarction).}
#' 
#' \item{stroke}{indicator of whether the participant had been
#' diagnosed with a cerebrovascular event prior to MRI (0=no, 1=diagnosis of a
#' transient ischemic attack, 2=diagnosis of stroke).}
#' 
#' \item{diabetes}{indicator of whether the participant had been
#' diagnosed with diabetes prior to MRI (0=no, 1=yes).}
#' 
#' \item{genhlth}{an indicator of the participant's view of their own
#' health (1=excellent, 2=very good, 3=good, 4=fair, 5=poor)}
#' 
#' \item{ldl}{a laboratory measure of low density lipoprotein (a kind
#' of cholesterol) in the participant's blood at the time of MRI (mg/dL).}
#' 
#' \item{alb}{a laboratory measure of albumin, a kind of protein, in
#' the participant's blood at the time of MRI (g/L).} 
#' 
#' \item{crt}{a
#' laboratory measure of creatinine, a waste product, in the participant's
#' blood at the time of MRI (mg/dL).} 
#' 
#' \item{plt}{a laboratory measure
#' of the number of platelets circulating in the participant's blood at the
#' time of MRI (1000 per cubic mm).} 
#' 
#' \item{sbp}{a measurement of the
#' participant's systolic blood pressure in their arm at the time of MRI (mm
#' Hg).} 
#' 
#' \item{aai}{the ratio of systolic blood pressure measured in
#' the participant's ankle at time of MRI to the systolic blood pressure in the
#' participant's arm.} 
#' 
#' \item{fev}{a measure of the forced expiratory
#' volume in the participant at the time of MRI (L/sec).} 
#' 
#' \item{dsst}{a
#' measure of cognitive function (Digit Symbol Substitution Test) for the
#' participant at the time of MRI. Maximum score possible is 100.}
#' 
#' \item{atrophy}{a measure of global brain activity detected on MRI.
#' Measurements range from 0 to 100, with 100 being the most severe atrophy.}
#' 
#' \item{whgrd}{a measure of white matter changes detected on MRI. 0
#' means no changes, 9 means marked changes.} 
#' 
#' \item{numinf}{a count of
#' the number of distinct regions identified on MRI scan which were suggestive
#' of infarcts.} 
#' 
#' \item{volinf}{a measure of the total volume of
#' infarct-like lesions found on MRI scan (cubic cm).}
#' 
#' \item{obstime}{the total time (in days) that the participant was
#' observed on study between the date of MRI and death or September 16, 1997,
#' whichever came first.} 
#' 
#' \item{death}{an indicator that the
#' participant was observed to die while on study. If 1, the number of days
#' recorded in \code{obstime} is the number of days from that participant's MRI
#' to their death. If 0, the number of days in \code{obstime} is the number of
#' days between that participant's MRI and September 16, 1997.} }
#' 
#' @source \url{http://www.emersonstatistics.com/datasets/mri.txt}
"mri"

#' Salary dataset
#' 
#' Data from a study of 1,597 faculty members at a single US university. Includes information on 
#' monthly salary each year from 1976 through 1995,  as well as sex, highest degree attained, 
#' year of highest degree, field, year hired, rank, and administrative duties. 
#' More information, including a coding key, is available at
#' \url{http://www.emersonstatistics.com/datasets/salary.doc}.
#' 
#'
#' @format A data frame with 19792 rows and 11 variables:
#' \describe{
#' 
#' \item{case}{case number}
#' 
#' \item{id}{identification number for the faculty member}
#' 
#' \item{sex}{M (male) or F (female)}
#' 
#' \item{deg}{highest degree attained: PhD, Prof (professional degree, eg, medicine or law), or Other (Master's or Bachelor's degree)}
#' 
#' \item{yrdeg}{year highest degree attained}
#' 
#' \item{field}{Arts (Arts and Humanities), Prof (professional school, e.g., Business, Law, Engineering or Public Affairs), or Other}
#' 
#' \item{startyr}{year in which the faculty member was hired (2 digits)}
#' 
#' \item{year}{year (2 digits)}
#' 
#' \item{rank}{rank of the faculty member in this year: Assist (Assistant), Assoc (Associate), or Full (Full)}
#' 
#' \item{admin}{indicator of whether the faculty member had administrative duties (eg, department chair) in this year: 1 (yes), or 0 (no)}
#' 
#' \item{salary}{monthly salary of the faculty member in this year in dollars}
#' 
#' }
#' 
#' @source \url{http://www.emersonstatistics.com/datasets/salary.txt}
"salary"

#' PSA dataset
#' 
#' Data from a study of 50 men having hormonally treated prostate cancer. Includes information on 
#' PSA levels, tumor characteristics, remission status, age, and disease state.
#' More information, including a coding key, is available at
#' \url{http://www.emersonstatistics.com/datasets/PSA.doc}.
#' 
#'
#' @format A data frame with 50 rows and 9 variables:
#' \describe{
#' 
#' \item{ptid}{patient identifier}
#' 
#' \item{nadirpsa}{lowest PSA value attained post therapy (ng/ml)}
#' 
#' \item{pretxpsa}{PSA value prior to therapy (ng/ml)}
#' 
#' \item{ps}{performance status (0= worst, 100= best)}
#' 
#' \item{bss}{bone scan score (1= least disease, 3= most)}
#' 
#' \item{grade}{tumor grade (1= least aggressive, 3= most)}
#' 
#' \item{age}{patient's age (years)}
#' 
#' \item{obstime}{time observed in remission (months)}
#' 
#' \item{inrem}{indicator whether patient still in remission at last follow-up (yes or no)}
#' 
#' }
#' 
#' @source \url{http://www.emersonstatistics.com/datasets/psa.txt}
"psa"

#' FEV dataset
#' 
#' Data from a study of 654 children on the relationship between smoking status and 
#' lung function (measured by FEV). Each row corresponds to a single clinic visit and contains
#' information on age, height, sex, FEV, and smoking status.
#' More information, including a coding key, is available at
#' \url{http://www.emersonstatistics.com/datasets/fev.doc}.
#' 
#'
#' @format A data frame with 654 rows and 7 variables:
#' \describe{
#' 
#' \item{seqnbr}{case number (the numbers 1 to 654)}
#' 
#' \item{subjid}{subject identification number (unique for each different child)}
#' 
#' \item{age}{subject age at time of measurement (years)}
#' 
#' \item{fev}{measured forced exhalation volume (liters per second)}
#' 
#' \item{height}{subject height at time of measurement (inches)}
#' 
#' \item{sex}{subject sex}
#' 
#' \item{smoke}{smoking habits ("yes" or "no")}
#' } 
#' 
#' @source \url{http://www.emersonstatistics.com/datasets/fev.txt}
"fev"