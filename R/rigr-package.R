

#' %% ~~ data name/kind ... ~~ MRI data
#' 
#' %% ~~ A concise (1-5 lines) description of the dataset. ~~ An MRI data set
#' hosted on Scott Emerson's webpage. More detailed description is hosted
#' there, at ``http://www.emersonstatistics.com/datasets/mri.pdf''.
#' 
#' 
#' @name mri
#' @docType data
#' @format A data frame with 735 observations on the following 30 variables.
#' \describe{ \item{list("ptid")}{participant identification number.}
#' \item{list("mridate")}{the date on which the participant underwent MRI scan
#' in MMDDYY format.} \item{list("age")}{participant age at time of MRI, in
#' years.} \item{list("male")}{indicator of whether participant is male
#' (0=female, 1=male).} \item{list("race")}{indicator of participant's race
#' (1=white, 2=black, 3=Asian, 4=other).} \item{list("weight")}{participant's
#' weight at time of MRI (pounds).} \item{list("height")}{participant's height
#' at time of MRI (centimeters).} \item{list("packyrs")}{participant smoking
#' history in pack years (1 pack year = smoking 1 pack of cigarettes per day
#' for 1 year). A participant who has never smoked has 0 pack years.}
#' \item{list("yrsquit")}{number of years since quitting smoking. A current
#' smoker will have a nonzero packyrs and a 0 for yrsquit. A never smoker will
#' have a zero for both variables.} \item{list("alcoh")}{average alcohol intake
#' for the participant for the two weeks prior to MRI (drinks per week, where
#' one drink is 1 oz. whiskey, 4 oz. wine, or 12 oz.beer).}
#' \item{list("physact")}{physical activity of the participant for the week
#' prior to MRI (1,000 kcal).} \item{list("chf")}{indicator of whether the
#' participant had been diagnosed with congestive heart failure prior to MRI
#' (0=no, 1=yes).} \item{list("chd")}{indicator of whether the participant had
#' been diagnosed with coronary heart disease prior to MRI (0=no, 1=diagnosis
#' of angina, 2=diagnosis of myocardial infarction).}
#' \item{list("stroke")}{indicator of whether the participant had been
#' diagnosed with a cerebrovascular event prior to MRI (0=no, 1=diagnosis of a
#' transient ischemic attack, 2=diagnosis of stroke).}
#' \item{list("diabetes")}{indicator of whether the participant had been
#' diagnosed with diabetes prior to MRI (0=no, 1=yes).}
#' \item{list("genhlth")}{an indicator of the participant's view of their own
#' health (1=excellent, 2=very good, 3=good, 4=fair, 5=poor)}
#' \item{list("ldl")}{a laboratory measure of low density lipoprotein (a kind
#' of cholesterol) in the participant's blood at the time of MRI (mg/dL).}
#' \item{list("alb")}{a laboratory measure of albumin, a kind of protein, in
#' the participant's blood at the time of MRI (g/L).} \item{list("crt")}{a
#' laboratory measure of creatinine, a waste product, in the participant's
#' blood at the time of MRI (mg/dL).} \item{list("plt")}{a laboratory measure
#' of the number of platelets circulating in the participant's blood at the
#' time of MRI (1000 per cubic mm).} \item{list("sbp")}{a measurement of the
#' participant's systolic blood pressure in their arm at the time of MRI (mm
#' Hg).} \item{list("aai")}{the ratio of systolic blood pressure measured in
#' the participant's ankle at time of MRI to the systolic blood pressure in the
#' participant's arm.} \item{list("fev")}{a measure of the forced expiratory
#' volume in the participant at the time of MRI (L/sec).} \item{list("dsst")}{a
#' measure of cognitive function (Digit Symbol Substitution Test) for the
#' participant at the time of MRI. Maximum score possible is 100.}
#' \item{list("atrophy")}{a measure of global brain activity detected on MRI.
#' Measurements range from 0 to 100, with 100 being the most severe atrophy.}
#' \item{list("whgrd")}{a measure of white matter changes detected on MRI. 0
#' means no changes, 9 means marked changes.} \item{list("numinf")}{a count of
#' the number of distinct regions identified on MRi scan which were suggestive
#' of infarcts.} \item{list("volinf")}{a measure of the total volume of
#' infarct-like lesions found on MRI scan (cubic cm).}
#' \item{list("obstime")}{the total time (in days) that the participant was
#' observed on study between the date of MRI and death or September 16, 1997,
#' whichever came first.} \item{list("death")}{an indicator that the
#' participant was observed to die while on study. If 1, the number of days
#' recorded in \code{obstime} is the number of days from that participant's MRI
#' to their death. If 0, the number of days in \code{obstime} is the number of
#' days between that participant's MRI and September 16, 1997.} }
#' @keywords datasets
NULL





#' Descriptive Statistics, One Sample Inference, Regression, and Plotting in an
#' Introductory Statistics Course
#' 
#' Developed by Scott S. Emerson, M.D., Ph.D., Andrew J. Spieker, Brian D.
#' Williamson, Travis Y. Hee Wai, and Solomon Lim in the University of
#' Washington Department of Biostatistics. Aims to facilitate more widespread
#' use of R by implementing more intuitive layout and functionality for
#' existing R functions.
#' 
#' \tabular{ll}{ Package: \tab uwIntroStats\cr Type: \tab Package\cr Version:
#' \tab 0.0.3\cr Date: \tab 2015-09-01\cr License: \tab GPL-2\cr } A set of
#' tools designed to facilitate easy adoption of R for students in introductory
#' classes with little programming experience. Compiles output from existing
#' routines together in an intuitive format, and adds functionality to existing
#' functions. For instance, the regression function can perform linear models,
#' generalized linear models, Cox models, or generalized estimating equations.
#' The user can also specify multiple-partial F-tests to print out with the
#' model coefficients. We also give many routines for descriptive statistics
#' and plotting.
#' 
#' @name uwIntroStats-package
#' @aliases uwIntroStats-package uwIntroStats
#' @docType package
#' @author Scott S. Emerson, M.D., Ph.D, Andrew J. Spieker, and Brian D.
#' Williamson, Travis Y. Hee Wai, and Solomon Lim
#' 
#' Maintainer: Travis Y. Hee Wai <theewai@@uw.edu>
#' @keywords package
NULL



