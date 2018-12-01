#' Tobacco Use and Health - Simulated Dataset
#'
#' A simulated datasets of 1,000 subjects, with the following variables:
#'
#' \itemize{
#'   \item gender Factor with 2 levels: \dQuote{F} and \dQuote{M}, having 
#'     roughly 500 of each.
#'   \item age Numerical.
#'   \item age.gr Factor with 4 age categories.
#'   \item BMI Body Mass Index (numerical).
#'   \item smoker Factor (\dQuote{Yes} / \dQuote{No}).
#'   \item cigs.per.day Number of cigarettes smoked per day
#'     (numerical).
#'   \item diseased Factor (\dQuote{Yes} / \dQuote{No}).
#'   \item disease Character.
#'   \item samp.wgts Sampling weights (numerical).
#' }
#'
#' A note on simulation: probability for an individual to fall into
#' category \dQuote{diseased} is based on an arbitrary function
#' involving age, BMI and number of cigarettes per day.
#'
#' A copy of this dataset is also \strong{available in French} under
#' the name \dQuote{tabagisme}.
#'
#' @docType data
#' @keywords datasets
#' @name tobacco
#' @usage data(tobacco)
#' @format A data frame with 1000 rows and 9 variables
NULL
