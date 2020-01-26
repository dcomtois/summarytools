#' Tools to Quickly and Neatly Summarize Data
#'
#' \pkg{summarytools} provides users with functions to neatly and quickly
#' summarize numerical and categorical data. Data frame summaries, frequency
#' tables and cross-tabulations, as well as common descriptive (univariate)
#' statistics can be produced in a straightforward manner. Users with little to
#' no prior R programming experience but who are familiar with popular commercial
#' statistical software such as SAS, SPSS and Stata should feel right at home.
#'
#' These are the four core functions: 
#' \describe{
#'   \item{dfSummary}{Extensive yet legible data frame summaries.}
#'   \item{freq}{Frequency tables supporting weights and displaying proportions
#'   of valid and of total data, including cumulative proportions.}
#'   \item{descr}{All common univariate descriptive stats for single vectors
#'      or for all numerical vectors in a data frame.}
#'   \item{ctable}{Cross-tabulations for two categorical vectors or factors.
#'     Choose between \emph{Total}, \emph{Columns} or \emph{Rows} proportions.}
#' }
#'
#' \strong{Output formats} are:
#'  \describe{
#'    \item{plain ascii}{Ideal when looking at results in the console.}
#'    \item{rmarkdown}{Ideal when writing short papers or presentations.}
#'    \item{html}{This format is well integrated in RStudio (but will
#'    work with any browser). Use the \code{view()} function to see results
#'    appear directly in RStudio's Viewer or in your default Web
#'    Browser.}
#'  }
#'
"_PACKAGE"
