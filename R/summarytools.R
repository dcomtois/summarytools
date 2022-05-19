#' Tools to Quickly and Neatly Summarize Data
#'
#' \pkg{summarytools} is a collection of functions which neatly and quickly
#' summarize numerical and categorical data. Data frame summaries, frequency
#' tables and cross-tabulations, as well as common descriptive (univariate)
#' statistics can be produced in a straightforward manner. Users with little to
#' no prior R programming experience but who are familiar with popular commercial
#' statistical software such as SAS, SPSS and Stata will feel right at home.
#'
#' These are the four core functions: 
#' \describe{
#'   \item{dfSummary}{Extensive yet legible data frame summaries.}
#'   \item{freq}{Frequency tables supporting weights and displaying proportions
#'   of valid and of total data, including cumulative proportions.}
#'   \item{descr}{All common univariate descriptive stats applied to a single
#'   vector or to all numerical vectors contained in a data frame.}
#'   \item{ctable}{Cross-tabulations for pairs of categorical variables -- 
#'   accepting both numerical and character vectors, as well as factors.
#'   Choose between \emph{Total}, \emph{Columns} or \emph{Rows} proportions,
#'   and optionally display chi-square statistic (with corresponding p-value), 
#'   odds ratio, as well as risk ratio with flexible confidence intervals.}
#' }
#'
#' \strong{Choice of output formats}:
#'  \describe{
#'    \item{plain ascii}{Ideal when showing results in the R console.}
#'    \item{rmarkdown}{Perfect for writing short papers or presentations.}
#'    \item{html}{A format very well integrated in \emph{RStudio} -- but will
#'    work with any Web browser. Use the \code{\link{view}} function to display
#'    results directly in \emph{RStudio}'s viewer, or in your preferred Web
#'    browser.}
#'  }
#'
"_PACKAGE"
