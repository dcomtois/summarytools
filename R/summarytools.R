#' Extensive Summarizing Tools With Flexible Output
#'
#' \pkg{summarytools} provides users with functions to neatly and
#' simply summarize numerical and categorical data. Dataframe
#' summaries, frequency tables and cross-tabulations, as well as
#' common univariate statistics can be produced in a
#' straighforward manner, so that users with little to no prior
#' R programming experience can get to know their data rapidly.
#'
#' The four \dQuote{core} functions are:
#' \describe{
#'   \item{dfSummary}{Extensive yet very legible data frame summmaries.}
#'   \item{freq}{Frequency tables supporting weights and displaying valid,
#'      and total proportions, as well as cumulative proportions.}
#'   \item{descr}{All common univariate descriptive stats for single vectors
#'      or for all numerical vectors inside a data frame.}
#'   \item{ctable}{Cross-tabulations for two categorical vectors or factors.
#'     Choose between \emph{total}, \emph{column} or \emph{row} proportions.}
#' }
#'
#' Output formats are:
#'  \describe{
#'    \item{plain ascii}{Ideal when looking at results in the console.}
#'    \item{rmarkdown}{Ideal when writing short papers or presentations.}
#'    \item{html}{This format is especially well-integrated in
#'      \emph{RStudio} and offers an esthetically pleasant alternative to
#'      plain ascii. Just use \code{view()} function to see results appear
#'      directly in \emph{RStudio's Viewer} or in your preferred Web Browser.}
#'  }
#'
"_PACKAGE"
