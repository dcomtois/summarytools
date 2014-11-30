\name{view}
\alias{view}
\title{Wrapper function for \code{print.summarytools} to facilitate creating \emph{html} files.}
\description{
  takes a \code{summarytools} object as arguement and calls its print method using (by default) \code{method="viewer"}, which will display the html generated file in RStudio's Viewer pane.}
\usage{
  view(x, method = "viewer", ...)
}
\arguments{
  \item{x}{A \code{summarytools} object created with one of \emph{freq} (or \emph{frequencies}), \emph{desc} (or \emph{unistats}), and \emph{dfSummary}.}
  \item{method}{One of 3 possibilities: \dQuote{viewer} (default), \dQuote{browser}, \dQuote{pander}. To display the table without any formatting, use \code{as.table(x)}}
  \item{\dots}{Not used for now.}
}

\details{This function makes it easier to generate html files with a single line of code, for instance \code{view(freq(iris$Species))}. This create an html file in the user's temp directory, and the path to this file is returned as a string.}

\value{
    For \code{method=="pander"}, nothing \code{NULL} is returned; on the other hand. Using any of the two other methods (\dQuote{viewer} and \dQuote{browser}) will return the \emph{html} absolute file path in the temporary session directory.}

\author{
  Dominic Comtois <dominic.comtois@gmail.com>
}

\note{
Don't confuse \code{view} (lowercase) with the \code{utils::View} (capital V) function which allows visionning dataframes inside an RStudio pane. \code{view} allows one to quickly build a short formatted short report with the results of any of the 3 main \emph{summarytools} functions at hand.}

\examples{
  data(iris)
%  view(freq(iris$Species))
  view(desc(iris,transpose=TRUE), "pander")
  view(dfSummary(iris), "pander")
}

\keyword{ html }
\keyword{ stylized output }
\keyword{ formatted print }
