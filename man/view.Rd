\name{view}
\alias{view}
\title{Wrapper function around \code{print.summarytools} to facilitate generation of \emph{html} files.}
\description{
  Takes a \code{summarytools} object as argument and calls its print method using \code{method="viewer"} by default. This generates an \emph{html} file in the session's temporary directory and displays this file in \emph{RStudio}'s Viewer pane, falling back to default browser when used outside \emph{RStudio}.}
\usage{
  view(x, method = "viewer", ...)
}
\arguments{
  \item{x}{A \code{summarytools} object created with one of \code{freq}, \code{descr} and \code{dfSummary}.}
  \item{method}{One of the 3 following: \dQuote{viewer} (default), \dQuote{browser}, \dQuote{pander}. Note: To display \code{x} without any formatting at all, simply use \code{as.table(x)}}
  \item{\dots}{Not used for now.}
}

\details{This function makes it more practical to generate html files with a single line of code, such as in \code{view(freq(iris$Species))}.}

\value{
    For \code{method=="pander"}, nothing (\code{NULL}) is returned. With the two other methods (\dQuote{viewer} and \dQuote{browser}), returns the \emph{html} file's absolute path.}

\author{
  Dominic Comtois <dominic.comtois@gmail.com>
}

\examples{
  data(iris)
  \dontrun{view(dfSummary(iris))}
  \dontrun{view(descr(iris,transpose=TRUE), "browser")}
  view(freq(iris$Species), "pander")
}

\keyword{ html }
\keyword{ stylized output }
\keyword{ formatted print }
