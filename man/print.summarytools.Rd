\name{print.summarytools}
\alias{print.summarytools}
\alias{print}
\title{Print method for objects of class \code{summarytools}.}
\description{Display \code{summarytools} objects in the console or in \emph{RStudio}'s Viewer.}
\usage{
  \method{print}{summarytools}(x, method="pander", open=TRUE, \dots)
}

\arguments{
  \item{x}{summarytools object that was generated with \code{freq}, \code{descr} or \code{dfSummary}.}
  \item{method}{One of \dQuote{pander}, \dQuote{view} or \dQuote{browser}. Defaults to \dQuote{pander}.}
  \item{open}{Open the generated file with system's default viewer through \code{shell.exec}. Defaults to \code{TRUE}.}
  \item{\dots}{Additional arguments (not used for now).}
}

\value{
  \code{NULL} when \code{method="pander"} ; a \emph{file path} (returned invisibly) when \code{method="viewer"} or \code{method="browser"}. In the latter case, the \emph{html file} is passed to \code{shell.exec} which opens the file in default browser.}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\references{\url{https://github.com/dcomtois/summarytools}}
\examples{
  data(iris)
  print(freq(iris$Species))
}

\keyword{ print }
\keyword{ RStudio viewer }
\keyword{ html }
