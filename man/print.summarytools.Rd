\name{print.summarytools}
\alias{print.summarytools}
\alias{print}
\alias{view}
\title{Print Method for Objects of Class \code{summarytools}.}
\description{Display \code{summarytools} objects in the console, in Web Browser or in \emph{RStudio}'s Viewer.}
\usage{
\method{print}{summarytools}(x, method="pander", include.footer=TRUE, silent=FALSE, \dots)
view(x, method = "viewer", include.footer=TRUE, silent=FALSE, \dots)
}

\arguments{
  \item{x}{summarytools object that was generated with \code{freq}, \code{descr} or \code{dfSummary}.}
  \item{method}{One of \dQuote{pander}, \dQuote{viewer} or \dQuote{browser}. Defaults to \dQuote{pander} with \code{print()} and to \dQuote{viewer} with \code{view()}. If \dQuote{viewer} is used outside \emph{RStudio}, Web Browser will be used instead.}
  \item{include.footer}{Include footer (package name and version, R version, date) in HTML tables. \code{TRUE} by default. Has no effect when \code{method} is set to \dQuote{pander}.}
  \item{silent}{Hide console messages indicating location of temporary HTML file (relevant only when \code{method} is \dQuote{viewer} or \dQuote{browser})}
  \item{\dots}{Additional arguments (not used for now).}
}
\details{
The \code{view()} wrapper function makes it more practical to generate HTML files with a single line of code (see examples).

HTML tables are generated using \code{xtable::print.xtable} and use Bootstrap CSS. Ascii / markdown tables are generated with \code{pander::pander}. See References section for a list of available \emph{pander} options.
}
\value{
  \code{NULL} when \code{method="pander"} ; a \emph{file path} (returned invisibly) when \code{method="viewer"} or \code{method="browser"}. In the latter case, the HTML file is passed to \code{shell.exec}, thus opening file in Web Browser.}
\references{
\url{https://github.com/dcomtois/summarytools} 
\url{http://rapporter.github.io/pander/#general-options} 
\url{http://getbootstrap.com/css/#tables}
}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\seealso{
\code{\link{xtable}}, \code{\link{pander}} 
}
\examples{
  data(iris)
  \dontrun{view(dfSummary(iris))}
  \dontrun{view(descr(iris,transpose=TRUE), "browser")}
  print(freq(iris$Species), "pander")  
}
\keyword{ print }
\keyword{ methods }
