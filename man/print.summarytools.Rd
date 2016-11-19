\name{print.summarytools}
\alias{print.summarytools}
\alias{print}
\title{Print Method for Objects of Class \code{summarytools}.}
\description{Display \code{summarytools} objects in the console, in Web Browser or in \emph{RStudio}'s Viewer.}
\usage{
  \method{print}{summarytools}(x, method="pander", silent=FALSE, \dots)
}

\arguments{
  \item{x}{summarytools object that was generated with \code{freq}, \code{descr} or \code{dfSummary}.}
  \item{method}{One of \dQuote{pander}, \dQuote{viewer} or \dQuote{browser}. Defaults to \dQuote{pander}. If \dQuote{viewer} is used outside \emph{RStudio}, Web Browser will be used instead.}
  \item{silent}{Hide console messages indicating location of temporary HTML file (relevant only when \code{method} is \dQuote{viewer} or \dQuote{browser})}
  \item{\dots}{Additional arguments (not used for now).}
}
\details{
HTML tables are generated using \code{xtable::print.xtable} and use Bootstrap CSS. Ascii / markdown tables are generated with \code{pander::pander}. See References section for a list of available \emph{pander} options.
}
\value{
  \code{NULL} when \code{method="pander"} ; a \emph{file path} (returned invisibly) when \code{method="viewer"} or \code{method="browser"}. In the latter case, the HTML file is passed to \code{shell.exec} which will open the file in Web Browser.}
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
  print(freq(iris$Species))
}
\keyword{ print }
\keyword{ methods }
