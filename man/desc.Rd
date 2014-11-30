\name{descr}
\alias{descr}
\title{Descriptive Univariate Statistics for Numerical Data}
\description{Calculates mean, standard deviation, min, max, median, MAD, IQR, CV, skewness, SE.skewness, and kurtosis.}
\usage{
descr(x, na.rm=TRUE, style="simple", round.digits=2,
     justify="right", plain.ascii=TRUE, file=NA,
     append=FALSE, transpose=FALSE, ...)}
\arguments{
  \item{x}{Numerical vector.}
  \item{na.rm}{argument to be passed to statistical functions. Defaults to \code{TRUE}.}
  \item{style}{Style of pander tables. Defaults to dQuote{grid}.}
  \item{round.digits}{Number of sig. digits to keep. Defaults to \code{2}.}
  \item{justify}{\pkg{pander} argument. Defaults to \dQuote{right}.}
  \item{plain.ascii}{\pkg{pander} argument; when results are displayed in console, this avoids having markdown markup characters int the tables. Defaults to \code{TRUE}.}
  \item{file}{File name to write output to. Defaults to \code{NA}. To append output to an existing text file, use \code{append=TRUE}.}
  \item{append}{When \dQuote{file} argument is supplied, this parameter indicates whether to append output to existing file (\code{TRUE}) or to overwrite any existing file (\code{FALSE}, default).}
  \item{transpose}{Makes variables appears as columns, and stats as rows. Defaults to \code{FALSE}.}
  \item{\dots}{Additional arguments passed to \pkg{pander}.}
}
\value{A list with 2 elements; first one in a matrix (table) containing all the stats. Second, a table of statistics and table of counts (valid vs \code{<NA>} observations).}
\references{\url{https://github.com/dcomtois/summarytools}}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\examples{
data(iris)
descr(iris)
descr(iris, transpose=TRUE)
}
\keyword{univariate}
\keyword{descriptives}
