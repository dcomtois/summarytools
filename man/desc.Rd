\name{desc}
\alias{desc}
\alias{unistats}
\title{Descriptive univariate Statistics for Numerical Data}
\description{Calculates mean, standard deviation, min, max, median, MAD, IQR, CV, skewness, SE.skewness, and kurtosis. Optionally uses package 'pander' for prettier tables.}
\usage{
desc(x, na.rm=TRUE, round.digits=2, echo=TRUE, transpose=FALSE, style="simple",
     justify="right", plain.ascii=TRUE, file=NA, append=FALSE, ...)}
\arguments{
  \item{x}{Numerical vector.}
  \item{na.rm}{argument to be passed to statistical functions. Defaults to TRUE.}
  \item{round.digits}{Number of sig. digits to keep. Defaults to 2.}
  \item{echo}{If TRUE, pander tables will be displayed. Defaults to TRUE.}
  \item{transpose}{Makes variables appears as columns, and stats as rows. Defaults to FALSE.}
  \item{style}{Style of pander tables. Defaults to "grid".}
  \item{justify}{Pander argument. Defaults to "right".}
  \item{plain.ascii}{Pander argument. Defaults to TRUE.}
  \item{file}{File name to write output to. Defaults to NA. When this argument is supplied, the table is not echoed to the console. To append output into an existing text file, use append=TRUE.}
  \item{append}{When 'file' argument is supplied, this parameter indicates whether to append output to existing file (TRUE) or to overwrite any existing file (FALSE, default).}
  \item{\dots}{Additional arguments passed to pander().}
}
\value{A list with 2 elements; first one in a matrix (table) containing all the stats. if applicable, table of statistics and table of counts (valid vs <NA> observations).}
\references{\url{https://github.com/dcomtois/summarytools}}
\author{Dominic Comtois <dominic.comtois @ gmail.com>}
\examples{
data(iris)
desc(iris)
desc(iris, transpose=TRUE)
}
\keyword{univariate}
\keyword{descriptives}
