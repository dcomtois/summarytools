\name{freq}
\alias{freq}
\alias{frequencies}
\title{Frequency Tables for Discrete Data and Factors}
\description{Displays frequencies as well as valid/missing observation information. Optionally uses package 'pander' for prettier tables.}
\usage{
freq(x, round.digits=2, echo=TRUE, style="simple", justify="right",
            plain.ascii=TRUE, file=NA, append=FALSE, ...)
}
\arguments{
  \item{x}{Vector of discrete (categorical) data.}
  \item{round.digits}{Number of sig. digits to keep in output. Defaults to 2.}
  \item{echo}{If TRUE (default), pander tables will be displayed.}
  \item{style}{Style of pander tables, one of "simple" (default), "multiline", "grid" and "rmarkdown".}
  \item{justify}{Pander argument. Defaults to "right".}
  \item{plain.ascii}{Pander argument, defaults to TRUE. When set to FALSE, markdown markup characters will appear in output tables.}
  \item{file}{File name to write output to. Defaults to NA. When this argument is supplied, the table is not echoed to the console. To append output into an existing text file, use append=TRUE.}
  \item{append}{When 'file' argument is supplied, this parameter indicates whether to append output to existing file (TRUE) or to overwrite any existing file (FALSE, default).}
  \item{\dots}{Additional arguments passed to pander().}
}
\value{A frequency table of class 'matrix'.}
\author{Dominic Comtois <dominic.comtois at gmail.com>}
\examples{
colors <- c("blue","red","blue","yellow",NA)
frequencies(colors)
frequencies(colors, echo=TRUE)
}
\keyword{ frequencies }
\keyword{ frequency }
