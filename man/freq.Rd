\name{freq}
\alias{freq}
\title{Frequency Tables for Factors and other Discrete Data}
\description{Displays frequencies as well as valid/missing observation information.}
\usage{
freq(x, round.digits=2, style="simple", justify="right",
     plain.ascii=TRUE, file=NA, append=FALSE,
     escape.pipes=FALSE, ...)
}
\arguments{
  \item{x}{Vector of discrete (categorical) data.}
  \item{round.digits}{Number of sig. digits to keep in output. Defaults to \code{2}.}
  \item{style}{Style of pander tables, one of \dQuote{simple} (default), \dQuote{multiline}, \dQuote{grid} and \dQuote{rmarkdown}.}
  \item{justify}{\pkg{pander} argument. Defaults to "right".}
  \item{plain.ascii}{\pkg{pander} argument; when results are displayed in console, this avoids having \emph{markdown} markup characters int the tables. Defaults to TRUE.}
  \item{file}{File name to write output to. Defaults to \code{NA}. To append output to an existing text file, use \code{append=TRUE}.}
  \item{append}{When \dQuote{file} argument is supplied, this indicates whether to append output to existing file (\code{TRUE}) or to overwrite any existing file (\code{FALSE}, default). If \code{TRUE} and no file exists, a new file will be created.}
  \item{escape.pipe}{Only useful when \code{style='grid'} and \code{file} argument is not \code{NA}, in which case it will escape the pipe character (|) to allow Pandoc to correctly convert multiline cells.}
  \item{\dots}{Additional arguments passed to \pkg{pander}.}
}
\value{A frequency table of class \code{matrix}.}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\references{\url{https://github.com/dcomtois/summarytools}}
\examples{
  data(iris)
  freq(iris$Species)
  freq(iris$Species, style="rmarkdown")
}
\keyword{ frequencies }
\keyword{ frequency }
