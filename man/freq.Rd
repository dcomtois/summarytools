\name{freq}
\alias{freq}
\title{Frequency Tables for Factors and other Discrete Data}
\description{Displays weighted or unweighted frequencies including <NA> counts and proportions.}
\usage{
freq(x, round.digits=2, style="simple", justify="right",
     plain.ascii=TRUE, file=NA, append=FALSE,
     escape.pipe=FALSE, weights=NA,
     rescale.weights=FALSE, ...)
}
\arguments{
  \item{x}{Vector of discrete (categorical) data.}
  \item{round.digits}{Number of sig. digits to keep in output. Defaults to \code{2}.}
  \item{style}{Style of pander tables, one of \dQuote{simple} (default), \dQuote{grid} and \dQuote{rmarkdown}.}
  \item{justify}{\pkg{pander} argument. Defaults to "right".}
  \item{plain.ascii}{\pkg{pander} argument; when results are displayed in console, this avoids having \emph{markdown} markup characters int the tables. Defaults to TRUE.}
  \item{file}{File name to write output to. Defaults to \code{NA}. To append output to an existing text file, use \code{append=TRUE}.}
  \item{append}{When \dQuote{file} argument is supplied, this indicates whether to append output to existing file (\code{TRUE}) or to overwrite any existing file (\code{FALSE}, default). If \code{TRUE} and no file exists, a new file will be created.}
  \item{escape.pipe}{Only useful when \code{style='grid'} and \code{file} argument is not \code{NA}, in which case it will escape the pipe character (|) to allow Pandoc to correctly convert multiline cells.}
  \item{weights}{Vector of weights, having same length as x}
  \item{rescale.weights}{Logical parameter. When \code{TRUE}, the function makes the sum of weights equal to the length of x, so that the total N remains unchanged. \code{FALSE} by default.}
  \item{\dots}{Additional arguments passed to \pkg{pander}.}
}
\value{A frequency table of class \code{matrix} with attributes used by \pkg{summarytool}'s print method.}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\references{\url{https://github.com/dcomtois/summarytools}}
\examples{
data(tobacco)
freq(tobacco$gender)
freq(tobacco$gender, style="rmarkdown")
with(tobacco, by(smoker, gender, freq))
}
\keyword{ frequencies }
