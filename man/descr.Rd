\name{descr}
\alias{descr}
\title{Descriptive Univariate Statistics for Numerical Data}
\description{Calculates mean, standard deviation, min, max, median, mad, IQR (non-weighted only), CV, skewness (non-weighted only), SE.skewness (non-weighted only), and kurtosis (non-weighted only)}
\usage{
descr(x, na.rm=TRUE, round.digits=2, style="simple", justify="right",
      plain.ascii=TRUE, file=NA, append=FALSE, transpose=FALSE,
      escape.pipe=FALSE, weights=NA, rescale.weights=FALSE, ...)}
\arguments{
  \item{x}{Numerical vector.}
  \item{na.rm}{argument to be passed to statistical functions. Defaults to \code{TRUE}.}
  \item{round.digits}{Number of sig. digits to keep. Defaults to \code{2}.}
  \item{style}{Style of pander tables; one of \dQuote{simple} (default), \dQuote{grid} and \dQuote{rmarkdown}.}
  \item{justify}{\pkg{pander} argument. Defaults to \dQuote{right}.}
  \item{plain.ascii}{\pkg{pander} argument; when results are displayed in console, this avoids having \emph{markdown} markup characters in the tables. Defaults to TRUE, except when \code{style='rmarkdown'}, in which case it is set to FALSE (unless explicitly defined as TRUE).}
  \item{file}{File name to write output to. Defaults to \code{NA}. To append output to an existing text file, use \code{append=TRUE}.}
  \item{append}{When \dQuote{file} argument is supplied, this indicates whether to append output to existing file (\code{TRUE}) or to overwrite any existing file (\code{FALSE}, default). If \code{TRUE} and no file exists, a new file will be created.}
  \item{transpose}{Makes variables appears as columns, and stats as rows. Defaults to \code{FALSE}.}
  \item{escape.pipe}{Only useful when \code{style='grid'} and \code{file} argument is not \code{NA}, in which case it will escape the pipe character (|) in order for \emph{Pandoc} to correctly display table cells.}
  \item{weights}{Vector of weights, having same length as x}
  \item{rescale.weights}{Logical parameter. When \code{TRUE}, the function makes the sum of weights equal to the length of x, so that the total N remains unchanged. \code{FALSE} by default.}
  \item{\dots}{Additional arguments passed to \code{pander::pander()}.}
}
\details{
The default \code{plain.ascii=TRUE} option is there to make results appear clean in the console, but creates problems when generating markdown documents. For this reason, whenever \code{style='rmarkdown'}, the option is set to FALSE, unless \code{plain.ascii=TRUE} is stated explicitly. On the other hand, with \code{style='grid'}, if the intent is to produce markdown text, then the user needs to explicitly set \code{plain.ascii=FALSE}. 
}
\value{A list with 2 matrix elements: the stats themselves, and a summary of valid/missing cases. The list also has attributes used by \pkg{summarytool}'s print method.}
\references{\url{https://github.com/dcomtois/summarytools}}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\examples{
data(exams)
descr(exams)
descr(exams, transpose=TRUE)
data(tobacco)
with(tobacco, by(age, smoker, descr))
}
\keyword{univar}
