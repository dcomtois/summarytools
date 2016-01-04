\name{dfSummary}
\alias{dfSummary}
\title{Dataframe Summary}
\description{Summary of a dataframe consisting of: variable names and labels, factor levels, frequencies or numerical summary statistics, and valid/missing observations information.}
\usage{
dfSummary(x, style="multiline", justify="left",
          varnumbers=FALSE, max.distinct.values=10,
          trim.strings=FALSE, max.string.width=15,
          round.digits=2, split.cells=40,
          display.labels=FALSE, file=NA, append=FALSE,
          escape.pipe=FALSE, ...)
}
\arguments{
  \item{x}{A dataframe.}
  \item{style}{The style to be used in pander table. Defaults to \dQuote{multiline}. Alternatively, use \dQuote{grid}; other styles (\dQuote{simple} and \dQuote{rmarkdown} are not supported.}
  \item{justify}{\pkg{pander} argument. Defaults to \dQuote{left}.}
  \item{varnumbers}{Should a column with column number be included? Defaults to {FALSE}.}
  \item{max.distinct.values}{The maximum number of items to be displayed in the frequency cell. If variable has more distinct values, no frequency will be shown (only a message stating the number of distinct values).}
  \item{trim.strings}{For character variables, remove any white space at the beginning or end of the string. This will impact the frequencies so interpret the frequencies cell accordingly. Defaults to \code{FALSE}.}
  \item{max.string.width}{Limits the number of characters to display in the frequency tables. Defaults to \code{15}.}
  \item{round.digits}{Number of digits for rounding (used in numerical stats and in frequency tables).}
  \item{split.cells}{\pkg{pander} argument. Number of characters allowed on a line before splitting the cell. Defaults to 40.}
  \item{display.labels}{If \code{TRUE}, variable (as defined with \pkg{rapportools} or \pkg{Hmisc}'s \code{label} function) labels will be displayed. Defaults to FALSE.}
  \item{file}{The text file to be written to disk. Defaults to \code{NA}.}
  \item{append}{When \dQuote{file} argument is supplied, this indicates whether to append output to existing file (\code{TRUE}) or to overwrite any existing file (\code{FALSE}, default). If \code{TRUE} and no file exists, a new file will be created.}
    \item{escape.pipe}{Only useful when \code{style='grid'} and \code{file} argument is not \code{NA}, in which case it will escape the pipe character (|) to allow Pandoc to correctly convert multiline cells.}
  \item{\dots}{Additional arguments passed to \pkg{pander}.}
}

\value{A dataframe containing as many rows as there are columns in \code{x}.}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\references{\url{https://github.com/dcomtois/summarytools}}
\seealso{\code{\link{summary.data.frame}}}
\examples{
  \dontrun{view(dfSummary(iris))}
  data(tobacco)
  dfSummary(tobacco)
}
\keyword{ data.frame }
\keyword{ summary }
\keyword{ numerical summary }

