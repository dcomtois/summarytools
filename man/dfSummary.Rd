\name{dfSummary}
\alias{dfSummary}
\title{Dataframe Summary}
\description{Summary of a dataframe consisting of: variable names and labels, factor levels, frequencies or numerical summary statistics, and valid/missing observations information.}
\usage{
dfSummary(x, style="multiline", justify="left",
          max.distinct.values=10, trim.strings=FALSE,
          max.string.width=15, round.digits=2,
          file=NA, display.labels=FALSE, ...)
}
\arguments{
  \item{x}{A dataframe.}
  \item{style}{The style to be used in pander table. Defaults to \dQuote{multiline}. Alternatively, use \dQuote{grid}; other styles (\dQuote{simple} and \dQuote{rmarkdown} are not supported.}
  \item{justify}{\pkg{pander} argument. Defaults to \dQuote{left}.}
  \item{max.distinct.values}{The maximum number of items to be displayed in the frequency cell. If variable has more distinct values, no frequency will be shown (only a message stating the number of distinct values).}
  \item{trim.strings}{For character variables, remove any white space at the beginning or end of the string. This will impact the frequencies so interpret the frequency tables accordingly. Defaults to \code{FALSE}.}
  \item{max.string.width}{Limits the number of characters to display in the frequency tables. Defaults to \code{15}.}
  \item{round.digits}{Number of digits for rounding (used in numerical stats and in frequency tables).}
  \item{file}{The text file to be written to disk. Defaults to \code{NA}.}
  \item{display.labels}{If \code{TRUE}, a column containing Hmisc / rapportools-type labels will be displayed. Defaults to FALSE.}
  \item{\dots}{Additional arguments passed to \pkg{pander}.}
}
%\details{}
\value{A dataframe containing as many rows as there are columns in \code{x}.}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\references{\url{https://github.com/dcomtois/summarytools}}
\seealso{\code{\link{summary.data.frame}}}
\examples{
  data(iris)
  dfSummary(iris)
}
\keyword{ data.frame }
\keyword{ summary }
\keyword{ numerical summary }

