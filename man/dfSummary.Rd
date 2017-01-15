\name{dfSummary}
\alias{dfSummary}
\title{Dataframe Summary}
\description{Summary of a dataframe consisting of: variable names and labels, factor levels, frequencies or numerical summary statistics, and valid/missing observations information.}
\usage{
dfSummary(x, round.digits=2, style="multiline", justify="left",
          plain.ascii=TRUE, varnumbers=FALSE, 
          display.labels=any(sapply(X = x, FUN = Hmisc::label) != ""),
          max.distinct.values=10, trim.strings=FALSE, max.string.width=15,
          split.cells=40, ...)
}
\arguments{
  \item{x}{A dataframe.}
  \item{round.digits}{Number of digits for rounding (used in numerical stats and in frequency tables).}
  \item{style}{The style to be used in pander table. Defaults to \dQuote{multiline}. Alternatively, use \dQuote{grid}. Style \dQuote{simple} is not supported, and \dQuote{rmarkdown} will fall back on \dQuote{multiline}.}
  \item{justify}{\pkg{pander} argument. Defaults to \dQuote{left}.}
  \item{plain.ascii}{\pkg{pander} argument; when results are displayed in console, this avoids having \emph{markdown} markup characters in the tables. Defaults to TRUE, so make sure to change it to FALSE in markdown documents.}
  \item{varnumbers}{Should a column with column number be included? Defaults to {FALSE}.}
  \item{display.labels}{If \code{TRUE}, variable labels (as defined with \pkg{rapportools} or \pkg{Hmisc}'s \code{label} function) will be displayed. By default, labels are shown if any column has a defined label.}
  \item{max.distinct.values}{The maximum number of items to be displayed in the frequency cell. If variable has more distinct values, no frequency will be shown (only a message stating the number of distinct values).}
  \item{trim.strings}{For character variables, remove any white space at the beginning or end of the string. This will impact the frequencies so interpret the frequencies cell accordingly. Defaults to \code{FALSE}.}
  \item{max.string.width}{Limits the number of characters to display in the frequency tables. Defaults to \code{15}.}
  \item{split.cells}{\pkg{pander} argument. Number of characters allowed on a line before splitting the cell. Defaults to 40.}
  \item{\dots}{Additional arguments passed to \pkg{pander}.}
}

\value{A dataframe containing as many rows as there are columns in \code{x}.}
\references{\url{https://github.com/dcomtois/summarytools}}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\seealso{\code{\link{summary.data.frame}}}
\examples{
  \dontrun{view(dfSummary(iris))}
  data(tobacco)
  dfSummary(tobacco)
}
\keyword{ univar }
\keyword{ attribute }
\keyword{ classes }
