\name{dfSummary}
\alias{dfSummary}
\title{Dataframe Summary}
\description{Summary of a dataframe consisting of: variable names and labels, factor levels, frequencies or numerical summary statistics, and valid/missing observations information. Optionally uses package 'pander' to display pretty tables.}
\usage{
dfSummary(x, echo=TRUE, style="grid", justify="left",
          max.distinct.values=10, str.distinct.values="distinct values",
          trim.strings=FALSE, max.string.width=15, round.digits=2,
          file=NA, display.labels=FALSE, ...)
}
\arguments{
  \item{x}{A dataframe.}
  \item{echo}{If TRUE (default), pander tables will be displayed and resulting dataframe will be returned silently.}
  \item{style}{The style to be used in pander table. Defaults to "grid".}
  \item{justify}{Pander argument. Defaults to "left".}
  \item{max.distinct.values}{The maximum number of items to be displayed in the frequency cell. If variable has more distinct values, no frequency will be shown (only a message stating the number of distinct values).}
  \item{str.distinct.values}{For internationalization; defaults to "distinct values".}
  \item{trim.strings}{For character variables, remove any white space at the beginning or end of the string. This will impact the frequencies so interpret the frequency tables accordingly. Defaults to FALSE.}
  \item{max.string.width}{Limits the number of characters to display in the frequency tables. Defaults to 15.}
  \item{round.digits}{Number of digits for rounding (used in numerical stats and in freq tables).}
  \item{file}{The text file to be written to disk. Defaults to NA.}
  \item{display.labels}{If TRUE,a column containing Hmisc-type labels will be displayed. Defaults to FALSE.}
  \item{\dots}{Additional arguments passed to pander().}
}
%\details{}
\value{A dataframe containing as many rows as there are columns in x.}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\seealso{\code{\link{summary.data.frame}}}
\examples{
  dfSummary(iris)
}
\keyword{ data.frame }
\keyword{ summary }
\keyword{ numerical summary }

