\name{ctable}
\alias{ctable}
\title{
Cross-Tabulations for Two Categorical Variables
}
\description{
Produces a cross-tabulation with either row, column, or total proportions, as well as marginal sums. 
}
\usage{
ctable(x, y, round.digits = 1, style = "simple", justify = "right", prop = "t", 
       useNA = "ifany", totals = TRUE, plain.ascii = TRUE, dnn = c(substitute(x),
       substitute(y)), file = "", append = FALSE, escape.pipe=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{First categorical variable - its values will appear in rows}
  \item{y}{Second categorical variable - its values will appear in columns}
  \item{round.digits}{Number of sig. digits to keep. Defaults to \code{1}.}
  \item{style}{Style of pander tables; one of \dQuote{simple} (default), \dQuote{grid} and \dQuote{rmarkdown}.}
  \item{justify}{\pkg{pander} argument. Defaults to \dQuote{right}.}
  \item{prop}{What proportions to display; one of \dQuote{t} for total (default), \dQuote{r} for rows  and \dQuote{c} for columns. To omit proportions, use \code{NA}}
  \item{useNA}{Used by \code{base::table} -- One of \dQuote{no}, \dQuote{ifany}  (default), \dQuote{always}.}
  \item{totals}{Display row and column totals (defaults to \code{TRUE})}
  \item{plain.ascii}{\pkg{pander} argument; when results are displayed in console, this avoids having \emph{markdown} markup characters in the tables. Defaults to TRUE, except when \code{style='rmarkdown'}, in which case it is set to FALSE (unless explicitly defined as TRUE).}
  \item{dnn}{Two strings corresponding to the names of x and y variables. By default, the supplied arguments for x and y are used, as strings.}
  \item{file}{File name to write output to. Defaults to \code{""}. To append output to an existing text file, use \code{append=TRUE}.}
  \item{append}{When \dQuote{file} argument is supplied, this indicates whether to append output to existing file (\code{TRUE}) or to overwrite any existing file (\code{FALSE}, default). If \code{TRUE} and no file exists, a new file will be created.}
  \item{escape.pipe}{Only useful when \code{style='grid'} and \code{file} argument is not \code{NA}, in which case it will escape the pipe character (|) in order for \emph{Pandoc} to correctly display table cells.}
  \item{\dots}{Additional arguments passed to \code{pander::pander()}.}
}

\references{\url{https://github.com/dcomtois/summarytools}}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
\examples{
data("tobacco")
ctable(tobacco$gender, tobacco$smoker, prop="r")
}
\keyword{ classes category }
