\name{unistats}
\alias{unistats}
\title{Univariate Statistics for Numerical Data}
\description{Calculates mean, standard deviation, min, max, median, IQR, skewness, str.err skewness, and kurtosis. Optionally uses package 'pander' to display pretty tables.}
\usage{
unistats(x, na.rm=TRUE, round.digits=2, echo=TRUE, style="grid",
         justify="right", plain.ascii=TRUE, display.label=FALSE, ...)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Numerical vector.}
  \item{na.rm}{argument to be passed to statistical functions. Defaults to TRUE.}
  \item{round.digits}{Number of sig. digits to keep. Defaults to 2.}
  \item{echo}{If TRUE, pander tables will be displayed. Defaults to TRUE.}
  \item{style}{Style of pander tables. Defaults to "grid".}
  \item{justify}{Pander argument. Defaults to "right".}
  \item{plain.ascii}{Pander argument. Defaults to TRUE.}
  \item{display.label}{If TRUE, Hmisc-type labels will be displayed. Defaults to FALSE.}
  \item{\dots}{Additional arguments passed to pander().}
}

%\details{%%  ~~ If necessary, more details than the description above ~~}
\value{A list containing the variable's name, label if applicable, table of statistics and table of counts (valid vs <NA> observations).}
%\references{%% ~put references to the literature/web site here ~}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
%\note{%%  ~~further notes~~}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{
data(cars)
unistats(cars$speed)
unistats(cars$speed, echo=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univariate }


