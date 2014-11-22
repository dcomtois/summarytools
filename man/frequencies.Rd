\name{frequencies}
\alias{frequencies}
\title{Frequency Tables for Discrete Data and Factors.}
\description{Displays frequencies as well as valid/missing obs. information. Optionally uses package 'pander' to display pretty tables.}
\usage{
frequencies(x, round.digits=2, echo=TRUE, style="grid", justify="right",
            plain.ascii=TRUE, display.label=FALSE, ...)
}
\arguments{
  \item{x}{Atomic object of discrete (categorical) data.}
  \item{round.digits}{Number of sig. digits to keep. Defaults to 2.}
  \item{echo}{If TRUE, pander tables will be displayed. Defaults to TRUE.}
  \item{style}{Style of pander tables, one of "multiline", "grid" (default), "simple" and "rmarkdown".}
  \item{justify}{Pander argument. Defaults to "right".}
  \item{plain.ascii}{Pander argument. Defaults to TRUE.}
  \item{display.label}{If TRUE, Hmisc-type labels will be displayed. Defaults to FALSE.}
  \item{\dots}{Additional arguments passed to pander().}
}
%\details{%%  ~~ If necessary, more details than the description above ~~}
\value{A list containing the variable's name, label if applicable, frequency table and information on valid vs <NA> observations.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%\references{%% ~put references to the literature/web site here ~}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
%\note{%%  ~~further notes~~}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{
colors <- c("blue","red","blue","yellow",NA)
frequencies(colors)
frequencies(colors, echo=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ frequencies }
