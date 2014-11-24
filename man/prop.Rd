\name{prop}
\alias{prop}
\alias{properties}
\title{Dataframe Attributes Along With Individual Column Attributes}
\description{Generates a table containing variable information: type, class and other attributes, label and first observation. Optionally uses package 'pander' to display pretty tables.}
\usage{
properties(x, echo=TRUE, style="grid", justify="left",
           plain.ascii=TRUE, display.labels=FALSE, ...)
}
\arguments{
  \item{x}{A dataframe}
  \item{echo}{If TRUE (default), pander tables will be displayed and resulting dataframe will be returned silently.}
  \item{style}{Pander argument. Defaults to "grid".}
  \item{justify}{Pander argument. Defaults to "left".}
  \item{plain.ascii}{Pander argument. Defaults to TRUE.}
  \item{display.labels}{If TRUE, a column containing Hmisc-type labels will be added. Defaults to FALSE.}
  \item{\dots}{Additional arguments to be passed to pander.}
}

\value{A dataframe containing as many rows as there are columns in x.}
\author{Dominic Comtois <dominic.comtois@gmail.com>}
 \examples{
  properties(iris,echo=TRUE)
}
\keyword{ data.frame }
\keyword{ summary }
