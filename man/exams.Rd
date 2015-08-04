\name{exams}
\alias{exams}
\docType{data}
\title{
exams sample data
}
\description{
This is merely a sample dataframe to allow users to experiment with summarytools. A French equivalent called 'examens' is also available.
}
\usage{data("exams")}
\format{
  A data frame with 25 observations on the following 6 variables.
  \describe{
    \item{\code{french}}{a numeric vector}
    \item{\code{english}}{a numeric vector}
    \item{\code{math}}{a numeric vector}
    \item{\code{geo}}{a numeric vector}
    \item{\code{history}}{a numeric vector}
    \item{\code{economics}}{a numeric vector}
  }
}
\examples{
data(exams)
descr(exams, transpose = TRUE)
view(descr(exams))
dfSummary(exams)
}
\keyword{datasets}
