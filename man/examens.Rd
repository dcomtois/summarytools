\name{examens}
\alias{examens}
\docType{data}
\title{
examens sample data (French)
}
\description{
This is merely a sample dataframe to allow users to experiment with summarytools. Am English  equivalent called 'exams' is also available.
}
\usage{data("examens")}
\format{
  A data frame with 25 observations on the following 6 variables.
  \describe{
    \item{\code{francais}}{a numeric vector}
    \item{\code{anglais}}{a numeric vector}
    \item{\code{maths}}{a numeric vector}
    \item{\code{geo}}{a numeric vector}
    \item{\code{histoire}}{a numeric vector}
    \item{\code{economie}}{a numeric vector}
  }
}
\examples{
data(examens)
descr(examens, transpose = TRUE)
view(descr(examens))
dfSummary(examens)
}
\keyword{datasets}
