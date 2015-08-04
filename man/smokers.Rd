\name{smokers}
\alias{smokers}
\docType{data}
\title{
smokers sample data
}
\description{
This is merely a sample dataframe to allow users to experiment with summarytools. A French equivalent called 'fumeurs' is also available.
}
\usage{data("smokers")}
\format{
  A data frame with 300 observations on the following 5 variables.
  \describe{
    \item{\code{gender}}{Factor with 2 levels, 'F' and 'M'}
    \item{\code{age}}{Factor with 5 levels (age group categories)}
    \item{\code{smoker}}{Factor with 2 levels (smoking status)}
    \item{\code{diseased}}{Factor with 2 levels, 'diseased' and 'healthy'}
    \item{\code{bmi}}{Numerical vector (Body Mass Index)}
  }
}
\examples{
data(smokers)
descr(smokers$bmi)
view(dfSummary(smokers))
}
\keyword{datasets}
