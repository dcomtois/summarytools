\name{tobacco}
\alias{tobacco}
\docType{data}
\title{
tobacco sample data
}
\description{
This is merely a sample dataframe to allow users to experiment with summarytools. A French equivalent called 'tabagisme' is also available.
}
\usage{data("tobacco")}
\format{
  A data frame with 300 observations on the following 5 variables.
  \describe{
    \item{\code{gender}}{Factor with 2 levels, 'F' and 'M'}
    \item{\code{age}}{Numerical vector of ages}
    \item{\code{age.gr}}{Factor with 5 levels (age group categories)}
    \item{\code{BMI}}{Numerical vector of Body Mass Indexes}
    \item{\code{smoker}}{Factor with 2 levels, 'Non-smoker' and 'Smoker'}
    \item{\code{diseased}}{Factor with 2 levels, 'Healthy' and 'Diseased'}
  }
}
\examples{
data(tobacco)
freq(tobacco$gender)
}
\keyword{datasets}
