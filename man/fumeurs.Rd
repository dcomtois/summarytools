\name{fumeurs}
\alias{fumeurs}
\docType{data}
\title{
  fumeurs sample data (French)
}
\description{
This is merely a sample dataframe to allow users to experiment with summarytools. An english equivalent called 'smokers' is also available.
}
\usage{data("fumeurs")}
\format{
  A data frame with 300 observations on the following 5 variables.
  \describe{
    \item{\code{sexe}}{Factor with 2 levels, 'F' and 'H'}
    \item{\code{age}}{Factor with 5 levels (age categories)}
    \item{\code{fumeur}}{Factor with 2 levels (smoking status)}
    \item{\code{malade}}{Factor with 2 levels (diseased)}
    \item{\code{imc}}{Numerical vector (indice de masse corporelle / Body Mass Index)}
  }
}
\examples{
data(fumeurs)
freq(fumeurs$sexe)
}
\keyword{datasets}
