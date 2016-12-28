\name{cleartmp}
\alias{cleartmp}
\title{
  Delete Temporary HTML Files
}
\description{
Deletes temporary files created when using \code{method='browser'}, \code{method='viewer'}, or \code{view()} function.
}
\usage{
cleartmp(all = FALSE, silent = FALSE)
}
\arguments{
  \item{all}{
logical. When TRUE, all temporary summarytools files will be deleted. When FALSE (default), only the latest will.
}
  \item{silent}{
Hide confirmation messages (FALSE by default).
}
}
\value{
\code{NULL}
}
\author{
Dominic Comtois <dominic.comtois@gmail.com>
}
\keyword{ IO } % use one of  RShowDoc("KEYWORDS")
\keyword{ Utilities }