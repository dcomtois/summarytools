\name{cleartmp}
\alias{cleartmp}
\title{
  Delete Temporary HTML Files
}
\description{
Deletes temporary files created when using \code{method='browser'}, \code{method='viewer'}, or \code{view()} function.
}
\usage{
cleartmp(what = "last", silent = FALSE)
}
\arguments{
  \item{what}{
Either "last" (default) or "all".
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