\name{what.is}
\alias{what.is}
\title{Obtain most common information on data structures.}
\description{
  Takes any object as argument and returns (1) a dataframe with the class(es), type, mode and storage mode of the object as well as the dim, length and object.size; (2) a character vector of all the identifier functions (starting with "is.") that yield TRUE with the provided object; (3) a named character vector giving all attributes (for instance, "names", "row.names", "class", "dim", and so on) along with their lengths.}
\usage{
  what.is(x, show.all = FALSE, ignore.size.warn = FALSE)
}
\arguments{
  \item{x}{Any object.}
  \item{show.all}{When \code{TRUE}, all logical results from the \dQuote{is.} identifier functions will be displayed, accompanied by a warning when only the result applies only to the first element in the structure. \code{FALSE} by default.}
  \item{ignore.size.warn}{Set to \code{TRUE} to force execution of the function for large (>20 K bytes) objects. Defaults to \code{FALSE}.}
}

\details{The function is an attempt to centralize the most common \dQuote{macro-level} functions that describe the data at hand. Instead of calling, in turn, \code{class}, \code{typeof}, \code{dim}, and so on, a single call to \code{what.is} will readily give all that information.}

\value{
    A list with 3 named items: \dQuote{obj.properties} containing class, typeof, mode, storage.mode, dim and length; \dQuote{identifiers} containing the \dQuote{is.} identifier functions that yield \code{TRUE}; \dQuote{attributes} containing the names and lengths of attributes; optionally, a 4th item with all the identifier test results (when argument \code{show.all=TRUE})}

\author{
  Dominic Comtois <dominic.comtois@gmail.com>
}

\examples{
  what.is(1)
  what.is(NaN)
  what.is(iris3)
}

\keyword{ attributes }
\keyword{ is }
\keyword{ class }
\keyword{ type }
\keyword{ mode }
\keyword{ storage.mode }
\keyword{ length }
\keyword{ dim }
