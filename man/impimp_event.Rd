% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imimp_event.R
\name{impimp_event}
\alias{impimp_event}
\alias{is.impimp_event}
\title{Imprecise Events}
\usage{
impimp_event(..., isEventList = FALSE)

is.impimp_event(x)
}
\arguments{
\item{...}{these arguments are of the form \code{varname = value}.
For each component the varname should be a variable name from the
underlying data.frame and value a vector of possible outcomes;
may also be of length one.}

\item{isEventList}{logical; if \code{TRUE} and \code{...}
contains only a list object, this list is treated as if it was
an event specification, see \code{...}. Since this argument
follows \code{...} its name cannot be abbreviated.}

\item{x}{object to test for class \code{"impimp_event"}}
}
\value{
A object of class \code{"impimp_event"} as a list of lists,
where each sublist contains one point in the cartesian product,
spanned by the input values and variables.
}
\description{
Helper function to allow the generation of a set of
events as cartesian product.
}
\note{
There is no plausibility check on whether the supplied varnames
are actually contained in the data.frame for which the
resulting impimp_event object is later used for.
}
\examples{
## underlying data set: x1: 1:6, x2: 1:10

## subspace, requiring: x1 == 1 & ((x2 == 1 ) | (x2 == 2))
impimp_event(x1 = 1, x2 = c(1,2))

## subsapce containing all points whitin the Cartesian
## product of (x1 =) {1,2,3,6} x {5,8} (= x2)
# via  ... argument
impimp_event(x1 = c(1:3,6), x2 = c(5,8))
# via EVENTLIST
impimp_event(list(x1 = c(1:3,6), x2 = c(5,8)),
             isEventList = TRUE)

}
\seealso{
\code{\link{impest}}, \code{\link{impestcond}}
}
\keyword{robust}
