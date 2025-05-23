% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranef.r
\name{ranef2long}
\alias{ranef2long}
\alias{.re.data}
\title{Convert ranef() output to long format}
\usage{
ranef2long(x, idvar)

.re.data(d, i, idvar)
}
\arguments{
\item{x}{A \code{brmsfit} object}

\item{idvar}{A character string specifying the grouping variable name for the 
random effects.}

\item{d}{A \code{ranef} object}

\item{i}{an integer, which random effect to pull out}
}
\value{
A data.table object with the random effects in long format.
}
\description{
Convert ranef() output to long format
}
\keyword{internal}
