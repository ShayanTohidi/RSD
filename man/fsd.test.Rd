% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FSD.R
\name{fsd.test}
\alias{fsd.test}
\title{Compares random prospects by FSD}
\usage{
fsd.test(sd.obj)
}
\arguments{
\item{sd.obj}{StochasticDominance object.}
}
\value{
An integer, indicating the index of the dominant prospect.
}
\description{
It compares two random prospects by the first-order stochastic dominance (FSD).
}
\details{
A prospect dominates when its CDF is below the other one. It means that all
element of the CDF vector must be equal or smaller, and at least one element
should be smaller for the dominant prospect.

If neither prospect dominates the other by FSD, it returns 0. It means that
the CDFs intersect each other.
}
\examples{
sd = createStochasticDominance(outcome1 = c(1,4,7),
                               outcome2 = c(2,3,5),
                               prob1 = c(1/3,1/3,1/3),
                               prob2 = c(1/6,1/6,2/3))
fsd.test(sd)

}
