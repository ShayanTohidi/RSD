% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSD.R
\name{ssd.test}
\alias{ssd.test}
\title{Compares random prospects by SSD}
\usage{
ssd.test(sd.obj)
}
\arguments{
\item{sd.obj}{StochasticDominance object.}
}
\value{
A number. Indicating dominant prospect index.
}
\description{
It compares two random prospects by the second-order stochastic dominance (SSD).
}
\details{
A prospect dominates when its SSD is below the other one. It means that all
 element of the SSD vector must be equal or smaller, and at least one element
 should be smaller for the dominant prospect.

If neither prospect dominates the other by SSD, it returns 0. It means the
SSDs intersect each other.
}
\examples{
sd = createStochasticDominance(outcome1 = c(1,4,7),
                               outcome2 = c(2,3,5),
                               prob1 = c(1/3,1/3,1/3),
                               prob2 = c(1/6,1/6,2/3))
ssd.test(sd)

}
\seealso{
[ssd.calc()] for the calculation.
}
