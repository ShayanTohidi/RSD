% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASSD_THS.R
\name{assd.ths.test}
\alias{assd.ths.test}
\title{Compares random prospects by ASSD-THS}
\usage{
assd.ths.test(sd.obj)
}
\arguments{
\item{sd.obj}{StochasticDominance object.}
}
\value{
A list, including all the calculation details.
}
\description{
It uses the areas between the SSD functions, and the
expected values of the prospects to compare them based on the ASSD-THS rule.
If the violation area ratio is less than 0.5 for a prospect, and its expected
value is larger, it dominates the other by ASSD-THS.
}
\details{
epsilon shows the ratio of the violation. Smaller epsilon means more
decision-makers agree with the result.

The returned list has six elements: `winner` indicates the dominant prospect
index. It will be zero if neither dominates the other. `epsilon` is the ratio
of violated area to the total area between the SSDs. `area` is a vector, where
the values show the area between the SSDs correspond to each segment.
`total.area` is the total area between the SSDs. `positive.area` is the
amount of area where the `area` vector is positive, meaning the `ssd1` is
larger than `ssds2`. `negative.area` is like `positive.area` for negative
values.

If neither distribution dominates the other by ASSD-THS, the `winner` output will
be zero, and it happens only when the distribution with a higher expected
value has the `epsilon` which is larger than 0.5.
}
\seealso{
[expected.values(), area.btwn.ssd.calc()] for more details.
}
