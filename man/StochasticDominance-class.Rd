% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class.R
\docType{class}
\name{StochasticDominance-class}
\alias{StochasticDominance-class}
\title{StochasticDominance Class}
\description{
Represents two distributions (prospects) that are going to be compared using
Stochastic Dominance (SD).
}
\details{
It contains the input validation needed for comparing two prospects. For example,
having sorted `outcome`, each of `prob1` and `prob2` adds up to one, arguments
having the same lengths, and having matched probability, cumulative, and ssd
arguments.
}
\section{Slots}{

\describe{
\item{\code{outcome}}{Numeric vector. The combined outcome values in ascending order.}

\item{\code{prob1,prob2}}{Numeric vectors. Probabilities corresponding to the prospects.}

\item{\code{cdf1,cdf2}}{Numeric vectors. Cumulative values corresponding to the
prospects.}

\item{\code{ssd1,ssd2}}{Numeric vectors. SSD values corresponding to the prospects.}
}}

