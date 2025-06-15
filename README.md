# RSD

`RSD` (**R** **S**tochastic **D**ominance) is designed and developed to calculate
Stochastic Dominance (SD) and Almost Stochastic Dominance (ASD) in general. In
more details, given two probability mass functions (PMF), this package helps with:

* Calculate the first and second order SD values (FSD and SSD, respectively),
* Calculate the first and second order ASD values (AFSD and ASSD, respectively), 
* Compare two probability distributions by these methods.

# Installation

Install the released version of `RSD` from [CRAN]() by:

```r
install.packages("RSD")
```

Or from [GitHub](https://github.com/ShayanTohidi/RSD.git) by:

```r
install.packages("pak")
pak::pkg_install("ShayanTohidi/RSD")
```

# Getting Started

`RSD` provides a function, `createStochasticDominance()` to create the object to be
used in all other functions of this package. This function requires two discrete
distributions.

# Functionalities
