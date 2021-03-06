% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling.R
\name{sample_rvtable}
\alias{sample_rvtable}
\title{Sample A Random Variable Table}
\usage{
sample_rvtable(x, resample = FALSE, n = 10000, interp = TRUE,
  n.interp = 1e+05, decimals = NULL, density.args)
}
\arguments{
\item{x}{an \code{rvtable} object.}

\item{resample}{if \code{x} is a sample-based rvtable, return \code{x} unchanged if \code{resample=FALSE}. Otherwise resample from the samples in the Val column. Ignored if rvtable is distribution-based.}

\item{n}{bootstrap sample size.}

\item{interp}{linearly interpolate between observed values of Val before sampling if Val is continuous. Ignored for discrete random variables.}

\item{n.interp}{length of sequence of interpolated sampling points if. Ignored for discrete random variables or \code{interp=FALSE}.}

\item{decimals}{number of decimal places for rounding samples. Ignored for discrete random variables.}

\item{density.args}{optional arguments passed to \code{density}. If supplied, overrides the \code{density.args} attribute of \code{x}.}
}
\value{
an \code{rvtable} object where the Val column represents samples and the Prob column is dropped.
}
\description{
Bootstrap resampling with replacement from a distribution of a random variable stored in an rvtable.
}
\details{
Resample an rvtable yielding a new rvtable.
This is used to convert from a distribution-based rvtable, which has Val and Prob columns representing a distribution to one with only a Val column representing samples from a distribution.
This is often used preceding plotting data in an rvtable so that samples may be passed to plot code rather than a representation of a distribution using values and associated probabilities.
This function can also take a sample-based rvtable, in which case it will resample it if \code{resample=TRUE}.
If is sample-based rvtable is passed and the random variable is continuous, the generation of a new continuous density prior to resampling can be controlled via \code{density.args}.
All rvtable objects are either distribution-based or sample-based.
}
\examples{
x <- rvtable(rnorm(1000))
sample_rvtable(x, n=10)
x <- rvtable(sample(1:100, 50), discrete=TRUE)
y <- sample_rvtable(x, n=10)
sample_rvtable(y, n=8, resample=TRUE)
}
