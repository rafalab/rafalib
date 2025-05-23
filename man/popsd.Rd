% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/popvar.R
\name{popsd}
\alias{popsd}
\title{population standard deviation}
\usage{
popsd(x, na.rm = FALSE)
}
\arguments{
\item{x}{a numeric vector or an R object which is coercible to one by \code{as.vector(x, "numeric")}.}

\item{na.rm}{logical. Should missing values be removed?}
}
\description{
Returns the population standard deviation. Note that \code{\link{sd}} returns
the unbiased sample estimate of the population standard deviation. 
It simply multiplies the result of \code{\link{var}} by (n-1)  / n with n 
the population size and takes the square root.
}
