% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coxianAic.R
\name{coxianAic}
\alias{coxianAic}
\title{Akaike Information Criterion for Coxian Phase Type Distribution}
\usage{
coxianAic(data, lambda, mu)
}
\arguments{
\item{data}{A numeric vector of observed data points.}

\item{lambda}{A numeric vector of lambda values.}

\item{mu}{A numeric vector of mu values.}
}
\value{
The computed AIC value.
}
\description{
Computes the Akaike Information Criterion (AIC) for the fitted Coxian phase type distribution
given the data, lambda, and mu values.
}
\examples{
data_sample <- rexp(100, rate = 0.5)
lambda <- c(0.5, 0)
mu <- c(0.2, 0.4)
aic_value <- coxianAic(data_sample, lambda, mu)

}
