% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{binary_eofdata}
\alias{binary_eofdata}
\title{Example Dataset for a Binary Outcome at End of Follow-Up}
\format{A data table with 17,500 rows and 7 variables:
\describe{
  \item{time}{Time index.}
  \item{id_num}{Unique identifier for each individual.}
  \item{cov1}{Binary time-varying covariate.}
  \item{cov2}{Continuous time-varying covariate.}
  \item{cov3}{Continuous baseline covariate. For each individual, the baseline values are repeated at each time point.}
  \item{treat}{Binary treatment variable.}
  \item{outcome}{Binary outcome of interest. Because this outcome is only defined at the end of follow-up, values of \code{NA} are given in all other time points.}
}}
\usage{
binary_eofdata
}
\description{
A dataset consisting of 17,500 observations on 2,500 individuals over 7 time points. Each row in the dataset corresponds to the record of one individual at one time point.
}
\keyword{datasets}
