% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_data.R
\docType{data}
\name{multi_period_portf_1}
\alias{multi_period_portf_1}
\title{First multi-period portfolio & benchmark example with security-level data, for use in vignettes & unit tests}
\format{
A list with 6 members:
\describe{
  \item{Rp}{xts object with portfolio returns for each security and period}
  \item{Rb}{xts object with benchmark returns for each security and period}
  \item{wp}{xts object with portfolio weights for each security and period}
  \item{wb}{xts object with benchmark weights for each security and period}
  \item{periodicPortReturns}{xts object with total portfolio return for period}
  \item{periodicBmkReturns}{xts object with total benchmark return for each period}
}
}
\usage{
multi_period_portf_1
}
\description{
A test data set containing portfolio & benchmark returns for a large number of securities over several periods. 
For each period we have portfolio & benchmark weights and returns for each security.
In addition, the list also contains the periodic total portfolio & benchmark returns.
}
\keyword{datasets}
