% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DaviesLaker.R
\name{DaviesLaker}
\alias{DaviesLaker}
\title{calculates total attribution effects using Davies and Laker smoothing}
\usage{
DaviesLaker(
  Rp,
  wp,
  Rb,
  wb,
  annualization = "none",
  annualization_scale = NA,
  impute_returns = TRUE
)
}
\arguments{
\item{Rp}{xts of portfolio returns}

\item{wp}{xts of portfolio weights}

\item{Rb}{xts of benchmark returns}

\item{wb}{xts of benchmark weights}

\item{annualization}{Used to select the annualization method for multi-period returns, excess returns,
and attribution effects. May be any of:
\itemize{\item none - no annualized numbers are returned
\item standard 
\item proportional}
The default is 'none'. See vignette on annualization for detailed descriptions of the supported methods}

\item{annualization_scale}{Used to define a custom scale factor to use when annualizing returns. This is only
applicable if the periodicity of the data is daily and otherwise this parameter is ignored. May be any of:
\itemize{\item NA - use built-in scale factor
\item <numeric> - any numeric value
\item "monthly" - count the periods in terms of months (including fractional months) to determine scale}}

\item{impute_returns}{TRUE/FALSE, whether to impute returns from the benchmark, when the weight of the asset
is zero in the portfolio & vice-versa. These imputed returns are only used in the calculations
for attributions effects but not for cumulative asset returns (annualized or otherwise). Defaults to TRUE.}
}
\value{
This function returns the data.frame with original attribution 
effects and total attribution effects over multiple periods
}
\description{
Calculates total attribution effects over multiple periods using 
Davies and Laker linking method. Used internally by the 
\code{\link{Attribution}} function. Arithmetic attribution effects do not 
naturally link over time. This function uses Davies and Laker linking method
to compute total attribution effects and uses Brinson, Hood & Beebower approach 
to defining allocation, selection & interaction effects. 
Arithmetic excess returns are decomposed as follows:
\deqn{R_{p} - R_{b} = Allocation + Selection + Interaction}{Rp - Rb = 
Allocation + Selection + Interaction}
\deqn{Allocation = \prod^{T}_{t=1}(1+bs_{t})-\prod^{T}_{t=1}(1+R_{bt})}
\deqn{Selection = \prod^{T}_{t=1}(1+rs_{t})-\prod^{T}_{t=1}(1+R_{bt})}
\deqn{Interaction = \prod^{T}_{t=1}(1+R_{pt})-\prod^{T}_{t=1}(1+rs_{t})-
\prod^{T}_{t=1}(1+bs_{t})+\prod^{T}_{t=1}(1+R_{bt})}
\eqn{R_{pi}}{Rpi} - portfolio returns at period \eqn{i}, 
\eqn{R_{bi}}{Rbi} - benchmark returns at period \eqn{i},
\eqn{rs_{i}}{rsi} - selection notional fund returns at period \eqn{i}, 
\eqn{bs_{i}}{bsi} - allocation notional fund returns at period \eqn{i}, 
\eqn{T} - number of periods
}
\examples{

data(attrib)
DaviesLaker(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], wb = attrib.weights[2, ])

}
\references{
Bacon, C. \emph{Practical Portfolio Performance Measurement and
Attribution}. Wiley. 2004. p. 201-204 \cr Davies, O. and Laker, D. (2001) 
\emph{Multiple-period performance attribution using the Brinson model}. 
Journal of Performance Measurement. Fall. p. 12-22 \cr
}
\seealso{
\code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
\code{\link{Grap}} \cr \code{\link{Carino}} \cr
\code{\link{Attribution.geometric}} \cr \code{\link{Frongello}}
}
\author{
Andrii Babii
}
\keyword{Davies}
\keyword{Laker}
\keyword{and}
\keyword{arithmetic}
\keyword{attribution,}
\keyword{linking}
