#' Simple three category portfolio & benchmark example for use in vignettes
#' 
#' A simple three category portfolio & benchmark test data set that contains portfolio & benchmark weights and returns.
"simple_portf_1"

#' Simple portfolio & benchmark example, categorized into sectors, for use in vignettes
#' 
#' A simple portfolio & benchmark test data set, categorized into sectors, that contains portfolio & benchmark weights and returns for each category.
"simple_portf_2"

#' Simple multi-currency portfolio & benchmark example, categorized into sectors, for use in vignettes
#' 
#' A simple portfolio & benchmark test data set, categorized into sectors, that contains portfolio & benchmark weights and 
#' returns for each category in both local and base currency terms.
"simple_multi_curr_portf_1"

#' Simple multi-currency portfolio & benchmark example, categorized into regions, for use in vignettes
#' 
#' A simple portfolio & benchmark test data set, categorized into regions, that contains portfolio & benchmark weights and 
#' returns for each category, portfolio & benchmark forward contract weights and spot ares and forward rates for the currencies 
#' corresponding to each region.
"simple_multi_curr_portf_2"

#' Simple multi-currency spot and forward rates for each currency in each region in portfolio & benchmark example, for use in vignettes
#' 
#' Spot and forward rates for a simple portfolio & benchmark test data set, categorized into regions, for the currencies 
#' corresponding to each region.
"simple_multi_curr_portf_2_spot_forward_rates"

#' First multi-period portfolio & benchmark example with security-level data, for use in vignettes
#' 
#' A test data set containing portfolio & benchmark returns for a large number of securities over several periods. 
#' For each period we have portfolio & benchmark weights and returns for each security.
#' In addition, the list also contains the periodic total portfolio & benchmark returns.
#' @format A list with 6 members:
#' \describe{
#'   \item{Rp}{xts object with portfolio returns for each security and period}
#'   \item{Rb}{xts object with benchmark returns for each security and period}
#'   \item{wp}{xts object with portfolio weights for each security and period}
#'   \item{wb}{xts object with benchmark weights for each security and period}
#'   \item{periodicPortReturns}{xts object with total portfolio return for period}
#'   \item{periodicBmkReturns}{xts object with total benchmark return for each period}
#' }
"multi_period_portf_1"

#' Second multi-period portfolio & benchmark example categorized into regions, for use in vignettes
#' 
#' A simple multi-peroid portfolio & benchmark test data set, categorized into regions, that contains 
#' portfolio & benchmark weights and returns for four periods
#' 
#' @format A list with 4 members:
#' \describe{
#'   \item{Rp}{xts object with portfolio returns for each region and period}
#'   \item{Rb}{xts object with benchmark returns for each region and period}
#'   \item{wp}{xts object with portfolio weights for each region and period}
#'   \item{wb}{xts object with benchmark weights for each region and period}
#' }
"multi_period_portf_2"

#' Third multi-period portfolio & benchmark example with security-level data, for use in vignettes
#' 
#' A test data set containing portfolio & benchmark returns for a large number of securities over several periods. 
#' For each period we have portfolio & benchmark weights and returns for each security.
#' In addition, the list also contains the periodic total portfolio & benchmark returns.
#' @format A list with 6 members:
#' \describe{
#'   \item{Rp}{xts object with portfolio returns for each security and period}
#'   \item{Rb}{xts object with benchmark returns for each security and period}
#'   \item{wp}{xts object with portfolio weights for each security and period}
#'   \item{wb}{xts object with benchmark weights for each security and period}
#'   \item{periodicPortReturns}{xts object with total portfolio return for period}
#'   \item{periodicBmkReturns}{xts object with total benchmark return for each period}
#' }
"multi_period_portf_3"
