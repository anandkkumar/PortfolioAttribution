library(PortfolioAttribution)
library(testthat)

context("Geometric Multi-currency Attribution Examples from Pratical Portfolio Performance Management and Attribution by Carl Bacon (2008)")
epsilon = 5e-5

cnames = c("UK equities", "Japanese equities", "US equities")
Wp = c(0.40, 0.30, 0.30)
Wb = c(0.40, 0.20, 0.40)

# Example using one-period time series data
Rp = xts(matrix(c(NA, NA, NA, 0.20, 0.047, 0.28),ncol=3,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rb = xts(matrix(c(NA, NA, NA, 0.10, 0.056, 0.296),ncol=3,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rpl = xts(matrix(c(NA, NA, NA, 0.20, -0.05, 0.06),ncol=3,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rbl = xts(matrix(c(NA, NA, NA, 0.10, -0.04, 0.08),ncol=3,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rbh = xts(matrix(c(NA, NA, NA, 0.10, -0.03, 0.10),ncol=3,byrow=TRUE,
                 dimnames=list(c("Rb", "Rb"),cnames)), 
          order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))


test_that("Multicurrency Geometric Example using data in Tables 6.7 and 6.8" , {
  attribution_results = Attribution.geometric(Rp, Wp, Rb, Wb, Rpl, Rbl, Rbh)
  
  expect_true(abs(attribution_results$Selection[2,"UK.equities"] - 0.038) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese.equities"] - (-0.0029)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US.equities"] - (-0.0057)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0295) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK.equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese.equities"] - (-0.0088)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US.equities"] - (-0.0034)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0122)) < epsilon)
  
  expect_true(abs(attribution_results$`Currency management`[2,"Hedging"] - (0.00095147)) < epsilon)
  expect_true(abs(attribution_results$`Currency management`[2,"Currency attribution"] - (-0.01040392)) < epsilon)
  
  expect_true(abs(attribution_results$`Excess returns`[2,"Geometric"] - 0.0073) < epsilon)
}
)
